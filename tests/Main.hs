{-# language BlockArguments #-}
{-# language QuasiQuotes #-}

module Main where

import Test.Hspec
import Data.String.QQ ( s )
import Weeder.New2
import GHC.Iface.Binary
import GHC.Types.Name.Cache ( initNameCache, NameCache )
import System.Process
import System.IO.Temp
import System.FilePath
import GHC.Types.Unique.Supply ( mkSplitUniqSupply )
import Data.IORef
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Binary
import GHC.Types.Name
import GHC.Unit.Module ( moduleName )
import GHC.Unit.Module.Name ( moduleNameString )
import qualified Data.Set as Set


main :: IO ()
main = hspec do
  describe "HIE analysis" do
    -- Test our interpretation of HIE trees
    describe "type classes" do
      it "detects the usage of type classes" do
        asts <- toHieAsts "TestModule"
          [s|
            module TestModule where

            is42 :: Int -> Bool
            is42 i = i == 42
          |]

        Set.map (fmap prettyName) (foldMap moduleDeclarations (getAsts asts)) `shouldBe` Set.fromList
          [ FunctionDeclaration "TestModule.is42"
              [ "GHC.Classes.==", "i", "$dEq" ]
          ]
        

    describe "function declarations" do
      it "detects function declarations" do
        asts <- toHieAsts "TestModule"
          [s|
            module TestModule where

            not :: Bool -> Bool
            not True = False
            not False = True
          |]

        Set.map (fmap prettyName) (foldMap moduleDeclarations (getAsts asts)) `shouldBe` Set.fromList
          [ FunctionDeclaration "TestModule.not"
              [ "GHC.Types.False", "GHC.Types.True" ]
          ]

    describe "data types" do
      it "detects record types" do
        asts <- toHieAsts "TestModule"
          [s|
            module TestModule where

            data RecordT = RecordCon
              { fieldA :: Bool
              , fieldB, fieldC :: Int
              }
          |]

        Set.map (fmap prettyName) (foldMap moduleDeclarations (getAsts asts)) `shouldBe` Set.fromList
          [ DataDeclaration "TestModule.RecordT" 
              [ Constructor 
                  { constructorName = "TestModule.RecordCon" 
                  , constructorFields = 
                      [ Field
                          { fieldName = "TestModule.fieldA"
                          , fieldUses = [ "GHC.Types.Bool" ]
                          }
                      , Field
                          { fieldName = "TestModule.fieldC"
                          , fieldUses = [ "GHC.Types.Int" ]
                          }
                      , Field
                          { fieldName = "TestModule.fieldB"
                          , fieldUses = [ "GHC.Types.Int" ]
                          }
                      ]
                  }
              ]
          ]

    describe "class declarations" do
      it "detects class declarations" do
        asts <- toHieAsts "TestModule"
          [s|
            module TestModule where

            class TestC a where
              methodA :: a -> Int
          |]

        Set.map (fmap prettyName) (foldMap moduleDeclarations (getAsts asts)) `shouldBe` Set.fromList
          [ ClassDeclaration
              { className = "TestModule.TestC"
              , classUses = [ "GHC.Types.Int", "a" ]
              }
          ]

      it "uses names from default declarations" do
        asts <- toHieAsts "TestModule"
          [s|
            {-# language DefaultSignatures #-}
            module TestModule where

            class TestC a where
              methodA :: a -> Int

              default methodA :: Eq a => a -> Int
              methodA x = if x == x then 42 else 0
          |]

        Set.map (fmap prettyName) (foldMap moduleDeclarations (getAsts asts)) `shouldBe` Set.fromList
          [ ClassDeclaration
              { className = "TestModule.TestC"
              , classUses = 
                  [ "GHC.Classes.==", "GHC.Classes.Eq", "GHC.Types.Int", "x"
                  , "$dEq", "$dEq", "a"
                  ]
              }
          ]


toHieAsts :: String -> String -> IO (HieASTs TypeIndex)
toHieAsts moduleName source = do
  withSystemTempDirectory "weeder" \tempDir -> do
    let sourcePath = tempDir </> "source.hs"
    writeFile sourcePath source

    (exitCode, stdout, stderr) <- 
      readCreateProcessWithExitCode 
        (proc "ghc-9.0.2" [ "-fwrite-ide-info", "-hiedir", tempDir, "-fno-code", sourcePath ] )
        ""

    putStrLn stderr

    ncu <- do
      uniqSupply <- mkSplitUniqSupply 'z'
      nameCacheRef <- newIORef (initNameCache uniqSupply [])
      return $ NCU $ atomicModifyIORef' nameCacheRef

    result <- readHieFile ncu (tempDir </> moduleName <.> "hie")
    return $ hie_asts $ hie_file_result result


prettyName :: Name -> String
prettyName n = foldMap prettyModule (nameModule_maybe n) <> occNameString (getOccName n)
  where
    prettyModule m = moduleNameString (moduleName m) <> "."
