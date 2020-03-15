{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

-- | This module provides an entry point to the Weeder executable.

module Weeder.Main ( main, mainWithConfig ) where

-- base
import Control.Monad ( guard )
import Control.Monad.IO.Class ( liftIO )
import Data.Bool
import Data.Foldable
import Text.Printf ( printf )

-- bytestring
import qualified Data.ByteString.Char8 as BS

-- containers
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- dhall
import qualified Dhall

-- directory
import System.Directory ( canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory )

-- filepath
import System.FilePath ( isExtensionOf )

-- ghc
import HieBin ( HieFileResult( HieFileResult, hie_file_result ) )
import HieBin ( readHieFile )
import Module ( moduleName, moduleNameString )
import NameCache ( initNameCache )
import OccName ( occNameString )
import SrcLoc ( realSrcSpanStart, srcLocCol, srcLocLine )
import UniqSupply ( mkSplitUniqSupply )

-- regex-tdfa
import Text.Regex.TDFA ( (=~) )

-- optparse-applicative
import Options.Applicative

-- transformers
import Control.Monad.Trans.State.Strict ( execStateT )

-- weeder
import Weeder
import Weeder.Config


-- | Parse command line arguments and into a 'Config' and run 'mainWithConfig'.
main :: IO ()
main = do
  configExpr <-
    execParser $
      info
        ( strOption
            (    long "config"
              <> help "A Dhall expression for Weeder's configuration. Can either be a file path (a Dhall import) or a literal Dhall expression."
              <> value "./weeder.dhall"
            )
        )
        mempty

  Dhall.input config configExpr >>= mainWithConfig


-- | Run Weeder in the current working directory with a given 'Config'.
--
-- This will recursively find all @.hie@ files in the current directory, perform
-- analysis, and report all unused definitions according to the 'Config'.
mainWithConfig :: Config -> IO ()
mainWithConfig Config{ rootPatterns, typeClassRoots } = do
  hieFilePaths <-
    getHieFilesIn "./."

  nameCache <- do
    uniqSupply <- mkSplitUniqSupply 'z'
    return ( initNameCache uniqSupply [] )

  analysis <-
    flip execStateT emptyAnalysis do
      for_ hieFilePaths \hieFilePath -> do
        ( HieFileResult{ hie_file_result }, _ ) <-
          liftIO ( readHieFile nameCache hieFilePath )

        analyseHieFile hie_file_result

  let
    roots =
      Set.filter
        ( \d ->
            any
              ( ( moduleNameString ( moduleName ( declModule d ) ) <> "." <> occNameString ( declOccName d ) ) =~ )
              rootPatterns
        )
        ( allDeclarations analysis )

    reachableSet =
      reachable
        analysis
        ( Set.map DeclarationRoot roots <> bool mempty ( Set.map DeclarationRoot ( implicitRoots analysis ) ) typeClassRoots )

    dead =
      allDeclarations analysis Set.\\ reachableSet

    warnings =
      Map.unionsWith (++) $
      foldMap
        ( \d ->
            fold $ do
              moduleFilePath <- Map.lookup ( declModule d ) ( modulePaths analysis )
              moduleSource <- Map.lookup ( declModule d ) ( moduleSource analysis )

              spans <- Map.lookup d ( declarationSites analysis )
              guard $ not $ null spans

              let snippets = do
                    srcSpan <- Set.toList spans

                    let start = realSrcSpanStart srcSpan
                    let firstLine = max 0 ( srcLocLine start - 3 )

                    return ( start, take 5 $ drop firstLine $ zip [1..] $ BS.lines moduleSource )

              return [ Map.singleton moduleFilePath ( liftA2 (,) snippets (pure d) ) ]
        )
        dead

  for_ ( Map.toList warnings ) \( path, declarations ) ->
    for_ declarations \( ( start, snippet ), d ) -> do
      putStrLn $
        unwords
          [ foldMap ( <> ":" ) [ path, show ( srcLocLine start ), show ( srcLocCol start ) ]
          , "error:"
          , occNameString ( declOccName d )
          , "is unused"
          ]

      putStrLn ""
      for_ snippet \( n, line ) ->
        putStrLn $
             replicate 4 ' '
          <> printf "% 4d" ( n :: Int )
          <> " ┃ "
          <> BS.unpack line
      putStrLn ""

      putStrLn $
           replicate 4 ' '
        <> "Delete this definition or add ‘"
        <> moduleNameString ( moduleName ( declModule d ) )
        <> "."
        <> occNameString ( declOccName d )
        <> "’ as a root to fix this error."
      putStrLn ""
      putStrLn ""


-- | Recursively search for .hie files in given directory
getHieFilesIn :: FilePath -> IO [FilePath]
getHieFilesIn path = do
  exists <-
    doesPathExist path

  if exists
    then do
      isFile <-
        doesFileExist path

      if isFile && "hie" `isExtensionOf` path
        then do
          path' <-
            canonicalizePath path

          return [ path' ]

        else do
          isDir <-
            doesDirectoryExist path

          if isDir
            then do
              cnts <-
                listDirectory path

              withCurrentDirectory path ( foldMap getHieFilesIn cnts )

            else
              return []

    else
      return []
