{-# language BlockArguments #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeFamilies #-}

{-# options -Wno-orphans #-}

module Weeder.New2 where

import Data.Functor
import GHC.Unit.Module ( moduleName )
import Data.Foldable ( toList, for_ )
import GHC.Unit.Module.Name ( moduleNameString )
import Data.Set ( Set )
import Text.Regex.TDFA ( (=~) )
import qualified Data.Set as Set
import Algebra.Graph ( Graph, edge )
import GHC.Types.Unique.Supply ( mkSplitUniqSupply )
import Algebra.Graph.ToGraph ( dfs )
import Control.Monad ( guard )
import Data.Function ( (&) )
import GHC.Generics ( Generic )
import GHC.Types.Name ( Name, nameModule_maybe, occNameString, getOccName, nameOccName )
import qualified Data.Map.Strict as Map
import GHC.Iface.Ext.Binary
import System.Exit ( exitFailure )
import Data.List ( isSuffixOf, find )
import Control.Monad.IO.Class ( liftIO )
import GHC.Types.Name.Cache ( initNameCache )
import Data.IORef
import GHC.Iface.Ext.Types
import Data.Functor.Foldable
import System.FilePath ( isExtensionOf )
import System.Directory ( canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory )


data NameWithContext name = NameWithContext
  { name :: name
  , context :: Set ContextInfo
  }
  deriving stock (Functor)


instance Show name => Show (NameWithContext name) where
  show NameWithContext{ name } = show name


-- | A list of names in level-order from a given HIE node.
bfsNames :: HieASTF f [[NameWithContext Name]] -> [[NameWithContext Name]]
bfsNames NodeF{ sourcedNodeInfo, nodeChildren } =
  namesHere : foldr (zipWith' (++)) [] nodeChildren
  where
    namesHere =
      getSourcedNodeInfo sourcedNodeInfo & foldMap \nodeInfo ->
        nodeIdentifiers nodeInfo &
          Map.foldMapWithKey \identifier identifierDetails ->
            case identifier of
              Left _moduleName -> []
              Right name       ->
                pure NameWithContext
                  { name
                  , context = identInfo identifierDetails
                  }


zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ xs     []     = xs
zipWith' _ []     xs     = xs
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


data DataType name = DataType
  { name :: name
  , constructors :: [Constructor name]
  }
  deriving stock (Eq, Functor, Ord, Show)


findDataTypes
  :: [NameWithContext name]
  -> [Constructor name]
  -> HieASTF x [DataType name]
  -> [DataType name]
findDataTypes names constructors NodeF{ sourcedNodeInfo, nodeChildren } =
  foldMap pure typeHere <> mconcat nodeChildren
  where
    typeHere = do
      guard $
        foldMap nodeAnnotations (getSourcedNodeInfo sourcedNodeInfo) &
          any \(NodeAnnotation _ typeName) ->
            typeName == "TyClDecl"

      NameWithContext{ name } <-
        names & find \NameWithContext{ context } -> do
          context & any \case
            Decl DataDec _ -> True
            _              -> False

      return DataType{ name, constructors }


data Constructor name = Constructor
  { constructorName :: name
  , constructorFields :: [Field name]
  }
  deriving stock (Eq, Functor, Ord, Show)


findConstructors
  :: [NameWithContext name]
  -> [Field name]
  -> HieASTF x [Constructor name]
  -> [Constructor name]
findConstructors names fields NodeF{ sourcedNodeInfo, nodeChildren } =
  foldMap pure constructorsHere <> mconcat nodeChildren
  where
    constructorsHere = do
      guard $
        getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
          nodeAnnotations nodeInfo & any \(NodeAnnotation _ con) ->
            con == "ConDecl"

      NameWithContext{ name = constructorName } <-
        names & find \NameWithContext{ context } -> do
          context & any \case
            Decl ConDec _ -> True
            _             -> False

      return Constructor{ constructorName, constructorFields = fields }


data Field name = Field
  { fieldName :: name
  , fieldUses :: [name]
  }
  deriving stock (Eq, Functor, Ord, Show)


findFields
  :: [name]
  -> [NameWithContext name]
  -> HieASTF x [Field name]
  -> [Field name]
findFields uses names NodeF{ sourcedNodeInfo, nodeChildren } =
  foldMap id fieldsHere <> mconcat nodeChildren
  where
    declaredNames =
      names & filter \NameWithContext{ context } ->
        context & any \case
          RecField RecFieldDecl _ -> True
          _                       -> False

    fieldsHere = do
      guard $
        getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
          nodeAnnotations nodeInfo & any \(NodeAnnotation _ con) ->
            con == "ConDeclField"

      Just $
        declaredNames <&> \NameWithContext{ name } ->
          Field{ fieldName = name, fieldUses = uses }


data Function name = Function{ name :: name, uses :: [ name ] }
  deriving stock (Eq, Functor, Ord, Show)


findFunctions
  :: [name]
  -> [NameWithContext name]
  -> HieASTF x [Function name]
  -> [Function name]
findFunctions uses names NodeF{ sourcedNodeInfo, nodeChildren } =
  case functionHere of
    Nothing -> mconcat nodeChildren
    Just f  -> [f]
  where
    functionHere = do
      guard $
        getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
          Set.member (NodeAnnotation "FunBind" "HsBindLR") (nodeAnnotations nodeInfo)

      NameWithContext{ name } <-
        names & find \NameWithContext{ context } -> do
          context & any \case
            ValBind _ _ _ -> True
            _              -> False

      return Function{ uses, name }


data Class name = Class
  { className :: name
  , classUses :: [name]
  }
  deriving stock (Eq, Functor, Ord, Show)


findClasses
  :: [name]
  -> [NameWithContext name]
  -> HieASTF x [Class name]
  -> [Class name]
findClasses uses names NodeF{ sourcedNodeInfo, nodeChildren } =
  foldMap pure classHere <> mconcat nodeChildren
  where
    classHere = do
      guard $
        getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
          Set.member (NodeAnnotation "ClassDecl" "TyClDecl")
            (nodeAnnotations nodeInfo)

      NameWithContext{ name } <-
        names & find \NameWithContext{ context } -> do
          context & any \case
            _ -> True

      return Class{ className = name, classUses = uses }


data Instance name = Instance
  { instanceName :: name
  , instanceUses :: [name]
  }
  deriving stock (Eq, Functor, Ord, Show)


findInstances
  :: [name]
  -> [NameWithContext name]
  -> HieASTF x [Instance name]
  -> [Instance name]
findInstances uses names NodeF{ sourcedNodeInfo, nodeChildren } =
  foldMap pure instanceHere <> mconcat nodeChildren
  where
    instanceHere = do
      guard $
        getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
          Set.member (NodeAnnotation "ClsInstD" "InstDecl")
            (nodeAnnotations nodeInfo)

      NameWithContext{ name } <-
        names & find \NameWithContext{ context } -> do
          context & any \case
            _ -> True

      return Instance{ instanceName = name, instanceUses = uses }


collectUses :: HieASTF f [Name] -> [Name]
collectUses NodeF{ sourcedNodeInfo, nodeChildren } =
  namesHere ++ concat nodeChildren
  where
    namesHere =
      getSourcedNodeInfo sourcedNodeInfo & foldMap \nodeInfo ->
        nodeIdentifiers nodeInfo &
          Map.foldMapWithKey \identifier identifierDetails ->
            case identifier of
              Left _moduleName -> []
              Right name       -> do
                guard $
                  identInfo identifierDetails & any \case
                    Use -> True
                    RecField RecFieldAssign _ -> True
                    RecField RecFieldOcc _ -> True
                    _ -> False

                pure name


data ModuleAnalysis name = ModuleAnalysis
  { names :: [[NameWithContext name]]
  , dataTypes :: [DataType name]
  , namesInUse :: [name]
  , functions :: [Function name]
  , constructors :: [Constructor name]
  , fields :: [Field name]
  , classes :: [Class name]
  , instances :: [Instance name]
  }
  deriving stock (Functor, Show)


instance Semigroup (ModuleAnalysis name) where
  x <> y = ModuleAnalysis
    { names = names x <> names y
    , dataTypes = dataTypes x <> dataTypes y
    , namesInUse = namesInUse x <> namesInUse y
    , functions = functions x <> functions y
    , instances = instances x <> instances y
    , constructors =
        mappend
          (case x of ModuleAnalysis{ constructors } -> constructors)
          (case y of ModuleAnalysis{ constructors } -> constructors)
    , fields =
        mappend
          (case x of ModuleAnalysis{ fields } -> fields)
          (case y of ModuleAnalysis{ fields } -> fields)
    , classes =
        mappend
          (case x of ModuleAnalysis{ classes } -> classes)
          (case y of ModuleAnalysis{ classes } -> classes)
    }


instance Monoid (ModuleAnalysis name) where
  mempty = ModuleAnalysis
    { names = []
    , dataTypes = []
    , namesInUse = mempty
    , functions = mempty
    , constructors = mempty
    , fields = mempty
    , classes = mempty
    , instances = mempty
    }


analyze :: HieAST a -> ModuleAnalysis Name
analyze = fold \node -> do
  let names' = bfsNames (names <$> node)

      namesInUseHere = collectUses $ namesInUse <$> node

      dataTypes' =
        findDataTypes
          (concat names')
          constructors'
          (dataTypes <$> node)

      constructors' =
        findConstructors
          (concat names')
          fields'
          (node <&> \ModuleAnalysis{ constructors } -> constructors)

      fields' =
        findFields namesInUseHere (concat names') (fields <$> node)

      functions' =
        findFunctions namesInUseHere (concat names') (functions <$> node)

      classes' =
        findClasses namesInUseHere (concat names') (classes <$> node)

      instances' =
        findInstances namesInUseHere (concat names') (instances <$> node)

      declarationsHere = not $ null dataTypes'

      namesInUse'
        | declarationsHere = []
        | otherwise        = namesInUseHere

  ModuleAnalysis
    { names = names'
    , dataTypes = dataTypes'
    , namesInUse = namesInUse'
    , functions = functions'
    , constructors = constructors'
    , fields = fields'
    , classes = classes'
    , instances = instances'
    }



data Analysis name = Analysis
  { implicitRoots :: Set name
  , dependencyGraph :: Graph name
  , allDeclarations :: Set name
  }


instance Ord name => Semigroup (Analysis name) where
  x <> y = Analysis
    { implicitRoots = implicitRoots x <> implicitRoots y
    , dependencyGraph = dependencyGraph x <> dependencyGraph y
    , allDeclarations = allDeclarations x <> allDeclarations y
    }

instance Ord name => Monoid (Analysis name) where
  mempty = Analysis mempty mempty mempty


finalizeAnalysis :: Ord name => ModuleAnalysis name -> Analysis name
finalizeAnalysis ModuleAnalysis{..} = Analysis
  { implicitRoots =
      instances & foldMap \Instance{ instanceName } ->
        Set.singleton instanceName
  , dependencyGraph = mconcat
      [ functions & foldMap \Function{ name, uses } ->
          foldMap (edge name) uses
      , instances & foldMap \Instance{ instanceName, instanceUses } ->
          foldMap (edge instanceName) instanceUses
      ]
  , allDeclarations = mconcat
      [ functions & foldMap \Function{ name } ->
          Set.singleton name
      ]
  }


-- | This is the same as 'HieAST', but with recursion made explicit (it is the
-- pattern functor for 'HieAST').
data HieASTF a b = NodeF
  { sourcedNodeInfo :: SourcedNodeInfo a
  , nodeSpan :: Span
  , nodeChildren :: [b]
  }
  deriving stock (Functor, Generic)


type instance Base (HieAST a) = HieASTF a


instance Recursive (HieAST a) where
  project Node{..} = NodeF{..}


instance Corecursive (HieAST a) where
  embed NodeF{..} = Node{..}


main :: [String] -> [FilePath] -> IO ()
main rootPatterns hieDirectories = do
  hieFilePaths <-
    concat <$>
      traverse ( getFilesIn hieExt )
        ( if null hieDirectories
          then ["./."]
          else hieDirectories
        )

  ncu <- do
    uniqSupply <- mkSplitUniqSupply 'z'
    nameCacheRef <- newIORef (initNameCache uniqSupply [])
    return $ NCU $ atomicModifyIORef' nameCacheRef

  decls <-
    hieFilePaths & foldMap \hieFilePath -> do
      hieFileResult <- liftIO ( readCompatibleHieFileOrExit ncu hieFilePath )
      let hsFileExists = any ( hie_hs_file hieFileResult `isSuffixOf` ) hsFilePaths
      return if requireHsFiles ==> hsFileExists
        then foldMap analyze (getAsts (hie_asts hieFileResult))
        else mempty

  mapM_ print $ functions $
    decls <&> \name ->
      foldMap (\m -> moduleNameString (moduleName m) <> ".") (nameModule_maybe name) <>
      occNameString (nameOccName name)

  let analysis = finalizeAnalysis decls

  let roots =
        allDeclarations analysis & Set.filter \name ->
          rootPatterns & any \pattern ->
            nameModule_maybe name & any \m ->
              (moduleNameString (moduleName m) <> "." <> occNameString (getOccName name)) =~ pattern

  -- for_ decls $ print . fmap \name ->
  --   foldMap (\m -> moduleNameString (moduleName m) <> ".") (nameModule_maybe name) <>
  --   occNameString (nameOccName name)

  let reachableSet = Set.fromList $ dfs (toList (implicitRoots analysis <> roots)) (dependencyGraph analysis)

  let dead = allDeclarations analysis Set.\\ reachableSet

  for_ dead \name -> do
    for_ (nameModule_maybe name) \m -> do
      putStr $ moduleNameString (moduleName m) <> "."
      putStrLn $ occNameString $ nameOccName name

  where
    requireHsFiles = False
    hsFilePaths = []
    hieExt = ".hie"


getFilesIn
  :: String
  -- ^ Only files with this extension are considered
  -> FilePath
  -- ^ Directory to look in
  -> IO [FilePath]
getFilesIn ext path = do
  exists <-
    doesPathExist path

  if exists
    then do
      isFile <-
        doesFileExist path

      if isFile && ext `isExtensionOf` path
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

              withCurrentDirectory path ( foldMap ( getFilesIn ext ) cnts )

            else
              return []

    else
      return []


infixr 5 ==>


-- | An infix operator for logical implication
(==>) :: Bool -> Bool -> Bool
True  ==> x = x
False ==> _ = True


-- | Read a .hie file, exiting if it's an incompatible version.
readCompatibleHieFileOrExit :: NameCacheUpdater -> FilePath -> IO HieFile
readCompatibleHieFileOrExit ncu path = do
  res <- readHieFileWithVersion (\(v, _) -> v == hieVersion) ncu path
  case res of
    Right HieFileResult{ hie_file_result } ->
      return hie_file_result
    Left ( v, _ghcVersion ) -> do
      putStrLn $ "incompatible hie file: " <> path
      putStrLn $ "    this version of weeder was compiled with GHC version "
               <> show hieVersion
      putStrLn $ "    the hie files in this project were generated with GHC version "
               <> show v
      putStrLn $ "    weeder must be built with the same GHC version"
               <> " as the project it is used on"
      exitFailure

