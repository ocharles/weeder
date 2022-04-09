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

import Data.Monoid
import Control.Applicative ( (<|>), Alternative )
import Data.Functor
import GHC.Unit.Module ( moduleName )
import Data.Foldable ( toList, for_, asum )
import GHC.Unit.Module.Name ( moduleNameString )
import Data.Set ( Set )
import Text.Regex.TDFA ( (=~) )
import qualified Data.Set as Set
import Algebra.Graph ( Graph, edge, empty, overlay, vertex, vertexList )
import GHC.Types.Unique.Supply ( mkSplitUniqSupply )
import Algebra.Graph.ToGraph ( dfs )
import Control.Monad ( guard )
import Data.Function ( (&) )
import Data.Maybe ( listToMaybe )
import GHC.Generics ( Generic )
import GHC.Types.Name ( Name, nameModule_maybe, occNameString, getOccName, nameOccName )
import qualified Data.Map.Strict as Map
import GHC.Iface.Ext.Ast
import GHC.Iface.Ext.Binary
import System.Exit ( exitFailure )
import Data.List ( isSuffixOf, find )
import Control.Monad.IO.Class ( liftIO )
import GHC.Types.Name.Cache ( initNameCache, NameCache )
import Data.IORef
import GHC.Iface.Ext.Types
import Data.Functor.Foldable
import System.FilePath ( isExtensionOf )
import System.Directory ( canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory )


data Declaration name
  = FunctionDeclaration
      { functionName :: name
      , functionUses :: [name]
      }
  | InstanceDeclaration
      { instanceUses :: [name]
      }
  | ClassDeclaration
      { className :: name
      , classUses :: [name]
      }
  | DataDeclaration
      { typeName :: name
      , constructors :: [Constructor name]
      }
  deriving stock (Eq, Functor, Ord, Show)


data Constructor name = Constructor 
  { constructorName :: name 
  , constructorFields :: [Field name]
  }
  deriving stock (Eq, Functor, Ord, Show)


data Field name = Field
  { fieldName :: name
  , fieldUses :: [name]
  }
  deriving stock (Eq, Functor, Ord, Show)


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


data NameWithContext name = NameWithContext
  { name :: name
  , context :: Set ContextInfo
  }
  deriving stock (Functor)


instance Show name => Show (NameWithContext name) where
  show NameWithContext{ name } = show name


data DataType name = DataType{ name :: name, uses :: [ name ] }
  deriving stock (Eq, Functor, Ord, Show)


findDataTypes 
  :: [name]
  -> [NameWithContext name] 
  -> HieASTF x [DataType name]
  -> [DataType name]
findDataTypes uses names NodeF{ sourcedNodeInfo, nodeChildren } = 
  foldMap pure typeHere <> mconcat nodeChildren 
  where
    typeHere = do
      guard $ 
        foldMap nodeAnnotations (getSourcedNodeInfo sourcedNodeInfo) & 
          any \(_, typeName) -> 
            typeName == "TyClDecl"
          

      NameWithContext{ name } <-
        names & find \NameWithContext{ context } -> do
          context & any \case
            Decl DataDec _ -> True
            _              -> False

      return DataType{ uses, name }
        

-- findDeclarations 
--   :: [Name]
--   -> HieASTF a (Set Name, Set (Declaration Name)) 
--   -> Set (Declaration Name)
-- findDeclarations = undefined


-- findUses :: HieASTF a (Set Name) -> Set Name
-- findUses = undefined


data ModuleAnalysis name = ModuleAnalysis
  { types :: [name]
  , names :: [[NameWithContext name]]
  , dataTypes :: [DataType name]
  , namesInUse :: [name]
  }
  deriving stock (Functor, Show)


instance Semigroup (ModuleAnalysis name) where
  x <> y = ModuleAnalysis
    { types = types x <> types y 
    , names = names x <> names y
    , dataTypes = dataTypes x <> dataTypes y
    , namesInUse = namesInUse x <> namesInUse y
    }


instance Monoid (ModuleAnalysis name) where
  mempty = ModuleAnalysis
    { types = []
    , names = []
    , dataTypes = []
    , namesInUse = mempty
    }


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
                guard $ Use `elem` identInfo identifierDetails 
                pure name


analyze :: HieAST a -> ModuleAnalysis Name
analyze = fold \node -> do
  let names' = bfsNames (names <$> node)

  let namesInUseHere = collectUses $ namesInUse <$> node

  let dataTypes' = 
        findDataTypes namesInUseHere (concat names') (dataTypes <$> node)

  let declarationsHere = not $ null dataTypes'

  let namesInUse' 
        | declarationsHere = []
        | otherwise        = namesInUseHere

  ModuleAnalysis
    { names = names'
    , dataTypes = dataTypes'
    , types = []
    , namesInUse = namesInUse'
    }



-- moduleDeclarations :: HieAST x -> Set (Declaration Name)
-- moduleDeclarations = 
--   para \node@NodeF{ nodeChildren } ->
--     case parseNodeType $ embed $ fst <$> node of
--       Just (ValueBinding declarationName) -> 
--         Set.singleton 
--           FunctionDeclaration
--             { functionName = declarationName
--             , functionUses = toList $ references $ embed $ fst <$> node
--             }

--       Just InstanceNode -> 
--         Set.singleton 
--           InstanceDeclaration
--             { instanceUses = toList $ references $ embed $ fst <$> node
--             }

--       Just (DataNode name) -> 
--         Set.singleton 
--           DataDeclaration
--             { typeName = name
--             , constructors = findConstructors $ embed $ fst <$> node
--             }

--       Just (ClassNode name) -> 
--         Set.singleton 
--           ClassDeclaration
--             { className = name
--             , classUses = toList $ references $ embed $ fst <$> node
--             }

--       _ ->
--         foldMap snd nodeChildren


-- findConstructors :: HieAST x -> [Constructor Name]
-- findConstructors = para go where
--   go node@NodeF{ nodeChildren, sourcedNodeInfo } =
--     case isConstructor of
--       True -> 
--         conName & foldMap \conName ->
--           pure Constructor
--             { constructorName = conName
--             , constructorFields = concatMap findFields $ fst <$> nodeChildren
--             }
--         where
--           conName = 
--             listToMaybe $ embed (fst <$> node) & findNearestName \case
--               Decl ConDec _ -> True
--               _ -> False

--       False -> 
--         concatMap snd nodeChildren

--     where
--       isConstructor =
--         getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
--           nodeAnnotations nodeInfo & any \(_, con ->
--             con == "ConDecl"


-- findFields :: HieAST x -> [Field Name]
-- findFields = para go where
--   go node@NodeF{ nodeChildren, sourcedNodeInfo } =
--     case isField of
--       True -> 
--         names <&> \name -> 
--           Field
--             { fieldName = name
--             , fieldUses = toList $ sourceReferences $ embed $ fst <$> node
--             }
--         where
--           names = toList $ Set.fromList $ embed (fst <$> node) & findNearestName \case
--             RecField RecFieldDecl _ -> True
--             _ -> False


--       False -> 
--         concatMap snd nodeChildren

--     where
--       isField =
--         getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
--           nodeAnnotations nodeInfo & any \(_, con) ->
--             con == "ConDeclField"


-- data Analysis name = Analysis
--   { implicitRoots :: Set name
--   , dependencyGraph :: Graph name
--   , allDeclarations :: Set name
--   }


-- instance Ord name => Semigroup (Analysis name) where
--   x <> y = Analysis
--     { implicitRoots = implicitRoots x <> implicitRoots y
--     , dependencyGraph = dependencyGraph x <> dependencyGraph y
--     , allDeclarations = allDeclarations x <> allDeclarations y
--     }

-- instance Ord name => Monoid (Analysis name) where
--   mempty = Analysis mempty mempty mempty


-- analyzeDeclarations :: (Foldable f, Ord name) => f (Declaration name) -> Analysis name
-- analyzeDeclarations = foldMap \case
--   FunctionDeclaration{ functionName, functionUses } -> 
--     Analysis
--       { implicitRoots = mempty
--       , dependencyGraph = foldMap (edge functionName) functionUses
--       , allDeclarations = Set.singleton functionName
--       }
      
--   InstanceDeclaration{ instanceUses } -> 
--     Analysis
--       { implicitRoots = Set.fromList instanceUses
--       , dependencyGraph = mempty
--       , allDeclarations = mempty
--       }

--   ClassDeclaration{ className, classUses } -> 
--     Analysis
--       { implicitRoots = Set.singleton className
--       , dependencyGraph = foldMap (edge className) classUses 
--       , allDeclarations = Set.singleton className
--       }

--   DataDeclaration{ typeName, constructors } ->
--     Analysis
--       { implicitRoots = mempty
--       , dependencyGraph =
--           constructors & foldMap \constructor ->
--             mconcat
--               [ -- Constructor usage keeps the type alive
--                 edge (constructorName constructor) typeName 

--               , constructorFields constructor & foldMap \field ->
--                   mconcat
--                     [ -- Field usage keeps a constructor alive
--                       edge (fieldName field) (constructorName constructor)
                    
--                       -- Constructor usage keeps field alive
--                     , edge (constructorName constructor) (fieldName field)
--                     ]
--               ]

--       , allDeclarations = mconcat
--           [ Set.singleton typeName 
--           , constructors & foldMap \constructor ->
--               mconcat
--                 [ Set.singleton $ constructorName constructor
--                 , constructorFields constructor & foldMap \field ->
--                     Set.singleton $ fieldName field
--                 ]
--           ]
--       }


-- references :: HieAST x -> Set Name
-- references = fold go where
--   go node@NodeF{ nodeChildren } = ourUses <> mconcat nodeChildren
--     where
--       ourUses = Set.fromList $ node & filteredIdentifiers \case
--         Use -> True
--         RecField RecFieldAssign _ -> True
--         RecField RecFieldMatch _ -> True
--         EvidenceVarUse -> True
--         _   -> False


-- sourceReferences :: HieAST x -> [Name]
-- sourceReferences = fold go where
--   go node@NodeF{ nodeChildren, sourcedNodeInfo } = 
--     ourUses sourcedNodeInfo <> mconcat nodeChildren

--   ourUses sourcedNodeInfo = 
--     Map.lookup SourceInfo (getSourcedNodeInfo sourcedNodeInfo) & 
--       foldMap \nodeInfo ->
--         nodeIdentifiers nodeInfo & 
--           Map.foldMapWithKey \identifier identifierDetails ->
--             case identifier of
--               Left _moduleName -> 
--                 []

--               Right name -> do
--                 guard $ any p $ identInfo identifierDetails
--                 pure name

--   p = \case
--     Use -> True
--     _ -> False


-- filteredIdentifiers :: (ContextInfo -> Bool) -> HieASTF a b -> [Name]
-- filteredIdentifiers p NodeF{ sourcedNodeInfo } =
--   getSourcedNodeInfo sourcedNodeInfo & foldMap \nodeInfo ->
--     nodeIdentifiers nodeInfo & Map.foldMapWithKey \identifier identifierDetails ->
--       case identifier of
--         Left _moduleName -> 
--           []

--         Right name -> do
--           guard $ any p $ identInfo identifierDetails
--           pure name


-- findNearestName 
--   :: (ContextInfo -> Bool) -> HieAST a -> [Name]
-- findNearestName p = fold go where
--   go NodeF{ sourcedNodeInfo, nodeChildren } = nameHere <|> asum nodeChildren
--     where
--       nameHere =
--         getSourcedNodeInfo sourcedNodeInfo & foldMap \nodeInfo ->
--           nodeIdentifiers nodeInfo & Map.foldMapWithKey \identifier identifierDetails ->
--             case identifier of
--               Right name | any p $ identInfo identifierDetails -> pure name
--               _ -> mempty



-- -- | A classification of HIE nodes
-- data NodeType
--   = InstanceNode

--     -- | A node that begins a declaration.
--   | ValueBinding Name

--   | DataNode Name
--   | ClassNode Name
--   | EvidenceBinding Name [Name]


-- parseNodeType :: HieAST a -> Maybe NodeType
-- parseNodeType node@Node{ sourcedNodeInfo } = 
--       instanceDeclaration 
--   <|> valueBinding
--   <|> dataDecl
--   <|> classDecl
--   <|> evidenceBinding
--   where
--     evidenceBinding = Nothing

--     classDecl = ClassNode <$> do
--       guard $ getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
--         Set.member ("ClassDecl", "TyClDecl") $ nodeAnnotations nodeInfo

--       listToMaybe $ node & findNearestName \case
--         Decl ClassDec _ -> True
--         _ -> False
    
--     dataDecl = DataNode <$> do
--       guard $ getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
--         Set.member ("DataDecl", "TyClDecl") $ nodeAnnotations nodeInfo

--       listToMaybe $ node & findNearestName \case
--         Decl DataDec _ -> True
--         _ -> False

--     instanceDeclaration = InstanceNode <$ do
--       guard $ getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
--         Set.member ("ClsInstD", "InstDecl") $ nodeAnnotations nodeInfo

--     valueBinding = ValueBinding <$> do
--       guard $ getSourcedNodeInfo sourcedNodeInfo & any \nodeInfo ->
--         Set.member ("FunBind", "HsBindLR") $ nodeAnnotations nodeInfo

--       listToMaybe $ node & findNearestName \case
--         ValBind _ _ _ -> True
--         MatchBind -> True
--         _ -> False


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

  mapM_ print $ dataTypes $
    decls <&> \name ->
      foldMap (\m -> moduleNameString (moduleName m) <> ".") (nameModule_maybe name) <>
      occNameString (nameOccName name)

  -- let analysis = analyzeDeclarations decls

  -- let roots =
  --       allDeclarations analysis & Set.filter \name ->
  --         rootPatterns & any \pattern ->
  --           nameModule_maybe name & any \m ->
  --             (moduleNameString (moduleName m) <> "." <> occNameString (getOccName name)) =~ pattern

  -- -- for_ decls $ print . fmap \name -> 
  -- --   foldMap (\m -> moduleNameString (moduleName m) <> ".") (nameModule_maybe name) <>
  -- --   occNameString (nameOccName name)

  -- let reachableSet = Set.fromList $ dfs (toList (implicitRoots analysis <> roots)) (dependencyGraph analysis)

  -- let dead = allDeclarations analysis Set.\\ reachableSet

  -- for_ dead \name -> do
  --   for_ (nameModule_maybe name) \m -> do
  --     putStr $ moduleNameString (moduleName m) <> "."
  --     putStrLn $ occNameString $ nameOccName name

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

