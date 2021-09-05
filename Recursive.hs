{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeFamilies #-}

module Recursive where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Functor.Base as Tree ( TreeF(..) )
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
import Algebra.Graph.Export.Dot
import Control.Monad ( guard )
import Data.Maybe ( fromMaybe )
import GHC.Generics ( Generic )
import Debug.Trace
import Data.List ( find )
import Data.Tree (Tree, Forest)
import qualified Data.Tree as Tree
import Data.Function ( (&) )
import Data.Text ( pack )
import Data.Bifunctor
import GHC.Data.FastString
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Foldable ( toList )
import GHC.Types.Name ( Name, nameStableString, isInternalName, nameUnique )
import GHC.Types.Name.Cache ( initNameCache, NameCache )
import GHC.Types.SrcLoc ( RealSrcSpan )
import GHC.Types.Unique.Supply ( mkSplitUniqSupply )
import Data.Functor.Foldable
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types hiding ( Identifier )
import Data.Set ( Set )
import GHC.Utils.Outputable ( ppr, showSDocUnsafe )
import Control.Monad.Omega


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


encodeHieAST :: HieAST a -> Value
encodeHieAST = cata \NodeF{ sourcedNodeInfo, nodeSpan, nodeChildren } ->
  let
    info = object $ map (uncurry originInfo) $ Map.toList $ getSourcedNodeInfo sourcedNodeInfo
      where
        originInfo SourceInfo nodeInfo = "source" .= encodeNodeInfo nodeInfo
        originInfo GeneratedInfo nodeInfo = "generated" .= encodeNodeInfo nodeInfo

    encodeNodeInfo NodeInfo{ nodeAnnotations, nodeIdentifiers } = object
      [ "annotations" .= foldMap ((:[]) . bimap (pack . unpackFS) unpackFS) nodeAnnotations
      , "identifiers" .= identifiers
      ]
      where
        identifiers = object $ foldMap (pure . uncurry identifier) $ Map.toList nodeIdentifiers

        identifier (Left moduleName) details = pack (showSDocUnsafe (ppr moduleName)) .= encodeDetails details
        identifier (Right name) details = pack (nameStableString name) .= encodeDetails details

    encodeDetails IdentifierDetails{ identInfo } = identInfo & foldMap \contextInfo ->
      [ showSDocUnsafe (ppr contextInfo) ]

  in
  object
    [ "span" .= showSDocUnsafe (ppr nodeSpan)
    , "children" .= nodeChildren
    , "info" .= info
    ]


data Declaration = Declaration
  { declType :: String
  , name :: Name
  , uses :: [Name]
  , span :: RealSrcSpan
  }
  deriving stock Show


instance Show Name where
  show name = nameStableString name <> "(" <> show (nameUnique name) <> ")"


data Identifier = Identifier
  { identifierName :: Name
  , contextInfo :: Set ContextInfo
  , origin :: NodeOrigin
  }


instance Show Identifier where
  show = show . identifierName


identifiers :: SourcedNodeInfo a -> [Identifier]
identifiers sourcedNodeInfo = Map.lookup SourceInfo (getSourcedNodeInfo sourcedNodeInfo) & foldMap \(NodeInfo{ nodeIdentifiers }) ->
  Map.toList nodeIdentifiers & foldMap \(identifier, IdentifierDetails{ identInfo }) ->
    case identifier of
      Left {} -> mempty
      Right name
        -- | isInternalName name -> mempty
        | otherwise -> pure Identifier
            { identifierName = name
            , contextInfo = identInfo
            , origin = SourceInfo
            }


-- | Convert a HieAST into a forest of Weeder AST nodes and a tree of names.
toASTs :: HieAST a -> NodeAnalysis
toASTs ast = cata analyseNode ast initialAnalysisState


type Algebra f a = f a -> a


data NodeAnalysis = NodeAnalysis
  { -- | A 'Forest' of 'Declaration's under this node. We use a 'Forest'
    -- because most nodes don't define declarations at all.
    nodeDeclarations :: Forest Declaration
  , -- | All 'Identifier's a given 'HieAST' node uses, include transitive uses.
    nodeUses :: [Name]
  , -- | All 'Identifier's a given 'HieAST' node declares, include transitive uses.
    nodeDeclares :: [Name]
  }
  deriving stock Show


data NameInfo = NameInfo RealSrcSpan
  deriving stock Show


-- | As we walk a HieAST tree, we will move into different contexts, which
-- changes the type of analysis we need to do.
data AnalysisMode
  = -- | We're at the top-level of analysis
    AnalysingTop
  | -- | We've entered a @module@ declaration
    AnalysingModule
  | -- | We've entered a @data@ or @newtype@ declaration
    AnalysingData
  | -- | We've entered a constructor declaration in a data type
    AnalysingConstructor
  | -- | We've entered some type of binding
    AnalysingBind
  | -- | We've entered some type of binding from within another binding
    AnalysingNestedBind
  | -- | We've entered a type class instance declaration
    AnalysingInstance
  | -- | We've entered the @deriving@ clauses of a data type
    AnalysingDeriving
  | -- | We've entered a @type@ declaration
    AnalysingTypeAlias
  | -- | We've entered a type signature
    AnalysingTypeSignature
  deriving stock Show


data AnalysisState = AnalysisState
  { uses :: [Name] -- ^ All identifiers currently being referred to
  , mode :: AnalysisMode -- ^ The current mode of analysis
  }


initialAnalysisState :: AnalysisState
initialAnalysisState = AnalysisState
  { uses = []
  , mode = AnalysingTop
  }


analyseNode :: Algebra (HieASTF a) (AnalysisState -> NodeAnalysis)
analyseNode node@NodeF{ sourcedNodeInfo, nodeChildren, nodeSpan } analysisState =
  case declarations of
    [] ->
      -- This node doesn't declare anything, so inherit any declarations from
      -- child nodes, and bubble up uses and declarations.
      NodeAnalysis
        { nodeDeclarations = childDeclarations
        , nodeUses = transitiveUses
        , nodeDeclares = transitiveDeclares
        }

    xs ->
      -- This node declares new names, so return a new forest. Each
      -- declaration begins a new tree, whose children are all declarations
      -- in nodes below this node.
      NodeAnalysis
        { nodeDeclarations = xs
        , nodeUses = []
        , nodeDeclares = []
        }

  where
    newMode
      | node `hasAnnotation` ((== "DataDecl") . fst) = Just AnalysingData
      | node `hasAnnotation` ((== "ConDecl") . snd) = Just AnalysingConstructor
      | node `hasAnnotation` (== ("FunBind", "HsBindLR")) = case mode analysisState of
          AnalysingBind        -> Just AnalysingNestedBind
          AnalysingInstance    -> Nothing
          AnalysingNestedBind  -> Nothing
          _                    -> Just AnalysingBind
      | node `hasAnnotation` (== ("ClsInstD","InstDecl")) = Just AnalysingInstance
      | node `hasAnnotation` (== ("HsDerivingClause","HsDerivingClause")) = Just AnalysingInstance
      | node `hasAnnotation` (== ("SynDecl","TyClDecl")) = Just AnalysingTypeAlias
      | node `hasAnnotation` (== ("Module","Module")) = Just AnalysingModule
      | node `hasAnnotation` (== ("TypeSig","Sig")) =
          case mode analysisState of
            AnalysingBind -> Nothing
            _ -> Just AnalysingTypeSignature
      | otherwise = Nothing

    beginsDeclaration = maybe False (const True) newMode
    mode' = fromMaybe (mode analysisState) newMode

    analysisState' = analysisState
      { mode = mode'
      , uses = transitiveUses
      }

    nodeChildren' = map (\f -> f analysisState') nodeChildren
    childDeclarations = concatMap nodeDeclarations nodeChildren'

    identifiersHere = identifiers sourcedNodeInfo

    declarations =
      case mode' of
        AnalysingModule ->
          -- If we're analysing the module as a whole, we need to be on the
          -- look out for dictionary declarations.
          concat
            [ evidenceBindings & map \(name, uses) -> pure Declaration
                { name, uses
                , declType = "ModuleEvidence"
                , span = nodeSpan
                }
            , childDeclarations
            ]

        _ ->
          case newMode of
            Just _ -> transitiveDeclares & map \name -> Tree.Node
              { rootLabel = Declaration
                  { name
                  , declType = show mode'
                  , uses = transitiveUses
                  , span = nodeSpan
                  }
              , subForest = childDeclarations
              }

            Nothing -> []

    transitiveUses :: [Name]
    transitiveUses = ourUses ++ concatMap nodeUses nodeChildren'
      where
        ourUses = do
          Identifier{ identifierName, contextInfo } <- identifiersHere

          guard do
            contextInfo & any \case
              Use            -> True
              EvidenceVarUse -> True
              _              -> False

          return identifierName

    transitiveDeclares :: [Name]
    transitiveDeclares = declaredNames ++ concatMap nodeDeclares nodeChildren'

    evidenceBindings = identifiersHere & foldMap \Identifier{ identifierName, contextInfo } ->
      contextInfo & foldMap \case
        EvidenceVarBind (EvLetBind (EvBindDeps deps)) _ _ -> [(identifierName, deps)]
        _                                                 -> mempty

    declaredNames = do
      Identifier{ identifierName, contextInfo } <- identifiersHere

      guard do
        contextInfo & any
          case mode' of
            AnalysingNestedBind -> \_ ->
              -- We never try and find declarations within nested binds, as they
              -- aren't reachable from outside the top-level binding. This means we
              -- skip declaring where- and let-bound variables.
              False

            AnalysingInstance -> \case
              -- If we're analysing an instance declaration, we're only interested
              -- in 'EvidenceVarBind' identifiers, as these name the instance
              -- dictionary. Nothing in a type class instance can really be "dead",
              -- so there's no need to find these declarations.
              EvidenceVarBind _ ModuleScope _ -> True
              _                               -> False

            AnalysingDeriving -> \case
              -- Find any instance declarations from deriving clauses.
              EvidenceVarBind _ ModuleScope _ -> True
              _                               -> False

            AnalysingData -> \case
              -- If we're analysing a data declaration, we need to find the name of
              -- the type being declared.
              Decl DataDec _ -> True
              _              -> False

            AnalysingTypeAlias -> \case
              -- If we're analysing a data declaration, we need to find the name of
              -- the type being declared.
              Decl SynDec _ -> True
              _             -> False

            AnalysingBind -> \case
              MatchBind               -> True
              RecField RecFieldDecl _ -> True
              _                       -> False

            AnalysingConstructor -> \case
              Decl ConDec _ -> True
              _             -> False

            AnalysingModule -> \case
              -- In module analysis, we need to find evidence bindings. These are
              -- essentially the imports of type class instances from other
              -- modules.
              EvidenceVarBind _ ModuleScope _ -> True
              _                               -> False

            AnalysingTypeSignature -> \case
              TyDecl -> True
              _      -> False

      return identifierName


dependencyGraph :: NodeAnalysis -> AdjacencyMap Name
dependencyGraph NodeAnalysis{ nodeDeclarations } = overlays do
  declarationTree <- nodeDeclarations
  return $ cata go declarationTree
  where
    -- onlyDeclarations g = g >>= \name -> name <$ guard (Set.member name declNames)
    declNames = foldMap (foldMap (\Declaration{ name } -> Set.singleton name)) nodeDeclarations

    go (Tree.NodeF Declaration{ name, uses } subgraphs) =
      connect declarationGraph (overlays subgraphs)
      where
        declarationGraph =
          overlay (vertex name) $ overlays do
            use <- uses
            return $ edge name use


hasAnnotation :: HieASTF a b -> ((FastString, FastString) -> Bool) -> Bool
hasAnnotation NodeF{ sourcedNodeInfo } f =
  getSourcedNodeInfo sourcedNodeInfo & any \NodeInfo{ nodeAnnotations } ->
    any f nodeAnnotations


loadHie :: FilePath -> IO [HieAST TypeIndex]
loadHie path = do
  nameCache <- do
    uniqSupply <- mkSplitUniqSupply 'z'
    return $ initNameCache uniqSupply []

  HieFileResult{ hie_file_result } <- readHieFile (NCU (\f -> return $ snd $ f nameCache)) path
  let HieFile{ hie_asts } = hie_file_result
  return $ toList $ getAsts hie_asts


dead :: AdjacencyMap Name -> Name -> Set Name
dead g root = allDecls Set.\\ Set.fromList (dfs [root] g)
  where
    allDecls = Set.fromList $ vertexList g
