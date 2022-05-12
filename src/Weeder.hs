{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}

module Weeder
  ( -- * Analysis
    Analysis(..)
  , analyseHieFile
  , emptyAnalysis
  , allDeclarations

    -- ** Reachability
  , Root(..)
  , reachable

    -- * Declarations
  , Declaration(..)
  )
   where

-- algebraic-graphs
import Algebra.Graph ( Graph, edge, empty, overlay, vertex, vertexList )
import Algebra.Graph.ToGraph ( dfs )

-- base
import Control.Applicative ( Alternative )
import Control.Monad ( guard, msum, when )
import Data.Foldable ( for_, traverse_ )
import Data.List ( intercalate )
import Data.Monoid ( First( First ) )
import GHC.Generics ( Generic )
import Prelude hiding ( span )

-- containers
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Sequence ( Seq )
import Data.Set ( Set )
import qualified Data.Set as Set

-- generic-lens
import Data.Generics.Labels ()

-- ghc
import GHC.Data.FastString ( unpackFS )
import GHC.Types.Avail
  ( AvailInfo( Avail, AvailTC )
  , GreName( NormalGreName, FieldGreName )
  )
import GHC.Types.FieldLabel ( FieldLabel( FieldLabel, flSelector ) )
import GHC.Iface.Ext.Types
  ( BindType( RegularBind )
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl, ClassTyDecl )
  , DeclType( DataDec, ClassDec, ConDec )
  , HieAST( Node, nodeChildren, nodeSpan, sourcedNodeInfo )
  , HieASTs( HieASTs )
  , HieFile( HieFile, hie_asts, hie_exports, hie_module, hie_hs_file )
  , IdentifierDetails( IdentifierDetails, identInfo )
  , NodeAnnotation( NodeAnnotation, nodeAnnotType )
  , NodeInfo( nodeIdentifiers, nodeAnnotations )
  , Scope( ModuleScope )
  , getSourcedNodeInfo
  )
import GHC.Unit.Module ( Module, moduleStableString )
import GHC.Types.Name
  ( Name, nameModule_maybe, nameOccName
  , OccName
  , isDataOcc
  , isDataSymOcc
  , isTcOcc
  , isTvOcc
  , isVarOcc
  , occNameString
  )
import GHC.Types.SrcLoc ( RealSrcSpan, realSrcSpanEnd, realSrcSpanStart )

-- lens
import Control.Lens ( (%=) )

-- mtl
import Control.Monad.State.Class ( MonadState )

-- transformers
import Control.Monad.Trans.Maybe ( runMaybeT )


data Declaration =
  Declaration
    { declModule :: Module
      -- ^ The module this declaration occurs in.
    , declOccName :: OccName
      -- ^ The symbol name of a declaration.
    }
  deriving
    ( Eq, Ord )


instance Show Declaration where
  show =
    declarationStableName


declarationStableName :: Declaration -> String
declarationStableName Declaration { declModule, declOccName } =
  let
    namespace
      | isVarOcc declOccName     = "var"
      | isTvOcc declOccName      = "tv"
      | isTcOcc declOccName      = "tc"
      | isDataOcc declOccName    = "data"
      | isDataSymOcc declOccName = "dataSym"
      | otherwise                = "unknown"

    in
    intercalate "$" [ namespace, moduleStableString declModule, "$", occNameString declOccName ]


-- | All information maintained by 'analyseHieFile'.
data Analysis =
  Analysis
    { dependencyGraph :: Graph Declaration
      -- ^ A graph between declarations, capturing dependencies.
    , declarationSites :: Map Declaration ( Set RealSrcSpan )
      -- ^ A partial mapping between declarations and their definition site.
      -- This Map is partial as we don't always know where a Declaration was
      -- defined (e.g., it may come from a package without source code).
      -- We capture a set of spans, because a declaration may be defined in
      -- multiple locations, e.g., a type signature for a function separate
      -- from its definition.
    , implicitRoots :: Set Declaration
      -- ^ The Set of all Declarations that are always reachable. This is used
      -- to capture knowledge not yet modelled in weeder, such as instance
      -- declarations depending on top-level functions.
    , exports :: Map Module ( Set Declaration )
      -- ^ All exports for a given module.
    , modulePaths :: Map Module FilePath
      -- ^ A map from modules to the file path to the .hs file defining them.
    }
  deriving
    ( Generic )


-- | The empty analysis - the result of analysing zero @.hie@ files.
emptyAnalysis :: Analysis
emptyAnalysis = Analysis empty mempty mempty mempty mempty


-- | A root for reachability analysis.
data Root
  = -- | A given declaration is a root.
    DeclarationRoot Declaration
  | -- | All exported declarations in a module are roots.
    ModuleRoot Module
  deriving
    ( Eq, Ord )


-- | Determine the set of all declaration reachable from a set of roots.
reachable :: Analysis -> Set Root -> Set Declaration
reachable Analysis{ dependencyGraph, exports } roots =
  Set.fromList ( dfs ( foldMap rootDeclarations roots ) dependencyGraph )

  where

    rootDeclarations = \case
      DeclarationRoot d -> [ d ]
      ModuleRoot m -> foldMap Set.toList ( Map.lookup m exports )


-- | The set of all known declarations, including usages.
allDeclarations :: Analysis -> Set Declaration
allDeclarations Analysis{ dependencyGraph } =
  Set.fromList ( vertexList dependencyGraph )


-- | Incrementally update 'Analysis' with information in a 'HieFile'.
analyseHieFile :: MonadState Analysis m => HieFile -> m ()
analyseHieFile HieFile{ hie_asts = HieASTs hieASTs, hie_exports, hie_module, hie_hs_file } = do
  #modulePaths %= Map.insert hie_module hie_hs_file

  for_ hieASTs \ast -> do
    addAllDeclarations ast
    topLevelAnalysis ast

  for_ hie_exports ( analyseExport hie_module )


analyseExport :: MonadState Analysis m => Module -> AvailInfo -> m ()
analyseExport m = \case
  Avail (NormalGreName name) ->
    traverse_ addExport $ nameToDeclaration name

  Avail (FieldGreName (FieldLabel{ flSelector })) ->
    traverse_ addExport $ nameToDeclaration flSelector

  AvailTC name pieces -> do
    for_ ( nameToDeclaration name ) addExport

    for_ pieces \case
      NormalGreName name ->
        traverse_ addExport $ nameToDeclaration name

      FieldGreName (FieldLabel{ flSelector }) ->
        traverse_ addExport $ nameToDeclaration flSelector

  where

    addExport :: MonadState Analysis m => Declaration -> m ()
    addExport d = #exports %= Map.insertWith (<>) m ( Set.singleton d )


-- | @addDependency x y@ adds the information that @x@ depends on @y@.
addDependency :: MonadState Analysis m => Declaration -> Declaration -> m ()
addDependency x y =
  #dependencyGraph %= overlay ( edge x y )


addImplicitRoot :: MonadState Analysis m => Declaration -> m ()
addImplicitRoot x =
  #implicitRoots %= Set.insert x


define :: MonadState Analysis m => Declaration -> RealSrcSpan -> m ()
define decl span =
  when ( realSrcSpanStart span /= realSrcSpanEnd span ) do
    #declarationSites %= Map.insertWith Set.union decl ( Set.singleton span )
    #dependencyGraph %= overlay ( vertex decl )


addDeclaration :: MonadState Analysis m => Declaration -> m ()
addDeclaration decl =
  #dependencyGraph %= overlay ( vertex decl )


-- | Try and add vertices for all declarations in an AST - both
-- those declared here, and those referred to from here.
addAllDeclarations :: ( MonadState Analysis m ) => HieAST a -> m ()
addAllDeclarations n@Node{ nodeChildren } = do
  for_ ( findIdentifiers ( const True ) n ) addDeclaration

  for_ nodeChildren addAllDeclarations


topLevelAnalysis :: MonadState Analysis m => HieAST a -> m ()
topLevelAnalysis n@Node{ nodeChildren } = do
  analysed <-
    runMaybeT
      ( msum
          [
          --   analyseStandaloneDeriving n
          -- ,
            analyseInstanceDeclaration n
          , analyseBinding n
          , analyseRewriteRule n
          , analyseClassDeclaration n
          , analyseDataDeclaration n
          , analysePatternSynonyms n
          ]
      )

  case analysed of
    Nothing ->
      -- We didn't find a top level declaration here, check all this nodes
      -- children.
      traverse_ topLevelAnalysis nodeChildren

    Just () ->
      -- Top level analysis succeeded, there's nothing more to do for this node.
      return ()


analyseBinding :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseBinding n@Node{ nodeSpan, sourcedNodeInfo } = do
  let bindAnns = Set.fromList [("FunBind", "HsBindLR"), ("PatBind", "HsBindLR")]
  guard $ any (not . Set.disjoint bindAnns . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( findDeclarations n ) \d -> do
    define d nodeSpan

    for_ ( uses n ) $ addDependency d


analyseRewriteRule :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseRewriteRule n@Node{ sourcedNodeInfo } = do
  guard $ any (Set.member ("HsRule", "RuleDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( uses n ) addImplicitRoot


analyseInstanceDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseInstanceDeclaration n@Node{ sourcedNodeInfo } = do
  guard $ any (Set.member ("ClsInstD", "InstDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  traverse_ addImplicitRoot ( uses n )


analyseClassDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseClassDeclaration n@Node{ sourcedNodeInfo } = do
  guard $ any (Set.member ("ClassDecl", "TyClDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( findIdentifiers isClassDeclaration n ) $
    for_ ( findIdentifiers ( const True ) n ) . addDependency

  where

    isClassDeclaration =
      not . Set.null . Set.filter \case
        Decl ClassDec _ ->
          True

        _ ->
          False


analyseDataDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseDataDeclaration n@Node{ sourcedNodeInfo } = do
  guard $ any (Set.member ("DataDecl", "TyClDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_
    ( foldMap
        ( First . Just )
        ( findIdentifiers ( any isDataDec ) n )
    )
    \dataTypeName ->
      for_ ( constructors n ) \constructor ->
        for_ ( foldMap ( First . Just ) ( findIdentifiers ( any isConDec ) constructor ) ) \conDec -> do
          addDependency conDec dataTypeName

          for_ ( uses constructor ) ( addDependency conDec )

  where

    isDataDec = \case
      Decl DataDec _ -> True
      _              -> False

    isConDec = \case
      Decl ConDec _ -> True
      _             -> False


constructors :: HieAST a -> Seq ( HieAST a )
constructors n@Node{ nodeChildren, sourcedNodeInfo } =
  if any (any ( ("ConDecl" ==) . unpackFS . nodeAnnotType) . nodeAnnotations) (getSourcedNodeInfo sourcedNodeInfo) then
    pure n

  else
    foldMap constructors nodeChildren

analysePatternSynonyms :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analysePatternSynonyms n@Node{ sourcedNodeInfo } = do
  guard $ any (Set.member ("PatSynBind", "HsBindLR") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( findDeclarations n ) $ for_ ( uses n ) . addDependency

findDeclarations :: HieAST a -> Seq Declaration
findDeclarations =
  findIdentifiers
    (   not
      . Set.null
      . Set.filter \case
          -- Things that count as declarations
          ValBind RegularBind ModuleScope _ -> True
          PatternBind ModuleScope _ _       -> True
          Decl _ _                          -> True
          TyDecl                            -> True
          ClassTyDecl{}                     -> True

          -- Anything else is not a declaration
          _ -> False
    )


findIdentifiers
  :: ( Set ContextInfo -> Bool )
  -> HieAST a
  -> Seq Declaration
findIdentifiers f Node{ sourcedNodeInfo, nodeChildren } =
     foldMap
       ( \case
           ( Left _, _ ) ->
             mempty

           ( Right name, IdentifierDetails{ identInfo } ) ->
             if f identInfo then
               foldMap pure ( nameToDeclaration name )

             else
               mempty
           )
       (foldMap (Map.toList . nodeIdentifiers) (getSourcedNodeInfo sourcedNodeInfo))
  <> foldMap ( findIdentifiers f ) nodeChildren


uses :: HieAST a -> Set Declaration
uses =
    foldMap Set.singleton
  . findIdentifiers \identInfo -> Use `Set.member` identInfo


nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <- nameModule_maybe name
  return Declaration { declModule = m, declOccName = nameOccName name }


unNodeAnnotation :: NodeAnnotation -> (String, String)
unNodeAnnotation (NodeAnnotation x y) = (unpackFS x, unpackFS y)
