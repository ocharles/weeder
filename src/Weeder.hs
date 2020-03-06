{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Weeder
  ( Declaration( Declaration, declModule, declOccName )
  , Root(..)
  , allDeclarations
  , modulePaths
  , analyseHieFile
  , declarationSites
  , declarationStableName
  , dependencyGraph
  , emptyAnalysis
  , implicitRoots
  , reachable
  , nameToDeclaration
  )
  where

import "algebraic-graphs" Algebra.Graph ( Graph, edge, empty, overlay, vertex, vertexList )
import "algebraic-graphs" Algebra.Graph.ToGraph ( dfs )

import "base" Control.Applicative ( Alternative )
import "base" Control.Monad ( guard, msum, when )
import "base" Data.Foldable ( for_, traverse_ )
import "base" Data.List ( intercalate )
import "base" Data.Monoid ( First( First ) )
import "base" GHC.Generics ( Generic )
import "base" Prelude hiding ( span )

import "containers" Data.Map.Strict ( Map )
import qualified "containers" Data.Map.Strict as Map
import "containers" Data.Sequence ( Seq )
import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as Set

import "ghc" Avail ( AvailInfo( Avail, AvailTC ) )
import "ghc" FieldLabel ( FieldLbl( FieldLabel, flSelector ) )
import "ghc" HieTypes
  ( BindType( RegularBind )
  , DeclType( DataDec, ClassDec, ConDec )
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl, ClassTyDecl )
  , HieAST( Node, nodeInfo, nodeChildren, nodeSpan )
  , HieASTs( HieASTs )
  , HieFile( HieFile, hie_asts, hie_exports, hie_module, hie_hs_file )
  , IdentifierDetails( IdentifierDetails, identInfo )
  , NodeInfo( NodeInfo, nodeIdentifiers, nodeAnnotations )
  , Scope( ModuleScope )
  )
import "ghc" Module ( Module, moduleStableString )
import "ghc" Name ( Name, nameOccName, nameModule_maybe )
import "ghc" OccName
  ( OccName
  , isDataOcc
  , isDataSymOcc
  , isTcOcc
  , isTvOcc
  , isVarOcc
  , occNameString
  )
import "ghc" SrcLoc ( RealSrcSpan, realSrcSpanStart, realSrcSpanEnd )

import "generic-lens" Data.Generics.Labels ()

import "lens" Control.Lens ( (%=) )

import "mtl" Control.Monad.State.Class ( MonadState )

import "transformers" Control.Monad.Trans.Maybe ( runMaybeT )


data Declaration =
  Declaration
    { declModule :: Module
    , declOccName :: OccName
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


emptyAnalysis :: Analysis
emptyAnalysis =
  Analysis empty mempty mempty mempty mempty


data Root
  = -- | A given declaration is a root
    DeclarationRoot Declaration
  | -- | All exported declarations in a module are roots
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


analyseHieFile :: MonadState Analysis m => HieFile -> m ()
analyseHieFile HieFile{ hie_asts = HieASTs hieASTs, hie_exports, hie_module, hie_hs_file } = do
  #modulePaths %= Map.insert hie_module hie_hs_file

  for_ hieASTs \ast -> do
    addAllDeclarations ast
    topLevelAnalysis ast

  for_ hie_exports ( analyseExport hie_module )


analyseExport :: MonadState Analysis m => Module -> AvailInfo -> m ()
analyseExport m = \case
  Avail name ->
    for_ ( nameToDeclaration name ) addExport

  AvailTC name pieces fields -> do
    for_ ( nameToDeclaration name ) addExport
    for_ pieces ( traverse_ addExport . nameToDeclaration )
    for_ fields \FieldLabel{ flSelector } -> for_ ( nameToDeclaration flSelector ) addExport

  where

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


-- | Try and analyse binding-like nodes. This includes function bindings,
-- type signatures, pattern bindings and type synonyms.
analyseBinding :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseBinding n@Node{ nodeSpan, nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard
   ( or
       [ ( "TypeSig", "Sig" ) `Set.member` nodeAnnotations
       , ( "FunBind", "HsBindLR" ) `Set.member` nodeAnnotations
       -- , ( "PatBind", "HsBindLR" ) `Set.member` nodeAnnotations
       -- , ( "SynDecl", "TyClDecl" ) `Set.member` nodeAnnotations
       ]
   )

  for_ ( findDeclarations n ) \d -> do
    define d nodeSpan

    for_ ( uses n ) \use ->
      addDependency d use


analyseRewriteRule :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseRewriteRule n@Node{ nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "HsRule", "RuleDecl" ) `Set.member` nodeAnnotations )

  for_ ( uses n ) addImplicitRoot


analyseInstanceDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseInstanceDeclaration n@Node{ nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "ClsInstD", "InstDecl" ) `Set.member` nodeAnnotations )

  traverse_ addImplicitRoot ( uses n )


analyseClassDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseClassDeclaration n@Node{ nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "ClassDecl", "TyClDecl" ) `Set.member` nodeAnnotations )

  for_ ( findIdentifiers isClassDeclaration n ) \d ->
    for_ ( findIdentifiers ( const True ) n ) ( addDependency d )

  where

    isClassDeclaration =
      not . Set.null . Set.filter \case
        Decl ClassDec _ ->
          True

        _ ->
          False


analyseDataDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseDataDeclaration n@Node { nodeSpan, nodeInfo = NodeInfo{ nodeAnnotations } } = do
  guard ( ( "DataDecl", "TyClDecl" ) `Set.member` nodeAnnotations )

  for_
    ( foldMap
        ( First . Just )
        ( findIdentifiers ( any isDataDec ) n )
    )
    \dataTypeName -> do
      define dataTypeName nodeSpan

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
constructors n@Node { nodeChildren, nodeInfo = NodeInfo{ nodeAnnotations } } =
  if any ( \( _, t ) -> t == "ConDecl" ) nodeAnnotations then
    pure n

  else
    foldMap constructors nodeChildren


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
findIdentifiers f Node{ nodeInfo = NodeInfo{ nodeIdentifiers }, nodeChildren } =
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

       ( Map.toList nodeIdentifiers )
  <> foldMap ( findIdentifiers f ) nodeChildren


uses :: HieAST a -> Set Declaration
uses =
    foldMap Set.singleton
  . findIdentifiers \identInfo -> Use `Set.member` identInfo



nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <- nameModule_maybe name
  return Declaration { declModule = m, declOccName = nameOccName name }
