{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language ViewPatterns #-}

module Weeder
  ( -- * Analysis
    Analysis(..)
  , analyseEvidenceUses
  , analyseHieFile
  , emptyAnalysis
  , EvidenceResolution(..)
  , outputableDeclarations
  , localDeclarations

    -- ** Reachability
  , Root(..)
  , reachable

    -- * Declarations
  , Declaration(..)
  )
   where

-- algebraic-graphs
import Algebra.Graph ( Graph, edge, empty, overlay, vertex, stars, star, overlays, vertexSet )
import Algebra.Graph.ToGraph ( dfs )

-- base
import Control.Applicative ( Alternative )
import Control.Monad ( guard, msum, when, unless, mzero )
import Data.Traversable ( for )
import Data.Maybe ( mapMaybe )
import Data.Foldable ( foldl', for_, traverse_, toList )
import Data.List ( intercalate, find )
import Data.Monoid ( First( First ), getFirst )
import GHC.Generics ( Generic )
import Prelude hiding ( span )

-- containers
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Sequence ( Seq )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Graph ( flattenSCC, stronglyConnComp )
import Data.Tree (Tree)
import qualified Data.Tree as Tree

-- generic-lens
import Data.Generics.Labels ()

-- ghc
import GHC.Types.Avail ( AvailInfo, availName, availNames )
import GHC.Data.FastString ( unpackFS )
import GHC.Iface.Ext.Types
  ( BindType( RegularBind )
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl, ClassTyDecl, EvidenceVarBind, RecField )
  , DeclType( DataDec, ClassDec, ConDec, SynDec, FamDec )
  , EvVarSource ( EvInstBind, cls, EvLetBind )
  , HieAST( Node, nodeChildren, nodeSpan, sourcedNodeInfo )
  , HieASTs( HieASTs )
  , HieFile( HieFile, hie_asts, hie_exports, hie_module, hie_hs_file, hie_types )
  , HieType( HTyVarTy, HAppTy, HTyConApp, HForAllTy, HFunTy, HQualTy, HLitTy, HCastTy, HCoercionTy )
  , HieArgs( HieArgs )
  , HieTypeFix( Roll )
  , IdentifierDetails( IdentifierDetails, identInfo, identType )
  , NodeAnnotation( NodeAnnotation, nodeAnnotType )
  , NodeInfo( nodeIdentifiers, nodeAnnotations )
  , Scope( ModuleScope )
  , RecFieldContext ( RecFieldOcc )
  , TypeIndex
  , getSourcedNodeInfo
  , getEvBindDeps
  )
import GHC.Iface.Ext.Utils
  ( RefMap
  , generateReferencesMap
  , isEvidenceBind
  , findEvidenceUse
  , hieTypeToIface
  , recoverFullType
  )
import GHC.Unit.Module ( Module, moduleStableString )
import GHC.Utils.Outputable ( defaultSDocContext, showSDocOneLine )
import GHC.Iface.Type
  ( ShowForAllFlag (ShowForAllWhen)
  , pprIfaceSigmaType
  , IfaceTyCon (IfaceTyCon, ifaceTyConName)
  )
import GHC.Types.Name
  ( Name, nameModule_maybe, nameOccName, isExternalName
  , OccName
  , isDataOcc
  , isDataSymOcc
  , isTcOcc
  , isTvOcc
  , isVarOcc
  , occNameString
  )
import GHC.Types.SrcLoc ( RealSrcSpan, realSrcSpanEnd, realSrcSpanStart, srcLocLine, srcLocCol )

-- lens
import Control.Lens ( (%=), (.=) )

-- mtl
import Control.Monad.State.Class ( MonadState )
import Control.Monad.State.Strict ( get )
import Control.Monad.Reader.Class ( MonadReader, asks )

-- parallel
import Control.Parallel.Strategies ( NFData )

-- transformers
import Control.Monad.Trans.Maybe ( runMaybeT )
import Control.Monad.Trans.Reader ( runReaderT )

-- weeder
import Weeder.Config ( Config, ConfigType( Config, typeClassRoots, unusedTypes ) )


data Declaration =
  Declaration
    { declModule :: Module
      -- ^ The module this declaration occurs in.
    , declOccName :: OccName
      -- ^ The symbol name of a declaration.
    }
  deriving
    ( Eq, Ord, Generic, NFData )


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
    , declarationSites :: Map Declaration (Set (Int, Int))
      -- ^ A partial mapping between declarations and their line numbers.
      -- This Map is partial as we don't always know where a Declaration was
      -- defined (e.g., it may come from a package without source code).
      -- We capture a set of sites, because a declaration may be defined in
      -- multiple locations, e.g., a type signature for a function separate
      -- from its definition.
    , implicitRoots :: Set Root
      -- ^ Stores information on Declarations that may be automatically marked
      -- as always reachable. This is used, for example, to capture knowledge
      -- not yet modelled in weeder, or to mark all instances of a class as
      -- roots.
    , exports :: Map Module ( Set Declaration )
      -- ^ All exports for a given module.
    , modulePaths :: Map Module FilePath
      -- ^ A map from modules to the file path to the .hs file defining them.
    , prettyPrintedType :: Map Declaration String
      -- ^ Used to match against the types of instances and to replace the
      -- appearance of declarations in the output
    , requestedEvidence :: Map Declaration (Set Name)
      -- ^ Map from declarations to the names containing evidence uses that
      -- should be followed and treated as dependencies of the declaration.
      -- Always empty in the 'Analysis' that 'analyseHieFile' produces, as it
      -- resolves these against the .hie file being analysed.
    , pendingEvidence :: Map Declaration (Set Declaration)
      -- ^ Evidence uses that are not bound in the module that uses them,
      -- keyed by the declaration that uses them. These are resolved through
      -- 'evidenceExports' by 'analyseEvidenceUses' once all .hie files have
      -- been analysed.
    , evidenceExports :: Map Declaration EvidenceResolution
      -- ^ What each module level evidence binding resolves to in its defining
      -- module, for resolving 'pendingEvidence' in other modules.
    }
  deriving
    ( Generic, NFData )


instance Semigroup Analysis where
  (<>) (Analysis a1 b1 c1 d1 e1 f1 g1 h1 i1) (Analysis a2 b2 c2 d2 e2 f2 g2 h2 i2)=
    Analysis (a1 `overlay` a2) (Map.unionWith (<>) b1 b2) (c1 <> c2) (Map.unionWith (<>) d1 d2) (e1 <> e2) (f1 <> f2) (Map.unionWith (<>) g1 g2) (Map.unionWith (<>) h1 h2) (Map.unionWith (<>) i1 i2)


-- | What an evidence variable resolves to using the references of a single
-- module.
data EvidenceResolution =
  EvidenceResolution
    { resolvedInstances :: !( Set Declaration )
      -- ^ Instance bindings the evidence was traced to, and any module level
      -- evidence let bindings on the way.
    , externalEvidence :: !( Set Declaration )
      -- ^ Evidence bound in another module, which is traced further through
      -- 'evidenceExports'.
    }
  deriving
    ( Generic, NFData )


instance Semigroup EvidenceResolution where
  EvidenceResolution a1 b1 <> EvidenceResolution a2 b2 =
    EvidenceResolution (a1 <> a2) (b1 <> b2)


instance Monoid EvidenceResolution where
  mempty = EvidenceResolution mempty mempty


instance Monoid Analysis where
  mempty = emptyAnalysis


data AnalysisInfo =
  AnalysisInfo
    { currentHieFile :: HieFile
    , weederConfig :: Config
    }


-- | The empty analysis - the result of analysing zero @.hie@ files.
emptyAnalysis :: Analysis
emptyAnalysis = Analysis empty mempty mempty mempty mempty mempty mempty mempty mempty


-- | A root for reachability analysis.
data Root
  = -- | A given declaration is a root.
    DeclarationRoot Declaration
  | -- | We store extra information for instances in order to be able
    -- to specify e.g. all instances of a class as roots.
    InstanceRoot
      Declaration -- ^ Declaration of the instance
      Declaration -- ^ Declaration of the parent class
  | -- | All exported declarations in a module are roots.
    ModuleRoot Module
  deriving
    ( Eq, Ord, Generic, NFData )


-- | Determine the set of all declaration reachable from a set of roots.
reachable :: Analysis -> Set Root -> Set Declaration
reachable Analysis{ dependencyGraph, exports } roots =
  Set.fromList ( dfs dependencyGraph ( foldMap rootDeclarations roots ) )

  where

    rootDeclarations = \case
      DeclarationRoot d -> [ d ]
      InstanceRoot d _ -> [ d ] -- filter InstanceRoots in `Main.hs`
      ModuleRoot m -> foldMap Set.toList ( Map.lookup m exports )

-- | The set of all declarations that could possibly
-- appear in the output.
outputableDeclarations :: Analysis -> Set Declaration
outputableDeclarations Analysis{ declarationSites } =
  Map.keysSet declarationSites


-- | Every declaration defined in one of the analysed modules, including ones
-- that never appear in the output (such as types and constructors when
-- @unused-types@ is disabled). Used to decide whether a configured root matches
-- any real identifier in the project, as opposed to whether it keeps anything
-- alive. Declarations from external packages — which appear in the graph only
-- as dependency targets — are excluded, so a root that matches only an external
-- symbol is still reported as unused.
localDeclarations :: Analysis -> Set Declaration
localDeclarations Analysis{ dependencyGraph, modulePaths } =
  Set.filter (\d -> declModule d `Map.member` modulePaths) (vertexSet dependencyGraph)


-- Generate an initial graph of the current HieFile.
initialGraph :: AnalysisInfo -> Graph Declaration
initialGraph info =
  let hf@HieFile{ hie_asts = HieASTs hieAsts } = currentHieFile info
      Config{ unusedTypes } = weederConfig info
      asts = Map.elems hieAsts
      decls = concatMap (toList . findIdentifiers' (const True)) asts
  in if unusedTypes
    then stars do
      (d, IdentifierDetails{identType}, _) <- decls
      t <- maybe mzero pure identType
      let ns = Set.toList $ typeToNames (lookupType hf t)
          ds = mapMaybe nameToDeclaration ns
      guard $ not (null ds)
      pure (d, ds)
    else mempty


-- | Incrementally update 'Analysis' with information in a 'HieFile'.
analyseHieFile :: (MonadState Analysis m) => Config -> HieFile -> m ()
analyseHieFile weederConfig hieFile =
  let info = AnalysisInfo hieFile weederConfig
   in runReaderT analyseHieFile' info


analyseHieFile' :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => m ()
analyseHieFile' = do
  HieFile{ hie_asts = HieASTs hieASTs, hie_exports, hie_module, hie_hs_file } <- asks currentHieFile
  #modulePaths %= Map.insert hie_module hie_hs_file

  g <- asks initialGraph
  #dependencyGraph %= overlay g

  for_ hieASTs topLevelAnalysis

  for_ hie_exports ( analyseExport hie_module )

  Config{ typeClassRoots } <- asks weederConfig
  unless typeClassRoots resolveFileEvidence


lookupType :: HieFile -> TypeIndex -> HieTypeFix
lookupType hf t = recoverFullType t $ hie_types hf


lookupPprType :: MonadReader AnalysisInfo m => TypeIndex -> m String
lookupPprType t = do
  hf <- asks currentHieFile
  pure . renderType $ lookupType hf t

  where

    renderType = showSDocOneLine defaultSDocContext . pprIfaceSigmaType ShowForAllWhen . hieTypeToIface


-- | Names mentioned within the type.
typeToNames :: HieTypeFix -> Set Name
typeToNames (Roll t) = case t of
  HTyVarTy n -> Set.singleton n

  HAppTy a (HieArgs args) ->
    typeToNames a <> hieArgsTypes args

  HTyConApp (IfaceTyCon{ifaceTyConName}) (HieArgs args) ->
    Set.singleton ifaceTyConName <> hieArgsTypes args

  HForAllTy _ a -> typeToNames a

  HFunTy _mult b c ->
    typeToNames b <> typeToNames c

  HQualTy a b ->
    typeToNames a <> typeToNames b

  HLitTy _ -> mempty

  HCastTy a -> typeToNames a

  HCoercionTy -> mempty

  where

    hieArgsTypes :: [(Bool, HieTypeFix)] -> Set Name
    hieArgsTypes = foldMap (typeToNames . snd) . filter fst


analyseExport :: MonadState Analysis m => Module -> AvailInfo -> m ()
analyseExport m a =
  traverse_ (traverse_ addExport . nameToDeclaration) (availName a : availNames a)
  where

    addExport :: MonadState Analysis m => Declaration -> m ()
    addExport d = #exports %= Map.insertWith (<>) m ( Set.singleton d )


-- | @addDependency x y@ adds the information that @x@ depends on @y@.
addDependency :: MonadState Analysis m => Declaration -> Declaration -> m ()
addDependency x y =
  #dependencyGraph %= overlay ( edge x y )


addImplicitRoot :: MonadState Analysis m => Declaration -> m ()
addImplicitRoot x =
  #implicitRoots %= Set.insert (DeclarationRoot x)


addInstanceRoot :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => Declaration -> TypeIndex -> Name -> m ()
addInstanceRoot x t cls = do
  for_ (nameToDeclaration cls) \cls' ->
    #implicitRoots %= Set.insert (InstanceRoot x cls')

  -- since instances will not appear in the output if typeClassRoots is True
  Config{ typeClassRoots } <- asks weederConfig
  unless typeClassRoots $ do
    str <- lookupPprType t
    #prettyPrintedType %= Map.insert x str


define :: MonadState Analysis m => Declaration -> RealSrcSpan -> m ()
define decl span =
  when ( realSrcSpanStart span /= realSrcSpanEnd span ) do
    let start = realSrcSpanStart span
    let loc = (srcLocLine start, srcLocCol start)
    #declarationSites %= Map.insertWith Set.union decl ( Set.singleton loc)
    #dependencyGraph %= overlay ( vertex decl )


topLevelAnalysis :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
topLevelAnalysis n@Node{ nodeChildren } = do
  Config{ unusedTypes } <- asks weederConfig
  analysed <-
    runMaybeT
      ( msum $
          [
            analyseStandaloneDeriving n
          , analyseInstanceDeclaration n
          , analyseBinding n
          , analyseRewriteRule n
          , analyseClassDeclaration n
          , analyseDataDeclaration n
          , analysePatternSynonyms n
          ] ++ if unusedTypes then
          [ analyseTypeSynonym n
          , analyseFamilyDeclaration n
          , analyseFamilyInstance n
          , analyseTypeSignature n
          ] else []
      )

  case analysed of
    Nothing ->
      -- We didn't find a top level declaration here, check all this nodes
      -- children.
      traverse_ topLevelAnalysis nodeChildren

    Just () ->
      -- Top level analysis succeeded, there's nothing more to do for this node.
      return ()


annsContain :: HieAST a -> (String, String) -> Bool
annsContain Node{ sourcedNodeInfo } ann =
  any (Set.member ann . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo


analyseBinding :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST a -> m ()
analyseBinding n@Node{ nodeSpan } = do
  let bindAnns = Set.fromList [("FunBind", "HsBindLR"), ("PatBind", "HsBindLR")]
  guard $ any (annsContain n) bindAnns

  for_ ( findDeclarations n ) \d -> do
    define d nodeSpan

    requestEvidence n d

    for_ ( uses n ) $ addDependency d


analyseRewriteRule :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseRewriteRule n = do
  guard $ annsContain n ("HsRule", "RuleDecl")

  for_ ( uses n ) addImplicitRoot


analyseInstanceDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
analyseInstanceDeclaration n@Node{ nodeSpan } = do
  guard $ annsContain n ("ClsInstD", "InstDecl")

  for_ ( findEvInstBinds n ) \(d, cs, ids, _) -> do
    -- This makes instance declarations show up in
    -- the output if type-class-roots is set to False.
    define d nodeSpan

    requestEvidence n d

    for_ ( uses n ) $ addDependency d

    case identType ids of
      Just t -> for_ cs (addInstanceRoot d t)
      Nothing -> pure ()


analyseClassDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST a -> m ()
analyseClassDeclaration n@Node{ nodeSpan } = do
  guard $ annsContain n ("ClassDecl", "TyClDecl")

  for_ ( findIdentifiers isClassDeclaration n ) $ \d -> do
    define d nodeSpan

    requestEvidence n d

    (for_ ( findIdentifiers ( const True ) n ) . addDependency) d

  where

    isClassDeclaration =
      not . Set.null . Set.filter \case
        Decl ClassDec _ ->
          True

        _ ->
          False


analyseDataDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
analyseDataDeclaration n = do
  guard $ annsContain n ("DataDecl", "TyClDecl")

  Config{ unusedTypes } <- asks weederConfig

  for_
    ( foldMap
        ( First . Just )
        ( findIdentifiers ( any isDataDec ) n )
    )
    \dataTypeName -> do
      when unusedTypes $
        define dataTypeName (nodeSpan n)

      -- Without connecting constructors to the data declaration TypeAliasGADT.hs
      -- fails with a false positive for A
      conDecs <- for ( constructors n ) \constructor ->
        for ( foldMap ( First . Just ) ( findIdentifiers ( any isConDec ) constructor ) ) \conDec -> do
          addDependency conDec dataTypeName
          pure conDec

      -- To keep acyclicity in record declarations
      let isDependent d = Just d `elem` fmap getFirst conDecs

      for_ ( uses n ) (\d -> unless (isDependent d) $ addDependency dataTypeName d)

  for_ ( derivedInstances n ) \(d, cs, ids, ast) -> do
    define d (nodeSpan ast)

    requestEvidence ast d

    for_ ( uses ast ) $ addDependency d

    case identType ids of
      Just t -> for_ cs (addInstanceRoot d t)
      Nothing -> pure ()

  where

    isDataDec = \case
      Decl DataDec _ -> True
      _              -> False

    isConDec = \case
      Decl ConDec _ -> True
      _             -> False


constructors :: HieAST a -> Seq ( HieAST a )
constructors = findNodeTypes "ConDecl"


derivedInstances :: HieAST a -> Seq (Declaration, Set Name, IdentifierDetails a, HieAST a)
derivedInstances n = findNodeTypes "HsDerivingClause" n >>= findEvInstBinds


findNodeTypes :: String -> HieAST a -> Seq ( HieAST a )
findNodeTypes t n@Node{ nodeChildren, sourcedNodeInfo } =
  if any (any ( (t ==) . unpackFS . nodeAnnotType) . nodeAnnotations) (getSourcedNodeInfo sourcedNodeInfo) then
    pure n

  else
    foldMap (findNodeTypes t) nodeChildren


analyseStandaloneDeriving :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
analyseStandaloneDeriving n@Node{ nodeSpan } = do
  guard $ annsContain n ("DerivDecl", "DerivDecl")

  for_ (findEvInstBinds n) \(d, cs, ids, _) -> do
    define d nodeSpan

    requestEvidence n d

    for_ (uses n) $ addDependency d

    case identType ids of
      Just t -> for_ cs (addInstanceRoot d t)
      Nothing -> pure ()


analyseTypeSynonym :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseTypeSynonym n@Node{ nodeSpan } = do
  guard $ annsContain n ("SynDecl", "TyClDecl")

  for_ ( findIdentifiers isTypeSynonym n ) $ \d -> do
    define d nodeSpan

    for_ (uses n) (addDependency d)

  where

    isTypeSynonym =
      any \case
        Decl SynDec _ -> True
        _             -> False


analyseFamilyDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseFamilyDeclaration n@Node{ nodeSpan } = do
  guard $ annsContain n ("FamDecl", "TyClDecl")

  for_ ( findIdentifiers isFamDec n ) $ \d -> do
    define d nodeSpan

    for_ (uses n) (addDependency d)

  where

    isFamDec =
      any \case
        Decl FamDec _ -> True
        _             -> False


analyseFamilyInstance :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseFamilyInstance n = do
  guard $ annsContain n ("TyFamInstD", "InstDecl")

  for_ ( uses n ) addImplicitRoot


analyseTypeSignature :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseTypeSignature n = do
  guard $ annsContain n ("TypeSig", "Sig")

  for_ (findIdentifiers isTypeSigDecl n) $
    for_ ( uses n ) . addDependency

  where

    isTypeSigDecl =
      any \case
        TyDecl -> True
        _      -> False


analysePatternSynonyms :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analysePatternSynonyms n = do
  guard $ annsContain n ("PatSynBind", "HsBindLR")

  for_ ( findDeclarations n ) $ for_ ( uses n ) . addDependency


findEvInstBinds :: HieAST a -> Seq (Declaration, Set Name, IdentifierDetails a, HieAST a)
findEvInstBinds n = (\(d, ids, ast) -> (d, getClassNames ids, ids, ast)) <$>
  findIdentifiers'
    (   not
      . Set.null
      . getEvVarSources
    ) n

  where

    getEvVarSources :: Set ContextInfo -> Set EvVarSource
    getEvVarSources = foldMap (maybe mempty Set.singleton) .
      Set.map \case
        EvidenceVarBind a@EvInstBind{} ModuleScope _ -> Just a
        _ -> Nothing

    getClassNames :: IdentifierDetails a -> Set Name
    getClassNames =
      Set.map cls
      . getEvVarSources
      . identInfo


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
findIdentifiers f = fmap (\(d, _, _) -> d) . findIdentifiers' f


-- | Version of findIdentifiers containing more information,
-- namely the IdentifierDetails of the declaration and the
-- node it was found in.
findIdentifiers'
  :: ( Set ContextInfo -> Bool )
  -> HieAST a
  -> Seq (Declaration, IdentifierDetails a, HieAST a)
findIdentifiers' f n@Node{ sourcedNodeInfo, nodeChildren } =
     foldMap
       (\case
           ( Left _, _ ) ->
             mempty

           ( Right name, ids@IdentifierDetails{ identInfo } ) ->
             if f identInfo then
               (, ids, n) <$> foldMap pure (nameToDeclaration name)

             else
               mempty
           )
       (foldMap (Map.toList . nodeIdentifiers) (getSourcedNodeInfo sourcedNodeInfo))
  <> foldMap ( findIdentifiers' f ) nodeChildren


uses :: HieAST a -> Set Declaration
uses =
    foldMap Set.singleton
  . findIdentifiers (any isUse)

isUse :: ContextInfo -> Bool
isUse = \case
  Use -> True
  -- not RecFieldMatch and RecFieldDecl because they occur under
  -- data declarations, which we do not want to add as dependencies
  -- because that would make the graph no longer acyclic
  -- RecFieldAssign will be most likely accompanied by the constructor
  RecField RecFieldOcc _ -> True
  _ -> False


nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <- nameModule_maybe name
  return Declaration { declModule = m, declOccName = nameOccName name }


unNodeAnnotation :: NodeAnnotation -> (String, String)
unNodeAnnotation (NodeAnnotation x y) = (unpackFS x, unpackFS y)


-- | Add evidence uses found under the given node to 'requestedEvidence'.
requestEvidence :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST a -> Declaration -> m ()
requestEvidence n d = do
  Config{ typeClassRoots } <- asks weederConfig

  -- If type-class-roots flag is set then we don't need to follow
  -- evidence uses as the binding sites will be roots anyway
  unless typeClassRoots $
    #requestedEvidence %= Map.insertWith (<>) d (Set.fromList names)

  where

    names = concat . Tree.flatten $ evidenceUseTree n

    evidenceUseTree :: HieAST a -> Tree [Name]
    evidenceUseTree Node{ sourcedNodeInfo, nodeChildren } = Tree.Node
      { Tree.rootLabel = concatMap (findEvidenceUse . nodeIdentifiers) (getSourcedNodeInfo sourcedNodeInfo)
      , Tree.subForest = map evidenceUseTree nodeChildren
      }


-- | Resolve the evidence uses requested while analysing the current .hie file
-- using only that file's references, and add a dependency on everything they
-- resolve to. Evidence not bound in this module is recorded in
-- 'pendingEvidence', and the module level evidence bound here in
-- 'evidenceExports', so that 'analyseEvidenceUses' can connect the two
-- without keeping every .hie file in memory.
resolveFileEvidence :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => m ()
resolveFileEvidence = do
  HieFile{ hie_asts = HieASTs hieASTs } <- asks currentHieFile
  Analysis{ requestedEvidence } <- get

  let rf = generateReferencesMap ( Map.elems hieASTs )

      boundHere =
        [ n
        | ( Right n, refs ) <- Map.toList rf
        , isExternalName n
        , any ( any isEvidenceBind . identInfo . snd ) refs
        ]

      resolutions =
        resolveEvidenceLocally rf $
          Set.toList ( mconcat ( Map.elems requestedEvidence ) ) ++ boundHere

      resolutionOf n = Map.findWithDefault mempty n resolutions

  for_ ( Map.toList requestedEvidence ) \( d, names ) -> do
    let EvidenceResolution{ resolvedInstances, externalEvidence } = foldMap resolutionOf names
    #dependencyGraph %= overlay ( star d ( Set.toList resolvedInstances ) )
    unless ( Set.null externalEvidence ) $
      #pendingEvidence %= Map.insertWith (<>) d externalEvidence

  for_ boundHere \n ->
    for_ ( nameToDeclaration n ) \d ->
      #evidenceExports %= Map.insert d ( resolutionOf n )

  #requestedEvidence .= mempty


-- | Follow evidence variables back to the instance bindings they use, through
-- any evidence let bindings, using the references of a single module. Adapted
-- from GHC's getEvidenceTree.
resolveEvidenceLocally :: RefMap a -> [ Name ] -> Map Name EvidenceResolution
resolveEvidenceLocally rf vars =
  Data.Foldable.foldl' resolveComponent mempty ( stronglyConnComp nodes )

  where

    -- Recursive dictionaries make the evidence graph cyclic, so we resolve it
    -- a strongly connected component at a time: every variable in a component
    -- reaches the same evidence. 'stronglyConnComp' returns components in
    -- reverse topological order, so the components a component depends on
    -- have always been resolved before it.
    -- Dependencies within the component are not in 'resolved' yet, so they
    -- contribute nothing beyond their own evidence, which is already included.
    resolveComponent resolved component =
      let members = flattenSCC component
          resolution =
            foldMap ( \( r, _, _ ) -> r ) members
              <> foldMap
                   ( \dep -> Map.findWithDefault mempty dep resolved )
                   [ dep | ( _, _, deps ) <- members, dep <- deps ]
      in Data.Foldable.foldl'
           ( \m ( _, var, _ ) -> Map.insert var resolution m )
           resolved
           members

    nodes =
      [ ( ( r, var, deps ), var, deps ) | ( var, ( r, deps ) ) <- Map.toList ( explore mempty vars ) ]

    explore seen [] = seen
    explore seen ( var : rest )
      | Map.member var seen = explore seen rest
      | otherwise =
          let node@( _, deps ) = evidenceNode var
          in explore ( Map.insert var node seen ) ( deps ++ rest )

    -- What a single evidence variable contributes itself, and the evidence
    -- variables it depends on.
    evidenceNode var =
      case Map.lookup ( Right var ) rf >>= find ( any isEvidenceBind . identInfo . snd ) of
        Nothing
          | isExternalName var -> ( mempty{ externalEvidence = declarations var }, [] )
          | otherwise -> ( mempty, [] )
        Just ( _, dets )
          | Just _ <- identType dets -> firstBinding var ( Set.toList ( identInfo dets ) )
          | otherwise -> ( mempty, [] )

    firstBinding _ [] = ( mempty, [] )
    firstBinding var ( det : dets ) =
      case det of
        EvidenceVarBind ( EvLetBind ( getEvBindDeps -> deps ) ) _ _ ->
          ( mempty{ resolvedInstances = declarations var }, deps )
        EvidenceVarBind EvInstBind{} ModuleScope _ ->
          ( mempty{ resolvedInstances = declarations var }, [] )
        _ ->
          firstBinding var dets

    declarations = foldMap Set.singleton . nameToDeclaration


-- | Connect evidence uses listed under 'pendingEvidence' to the instances that
-- the defining modules resolved them to in 'evidenceExports'.
analyseEvidenceUses :: Analysis -> Analysis
analyseEvidenceUses a@Analysis{ pendingEvidence, evidenceExports, dependencyGraph } =
  let resolved =
        Map.fromSet resolve ( mconcat ( Map.elems pendingEvidence ) )

      resolve d = go ( Set.singleton d ) [ d ] mempty

      go _ [] acc = acc
      go seen ( d : ds ) acc =
        case Map.lookup d evidenceExports of
          Nothing -> go seen ds acc
          Just EvidenceResolution{ resolvedInstances, externalEvidence } ->
            let new = externalEvidence Set.\\ seen
            in go ( seen <> new ) ( Set.toList new ++ ds ) ( acc <> resolvedInstances )

      graphs =
        [ star d ( Set.toList ( foldMap ( resolved Map.! ) ds ) )
        | ( d, ds ) <- Map.toList pendingEvidence
        ]

   in a { dependencyGraph = overlays ( dependencyGraph : graphs ) }
