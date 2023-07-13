{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}

module Weeder
  ( -- * Analysis
    Analysis(..)
  , analyseHieFiles
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
import Control.Monad ( guard, msum, when, unless )
import Data.Maybe ( mapMaybe )
import Data.Foldable ( for_, traverse_ )
import Data.Function ( (&) )
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
import Data.Tree (Tree)
import qualified Data.Tree as Tree

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
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl, ClassTyDecl, EvidenceVarBind, RecField )
  , DeclType( DataDec, ClassDec, ConDec, SynDec, FamDec )
  , EvVarSource ( EvInstBind, cls )
  , HieAST( Node, nodeChildren, nodeSpan, sourcedNodeInfo )
  , HieASTs( HieASTs, getAsts )
  , HieFile( HieFile, hie_asts, hie_exports, hie_module, hie_hs_file, hie_types )
  , HieType( HTyVarTy, HAppTy, HTyConApp, HForAllTy, HFunTy, HQualTy, HLitTy, HCastTy, HCoercionTy )
  , HieArgs( HieArgs )
  , HieTypeFix( Roll )
  , IdentifierDetails( IdentifierDetails, identInfo, identType )
  , NodeAnnotation( NodeAnnotation, nodeAnnotType )
  , NodeInfo( nodeIdentifiers, nodeAnnotations )
  , Scope( ModuleScope )
  , RecFieldContext ( RecFieldOcc, RecFieldDecl )
  , TypeIndex
  , getSourcedNodeInfo
  )
import GHC.Iface.Ext.Utils
  ( EvidenceInfo( EvidenceInfo, evidenceVar )
  , RefMap
  , findEvidenceUse
  , getEvidenceTree
  , generateReferencesMap
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
import Control.Monad.Reader.Class ( MonadReader, asks, ask)

-- transformers
import Control.Monad.Trans.Maybe ( runMaybeT )
import Control.Monad.Trans.Reader ( runReaderT )

-- weeder
import Weeder.Config ( Config( Config, typeClassRoots ) )


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
    }
  deriving
    ( Generic )


data AnalysisInfo =
  AnalysisInfo
    { currentHieFile :: HieFile
    , weederConfig :: Config
    , refMap :: RefMap TypeIndex
    }


-- | The empty analysis - the result of analysing zero @.hie@ files.
emptyAnalysis :: Analysis
emptyAnalysis = Analysis empty mempty mempty mempty mempty mempty


-- | A root for reachability analysis.
data Root
  = -- | A given declaration is a root.
    DeclarationRoot Declaration
  | -- | We store extra information for instances in order to be able
    -- to specify e.g. all instances of a class as roots.
    InstanceRoot Declaration
      OccName -- ^ Name of the parent class
  | -- | All exported declarations in a module are roots.
    ModuleRoot Module
  deriving
    ( Eq, Ord )


-- | Determine the set of all declaration reachable from a set of roots.
reachable :: Analysis -> Set Root -> Set Declaration
reachable Analysis{ dependencyGraph, exports } roots =
  Set.fromList ( dfs dependencyGraph ( foldMap rootDeclarations roots ) )

  where

    rootDeclarations = \case
      DeclarationRoot d -> [ d ]
      InstanceRoot d _ -> [ d ] -- filter InstanceRoots in `Main.hs`
      ModuleRoot m -> foldMap Set.toList ( Map.lookup m exports )


-- | The set of all known declarations, including usages.
allDeclarations :: Analysis -> Set Declaration
allDeclarations Analysis{ dependencyGraph } =
  Set.fromList ( vertexList dependencyGraph )


-- | Incrementally update 'Analysis' with information in a 'HieFile'.
analyseHieFile :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => m ()
analyseHieFile = do
  HieFile{ hie_asts = HieASTs hieASTs, hie_exports, hie_module, hie_hs_file } <- asks currentHieFile
  #modulePaths %= Map.insert hie_module hie_hs_file

  for_ hieASTs \ast -> do
    addAllDeclarations ast
    topLevelAnalysis ast

  for_ hie_exports ( analyseExport hie_module )


lookupType :: MonadReader AnalysisInfo m => TypeIndex -> m HieTypeFix
lookupType t = recoverFullType t . hie_types <$> asks currentHieFile


lookupPprType :: MonadReader AnalysisInfo m => TypeIndex -> m String
lookupPprType = fmap renderType . lookupType

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

  HForAllTy _ _ -> mempty

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


-- | Incrementally update 'Analysis' with information in every 'HieFile'.
analyseHieFiles :: (Foldable f, MonadState Analysis m) => Config -> f HieFile -> m ()
analyseHieFiles weederConfig hieFiles = do
  for_ hieFiles \hieFile -> do
    let info = AnalysisInfo hieFile weederConfig rf
    runReaderT analyseHieFile info

  where

    asts = concatMap (Map.elems . getAsts . hie_asts) hieFiles

    rf = generateReferencesMap asts


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
  #implicitRoots %= Set.insert (DeclarationRoot x)


addInstanceRoot :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => Declaration -> TypeIndex -> Name -> m ()
addInstanceRoot x t cls = do
  #implicitRoots %= Set.insert (InstanceRoot x (nameOccName cls))

  -- since instances will not appear in the output if typeClassRoots is True
  Config{ typeClassRoots } <- asks weederConfig
  unless typeClassRoots $ do
    str <- lookupPprType t
    #prettyPrintedType %= Map.insert x str


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
addAllDeclarations :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
addAllDeclarations n = do
  for_ ( findIdentifiers' ( const True ) n ) 
    \(d, IdentifierDetails{ identType }, _) -> do
      addDeclaration d
      case identType of
        Just t -> do
          hieType <- lookupType t
          let names = typeToNames hieType
          traverse_ (traverse_ (addDependency d) . nameToDeclaration) names
        Nothing -> pure ()


topLevelAnalysis :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
topLevelAnalysis n@Node{ nodeChildren } = do
  analysed <-
    runMaybeT
      ( msum
          [
            analyseStandaloneDeriving n
          , analyseInstanceDeclaration n
          , analyseBinding n
          , analyseRewriteRule n
          , analyseClassDeclaration n
          , analyseDataDeclaration n
          , analysePatternSynonyms n
          , analyseTypeSynonym n
          , analyseFamilyDeclaration n
          , analyseFamilyInstance n
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


analyseBinding :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST a -> m ()
analyseBinding n@Node{ nodeSpan, sourcedNodeInfo } = do
  let bindAnns = Set.fromList [("FunBind", "HsBindLR"), ("PatBind", "HsBindLR")]
  guard $ any (not . Set.disjoint bindAnns . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( findDeclarations n ) \d -> do
    define d nodeSpan

    followEvidenceUses n d

    for_ ( uses n ) $ addDependency d


analyseRewriteRule :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseRewriteRule n@Node{ sourcedNodeInfo } = do
  guard $ any (Set.member ("HsRule", "RuleDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( uses n ) addImplicitRoot


analyseInstanceDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
analyseInstanceDeclaration n@Node{ nodeSpan, sourcedNodeInfo } = do
  guard $ any (Set.member ("ClsInstD", "InstDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( findEvInstBinds n ) \(d, cs, ids, _) -> do
    -- This makes instance declarations show up in 
    -- the output if type-class-roots is set to False.
    define d nodeSpan

    followEvidenceUses n d

    for_ ( uses n ) $ addDependency d

    case identType ids of
      Just t -> for_ cs (addInstanceRoot d t)
      Nothing -> pure ()


analyseClassDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST a -> m ()
analyseClassDeclaration n@Node{ nodeSpan, sourcedNodeInfo } = do
  guard $ any (Set.member ("ClassDecl", "TyClDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( findIdentifiers isClassDeclaration n ) $ \d -> do
    define d nodeSpan

    followEvidenceUses n d

    (for_ ( findIdentifiers ( const True ) n ) . addDependency) d

  where

    isClassDeclaration =
      not . Set.null . Set.filter \case
        Decl ClassDec _ ->
          True

        _ ->
          False


analyseDataDeclaration :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
analyseDataDeclaration n@Node{ sourcedNodeInfo } = do
  guard $ any (Set.member ("DataDecl", "TyClDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_
    ( foldMap
        ( First . Just )
        ( findIdentifiers ( any isDataDec ) n )
    )
    \dataTypeName -> do
      define dataTypeName (nodeSpan n)

      for_ ( constructors n ) \constructor ->
        for_ ( foldMap ( First . Just ) ( findIdentifiers ( any isConDec ) constructor ) ) \conDec -> do
          addDependency conDec dataTypeName

          -- uncomment to make unused constructors show up in the output
          --define conDec (nodeSpan constructor)

          for_ ( uses constructor ) ( addDependency conDec )

          -- Connecting record fields to their constructors
          for_ ( recFieldDecls constructor ) \recFieldDec ->
            for_ ( foldMap ( First . Just ) ( findIdentifiers ( any isRecFieldDec ) recFieldDec ) ) 
              (`addDependency` conDec)

  for_ ( derivedInstances n ) \(d, cs, ids, ast) -> do
    define d (nodeSpan ast)

    followEvidenceUses ast d

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

    isRecFieldDec = \case
      RecField RecFieldDecl _ -> True
      _                       -> False


constructors :: HieAST a -> Seq ( HieAST a )
constructors = findNodeTypes "ConDecl"


recFieldDecls :: HieAST a -> Seq ( HieAST a )
recFieldDecls = findNodeTypes "ConDeclField"


derivedInstances :: HieAST a -> Seq (Declaration, Set Name, IdentifierDetails a, HieAST a)
derivedInstances n = findNodeTypes "HsDerivingClause" n >>= findEvInstBinds


findNodeTypes :: String -> HieAST a -> Seq ( HieAST a )
findNodeTypes t n@Node{ nodeChildren, sourcedNodeInfo } =
  if any (any ( (t ==) . unpackFS . nodeAnnotType) . nodeAnnotations) (getSourcedNodeInfo sourcedNodeInfo) then
    pure n

  else
    foldMap (findNodeTypes t) nodeChildren


analyseStandaloneDeriving :: ( Alternative m, MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST TypeIndex -> m ()
analyseStandaloneDeriving n@Node{ nodeSpan, sourcedNodeInfo } = do
  guard $ any (Set.member ("DerivDecl", "DerivDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ (findEvInstBinds n) \(d, cs, ids, _) -> do
    define d nodeSpan

    followEvidenceUses n d

    for_ (uses n) $ addDependency d

    case identType ids of
      Just t -> for_ cs (addInstanceRoot d t)
      Nothing -> pure ()


analyseTypeSynonym :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseTypeSynonym n@Node{ nodeSpan, sourcedNodeInfo } = do
  guard $ any (Set.member ("SynDecl", "TyClDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( findIdentifiers isTypeSynonym n ) $ \d -> do
    define d nodeSpan

    for_ (uses n) (addDependency d)

  where

    isTypeSynonym =
      any \case
        Decl SynDec _ -> True
        _             -> False


analyseFamilyDeclaration :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseFamilyDeclaration n@Node{ nodeSpan, sourcedNodeInfo } = do
  guard $ any (Set.member ("FamDecl", "TyClDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( findIdentifiers isFamDec n ) $ \d -> do
    define d nodeSpan

    for_ (uses n) (addDependency d)

  where

    isFamDec =
      any \case
        Decl FamDec _ -> True
        _             -> False


analyseFamilyInstance :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseFamilyInstance n@Node{ sourcedNodeInfo } = do
  guard $ any (Set.member ("TyFamInstD", "InstDecl") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

  for_ ( uses n ) addImplicitRoot


analysePatternSynonyms :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analysePatternSynonyms n@Node{ sourcedNodeInfo } = do
  guard $ any (Set.member ("PatSynBind", "HsBindLR") . Set.map unNodeAnnotation . nodeAnnotations) $ getSourcedNodeInfo sourcedNodeInfo

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

  where

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


-- | Follow evidence uses under the given node back to their instance bindings,
-- and connect the declaration to those bindings.
followEvidenceUses :: ( MonadState Analysis m, MonadReader AnalysisInfo m ) => HieAST a -> Declaration -> m ()
followEvidenceUses n d = do
  Config{ typeClassRoots } <- asks weederConfig
  AnalysisInfo{ refMap } <- ask

  let getEvidenceTrees = mapMaybe (getEvidenceTree refMap)
      evidenceInfos = concatMap Tree.flatten (getEvidenceTrees names)
      instanceEvidenceInfos = evidenceInfos & filter \case
        EvidenceInfo _ _ _ (Just (EvInstBind _ _, ModuleScope, _)) -> True
        _ -> False

  -- If type-class-roots flag is set then we don't need to follow evidence uses
  -- as the binding sites will be roots anyway
  unless typeClassRoots $ for_ instanceEvidenceInfos \ev -> do
    let name = nameToDeclaration (evidenceVar ev)
    mapM_ (addDependency d) name

  where

    names = concat . Tree.flatten $ evidenceUseTree n

    evidenceUseTree :: HieAST a -> Tree [Name]
    evidenceUseTree Node{ sourcedNodeInfo, nodeChildren } = Tree.Node
      { Tree.rootLabel = concatMap (findEvidenceUse . nodeIdentifiers) (getSourcedNodeInfo sourcedNodeInfo)
      , Tree.subForest = map evidenceUseTree nodeChildren
      }
