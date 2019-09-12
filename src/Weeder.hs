{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Main where

import "algebraic-graphs" Algebra.Graph ( Graph, edge, empty, overlay, overlays, vertex, vertexList )
import "algebraic-graphs" Algebra.Graph.ToGraph ( dfs )
import "algebraic-graphs" Algebra.Graph.Export.Dot ( defaultStyle, export, vertexAttributes, Attribute( (:=) ) )

import "base" Control.Applicative ( Alternative )
import "base" Control.Monad ( guard, msum )
import "base" Control.Monad.IO.Class ( liftIO )
import "base" Data.Foldable ( for_, traverse_, toList )
import "base" Data.Maybe ( maybeToList )
import "base" Data.List ( intercalate )
import "base" System.Environment ( getArgs )

import "containers" Data.Map.Strict ( Map )
import qualified "containers" Data.Map.Strict as Map
import "containers" Data.Set ( Set )
import qualified "containers" Data.Set as Set

import "ghc" DynFlags ( DynFlags, defaultDynFlags )
import "ghc" HieBin ( HieFileResult( HieFileResult, hie_file_result ) )
import "ghc" HieBin ( readHieFile )
import "ghc" HieDebug ( ppHie )
import "ghc" HieTypes
  ( BindType( RegularBind )
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl )
  , HieAST( Node, nodeInfo, nodeChildren, nodeSpan )
  , HieASTs( HieASTs )
  , HieFile( HieFile, hie_asts )
  , IdentifierDetails( IdentifierDetails, identInfo )
  , NodeInfo( NodeInfo, nodeIdentifiers, nodeAnnotations )
  , Scope( ModuleScope )
  )
import "ghc" Module
  ( DefUnitId( DefUnitId )
  , Module( Module )
  , UnitId( DefiniteUnitId )
  , mkModuleName
  , moduleNameString
  , moduleName
  , moduleStableString
  , stringToInstalledUnitId
  )
import "ghc" Name ( Name, nameOccName, nameModule_maybe )
import "ghc" NameCache ( initNameCache )
import "ghc" OccName
  ( OccName
  , isDataOcc
  , isDataSymOcc
  , isTcOcc
  , isTvOcc
  , isVarOcc
  , mkOccName
  , occNameString
  , varName
  )
import "ghc" Outputable ( Outputable, showSDoc )
import "ghc" SrcLoc ( RealSrcSpan )
import "ghc" SysTools ( initSysTools )
import "ghc" UniqSupply ( mkSplitUniqSupply )

import "ghc-paths" GHC.Paths ( libdir )

import "mtl" Control.Monad.State.Class ( MonadState, modify' )

import "transformers" Control.Monad.Trans.Maybe ( MaybeT, runMaybeT )
import "transformers" Control.Monad.Trans.State.Strict ( execStateT )


main :: IO ()
main = do
  hieFilePaths <-
    getArgs

  nameCache <- do
    uniqSupply <-
      mkSplitUniqSupply 'z'

    return ( initNameCache uniqSupply [] )

  dynFlags <- do
    systemSettings <-
      initSysTools libdir

    return ( defaultDynFlags systemSettings ( [], [] ) )

  analysis <-
    flip execStateT emptyAnalysis do
      for_ hieFilePaths \hieFilePath -> do
        ( HieFileResult{ hie_file_result = HieFile{ hie_asts = HieASTs hieASTs } }, _ ) <-
          liftIO ( readHieFile nameCache hieFilePath )

        traverse_ analyse hieASTs

        -- putStrLn ( foldMap ( showSDoc dynFlags . ppHie ) hieASTs )

        -- return
        --   ( overlays ( map ( dependencyGraph dynFlags ) ( toList hieASTs ) )
        --   , foldMap findRoots hieASTs
        --   )

  let
    reachableSet =
      reachable
        analysis
        ( Set.singleton
            ( Declaration
              { declModule =
                  Module
                    ( DefiniteUnitId ( DefUnitId ( stringToInstalledUnitId "main" ) ) )
                    ( mkModuleName "Main" )
              , declOccName =
                  mkOccName varName "main"
              }
            )
        )

  for_
    ( allDeclarations analysis Set.\\ reachableSet )
    \Declaration{ declModule, declOccName } ->
      putStrLn ( moduleNameString ( moduleName declModule ) <> "." <> occNameString declOccName )

  writeFile
    "graph.dot"
    ( export
        ( defaultStyle declarationStableName )
        { vertexAttributes = \v -> [ "label" := occNameString ( declOccName v ) ] }
        ( dependencyGraph analysis ) -- >>= \d -> if not ( d `Set.member` reachableSet ) then return d else empty )
    )


data Declaration =
  Declaration
    { declModule :: Module
    , declOccName :: OccName
    }
  deriving
    ( Eq, Ord )


declarationStableName :: Declaration -> String
declarationStableName Declaration { declModule, declOccName } =
  let
    namespace =
      if isVarOcc declOccName then
        "var"

      else if isTvOcc declOccName then
        "tv"

      else if isTcOcc declOccName then
        "tc"

      else if isDataOcc declOccName then
        "data"

      else if isDataSymOcc declOccName then
        "dataSym"

      else
        "unknown"

    in
    intercalate "$" [ namespace, moduleStableString declModule, "$", occNameString declOccName ]


data Analysis =
  Analysis
    { dependencyGraph :: Graph Declaration
      -- ^ A graph between declarations, capturing dependencies.
    , declarationSites :: Map Declaration RealSrcSpan
      -- ^ A partial mapping between declarations and their definition site.
      -- This Map is partial as we don't always know where a Declaration was
      -- defined (e.g., it may come from a package without source code).
    , implicitRoots :: Set Declaration
      -- ^ The Set of all Declarations that are always reachable. This is used
      -- to capture knowledge not yet modelled in weeder, such as instance
      -- declarations depending on top-level functions.
    }


emptyAnalysis :: Analysis
emptyAnalysis =
  Analysis empty mempty mempty


reachable :: Analysis -> Set Declaration -> Set Declaration
reachable Analysis{ dependencyGraph, implicitRoots } roots =
  Set.fromList
    ( dfs ( Set.toList ( Set.union implicitRoots roots ) ) dependencyGraph )


allDeclarations :: Analysis -> Set Declaration
allDeclarations Analysis{ dependencyGraph } =
  Set.fromList ( vertexList dependencyGraph )


analyse :: MonadState Analysis m => HieAST a -> m ()
analyse =
  traverse_ topLevelAnalysis . nodeChildren


-- | @addDependency x y@ adds the information that @x@ depends on @y@.
addDependency :: MonadState Analysis m => Declaration -> Declaration -> m ()
addDependency x y =
  modify' \a ->
    a { dependencyGraph = overlay ( dependencyGraph a ) ( edge x y ) }


addImplicitRoot :: MonadState Analysis m => Declaration -> m ()
addImplicitRoot x =
  modify' \a -> a { implicitRoots = implicitRoots a <> Set.singleton x }


topLevelAnalysis :: MonadState Analysis m => HieAST a -> m ()
topLevelAnalysis n@Node{ nodeInfo = NodeInfo{ nodeAnnotations }, nodeChildren } = do
  analysed <-
    runMaybeT
      ( msum
          [    guard
                  ( or
                      [ ( "FunBind", "HsBindLR" ) `Set.member` nodeAnnotations
                      , ( "PatBind", "HsBindLR" ) `Set.member` nodeAnnotations
                      , ( "SynDecl", "TyClDecl" ) `Set.member` nodeAnnotations
                      , ( "TypeSig", "Sig" ) `Set.member` nodeAnnotations
                      ]
                  )
            >> analyseFunBind n
          ,    guard ( ( "ClsInstD", "InstDecl" ) `Set.member` nodeAnnotations )
            >> analyseInstDecl n
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


-- | Try and analyse a HieAST node as if it's a FunBind
analyseFunBind :: ( Alternative m, MonadState Analysis m ) => HieAST a -> m ()
analyseFunBind n@Node{ nodeChildren }= do
  guard ( not ( null nodeChildren ) )

  declarations <-
    findDeclarations n

  for_ declarations \d ->
    for_ ( uses n ) \use ->
      addDependency d use


analyseInstDecl :: MonadState Analysis m => HieAST a -> m ()
analyseInstDecl n =
  traverse_ addImplicitRoot ( uses n )


--   else if
--     traverse_ dataType ( Map.keys decls )

--   else if ( "ClsInstD", "InstDecl" ) `Set.member` nodeAnnotations then
--     return empty

--   else
--     overlays <$> mapM calculateDependencyGraph nodeChildren


-- dataType :: MonadState Analysis m => HieAST a -> Declaration -> m ()
-- dataType n dataTypeName = do
--   modify' \a -> a { dependencyGraph = overlay ( dependencyGraph a ) ( vertex dataTypeName ) }

--   dataTypeConstructors <-
--     constructors n

--   for_ dataTypeConstructors \c ->
--     modify' \a -> a { dependencyGraph = overlay ( dependencyGraph a ) ( edge c dataTypeName ) }


-- constructors :: MonadState Analysis m => HieAST a -> m ( Set Declaration )
-- constructors n@Node{ nodeInfo = NodeInfo{ nodeAnnotations }, nodeChildren } =
--   if "ConDecl" `Set.member` Set.map snd nodeAnnotations then do
--     decls <-
--       declarations n

--     for_ decls \d ->
--       modify' \a ->
--         a
--           { dependencyGraph =
--               overlays
--                 ( dependencyGraph a
--                 : map ( edge d ) ( uses n )
--                 )
--           }

--     return decls

--   else
--     Set.unions <$> traverse constructors nodeChildren


findDeclarations :: MonadState Analysis m => HieAST a -> m ( Set Declaration )
findDeclarations Node{ nodeInfo = NodeInfo{ nodeIdentifiers }, nodeChildren, nodeSpan } = do
  let
    here =
      foldMap
        ( \case
            ( Left _, _ ) ->
              mempty

            ( Right name, IdentifierDetails{ identInfo } ) ->
              if
                Set.null
                  ( Set.filter
                      ( \case
                          ValBind RegularBind ModuleScope _ ->
                            True

                          PatternBind ModuleScope _ _ ->
                            True

                          Decl _ _ ->
                            True

                          TyDecl ->
                            True

                          _ ->
                            False
                      )
                      identInfo
                  )
              then
                mempty

              else
                foldMap ( \d -> Map.singleton d nodeSpan ) ( nameToDeclaration name )
        )
        ( Map.toList nodeIdentifiers )

  modify' \a -> a { declarationSites = declarationSites a <> here }

  Set.union ( Set.fromList ( Map.keys here) ) . Set.unions
    <$> traverse findDeclarations nodeChildren


-- -- Declarations that must be in the root set. We can't yet understand
-- -- reachability on these, so err on the side of caution and treat them as
-- -- implicitly reachable.
-- findRoots :: MonadState Analysis m => HieAST a -> m ( Set Declaration )
-- findRoots n@Node{ nodeInfo = NodeInfo{ nodeAnnotations }, nodeChildren } =
--   if ( "ClsInstD", "InstDecl" ) `Set.member` nodeAnnotations then
--     return ( Set.fromList ( uses n ) )

--   else if ( "DerivDecl", "DerivDecl" ) `Set.member` nodeAnnotations then
--     declarations n

--   else
--     foldMap findRoots nodeChildren


uses :: HieAST a -> Set Declaration
uses Node{ nodeInfo = NodeInfo{ nodeIdentifiers }, nodeChildren } =
     foldMap
       ( \case
           ( Left _, _ ) ->
             mempty

           ( Right name, IdentifierDetails{ identInfo } ) ->
             if Use `Set.member` identInfo then
               foldMap Set.singleton ( nameToDeclaration name )

             else
               mempty
           )

       ( Map.toList nodeIdentifiers )
  <> foldMap uses nodeChildren


nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <-
    nameModule_maybe name

  return Declaration { declModule = m, declOccName = nameOccName name }
