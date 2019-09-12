{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Main where

import "algebraic-graphs" Algebra.Graph ( Graph, edge, empty, overlay, overlays, vertex, vertexList )
import "algebraic-graphs" Algebra.Graph.ToGraph ( dfs )
import "algebraic-graphs" Algebra.Graph.Export.Dot ( defaultStyle, export, vertexAttributes, Attribute( (:=) ) )

import "base" Data.Foldable ( for_, toList )
import "base" Data.Maybe ( maybeToList )
import "base" Data.List ( intercalate )
import "base" Data.Traversable ( for )
import "base" System.Environment ( getArgs )

import qualified "containers" Data.Map.Strict as Map
import qualified "containers" Data.Set as Set

import "ghc" DynFlags ( DynFlags, defaultDynFlags )
import "ghc" HieBin ( HieFileResult( HieFileResult, hie_file_result ) )
import "ghc" HieBin ( readHieFile )
import "ghc" HieDebug ( ppHie )
import "ghc" HieTypes
  ( BindType( RegularBind )
  , ContextInfo( Decl, ValBind, PatternBind, Use, TyDecl )
  , HieAST( Node, nodeInfo, nodeChildren )
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
import "ghc" SysTools ( initSysTools )
import "ghc" UniqSupply ( mkSplitUniqSupply )

import "ghc-paths" GHC.Paths ( libdir )


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

  ( g, roots ) <-
    fmap
      ( \x -> ( overlays ( map fst x ), concatMap snd x ) )
      ( for hieFilePaths \hieFilePath -> do
          -- putStrLn ( "Processing " <> hieFilePath )

          ( HieFileResult{ hie_file_result = HieFile{ hie_asts = HieASTs hieASTs } }, _ ) <-
            readHieFile nameCache hieFilePath

          putStrLn ( foldMap ( showSDoc dynFlags . ppHie ) hieASTs )

          return
            ( overlays ( map ( dependencyGraph dynFlags ) ( toList hieASTs ) )
            , foldMap findRoots hieASTs
            )
      )

  let
    reachableSet =
      Set.fromList
        ( dfs
            ( Declaration
                { declModule =
                    Module
                      ( DefiniteUnitId ( DefUnitId ( stringToInstalledUnitId "ghc" ) ) )
                      ( mkModuleName "Main" )
                , declOccName =
                    mkOccName varName "main"
                }
            : roots
            )
            g
        )

    allDeclarations =
      Set.fromList ( vertexList g )

  for_
    ( allDeclarations Set.\\ reachableSet )
    \Declaration{ declModule, declOccName } ->
      putStrLn ( moduleNameString ( moduleName declModule ) <> "." <> occNameString declOccName )

  writeFile
    "graph.dot"
    ( export
        ( defaultStyle declarationStableName )
        { vertexAttributes = \v -> [ "label" :=  occNameString ( declOccName v ) ] }
        ( g ) -- >>= \d -> if not ( d `Set.member` reachableSet ) then return d else empty )
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


dependencyGraph :: Outputable a => DynFlags -> HieAST a -> Graph Declaration
dependencyGraph dynFlags n@Node{ nodeInfo = NodeInfo{ nodeAnnotations }, nodeChildren } =
  let
    decls =
      declarations n

  in
  if
    or
      [ and
          [ or
              [ ( "FunBind", "HsBindLR" ) `Set.member` nodeAnnotations
              , ( "PatBind", "HsBindLR" ) `Set.member` nodeAnnotations
              ]
          , not ( null nodeChildren )
          ]
      , ( "SynDecl", "TyClDecl" ) `Set.member` nodeAnnotations
      , ( "TypeSig", "Sig" ) `Set.member` nodeAnnotations
      ]
  then
    overlays
      ( map
          ( \d -> overlays ( map ( edge d ) ( foldMap uses nodeChildren ) ) )
          decls
      )

  else if ( "DataDecl", "TyClDecl" ) `Set.member` nodeAnnotations then
    overlays ( map ( dataType n ) decls )

  else if ( "ClsInstD", "InstDecl" ) `Set.member` nodeAnnotations then
    empty

  else
    overlays ( map ( dependencyGraph dynFlags ) nodeChildren )


dataType :: HieAST a -> Declaration -> Graph Declaration
dataType n dataTypeName =
  overlays
    (   vertex dataTypeName
      : map
          ( \( constructor, constructorUses ) ->
              overlay ( edge constructor dataTypeName ) constructorUses
          )
          ( constructors n )
    )


constructors :: HieAST a -> [ ( Declaration, Graph Declaration ) ]
constructors n@Node{ nodeInfo = NodeInfo{ nodeAnnotations }, nodeChildren } =
  if "ConDecl" `Set.member` Set.map snd nodeAnnotations then
    foldMap
      ( \decl -> [ ( decl, overlays ( map ( edge decl ) ( uses n ) ) ) ] )
      ( declarations n )

  else
    foldMap constructors nodeChildren


declarations :: HieAST a -> [ Declaration ]
declarations Node{ nodeInfo = NodeInfo{ nodeIdentifiers }, nodeChildren } =
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
               maybeToList ( nameToDeclaration name )
       )
       ( Map.toList nodeIdentifiers )
  <> foldMap declarations nodeChildren


-- Declarations that must be in the root set. We can't yet understand
-- reachability on these, so err on the side of caution and treat them as
-- implicitly reachable.
findRoots :: HieAST a -> [ Declaration ]
findRoots n@Node{ nodeInfo = NodeInfo{ nodeAnnotations }, nodeChildren } =
  if ( "ClsInstD", "InstDecl" ) `Set.member` nodeAnnotations then
    uses n

  else if ( "DerivDecl", "DerivDecl" ) `Set.member` nodeAnnotations then
    declarations n

  else
    foldMap findRoots nodeChildren


uses :: HieAST a -> [ Declaration ]
uses Node{ nodeInfo = NodeInfo{ nodeIdentifiers }, nodeChildren } =
     foldMap
       ( \case
           ( Left _, _ ) ->
             mempty

           ( Right name, IdentifierDetails{ identInfo } ) ->
             if Use `Set.member` identInfo then
               foldMap pure ( nameToDeclaration name )

             else
               []
           )

       ( Map.toList nodeIdentifiers )
  <> foldMap uses nodeChildren


nameToDeclaration :: Name -> Maybe Declaration
nameToDeclaration name = do
  m <-
    nameModule_maybe name

  return Declaration { declModule = m, declOccName = nameOccName name }
