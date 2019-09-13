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

import "ansi-terminal" System.Console.ANSI
  ( Color( Red, White )
  , ColorIntensity( Vivid )
  , ConsoleLayer( Foreground, Background )
  , SGR( SetColor )
  , setSGRCode
  )

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
import "ghc" SrcLoc ( RealSrcSpan, srcLocLine, srcLocCol, realSrcSpanStart, realSrcSpanEnd )
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
  -- print
  --   ( Set.map
  --       ( \d -> ( declarationStableName d, Map.lookup d ( declarationSites analysis ) ) )
  --       ( allDeclarations analysis Set.\\ reachableSet )
  --   )

  let
    highlightingMap =
      Map.unionsWith
        overlayHighlight
        ( foldMap
            ( \d ->
                foldMap
                  ( foldMap \m -> [ highlight m ] )
                  ( Map.lookup d ( declarationSites analysis ) )
            )
            ( allDeclarations analysis Set.\\ reachableSet )
        )

  source <-
    lines <$> readFile "src/Weeder.hs"

  putStrLn ( zipHighlighting ( Map.toList highlightingMap ) ( zip [1..] source ) )

  -- for_ ( allDeclarations analysis Set.\\ reachableSet ) \d ->
  --   print ( Map.lookup d ( declarationSites analysis ) )

  -- for_
  --   ( allDeclarations analysis Set.\\ reachableSet )
  --   \Declaration{ declModule, declOccName } ->
  --     putStrLn ( moduleNameString ( moduleName declModule ) <> "." <> occNameString declOccName )

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


addDeclaration :: MonadState Analysis m => Declaration -> RealSrcSpan -> m ()
addDeclaration decl span =
  modify' \a ->
    a
      { declarationSites =
          Map.unionWith
            Set.union
            ( declarationSites a )
            ( Map.singleton decl ( Set.singleton span ) )
      }


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
analyseFunBind n@Node{ nodeChildren, nodeSpan }= do
  guard ( not ( null nodeChildren ) )

  declarations <-
    findDeclarations n

  for_ declarations \d -> do
    addDeclaration d nodeSpan

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
                foldMap
                  ( \d -> Map.singleton d ( Set.singleton nodeSpan ) )
                  ( nameToDeclaration name )
        )
        ( Map.toList nodeIdentifiers )

  Set.union ( Set.fromList ( Map.keys here ) ) . Set.unions
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


class Foo a where
  foo :: a -> Bool

-- instance Foo Bool where
--   foo _ = unused



data Skip =
  Skip Int Highlight | SkipToEndOfLine
  deriving ( Show )


data Highlight =
  Highlight Int Skip | HighlightToEndOfLine
  deriving ( Show )


overlayHighlight :: Highlight -> Highlight -> Highlight
overlayHighlight HighlightToEndOfLine _ =
  HighlightToEndOfLine
overlayHighlight _ HighlightToEndOfLine =
  HighlightToEndOfLine
overlayHighlight ( Highlight x xs ) ( Highlight y ys ) =
  case compare x y of
    LT ->
      case dropSkip ( y - x ) xs of
        Left skip ->
          Highlight y ( overlaySkip skip ys )

        Right HighlightToEndOfLine ->
          HighlightToEndOfLine

        Right highlight ->
          overlaySkipHighlight ys highlight

    EQ ->
      Highlight x ( overlaySkip xs ys )

    GT ->
      overlayHighlight ( Highlight y ys ) ( Highlight x xs )



overlaySkip :: Skip -> Skip -> Skip
overlaySkip SkipToEndOfLine x =
  x
overlaySkip x SkipToEndOfLine =
  x
overlaySkip ( Skip x xs ) ( Skip y ys ) =
  case compare x y of
    LT ->
      Skip x ( overlaySkipHighlight ( Skip ( y - x ) ys ) xs )

    EQ ->
      Skip x ( overlayHighlight xs ys )

    GT ->
      Skip y ( overlaySkipHighlight ( Skip ( x - y ) xs ) ys )


overlaySkipHighlight :: Skip -> Highlight -> Highlight
overlaySkipHighlight _ HighlightToEndOfLine =
  HighlightToEndOfLine
overlaySkipHighlight SkipToEndOfLine h =
  h
overlaySkipHighlight ( Skip x xs ) ( Highlight y ys ) =
  case compare x y of
    LT ->
      case dropHighlight ( y - x ) xs of
        Left skip ->
          Highlight y ( overlaySkip skip ys )

        Right highlight ->
          case overlaySkipHighlight ys xs of
            HighlightToEndOfLine ->
              HighlightToEndOfLine

            Highlight z zs ->
              Highlight ( y + z ) zs

    EQ ->
      case overlaySkipHighlight ys xs of
        HighlightToEndOfLine ->
          HighlightToEndOfLine

        Highlight z zs ->
          Highlight ( y + z ) zs

    GT ->
      Highlight y ( overlaySkip ( Skip ( x - y ) xs ) ys )


dropSkip :: Int -> Skip -> Either Skip Highlight
dropSkip _ SkipToEndOfLine =
  Left SkipToEndOfLine
dropSkip x ( Skip y highlight ) =
  case compare x y of
    LT ->
      Left ( Skip ( y - x ) highlight )

    EQ ->
      Right highlight

    GT ->
      dropHighlight ( x - y ) highlight


dropHighlight :: Int -> Highlight -> Either Skip Highlight
dropHighlight _ HighlightToEndOfLine =
  Right HighlightToEndOfLine
dropHighlight x ( Highlight y skip ) =
  case compare x y of
    LT ->
      Right ( Highlight ( y - x ) skip )

    EQ ->
      Left skip

    GT ->
      dropSkip ( x - y ) skip


highlight :: RealSrcSpan -> Map Int Highlight
highlight span =
  if startLine == endLine then
    Map.singleton
      startLine
      ( Highlight
          0
          ( Skip
              ( startCol - 1 )
              ( Highlight
                  ( endCol - startCol )
                  SkipToEndOfLine
              )
          )
      )

  else
    Map.fromList
      ( concat
          [ pure ( startLine, Highlight 0 ( Skip ( startCol - 1 ) HighlightToEndOfLine ) )
          , [ ( l, HighlightToEndOfLine ) | l <- [ startLine + 1 .. endLine - 1 ] ]
          , pure ( endLine, Highlight ( endCol - 1 ) SkipToEndOfLine )
          ]
      )

  where

    startCol =
      srcLocCol start

    startLine =
      srcLocLine start

    endCol =
      srcLocCol end

    endLine =
      srcLocLine end

    start =
      realSrcSpanStart span

    end =
      realSrcSpanEnd span


zipHighlighting
  :: [ ( Int, Highlight ) ]
  -> [ ( Int, String ) ]
  -> String
zipHighlighting =
  highlightWithContext 3 1

  where

    highlightWithContext
      :: Int -> Int -> [ ( Int, Highlight ) ] -> [ ( Int, String ) ] -> String
    highlightWithContext _ currLine ( ( i, highlight ) : hs ) [] =
      ""
    highlightWithContext _ currLine [] _ =
      ""
    highlightWithContext n currLine ( ( i, highlight ) : hs ) ( ( linum, l ) : ls ) =
      case compare currLine i of
        LT | i - currLine > n ->
          highlightWithContext
            n
            ( currLine + 1 )
            ( ( i, highlight ) : hs )
            ls

        LT ->
             "    "
          ++ show linum
          ++ " │ "
          ++  l
          ++ "\n"
          ++ highlightWithContext
               ( n - 1 )
               ( currLine + 1 )
               ( ( i, highlight ) : hs )
               ls

        EQ ->
             "    "
          ++ show linum
          ++ " │ "
          ++ highlightString highlight l
          ++ "\n"
          ++ trailingContext 3 ( currLine + 1 ) hs ls

        GT ->
          error "Forgot to highlight something!"

    trailingContext
      :: Int -> Int -> [ ( Int, Highlight ) ] -> [ ( Int, String ) ] -> String
    trailingContext n currLine _ [] =
      ""
    trailingContext n currLine [] ( ( linum, l ) : ls ) =
      if n > 0 then
           "    "
        ++ show linum
        ++ " │ "
        ++ l
        ++ "\n"
        ++ trailingContext ( n - 1 ) ( currLine + 1 ) [] ls
      else
        ""
    trailingContext n currLine ( ( i, highlight ) : hs ) ( ( linum, l ) : ls ) =
      case compare currLine i of
        LT | n > 0 ->
             "    "
          ++ show linum
          ++ " │ "
          ++ l
          ++ "\n"
          ++ trailingContext ( n - 1 ) ( currLine + 1 ) ( ( i, highlight ) : hs ) ls

        LT ->
             "\n"
          ++ highlightWithContext 3 ( currLine + 1 ) ( ( i, highlight ) : hs ) ls

        EQ ->
             "    "
          ++ show linum
          ++ " │ "
          ++ highlightString highlight l
          ++ "\n"
          ++ trailingContext 3 ( currLine + 1 ) hs ls

        GT ->
          error "Forgot to highlight!"


highlightString :: Highlight -> String -> String
highlightString HighlightToEndOfLine s =
  hlCode
    <> s
    <> setSGRCode []
highlightString ( Highlight n skip ) s =
  hlCode
    <> take n s
    <> setSGRCode []
    <> skipThenHighlight skip ( drop n s )


skipThenHighlight :: Skip -> String -> String
skipThenHighlight SkipToEndOfLine s =
  s
skipThenHighlight ( Skip n h ) s =
  take n s <> highlightString h ( drop n s )


hlCode =
  setSGRCode [ SetColor Background Vivid Red, SetColor Foreground Vivid White ]
