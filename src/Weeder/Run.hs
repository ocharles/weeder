{-# language RecordWildCards #-}
{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Weeder.Run ( runWeeder, Weed(..), DeclarationWeed(..), DeadRoot(..), formatWeed ) where

-- base
import Control.Applicative ( liftA2 )
import Control.Monad ( guard )
import Data.List ( sortOn )
import Data.Foldable ( fold, foldl' )
import Data.Function ( (&) )

-- containers
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- ghc
import GHC.Plugins
  ( occNameString
  , unitString
  , moduleUnit
  , moduleName
  , moduleNameString
  )
import GHC.Iface.Ext.Types ( HieFile( hie_asts ), getAsts )
import GHC.Iface.Ext.Utils (generateReferencesMap)

-- parallel
import Control.Parallel (pseq)
import Control.Parallel.Strategies (parMap, rdeepseq)

-- regex-tdfa
import Text.Regex.TDFA ( matchTest )

-- transformers
import Control.Monad.State.Strict ( execState )

-- weeder
import Weeder
import Weeder.Config


-- | Something Weeder found that is unused: either a dead declaration in the
-- analysed code, or a configured root that matched no identifiers (a weed in
-- the configuration itself).
data Weed
  = WeedDeclaration DeclarationWeed
  | WeedRoot DeadRoot


-- | A dead declaration: code that is written but never reachable from a root.
data DeclarationWeed = DeclarationWeed
  { weedPackage :: String
  , weedPath :: FilePath
  , weedLine :: Int
  , weedCol :: Int
  , weedDeclaration :: Declaration
  , weedPrettyPrintedType :: Maybe String
  }


-- | A configured root that no longer applies because it matches nothing.
data DeadRoot
  = -- | A @roots@ pattern (its source string) that matched no declaration.
    DeadRootPattern String
  | -- | A @root-instances@ entry (pretty-printed) that matched no instance.
    DeadRootInstance String
  | -- | A @root-modules@ pattern (its source string) that matched no module.
    DeadRootModule String


formatWeed :: Weed -> String
formatWeed = \case
  WeedDeclaration DeclarationWeed{..} ->
    weedPackage <> ": " <> weedPath <> ":" <> show weedLine <> ":" <> show weedCol <> ": "
      <> case weedPrettyPrintedType of
        Nothing -> occNameString ( declOccName weedDeclaration )
        Just t -> "(Instance) :: " <> t
  WeedRoot (DeadRootPattern src) ->
    "no declaration matches roots entry " <> show src
  WeedRoot (DeadRootInstance s) ->
    "no instance matches root-instances entry " <> s
  WeedRoot (DeadRootModule src) ->
    "no module matches root-modules entry " <> show src

-- | Run Weeder on the given .hie files with the given 'Config'.
--
-- Returns a list of 'Weed's that can be displayed using
-- 'formatWeed', and the final 'Analysis'.
runWeeder :: Config -> [HieFile] -> ([Weed], Analysis)
runWeeder weederConfig@Config{ rootPatterns = Configured rootPatterns rootPatternsExplicit, typeClassRoots, rootInstances = Configured rootInstances rootInstancesExplicit, rootModules = Configured rootModules rootModulesExplicit } hieFiles =
  let
    asts = concatMap (Map.elems . getAsts . hie_asts) hieFiles

    rf = generateReferencesMap asts

    analyses =
      parMap rdeepseq (\hf -> execState (analyseHieFile weederConfig hf) emptyAnalysis) hieFiles

    analyseEvidenceUses' =
      if typeClassRoots
        then id
        else analyseEvidenceUses rf

    analysis1 =
      Data.Foldable.foldl' mappend mempty analyses

    -- Evaluating 'analysis1' first allows us to begin analysis
    -- while hieFiles is still being read (since rf depends on all hie files)
    analysis = analysis1 `pseq`
      analyseEvidenceUses' analysis1

    -- We limit ourselves to outputable declarations only rather than all
    -- declarations in the graph. This has a slight performance benefit,
    -- at the cost of having to assume that a non-outputable declaration
    -- will always either be an implicit root or irrelevant.
    roots =
      Set.filter
        ( \d ->
            any
              ( \p -> matchTest ( compiledRegex p ) ( displayDeclaration d ) )
              rootPatterns
        )
        ( outputableDeclarations analysis )

    matchingModules =
      Set.filter
        ((\s -> any (\p -> matchTest ( compiledRegex p ) s) rootModules) . moduleNameString . moduleName)
      ( Map.keysSet $ exports analysis )

    reachableSet =
      reachable
        analysis
        ( Set.map DeclarationRoot roots
        <> Set.map ModuleRoot matchingModules
        <> filterImplicitRoots analysis ( implicitRoots analysis )
        )

    -- We only care about dead declarations if they have a span assigned,
    -- since they don't show up in the output otherwise
    dead =
      outputableDeclarations analysis Set.\\ reachableSet

    warnings =
      Map.unionsWith (++) $
      foldMap
        ( \d ->
            fold $ do
              moduleFilePath <- Map.lookup ( declModule d ) ( modulePaths analysis )
              let packageName = unitString . moduleUnit . declModule $ d
              starts <- Map.lookup d ( declarationSites analysis )
              let locs = (,) packageName <$> Set.toList starts
              guard $ not $ null starts
              return [ Map.singleton moduleFilePath ( Control.Applicative.liftA2 (,) locs (pure d) ) ]
        )
        dead

    declarationWeeds =
      Map.toList warnings & concatMap \( weedPath, declarations ) ->
        sortOn fst declarations & map \( (weedPackage, (weedLine, weedCol)) , weedDeclaration ) ->
          WeedDeclaration DeclarationWeed
            { weedPrettyPrintedType = Map.lookup weedDeclaration (prettyPrintedType analysis)
            , weedPackage
            , weedPath
            , weedLine
            , weedCol
            , weedDeclaration
            }

    -- A @roots@ pattern that matches no identifier in the project is a weed in
    -- the configuration itself: it no longer applies to any declaration. We
    -- match against every local declaration rather than only the outputable
    -- ones, so that a root naming a real type or constructor is not flagged just
    -- because @unused-types@ happens to be disabled. Only patterns the user
    -- explicitly configured are reported, since pointing out that an
    -- unconfigured default is unused is not actionable.
    deadRootPatterns
      | not rootPatternsExplicit = []
      | otherwise =
          [ regexSource p
          | p <- rootPatterns
          , not $
              any
                ( \d -> matchTest ( compiledRegex p ) ( displayDeclaration d ) )
                ( localDeclarations analysis )
          ]

    -- A @root-instances@ entry that matches no instance is likewise a weed.
    -- When 'typeClassRoots' is set, @root-instances@ is ignored entirely, so we
    -- don't report its entries.
    instanceRoots =
      [ ( d, c ) | InstanceRoot d c <- Set.toList ( implicitRoots analysis ) ]

    deadRootInstances
      | typeClassRoots = []
      | not rootInstancesExplicit = []
      | otherwise =
          [ showInstancePattern ( regexSource <$> ip )
          | ip <- rootInstances
          , not $ any ( matchesInstancePattern analysis ip ) instanceRoots
          ]

    -- A @root-modules@ pattern that matches none of the modules Weeder analysed
    -- is also a weed.
    knownModuleNames =
      map ( moduleNameString . moduleName ) ( Map.keys ( modulePaths analysis ) )

    deadRootModules
      | not rootModulesExplicit = []
      | otherwise =
          [ regexSource p
          | p <- rootModules
          , not $ any ( \m -> matchTest ( compiledRegex p ) m ) knownModuleNames
          ]

    weeds =
      declarationWeeds
        <> map ( WeedRoot . DeadRootPattern ) deadRootPatterns
        <> map ( WeedRoot . DeadRootInstance ) deadRootInstances
        <> map ( WeedRoot . DeadRootModule ) deadRootModules

  in (weeds, analysis)

  where

    filterImplicitRoots :: Analysis -> Set Root -> Set Root
    filterImplicitRoots analysis = Set.filter $ \case
      DeclarationRoot _ -> True -- keep implicit roots for rewrite rules etc

      ModuleRoot _ -> True

      -- [tag:RootInstanceMatching] The reachability check here and the
      -- dead-root-instance check in 'deadRootInstances' must agree on what it
      -- means for a 'root-instances' entry to match an instance; both go
      -- through 'matchesInstancePattern'.
      InstanceRoot d c ->
        typeClassRoots
          || any ( \ip -> matchesInstancePattern analysis ip ( d, c ) ) rootInstances


-- | Does a @root-instances@ pattern match a given instance root (the
-- declaration of the instance and the declaration of its parent class)? An
-- absent field always matches.
--
-- [ref:RootInstanceMatching]
matchesInstancePattern
  :: Analysis -> InstancePattern CompiledRegex -> ( Declaration, Declaration ) -> Bool
matchesInstancePattern Analysis{ prettyPrintedType, modulePaths } ip ( d, c ) =
       maybe True moduleMatches ( modulePattern ip )
    && maybe True classMatches ( classPattern ip )
    && maybe True typeMatches ( instancePattern ip )
  where
    moduleMatches p =
      maybe False ( matchTest ( compiledRegex p ) ) ( Map.lookup ( declModule d ) modulePaths )

    classMatches p =
      matchTest ( compiledRegex p ) ( displayDeclaration c )

    typeMatches p =
      maybe False ( matchTest ( compiledRegex p ) ) ( Map.lookup d prettyPrintedType )


displayDeclaration :: Declaration -> String
displayDeclaration d =
  moduleNameString ( moduleName ( declModule d ) ) <> "." <> occNameString ( declOccName d )
