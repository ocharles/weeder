{-# language RecordWildCards #-}
{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Weeder.Run ( runWeeder, Weed(..), formatWeed ) where

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


data Weed = Weed
  { weedPackage :: String
  , weedPath :: FilePath
  , weedLine :: Int
  , weedCol :: Int
  , weedDeclaration :: Declaration
  , weedPrettyPrintedType :: Maybe String
  }


formatWeed :: Weed -> String
formatWeed Weed{..} =
  weedPackage <> ": " <> weedPath <> ":" <> show weedLine <> ":" <> show weedCol <> ": "
    <> case weedPrettyPrintedType of
      Nothing -> occNameString ( declOccName weedDeclaration )
      Just t -> "(Instance) :: " <> t

-- | Run Weeder on the given .hie files with the given 'Config'.
--
-- Returns a list of 'Weed's that can be displayed using
-- 'formatWeed', and the final 'Analysis'.
runWeeder :: Config -> [HieFile] -> ([Weed], Analysis)
runWeeder weederConfig@Config{ rootPatterns, typeClassRoots, rootInstances } hieFiles =
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
      foldl' mappend mempty analyses

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
              (`matchTest` displayDeclaration d)
              rootPatterns
        )
        ( outputableDeclarations analysis )

    reachableSet =
      reachable
        analysis
        ( Set.map DeclarationRoot roots <> filterImplicitRoots analysis ( implicitRoots analysis ) )

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
              return [ Map.singleton moduleFilePath ( liftA2 (,) locs (pure d) ) ]
        )
        dead

    weeds =
      Map.toList warnings & concatMap \( weedPath, declarations ) ->
        sortOn fst declarations & map \( (weedPackage, (weedLine, weedCol)) , weedDeclaration ) ->
          Weed { weedPrettyPrintedType = Map.lookup weedDeclaration (prettyPrintedType analysis)
               , weedPackage
               , weedPath
               , weedLine
               , weedCol
               , weedDeclaration
               }

  in (weeds, analysis)

  where

    filterImplicitRoots :: Analysis -> Set Root -> Set Root
    filterImplicitRoots Analysis{ prettyPrintedType, modulePaths } = Set.filter $ \case
      DeclarationRoot _ -> True -- keep implicit roots for rewrite rules etc

      ModuleRoot _ -> True

      InstanceRoot d c -> typeClassRoots || matchingType
        where
          matchingType = 
            let mt = Map.lookup d prettyPrintedType
                matches = maybe (const False) (flip matchTest) mt
            in any (maybe True matches) filteredInstances

          filteredInstances = 
            map instancePattern 
            . filter (maybe True (`matchTest` displayDeclaration c) . classPattern) 
            . filter (maybe True modulePathMatches . modulePattern) 
            $ rootInstances

          modulePathMatches p = maybe False (p `matchTest`) (Map.lookup ( declModule d ) modulePaths)


displayDeclaration :: Declaration -> String
displayDeclaration d = 
  moduleNameString ( moduleName ( declModule d ) ) <> "." <> occNameString ( declOccName d )
