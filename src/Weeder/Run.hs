{-# language RecordWildCards #-}
{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Weeder.Run ( runWeeder, Weed(..), formatWeed ) where

-- base
import Control.Applicative ( liftA2 )
import Control.Monad ( guard )
import Data.List ( sortOn )
import Data.Foldable ( fold )
import Data.Function ( (&) )

-- containers
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- ghc
import GHC.Plugins 
  ( RealSrcLoc
  , srcLocLine
  , occNameString
  , realSrcSpanStart
  , moduleName
  , moduleNameString 
  )
import GHC.Iface.Ext.Types ( HieFile )

-- regex-tdfa
import Text.Regex.TDFA ( (=~) )

-- transformers
import Control.Monad.State.Strict ( execState )

-- weeder
import Weeder
import Weeder.Config


data Weed = Weed
  { weedPath :: FilePath
  , weedLoc :: RealSrcLoc
  , weedDeclaration :: Declaration
  , weedPrettyPrintedType :: Maybe String
  }


formatWeed :: Weed -> String
formatWeed Weed{..} =
  weedPath <> ":" <> show ( srcLocLine weedLoc ) <> ": "
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
    analysis =
      execState ( analyseHieFiles weederConfig hieFiles ) emptyAnalysis

    roots =
      Set.filter
        ( \d ->
            any
              ( displayDeclaration d =~ )
              rootPatterns
        )
        ( allDeclarations analysis )

    reachableSet =
      reachable
        analysis
        ( Set.map DeclarationRoot roots <> filterImplicitRoots analysis ( implicitRoots analysis ) )

    dead =
      allDeclarations analysis Set.\\ reachableSet

    warnings =
      Map.unionsWith (++) $
      foldMap
        ( \d ->
            fold $ do
              moduleFilePath <- Map.lookup ( declModule d ) ( modulePaths analysis )
              spans <- Map.lookup d ( declarationSites analysis )
              guard $ not $ null spans
              let starts = map realSrcSpanStart $ Set.toList spans
              return [ Map.singleton moduleFilePath ( liftA2 (,) starts (pure d) ) ]
        )
        dead

    weeds =
      Map.toList warnings & concatMap \( weedPath, declarations ) ->
        sortOn (srcLocLine . fst) declarations & map \( weedLoc, weedDeclaration ) ->
          Weed { weedPrettyPrintedType = Map.lookup weedDeclaration (prettyPrintedType analysis)
               , weedPath
               , weedLoc
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
                matches = maybe (const False) (=~) mt
            in any (maybe True matches) filteredInstances

          filteredInstances :: Set (Maybe String)
          filteredInstances = 
            Set.map instancePattern 
            . Set.filter (maybe True (displayDeclaration c =~) . classPattern) 
            . Set.filter (maybe True modulePathMatches . modulePattern) 
            $ rootInstances

          modulePathMatches :: String -> Bool
          modulePathMatches p = maybe False (=~ p) (Map.lookup ( declModule d ) modulePaths)


displayDeclaration :: Declaration -> String
displayDeclaration d = 
  moduleNameString ( moduleName ( declModule d ) ) <> "." <> occNameString ( declOccName d )
