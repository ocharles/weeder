{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Weeder.Config where

-- containers
import Data.Set ( Set )
import qualified Data.Set as Set

-- dhall
import qualified Dhall

-- ghc
import Module
  ( DefUnitId( DefUnitId )
  , Module( Module )
  , UnitId( DefiniteUnitId )
  , mkModuleName
  , stringToInstalledUnitId
  )
import OccName
  ( mkOccName
  , varName
  )

-- weeder
import Weeder


-- | Configuration for Weeder analysis.
data Config = Config
  { ignore :: Set Declaration
    -- ^ The set of declarations that should not be reported to be weeds.
  , roots :: Set Root
    -- ^ The set of roots to consider always alive.
  , strict :: Bool
    -- ^ Enable strict analysis. Strict analysis means:
    --
    --   * The set of ignored declarations must be a subset of reported weeds
    --     (i.e., you are not trying to ignore something that is not considered
    --     a weed)
    --   * The set of roots only mentions declarations that Weeder can find
    --     (i.e., all mentioned roots point to known declarations or modules).
  , typeClassRoots :: Bool
    -- ^ If True, consider all declarations in a type class as part of the root
    -- set. Weeder is currently unable to identify whether or not a type class
    -- instance is used - enabling this option can prevent false positives.
  }


config :: Dhall.Decoder Config
config =
  Dhall.record do
    ignore <- Set.fromList <$> Dhall.field "ignore" ( Dhall.list declaration )
    roots <- Set.fromList <$> Dhall.field "roots" ( Dhall.list root )
    strict <- Dhall.field "strict" Dhall.bool
    typeClassRoots <- Dhall.field "type-class-roots" Dhall.bool

    return Config{..}


declaration :: Dhall.Decoder Declaration
declaration =
  Dhall.record do
    unitId <- Dhall.field "unit-id" Dhall.string
    moduleName <- Dhall.field "module" Dhall.string
    symbol <- Dhall.field "symbol" Dhall.string

    return
      Declaration
        { declModule =
            Module
              ( DefiniteUnitId ( DefUnitId ( stringToInstalledUnitId unitId ) ) )
              ( mkModuleName moduleName )
        , declOccName =
            mkOccName varName symbol
        }


root :: Dhall.Decoder Root
root =
  Dhall.union $ mconcat
    [ ModuleRoot <$> Dhall.constructor "Module" moduleDecoder
    , DeclarationRoot <$> Dhall.constructor "Declaration" declaration
    ]


moduleDecoder :: Dhall.Decoder Module
moduleDecoder =
  Dhall.record do
    unitId <- Dhall.field "unit-id" Dhall.string
    moduleName <- Dhall.field "module" Dhall.string

    return $
      Module
        ( DefiniteUnitId ( DefUnitId ( stringToInstalledUnitId unitId ) ) )
        ( mkModuleName moduleName )
