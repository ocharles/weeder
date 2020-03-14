{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Weeder.Config where

import qualified Dhall
import Weeder
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
import Data.Set ( Set )
import qualified Data.Set as Set


data Config = Config
  { ignore :: Set Declaration
  , roots :: Set Root
  , strict :: Bool
  }


config :: Dhall.Type Config
config =
  Dhall.record do
    ignore <- Set.fromList <$> Dhall.field "ignore" ( Dhall.list declaration )
    roots <- Set.fromList <$> Dhall.field "roots" ( Dhall.list root )
    strict <- Dhall.field "strict" Dhall.bool


    return Config{..}


declaration :: Dhall.Type Declaration
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


root :: Dhall.Type Root
root =
  Dhall.union $ mconcat
    [ ModuleRoot <$> Dhall.constructor "Module" moduleDecoder
    , DeclarationRoot <$> Dhall.constructor "Declaration" declaration
    ]


moduleDecoder :: Dhall.Type Module
moduleDecoder =
  Dhall.record do
    unitId <- Dhall.field "unit-id" Dhall.string
    moduleName <- Dhall.field "module" Dhall.string

    return $
      Module
        ( DefiniteUnitId ( DefUnitId ( stringToInstalledUnitId unitId ) ) )
        ( mkModuleName moduleName )
