{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}

module Weeder.Config 
  ( -- * Config
    Config(..)
  , configToToml
  , decodeNoDefaults
  , defaultConfig 
    -- * Marking instances as roots
  , InstancePattern
  , modulePattern
  , instancePattern
  , classPattern
  , pattern InstanceOnly
  , pattern ClassOnly
  , pattern ModuleOnly
  ) 
   where

-- base
import Control.Applicative ((<|>), empty)
import Data.Char (toLower)
import Data.List (intersperse, intercalate)

-- containers
import Data.Set ( Set )
import qualified Data.Set as Set

-- toml-reader
import qualified TOML


-- | Configuration for Weeder analysis.
data Config = Config
  { rootPatterns :: Set String
    -- ^ Any declarations matching these regular expressions will be added to
    -- the root set.
  , typeClassRoots :: Bool
    -- ^ If True, consider all declarations in a type class as part of the root
    -- set. Overrides root-instances.
  , rootInstances :: Set InstancePattern
    -- ^ All matching instances will be added to the root set. An absent field
    -- will always match.
  , unusedTypes :: Bool
    -- ^ Toggle to look for and output unused types. Type family instances will
    -- be marked as implicit roots.
  } deriving (Eq, Show)


-- | Construct via InstanceOnly, ClassOnly or ModuleOnly, 
-- and combine with the Semigroup instance
data InstancePattern = InstancePattern
  { instancePattern :: Maybe String
  , classPattern :: Maybe String
  , modulePattern :: Maybe String
  } deriving (Eq, Show, Ord)


instance Semigroup InstancePattern where
  InstancePattern i c m <> InstancePattern i' c' m' = 
    InstancePattern (i <> i') (c <> c') (m <> m')


pattern InstanceOnly, ClassOnly, ModuleOnly :: String -> InstancePattern
pattern InstanceOnly t = InstancePattern (Just t) Nothing Nothing
pattern ClassOnly c = InstancePattern Nothing (Just c) Nothing
pattern ModuleOnly m = InstancePattern Nothing Nothing (Just m)


defaultConfig :: Config
defaultConfig = Config
  { rootPatterns = Set.fromList [ "Main.main", "^Paths_.*"]
  , typeClassRoots = False
  , rootInstances = Set.fromList [ ClassOnly "\\.IsString$", ClassOnly "\\.IsList$" ]
  , unusedTypes = False
  }


instance TOML.DecodeTOML Config where
  tomlDecoder = do
    rootPatterns <- TOML.getFieldOr (rootPatterns defaultConfig) "roots"
    typeClassRoots <- TOML.getFieldOr (typeClassRoots defaultConfig) "type-class-roots"
    rootInstances <- TOML.getFieldOr (rootInstances defaultConfig) "root-instances" 
    unusedTypes <- TOML.getFieldOr (unusedTypes defaultConfig) "unused-types"

    pure Config{..}


decodeNoDefaults :: TOML.Decoder Config
decodeNoDefaults = do
  rootPatterns <- TOML.getField "roots"
  typeClassRoots <- TOML.getField "type-class-roots"
  rootInstances <- TOML.getField "root-instances"
  unusedTypes <- TOML.getField "unused-types"

  pure Config{..}


instance TOML.DecodeTOML InstancePattern where
  tomlDecoder = decodeInstancePattern


-- | Decoder for a value of any of the forms:
--
-- @{instance = t, class = c, module = m} -> InstanceClassAndModule t c m@
--
-- @a -> InstanceOnly a@
--
-- @{instance = t} -> InstanceOnly t@
--
-- @{class = m} -> ClassOnly c@
--
-- etc.
decodeInstancePattern :: TOML.Decoder InstancePattern
decodeInstancePattern = decodeTable <|> decodeStringLiteral <|> decodeInstanceError

  where

    decodeStringLiteral = InstanceOnly <$> TOML.tomlDecoder

    decodeTable = do
      t <- fmap InstanceOnly <$> TOML.getFieldOpt "instance"
      c <- fmap ClassOnly <$> TOML.getFieldOpt "class"
      m <- fmap ModuleOnly <$> TOML.getFieldOpt "module"
      maybe empty pure (t <> c <> m)
    
    decodeInstanceError = TOML.makeDecoder $
      TOML.invalidValue "Need to specify at least one of 'instance', 'class', or 'module'"


showInstancePattern :: InstancePattern -> String
showInstancePattern = \case
  InstanceOnly a -> show a
  p -> "{ " ++ table ++ " }"
    where
      table = intercalate ", " . filter (not . null) $
          [ maybe mempty typeField (instancePattern p)
          , maybe mempty classField (classPattern p)
          , maybe mempty moduleField (modulePattern p)
          ]
      typeField t = "instance = " ++ show t
      classField c = "class = " ++ show c
      moduleField m = "module = " ++ show m


configToToml :: Config -> String
configToToml Config{..}
  = unlines . intersperse mempty $
      [ "roots = " ++ show (Set.toList rootPatterns)
      , "type-class-roots = " ++ map toLower (show typeClassRoots)
      , "root-instances = " ++ "[" ++ intercalate "," (map showInstancePattern rootInstances') ++ "]"
      , "unused-types = " ++ map toLower (show unusedTypes)
      ]
  where
    rootInstances' = Set.toList rootInstances
