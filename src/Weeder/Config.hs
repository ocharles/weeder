{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}
{-# language FlexibleInstances #-}
{-# language DeriveTraversable #-}
{-# language NamedFieldPuns #-}

module Weeder.Config
  ( -- * Config
    Config
  , ConfigParsed
  , ConfigType(..)
  , compileConfig
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
import Data.Bifunctor (bimap)
import Data.Char (toLower)
import Data.List (intersperse, intercalate)

-- containers
import Data.Containers.ListUtils (nubOrd)

-- regex-tdfa
import Text.Regex.TDFA ( Regex, RegexOptions ( defaultExecOpt, defaultCompOpt ) )
import Text.Regex.TDFA.TDFA ( patternToRegex )
import Text.Regex.TDFA.ReadRegex ( parseRegex )

-- toml-reader
import qualified TOML


-- | Configuration for Weeder analysis.
type Config = ConfigType Regex


-- | Configuration that has been parsed from TOML (and can still be
-- converted back), but not yet compiled to a 'Config'.
type ConfigParsed = ConfigType String


-- | Underlying type for 'Config' and 'ConfigParsed'.
data ConfigType a = Config
  { rootPatterns :: [a]
    -- ^ Any declarations matching these regular expressions will be added to
    -- the root set.
  , typeClassRoots :: Bool
    -- ^ If True, consider all declarations in a type class as part of the root
    -- set. Overrides root-instances.
  , rootInstances :: [InstancePattern a]
    -- ^ All matching instances will be added to the root set. An absent field
    -- will always match.
  , unusedTypes :: Bool
    -- ^ Toggle to look for and output unused types. Type family instances will
    -- be marked as implicit roots.
  , rootModules :: [a]
    -- ^ All matching modules will be added to the root set.
  } deriving (Eq, Show, Functor, Foldable, Traversable)


-- | Construct via InstanceOnly, ClassOnly or ModuleOnly,
-- and combine with the Semigroup instance. The Semigroup
-- instance ignores duplicate fields, prioritising the
-- left argument.
data InstancePattern a = InstancePattern
  { instancePattern :: Maybe a
  , classPattern :: Maybe a
  , modulePattern :: Maybe a
  } deriving (Eq, Show, Ord, Functor, Foldable, Traversable)


instance Semigroup (InstancePattern a) where
  InstancePattern i c m <> InstancePattern i' c' m' =
    InstancePattern (i <|> i') (c <|> c') (m <|> m')


pattern InstanceOnly, ClassOnly, ModuleOnly :: a -> InstancePattern a
pattern InstanceOnly t = InstancePattern (Just t) Nothing Nothing
pattern ClassOnly c = InstancePattern Nothing (Just c) Nothing
pattern ModuleOnly m = InstancePattern Nothing Nothing (Just m)


defaultConfig :: ConfigParsed
defaultConfig = Config
  { rootPatterns = [ "Main.main", "^Paths_.*"]
  , typeClassRoots = False
  , rootInstances = [ ClassOnly "\\.IsString$", ClassOnly "\\.IsList$" ]
  , unusedTypes = False
  , rootModules = mempty
  }


instance TOML.DecodeTOML Config where
  tomlDecoder = do
    conf <- TOML.tomlDecoder
    either fail pure $ compileConfig conf


instance TOML.DecodeTOML ConfigParsed where
  tomlDecoder = do
    rootPatterns <- TOML.getFieldOr (rootPatterns defaultConfig) "roots"
    typeClassRoots <- TOML.getFieldOr (typeClassRoots defaultConfig) "type-class-roots"
    rootInstances <- TOML.getFieldOr (rootInstances defaultConfig) "root-instances"
    unusedTypes <- TOML.getFieldOr (unusedTypes defaultConfig) "unused-types"
    rootModules <- TOML.getFieldOr (rootModules defaultConfig) "root-modules"

    pure Config{..}


decodeNoDefaults :: TOML.Decoder Config
decodeNoDefaults = do
  rootPatterns <- TOML.getField "roots"
  typeClassRoots <- TOML.getField "type-class-roots"
  rootInstances <- TOML.getField "root-instances"
  unusedTypes <- TOML.getField "unused-types"
  rootModules <- TOML.getField "root-modules"

  either fail pure $ compileConfig Config{..}


instance TOML.DecodeTOML (InstancePattern String) where
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
decodeInstancePattern :: TOML.Decoder (InstancePattern String)
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


showInstancePattern :: Show a => InstancePattern a -> String
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


compileRegex :: String -> Either String Regex
compileRegex = bimap show (\p -> patternToRegex p defaultCompOpt defaultExecOpt) . parseRegex


compileConfig :: ConfigParsed -> Either String Config
compileConfig conf@Config{ rootInstances, rootPatterns, rootModules } =
  traverse compileRegex conf'
  where
    rootInstances' = nubOrd rootInstances
    rootPatterns' = nubOrd rootPatterns
    rootModules' = nubOrd rootModules
    conf' = conf{ rootInstances = rootInstances', rootPatterns = rootPatterns', rootModules = rootModules' }


configToToml :: ConfigParsed -> String
configToToml Config{..}
  = unlines . intersperse mempty $
      [ "roots = " ++ show rootPatterns
      , "type-class-roots = " ++ map toLower (show typeClassRoots)
      , "root-instances = " ++ "[" ++ intercalate "," (map showInstancePattern rootInstances') ++ "]"
      , "unused-types = " ++ map toLower (show unusedTypes)
      , "root-modules = " ++ show rootModules
      ]
  where
    rootInstances' = rootInstances
