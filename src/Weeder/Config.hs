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
    -- * Compiled regular expressions
  , CompiledRegex(..)
    -- * Configuration provenance
  , Configured(..)
    -- * Marking instances as roots
  , InstancePattern
  , modulePattern
  , instancePattern
  , classPattern
  , showInstancePattern
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

-- text
import Data.Text (Text)

-- regex-tdfa
import Text.Regex.TDFA ( Regex, RegexOptions ( defaultExecOpt, defaultCompOpt ) )
import Text.Regex.TDFA.TDFA ( patternToRegex )
import Text.Regex.TDFA.ReadRegex ( parseRegex )

-- toml-reader
import qualified TOML


-- | Configuration for Weeder analysis.
type Config = ConfigType CompiledRegex


-- | A compiled regular expression, paired with the source string it was
-- compiled from. We keep the source around so that we can report which
-- configured pattern is responsible when a pattern matches no identifiers.
data CompiledRegex = CompiledRegex
  { regexSource :: String
  , compiledRegex :: Regex
  }


-- | A configured value together with whether it was set explicitly (as opposed
-- to falling back to the default). We track this for the root sections so that
-- self-weeding only reports entries the user actually wrote: pointing out that
-- a default they never configured is unused would not be actionable.
data Configured a = Configured
  { configuredValue :: a
  , configuredExplicitly :: Bool
  } deriving (Eq, Show, Functor, Foldable, Traversable)


-- | Configuration that has been parsed from TOML (and can still be
-- converted back), but not yet compiled to a 'Config'.
type ConfigParsed = ConfigType String


-- | Underlying type for 'Config' and 'ConfigParsed'.
data ConfigType a = Config
  { rootPatterns :: Configured [a]
    -- ^ Any declarations matching these regular expressions will be added to
    -- the root set.
  , typeClassRoots :: Bool
    -- ^ If True, consider all declarations in a type class as part of the root
    -- set. Overrides root-instances.
  , rootInstances :: Configured [InstancePattern a]
    -- ^ All matching instances will be added to the root set. An absent field
    -- will always match.
  , unusedTypes :: Bool
    -- ^ Toggle to look for and output unused types. Type family instances will
    -- be marked as implicit roots.
  , rootModules :: Configured [a]
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
  { rootPatterns = Configured [ "Main.main", "^Paths_.*"] False
  , typeClassRoots = False
  , rootInstances = Configured [ ClassOnly "\\.IsString$", ClassOnly "\\.IsList$" ] False
  , unusedTypes = False
  , rootModules = Configured mempty False
  }


instance TOML.DecodeTOML Config where
  tomlDecoder = do
    conf <- TOML.tomlDecoder
    either fail pure $ compileConfig conf


instance TOML.DecodeTOML ConfigParsed where
  tomlDecoder = do
    rootPatterns <- getConfigured defaultRootPatterns "roots"
    typeClassRoots <- TOML.getFieldOr (typeClassRoots defaultConfig) "type-class-roots"
    rootInstances <- getConfigured defaultRootInstances "root-instances"
    unusedTypes <- TOML.getFieldOr (unusedTypes defaultConfig) "unused-types"
    rootModules <- getConfigured defaultRootModules "root-modules"

    pure Config{..}
    where
      Config
        { rootPatterns = Configured defaultRootPatterns _
        , rootInstances = Configured defaultRootInstances _
        , rootModules = Configured defaultRootModules _
        } = defaultConfig


-- | Decode an optional field, marking it 'configuredExplicitly' when present
-- and falling back to the given default otherwise.
getConfigured :: TOML.DecodeTOML a => a -> Text -> TOML.Decoder (Configured a)
getConfigured def key = configured def <$> TOML.getFieldOpt key


-- | A value from the TOML if present, or the given default otherwise, recording
-- in 'configuredExplicitly' which of the two it was.
configured :: a -> Maybe a -> Configured a
configured def = \case
  Just v  -> Configured v True
  Nothing -> Configured def False


decodeNoDefaults :: TOML.Decoder Config
decodeNoDefaults = do
  -- In this mode every field must be specified, so every root section is
  -- explicit by construction.
  rootPatterns <- explicit <$> TOML.getField "roots"
  typeClassRoots <- TOML.getField "type-class-roots"
  rootInstances <- explicit <$> TOML.getField "root-instances"
  unusedTypes <- TOML.getField "unused-types"
  rootModules <- explicit <$> TOML.getField "root-modules"

  either fail pure $ compileConfig Config{..}
  where
    explicit v = Configured v True


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


compileRegex :: String -> Either String CompiledRegex
compileRegex src =
  bimap show (\p -> CompiledRegex src (patternToRegex p defaultCompOpt defaultExecOpt)) (parseRegex src)


compileConfig :: ConfigParsed -> Either String Config
compileConfig conf@Config{ rootInstances, rootPatterns, rootModules } =
  traverse compileRegex conf'
  where
    conf' = conf
      { rootInstances = fmap nubOrd rootInstances
      , rootPatterns = fmap nubOrd rootPatterns
      , rootModules = fmap nubOrd rootModules
      }


configToToml :: ConfigParsed -> String
configToToml Config{..}
  = unlines . intersperse mempty $
      [ "roots = " ++ show (configuredValue rootPatterns)
      , "type-class-roots = " ++ map toLower (show typeClassRoots)
      , "root-instances = " ++ "[" ++ intercalate "," (map showInstancePattern (configuredValue rootInstances)) ++ "]"
      , "unused-types = " ++ map toLower (show unusedTypes)
      , "root-modules = " ++ show (configuredValue rootModules)
      ]
