{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}
{-# language GeneralisedNewtypeDeriving #-}

module Weeder.Config 
  ( -- * Config
    Config(..)
  , configToToml
  , decodeNoDefaults
  , defaultConfig 
    -- * Marking instances as roots
  , InstancePattern(..)
  , modulePattern
  , instancePattern
  , classPattern
  , pattern InstanceOnly
  , pattern ClassOnly
  , pattern ModuleOnly
  , pattern InstanceAndModule
  , pattern InstanceAndClass
  , pattern ClassAndModule
  , pattern InstanceClassAndModule
  ) 
   where

-- base
import Control.Applicative ((<|>), empty)
import Data.Char (toLower)
import Data.List (intersperse, intercalate)

-- containers
import Data.Set ( Set )
import qualified Data.Set as Set

-- these
import Data.These ( These( This, That, These ) )

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


type PatternWith = These String

type PatternWithModule = PatternWith String

-- Note that the Semigroup instance will merge ClassOnly "a" and ClassOnly "b" into
-- ClassOnly "ab" - this is probably not what one would want. It's still useful
-- for scenarios like ClassOnly "a" <> ModuleOnly "b" = ClassAndModule "a" "b"
newtype InstancePattern = InstancePattern (PatternWith PatternWithModule)
  deriving (Eq, Show, Ord, Semigroup)

pattern InstanceOnly, ClassOnly, ModuleOnly :: String -> InstancePattern
pattern InstanceOnly t = InstancePattern (This t)
pattern ClassOnly c = InstancePattern (That (This c))
pattern ModuleOnly m = InstancePattern (That (That m))

pattern InstanceAndModule, InstanceAndClass, ClassAndModule :: String -> String -> InstancePattern
pattern InstanceAndModule t m = InstancePattern (These t (That m))
pattern InstanceAndClass t c = InstancePattern (These t (This c))
pattern ClassAndModule c m = InstancePattern (That (These c m))

pattern InstanceClassAndModule :: String -> String -> String -> InstancePattern
pattern InstanceClassAndModule t c m = InstancePattern (These t (These c m))
{-# complete InstanceOnly, ClassOnly, ModuleOnly, InstanceAndModule, InstanceAndClass, ClassAndModule, InstanceClassAndModule #-}


modulePattern :: InstancePattern -> Maybe String
modulePattern = \case
  ModuleOnly m -> Just m
  InstanceAndModule _ m -> Just m
  ClassAndModule _ m -> Just m
  InstanceClassAndModule _ _ m -> Just m
  _ -> Nothing


instancePattern :: InstancePattern -> Maybe String
instancePattern = \case
  InstanceOnly t -> Just t
  InstanceAndModule t _ -> Just t
  InstanceAndClass t _ -> Just t
  InstanceClassAndModule t _ _ -> Just t
  _ -> Nothing


classPattern :: InstancePattern -> Maybe String
classPattern = \case
  ClassOnly c -> Just c
  InstanceAndClass _ c -> Just c
  ClassAndModule c _ -> Just c
  InstanceClassAndModule _ c _ -> Just c
  _ -> Nothing


defaultConfig :: Config
defaultConfig = Config
  { rootPatterns = Set.fromList [ "Main.main", "^Paths_.*"]
  , typeClassRoots = False
  , rootInstances = Set.fromList [ ClassOnly "IsString", ClassOnly "IsList" ]
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
