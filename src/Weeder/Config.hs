{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}

module Weeder.Config 
  ( Config(..)
  , configToToml
  , decodeNoDefaults
  , defaultConfig 
  , PatternWithModule(..)
  , modulePattern
  , mainPattern
  ) 
   where

-- base
import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.List (intersperse, intercalate)

-- containers
import Data.Set ( Set )
import qualified Data.Set as Set

-- text
import qualified Data.Text as T
import Data.Text (Text)

-- toml-reader
import qualified TOML


-- | Configuration for Weeder analysis.
data Config = Config
  { rootPatterns :: Set String
    -- ^ Any declarations matching these regular expressions will be added to
    -- the root set.
  , typeClassRoots :: Bool
    -- ^ If True, consider all declarations in a type class as part of the root
    -- set. Weeder is currently unable to identify whether or not a type class
    -- instance is used - enabling this option can prevent false positives.
  , rootClasses :: Set PatternWithModule
    -- ^ All instances of type classes matching these regular expressions will
    -- be added to the root set. Note that this does not mark the class itself
    -- as a root, so if the class has no instances then it will not be made
    -- reachable.
  , rootInstances :: Set PatternWithModule
    -- ^ All instances with types matching these regular expressions will 
    -- be added to the root set.
  } deriving (Eq, Show)


data PatternWithModule 
  = PatternOnly String 
    -- ^ Either a string literal or a TOML table 
    -- with only class/instance specified
  | ModuleOnly String 
    -- ^ TOML table with only module specified
  | PatternAndModule String String 
    -- ^ TOML table with both class/instance and module
  deriving (Eq, Ord, Show)


modulePattern :: PatternWithModule -> Maybe String
modulePattern = \case
  PatternOnly _ -> Nothing
  ModuleOnly m -> Just m
  PatternAndModule _ m -> Just m


mainPattern :: PatternWithModule -> Maybe String
mainPattern = \case
  PatternOnly n -> Just n
  ModuleOnly _ -> Nothing
  PatternAndModule n _ -> Just n


defaultConfig :: Config
defaultConfig = Config
  { rootPatterns = Set.fromList [ "Main.main", "^Paths_.*"]
  , typeClassRoots = False
  , rootClasses = Set.fromList [ PatternOnly "IsString", PatternOnly "IsList" ]
  , rootInstances = mempty
  }


instance TOML.DecodeTOML Config where
  tomlDecoder = do
    rootPatterns <- TOML.getFieldOr (rootPatterns defaultConfig) "roots"
    typeClassRoots <- TOML.getFieldOr (typeClassRoots defaultConfig) "type-class-roots"
    rootClasses <- maybe (rootClasses defaultConfig) Set.fromList <$> 
      TOML.getFieldOptWith (TOML.getArrayOf $ decodePatternWithModule "class") "root-classes" 
    rootInstances <- maybe (rootInstances defaultConfig) Set.fromList <$>
      TOML.getFieldOptWith (TOML.getArrayOf $ decodePatternWithModule "instance") "root-instances" 

    pure Config{..}


decodeNoDefaults :: TOML.Decoder Config
decodeNoDefaults = do
  rootPatterns <- TOML.getField "roots"
  typeClassRoots <- TOML.getField "type-class-roots"
  rootClasses <- Set.fromList <$> TOML.getFieldWith (TOML.getArrayOf $ decodePatternWithModule "class") "root-classes"
  rootInstances <- Set.fromList <$> TOML.getFieldWith (TOML.getArrayOf $ decodePatternWithModule "instance") "root-instances"

  pure Config{..}


-- | Decoder for a value of any of the forms:
--
-- @{<primaryField> = a, module = b} -> NameAndModule a b@
--
-- @a -> NameOnly a@
--
-- @{<primaryField> = a} -> NameOnly a@
--
-- @{module = b} -> ModuleOnly b@
decodePatternWithModule :: Text -> TOML.Decoder PatternWithModule
decodePatternWithModule primaryField = decodeNameAndModule <|> decodeNameOnly <|> decodeModuleOnly

  where

    decodeNameOnly = PatternOnly <$> (TOML.tomlDecoder <|> TOML.getField primaryField)

    decodeModuleOnly = ModuleOnly <$> TOML.getField "module"

    decodeNameAndModule = PatternAndModule <$> TOML.getField primaryField <*> TOML.getField "module"


showPatternWithModule :: Text -> PatternWithModule -> String
showPatternWithModule primaryField = \case
    PatternOnly a -> show a
    ModuleOnly b -> "{" ++ "module" ++ " = " ++ show b ++ "}"
    PatternAndModule a b -> "{" ++ T.unpack primaryField ++ " = " ++ show a ++ ", " ++ "module" ++ " = " ++ show b ++ "}"


configToToml :: Config -> String
configToToml Config{..}
  = unlines . intersperse mempty $
      [ "roots = " ++ show (Set.toList rootPatterns)
      , "type-class-roots = " ++ map toLower (show typeClassRoots)
      , "root-classes = " ++ "[" ++ intercalate "," (map (showPatternWithModule "class") rootClasses') ++ "]"
      , "root-instances = " ++ "[" ++ intercalate "," (map (showPatternWithModule "instance") rootInstances') ++ "]"
      ]
  where
    rootClasses' = Set.toList rootClasses
    rootInstances' = Set.toList rootInstances
