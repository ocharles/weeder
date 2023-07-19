{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TupleSections #-}
{-# language LambdaCase #-}

module Weeder.Config 
  ( Config(..)
  , configToToml
  , defaultConfig 
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
  , rootClasses :: Set (Maybe String, Maybe String)
    -- ^ All instances of type classes matching these regular expressions will
    -- be added to the root set. Note that this does not mark the class itself
    -- as a root, so if the class has no instances then it will not be made
    -- reachable. Specified as a pair (class, module) and a Nothing value
    -- always matches.
  , rootInstances :: Set (Maybe String, Maybe String)
    -- ^ All instances with types matching these regular expressions will 
    -- be added to the root set. Specified as a pair (instance, module) and
    -- a Nothing value always matches.
  } deriving Eq


defaultConfig :: Config
defaultConfig = Config
  { rootPatterns = Set.fromList [ "Main.main", "^Paths_.*"]
  , typeClassRoots = False
  , rootClasses = Set.fromList [ (Just "IsString", Nothing), (Just "IsList", Nothing) ]
  , rootInstances = mempty
  }


instance TOML.DecodeTOML Config where
  tomlDecoder = do
    rootPatterns <- TOML.getFieldOr (rootPatterns defaultConfig) "roots"
    typeClassRoots <- TOML.getFieldOr (typeClassRoots defaultConfig) "type-class-roots"
    rootClasses <- maybe (rootClasses defaultConfig) Set.fromList <$> 
      TOML.getFieldOptWith (TOML.getArrayOf $ decodeOptionalTable "class" "module") "root-classes" 
    rootInstances <- maybe (rootInstances defaultConfig) Set.fromList <$>
      TOML.getFieldOptWith (TOML.getArrayOf $ decodeOptionalTable "instance" "module") "root-instances" 

    pure Config{..}


-- | Decoder for a value of any of the forms:
--
-- @{<primary> = a, <secondary> = b} -> (Just a, Just b)@
--
-- @{<primary> = a} -> (Just a, Nothing)@
--
-- @{<secondary> = b} -> (Nothing, Just b)@
--
-- @a -> (Just a, Nothing)@
--
-- @{} -> (Nothing, Nothing)@
decodeOptionalTable :: (TOML.DecodeTOML a, TOML.DecodeTOML b) 
  => Text -- ^ Primary field name
  -> Text -- ^ Secondary field name
  -> TOML.Decoder (Maybe a, Maybe b)
decodeOptionalTable primary secondary = 
  ((,) <$> TOML.getFieldOpt primary <*> TOML.getFieldOpt secondary) 
    <|> fmap (,Nothing) TOML.tomlDecoder
  

showOptionalTable :: (Show a, Show b) => Text -> Text -> (Maybe a, Maybe b) -> String
showOptionalTable primary secondary = \case
    (Just a, Nothing) -> show a
    (Nothing, Just b) -> "{" ++ T.unpack secondary ++ " = " ++ show b ++ "}"
    (Just a, Just b) -> "{" ++ T.unpack primary ++ " = " ++ show a ++ ", " ++ T.unpack secondary ++ " = " ++ show b ++ "}"
    (Nothing, Nothing) -> "{}"


configToToml :: Config -> String
configToToml Config{..}
  = unlines . intersperse mempty $
      [ "roots = " ++ show (Set.toList rootPatterns)
      , "type-class-roots = " ++ map toLower (show typeClassRoots)
      , "root-classes = " ++ "[" ++ intercalate "," (map (showOptionalTable "class" "module") rootClasses') ++ "]"
      , "root-instances = " ++ "[" ++ intercalate "," (map (showOptionalTable "instance" "module") rootInstances') ++ "]"
      ]
  where
    rootClasses' = Set.toList rootClasses
    rootInstances' = Set.toList rootInstances
