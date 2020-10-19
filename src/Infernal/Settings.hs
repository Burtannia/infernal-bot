{-# LANGUAGE OverloadedStrings #-}

module Infernal.Settings where

import Data.Text
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?))
import Control.Applicative
import qualified Data.ByteString as BS (readFile)

data Settings = Settings
    { token :: Text
    , instagram :: Maybe UpdateLink
    , twitch :: Maybe UpdateLink
    , twitter :: Maybe UpdateLink
    , youtube :: Maybe UpdateLink
    , commands :: [Command]
    } deriving Show

data UpdateLink = UpdateLink
    { user :: Text
    , channel :: Text
    } deriving Show

type Response = Text

data Command = Command
    { name :: Text
    , response :: Response
    } deriving Show

instance FromJSON Settings where
    parseJSON (Y.Object v) = 
        Settings <$>
        v .: "token" <*>
        v .:? "instagram" <*>
        v .:? "twitch" <*>
        v .:? "twitter" <*>
        v .:? "youtube" <*>
        v .: "commands"
    parseJSON _ = fail "Invalid Settings"
    
instance FromJSON UpdateLink where
    parseJSON (Y.Object v) = 
        UpdateLink <$>
        v .: "user" <*>
        v .: "channel"
    parseJSON _ = fail "Invalid UpdateLink"

instance FromJSON Command where
    parseJSON (Y.Object v) = 
        Command <$>
        v .: "name" <*>
        v .: "response"
    parseJSON _ = fail "Invalid Command"

readSettings :: IO Settings
readSettings = do
    settingsYaml <- BS.readFile "settings.yaml"
    Y.decodeThrow settingsYaml