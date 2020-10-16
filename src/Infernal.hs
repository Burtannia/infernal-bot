{-# LANGUAGE OverloadedStrings #-}

module Infernal where

import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO
import UnliftIO.Concurrent (threadDelay)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Infernal.Settings

runInfernal :: IO ()
runInfernal = do
    settings <- readSettings
    userFacingError <- runDiscord $ def
        { discordToken = token settings
        , discordOnEvent = eventHandler }
    TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
        MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
                _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
                threadDelay (4 * 10^6)
                _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
                pure ()
        _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower