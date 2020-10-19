{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Infernal where

import Control.Monad (when, void, liftM)
import Control.Monad.Trans.Class (lift)
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text as T (tail)
import qualified Data.Text.IO as TIO
import UnliftIO.Concurrent (threadDelay)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Infernal.Settings
import Infernal.Utils

runInfernal :: IO ()
runInfernal = do
    settings <- readSettings
    userFacingError <- runDiscord $ def
        { discordToken = token settings
        , discordOnEvent = eventHandler settings }
    TIO.putStrLn userFacingError

findResponse :: Text -> [Command] -> Maybe Response
findResponse t cs = firstJust $ map (matchCmd t) cs

matchCmd :: Text -> Command -> Maybe Response
matchCmd t Command {..}
    | t == name = Just response
    | otherwise = Nothing

postTextIn :: ChannelId -> Text -> DiscordHandler ()
postTextIn ch t = void (restCall $ R.CreateMessage ch t)

-- monad transformer around DiscordHandler to contain Settings?

cmdHandler :: Message -> Settings -> DiscordHandler ()
cmdHandler m Settings {..} = whenIsJust mResponse (postTextIn channel)
    where
        mResponse = findResponse (T.tail $ messageText m) commands
        channel = messageChannel m

eventHandler :: Settings -> Event -> DiscordHandler ()
eventHandler settings event = case event of
        MessageCreate m -> do
            lift (print m)
            when (isUserCmd m) $ cmdHandler m settings
        _ -> return ()

isUserCmd :: Message -> Bool
isUserCmd m = fromUser m && isCmd m

fromUser :: Message -> Bool
fromUser = not . fromBot

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isCmd :: Message -> Bool
isCmd = ("!" `isPrefixOf`) . messageText