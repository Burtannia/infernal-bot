module Infernal where

import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Commands.Context (useFullContext)
import           Calamity.Metrics.Noop
import           Control.Lens
import           Control.Monad
import qualified Data.Aeson                as Aeson
import           Data.Flags                ((.+.))
import           Data.Generics.Labels      ()
import           Data.Maybe
import           Data.Text.Lazy            (Text)
import Data.Foldable (for_)
import qualified Df1
import qualified Di
import           DiPolysemy
import qualified Polysemy                  as P
import System.Exit
import           Options.Generic hiding (Text)
import qualified Data.HashTable.IO as H

import Infernal.Challenge
import Infernal.Config

type HashMap k v = H.BasicHashTable k v

mkChallengeMap :: IO (HashMap (Snowflake User) Challenge)
mkChallengeMap = H.new

newtype ShowMsg a = ShowMsg a

instance (Show a) => Df1.ToMessage (ShowMsg a) where
    message (ShowMsg x) = Df1.message $ show x

main :: IO ()
main = do
    opts <- unwrapRecord @_ @CLIOptions "InfernalBot"
    path <- case opts ^. #config of
        Just path -> pure path
        Nothing -> die "Error: no config specified"
    cfg <- Aeson.eitherDecodeFileStrict path >>= either die pure
    runBotWith cfg

channelIsDM :: Channel -> Bool
channelIsDM (DMChannel' _) = True
channelIsDM _ = False

isHuman :: User -> Bool
isHuman user = fromMaybe True (user ^. #bot)

runBotWith :: Config -> IO ()
runBotWith cfg = Di.new $ \di ->
    void
    . P.runFinal
    . P.embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . useConstantPrefix (cfg ^. #commandPrefix . lazy)
    . useFullContext
    . runBotIO
        (BotToken (cfg ^. #botToken . lazy))
        (defaultIntents .+. intentGuildMembers)
    $ do 
        info @Text "Bot starting up!"
        challenges <- P.embed mkChallengeMap

        _ <- react @'GuildMemberAddEvt $ \mem -> do
            info @Text "User joined"
            void . invoke $ AddGuildMemberRole (mem ^. #guildID) (mem ^. #id) (cfg ^. #verifiedRole)
            info @Text "Challenging user"
            mguild <- upgrade (mem ^. #guildID)
            let guildName = fromMaybe "the guild" $ fmap (^. #name) mguild
            challenge <- P.embed $
                mkChallenge (mem ^. #id) (mem ^. #guildID) (cfg ^. #challengeAttempts)
            P.embed $ H.insert challenges (mem ^. #id) challenge
            void . tell mem $ showChallenge guildName challenge

        _ <- react @'MessageCreateEvt $ \msg -> do
            mchannel <- upgrade (msg ^. #channelID)
            muser <- upgrade (msg ^. #author)
            let isDM = maybe False channelIsDM mchannel
            for_ muser $ \user ->
                when (isDM && isHuman user) $ do              
                    info @Text "DM received"
                    -- if challenge available for user
                        -- check response
                        -- update hashmap
                        -- if X failures, kick

        addCommands $ do
            helpCommand