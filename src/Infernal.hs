{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Infernal where

import           Calamity                       (EventType (GuildMemberAddEvt, GuildMemberRemoveEvt, MessageCreateEvt),
                                                 GuildRequest (AddGuildMemberRole, RemoveGuildMember),
                                                 Snowflake, Token (BotToken),
                                                 Upgradeable (upgrade), User,
                                                 defaultIntents,
                                                 intentGuildMembers,
                                                 intentGuildPresences, invoke,
                                                 react, runBotIO, tell)
import           Calamity.Cache.InMemory        (runCacheInMemory)
import           Calamity.Commands              (addCommands, command,
                                                 helpCommand, useConstantPrefix)
import           Calamity.Commands.Context      (useFullContext)
import qualified Calamity.Internal.SnowflakeMap as SM
import           Calamity.Metrics.Noop          (runMetricsNoop)
import           Control.Concurrent             (MVar, newMVar)
import           Control.Lens                   (lazy, (&), (.~), (^.))
import           Control.Monad                  (forM_, void, when)
import qualified Data.Aeson                     as Aeson
import           Data.Flags                     ((.+.))
import           Data.Foldable                  (for_)
import           Data.Generics.Labels           ()
import           Data.Text.Lazy                 (Text)
import qualified Data.Vector.Unboxing           as V (notElem)
import qualified Database.Persist.Sqlite        as DB
import qualified Di
import           DiPolysemy                     (debug, info, runDiToIO,
                                                 warning)
import           Options.Generic                (unwrapRecord)
import qualified Polysemy                       as P
import qualified Polysemy.Async                 as P
import           System.Exit                    (die)
import           TextShow                       (showtl)

import           Infernal.Challenge             (ChallengeMap, checkResponse,
                                                 deleteChallenge, evictLoop,
                                                 insertChallenge,
                                                 lookupChallenge,
                                                 mkChallengeMap, newChallenge,
                                                 showChallenge)
import           Infernal.Config                (CLIOptions, Config)
import           Infernal.Database              (db, runPersistWith)
import           Infernal.Schema                (migrateAll)
import           Infernal.Utils                 (canVerify, channelIsDM,
                                                 isHuman)

main :: IO ()
main = do
    opts <- unwrapRecord @_ @CLIOptions "InfernalBot"
    path <- case opts ^. #config of
        Just path -> pure path
        Nothing   -> die "Error: no config specified"
    cfg <- Aeson.eitherDecodeFileStrict path >>= either die pure
    chvar <- mkChallengeMap >>= newMVar
    runBotWith cfg chvar

runBotWith :: Config -> MVar ChallengeMap -> IO ()
runBotWith cfg chvar = Di.new $ \di ->
    void
    . P.runFinal
    . P.embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . runPersistWith (cfg ^. #database)
    . useConstantPrefix (cfg ^. #commandPrefix . lazy)
    . useFullContext
    . runBotIO
        (BotToken (cfg ^. #botToken . lazy))
        (defaultIntents .+. intentGuildMembers .+. intentGuildPresences)
    $ do
        db $ DB.runMigration migrateAll
        info @Text "Bot starting up!"

        P.asyncToIOFinal $ P.async $ evictLoop chvar (cfg ^. #challengeEvictScanMins)

        react @'GuildMemberAddEvt $ \mem -> do
            info @Text $ "Member " <> showtl (mem ^. #id) <> " joined, sending challenge"
            mguild <- upgrade (mem ^. #guildID)
            let guildName = maybe "the guild" (^. #name) mguild
            challenge <- P.embed $ newChallenge chvar cfg mem
            void . tell mem $ showChallenge guildName challenge

        react @'GuildMemberRemoveEvt $ \mem -> do
            info @Text $ "Member " <> showtl (mem ^. #id) <> " left/removed"
            P.embed $ deleteChallenge chvar (mem ^. #id)

        react @'MessageCreateEvt $ \msg -> do
            mchannel <- upgrade (msg ^. #channelID)
            muser <- upgrade (msg ^. #author)
            let isDM = maybe False channelIsDM mchannel
            for_ muser $ \user ->
                when (isDM && isHuman user) $ do
                    info @Text $ "DM received from " <> showtl (user ^. #id)
                    mc <- P.embed $ lookupChallenge chvar (user ^. #id)
                    for_ mc $ \challenge -> do
                        if checkResponse (msg ^. #content) challenge
                            then do
                                info @Text "Correct response received"
                                mguild <- upgrade (challenge ^. #challengeGuildID)
                                case mguild of
                                    Nothing -> do
                                        warning @Text "Guild from challenge was nothing"
                                    Just g -> do
                                        void . tell msg $ "Thank you! You are now verified in " <> (g ^. #name) <> "!"
                                        void . invoke $ AddGuildMemberRole (challenge ^. #challengeGuildID) (user ^. #id) (cfg ^. #verifiedRole)
                                        P.embed $ deleteChallenge chvar (user ^. #id)
                            else do
                                info @Text "Incorrect response received"
                                let remaining = (challenge ^. #challengeAttemptsRemaining) - 1
                                if remaining == 0
                                    then do
                                        info @Text "Challenge failure, attempts exhausted"
                                        void $ tell @Text msg
                                            "Incorrect, you are out of attempts and will now be kicked from the server."
                                        void . invoke $ RemoveGuildMember (challenge ^. #challengeGuildID) (user ^. #id)
                                        -- Challenge is removed via GuildMemberRemove event handler
                                    else do
                                        debug @Text "Updating attempts"
                                        void $ tell @Text msg $
                                            "Incorrect, you have "
                                            <> showtl remaining
                                            <> " attempts remaining."
                                        P.embed $ insertChallenge chvar (user ^. #id)
                                            (challenge & #challengeAttemptsRemaining .~ remaining)

        addCommands $ do
            let vRole = cfg ^. #verifiedRole

            helpCommand

            command @'[Snowflake User] "verify" $
                \ctx userID -> case ctx ^. #guild of
                    Just guild -> do
                        info @Text $ "Manually verifying user " <> showtl userID
                        void . invoke $ AddGuildMemberRole guild userID vRole
                    Nothing -> do
                        info @Text "Can only verify users in guilds."
                        void $ tell @Text ctx "Can only verify users in guilds."

            command @'[] "verifyAll" $
                \ctx -> case ctx ^. #guild of
                    Just guild -> do
                        hasPerm <- canVerify (ctx ^. #member)

                        if hasPerm
                            then do
                                info @Text "Manually verifying all users"
                                P.embed $ print $ SM.toList (guild ^. #members)
                                forM_ (SM.elems (guild ^. #members)) $ \mem -> do
                                    if vRole `V.notElem` (mem ^. #roles)
                                        then do
                                            debug @Text "Add verified role"
                                            void . invoke $ AddGuildMemberRole guild (mem ^. #id) vRole
                                        else
                                            debug @Text "Already had verified role"
                            else do
                                info @Text "ManageRole permission not found."
                                void $ tell @Text ctx "Permission Denied"

                    Nothing -> do
                        info @Text "Can only verify users in guilds."
                        void $ tell @Text ctx "Can only verify users in guilds."
