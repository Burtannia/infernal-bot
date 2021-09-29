{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Infernal where

import           Calamity                       (EventType (..), Guild,
                                                 GuildRequest (AddGuildMemberRole, RemoveGuildMember),
                                                 Message, Snowflake,
                                                 Token (BotToken),
                                                 Upgradeable (upgrade), User,
                                                 UserRequest (LeaveGuild),
                                                 defaultIntents, getID,
                                                 intentGuildMembers,
                                                 intentGuildPresences, invoke,
                                                 react, runBotIO, tell)
import           Calamity.Cache.InMemory        (runCacheInMemory)
import           Calamity.Commands              (addCommands, command,
                                                 helpCommand, useConstantPrefix)
import           Calamity.Commands.Context      (useFullContext)
import qualified Calamity.Internal.SnowflakeMap as SM
import           Calamity.Metrics.Noop          (runMetricsNoop)
import           Control.Lens                   (lazy, (^.))
import           Control.Monad                  (forM_, unless, void, when)
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

import           Infernal.Challenge             (ChallengeResult (..),
                                                 checkChallenge,
                                                 deleteChallenge,
                                                 deleteChallenge', evictLoop,
                                                 newChallenge, showChallenge)
import           Infernal.Config                (CLIOptions, Config)
import           Infernal.Database              (PersistBotC, db,
                                                 runPersistWith)
import           Infernal.Schema                (migrateAll)
import           Infernal.Utils                 (canVerify, channelIsDM,
                                                 isHuman)
import qualified Polysemy.Reader                as P

main :: IO ()
main = do
    opts <- unwrapRecord @_ @CLIOptions "InfernalBot"
    path <- case opts ^. #config of
        Just path -> pure path
        Nothing   -> die "Error: no config specified"
    cfg <- Aeson.eitherDecodeFileStrict path >>= either die pure
    runBotWith cfg

runBotWith :: Config -> IO ()
runBotWith cfg = Di.new $ \di ->
    void
    . P.runFinal
    . P.embedToFinal @IO
    . P.runReader cfg
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

        guild <- do
            mg <- upgrade (cfg ^. #guildID)
            maybe (P.embed $ die "Invalid config guild") pure mg

        unless (cfg ^. #firstTimeStart) $ challengeNewMembers guild

        P.asyncToIOFinal $ P.async $ evictLoop (cfg ^. #challengeEvictScanMins)

        react @'GuildCreateEvt $ \(g, _) -> do
            let joinedID = getID g
            when (joinedID /= (cfg ^. #guildID)) $ do
                warning @Text $ "Leaving unauthorized guild: " <> showtl joinedID
                void . invoke $ LeaveGuild g

        react @'GuildMemberAddEvt $ \mem -> do
            info @Text $ "Member " <> showtl (mem ^. #id) <> " joined, sending challenge"
            mguild <- upgrade (mem ^. #guildID)
            let guildName = maybe "the guild" (^. #name) mguild
            challenge <- newChallenge mem
            void . tell mem $ showChallenge guildName challenge

        react @'GuildMemberRemoveEvt $ \mem -> do
            info @Text $ "Member " <> showtl (mem ^. #id) <> " left/removed"
            deleteChallenge' (mem ^. #id)

        react @'MessageCreateEvt $ \msg -> do
            mchannel <- upgrade (msg ^. #channelID)
            muser <- upgrade (msg ^. #author)
            let isDM = maybe False channelIsDM mchannel
            for_ muser $ \user ->
                when (isDM && isHuman user) $
                    runChallengeCheck user msg

        addCommands $ do
            let vRole = cfg ^. #verifiedRole

            helpCommand

            command @'[Snowflake User] "verify" $
                \ctx userID -> case ctx ^. #guild of
                    Just guild -> do
                        info @Text $ "Manually verifying user " <> showtl userID
                        void . invoke $ AddGuildMemberRole guild userID vRole
                        deleteChallenge' userID
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
                                forM_ (SM.elems (guild ^. #members)) $ \mem -> do
                                    if vRole `V.notElem` (mem ^. #roles)
                                        then do
                                            debug @Text "Add verified role"
                                            void . invoke $ AddGuildMemberRole guild (mem ^. #id) vRole
                                            deleteChallenge' (mem ^. #id)
                                        else
                                            debug @Text "Already had verified role"
                            else do
                                info @Text "ManageRole permission not found."
                                void $ tell @Text ctx "Permission Denied"

                    Nothing -> do
                        info @Text "Can only verify users in guilds."
                        void $ tell @Text ctx "Can only verify users in guilds."

runChallengeCheck :: PersistBotC r => User -> Message -> P.Sem r ()
runChallengeCheck user msg = do
    cfg <- P.ask @Config
    let userID = user ^. #id
    info @Text $ "DM received from " <> showtl userID
    res <- checkChallenge userID (msg ^. #content)
    case res of
        NotFound -> do
            void . tell @Text msg $
                "Sorry, we were unable to verify you."
                <> " Please try leaving the guild and rejoining. (Error code: 1)"

        Incorrect ch rem
            | rem > 0 -> do
                void $ tell @Text msg $
                    "Incorrect, you have "
                    <> showtl rem
                    <> " attempts remaining."
            | otherwise -> do
                info @Text "Challenge failure, attempts exhausted"
                void $ tell @Text msg
                    "Incorrect, you are out of attempts and will now be kicked from the server."
                void . invoke $ RemoveGuildMember (ch ^. #challengeGuildID) userID
                -- Challenge is removed via GuildMemberRemove event handler

        Correct ch -> do
            let guildID = ch ^. #challengeGuildID
            mguild <- upgrade guildID
            case mguild of
                Nothing -> do
                    warning @Text "Guild from challenge was nothing (this shouldn't happen)"
                    void . tell @Text msg $
                        "Sorry, we were unable to verify you."
                        <> " Please try leaving the guild and rejoining. (Error code: 2)"
                Just g -> do
                    void . tell msg $ "Thank you! You are now verified in " <> (g ^. #name) <> "!"
                    void . invoke $ AddGuildMemberRole guildID userID (cfg ^. #verifiedRole)
                    deleteChallenge user

challengeNewMembers :: PersistBotC r => Guild -> P.Sem r ()
challengeNewMembers guild = do
    cfg <- P.ask @Config
    let vRole = cfg ^. #verifiedRole
    forM_ (SM.elems (guild ^. #members)) $ \mem -> do
        if vRole `V.notElem` (mem ^. #roles)
            then do
                debug @Text "Add verified role"
                challenge <- newChallenge mem
                void . tell mem $ showChallenge (guild ^. #name) challenge
            else
                debug @Text "Already had verified role"
