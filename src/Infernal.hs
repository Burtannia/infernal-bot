module Infernal where

import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Commands.Context (useFullContext)
import           Calamity.Metrics.Noop
import qualified Calamity.Internal.SnowflakeMap as SM
import           Control.Lens
import           Control.Monad
import qualified Data.Aeson                as Aeson
import           Data.Flags                ((.+.), containsAll)
import           Data.Generics.Labels      ()
import           Data.Maybe
import           Data.Text.Lazy
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Foldable (for_)
import qualified Data.Vector.Unboxing as V (elem, notElem)
import qualified Df1
import qualified Di
import           DiPolysemy
import qualified Polysemy                  as P
import System.Exit
import           Options.Generic hiding (Text)
import qualified Data.HashTable.IO as H
import Control.Concurrent.MVar
import Control.Concurrent
import TextShow (showtl)

import Infernal.Challenge
import Infernal.Config

type HashMap k v = H.BasicHashTable k v

type ChallengeMap = HashMap (Snowflake User) Challenge

mkChallengeMap :: IO ChallengeMap
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
    chvar <- mkChallengeMap >>= newMVar
    _ <- forkIO $ evictThread cfg chvar
    runBotWith cfg chvar

minsToMicroSeconds :: Int -> Int
minsToMicroSeconds mins = mins * 60 * 1000 * 1000

expired :: Challenge -> UTCTime -> Bool
expired challenge now = (challenge ^. #expiry) >= now

evictThread :: Config -> MVar ChallengeMap -> IO ()
evictThread cfg chvar = Di.new $ \di ->
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
    $ evictLoop chvar (cfg ^. #challengeEvictScanMins)

evictLoop :: BotC r => MVar ChallengeMap -> Int -> P.Sem r ()
evictLoop chvar sleepMins = do
    evictExpired chvar
    P.embed $ threadDelay $ minsToMicroSeconds sleepMins
    evictLoop chvar sleepMins

evictExpired :: BotC r => MVar ChallengeMap -> P.Sem r ()
evictExpired chvar = do
    info @Text "Evicting"
    now <- P.embed getCurrentTime
    chs <- P.embed $ withMVar chvar H.toList
    let toEvicts = [ (k,v) | (k,v) <- chs, v ^. #expiry >= now ]
    chs' <- P.embed $ takeMVar chvar
    flip mapM_ toEvicts $ \(k,v) -> do
        P.embed $ H.delete chs' k
        void . tell @Text k $ "You took too long to respond and will now be kicked."
        void . invoke $ RemoveGuildMember (v ^. #guildID) k
    P.embed $ putMVar chvar chs'
    info @Text "Evicting Complete"

channelIsDM :: Channel -> Bool
channelIsDM (DMChannel' _) = True
channelIsDM _ = False

isHuman :: User -> Bool
isHuman user = fromMaybe True (user ^. #bot)

newChallenge :: MVar ChallengeMap -> Config -> Member -> IO Challenge
newChallenge chvar cfg mem = do
    let userID = mem ^. #id
    ch <- mkChallenge cfg userID (mem ^. #guildID)
    insertChallenge chvar userID ch
    return ch

insertChallenge :: MVar ChallengeMap -> Snowflake User -> Challenge -> IO ()
insertChallenge chvar userID ch =
    withMVar chvar $ \chs -> H.insert chs userID ch

lookupChallenge :: MVar ChallengeMap -> Snowflake User -> IO (Maybe Challenge)
lookupChallenge chvar userID =
    withMVar chvar $ (flip H.lookup) userID
    
deleteChallenge :: MVar ChallengeMap -> Snowflake User -> IO ()
deleteChallenge chvar userID =
    withMVar chvar $ (flip H.delete) userID

runBotWith :: Config -> MVar ChallengeMap -> IO ()
runBotWith cfg chvar = Di.new $ \di ->
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
        (defaultIntents .+. intentGuildMembers .+. intentGuildPresences)
    $ do 
        info @Text "Bot starting up!"

        _ <- react @'GuildMemberAddEvt $ \mem -> do
            info @Text $ "Member " <> showtl (mem ^. #id) <> " joined, sending challenge"
            mguild <- upgrade (mem ^. #guildID)
            let guildName = fromMaybe "the guild" $ fmap (^. #name) mguild
            challenge <- P.embed $ newChallenge chvar cfg mem
            void . tell mem $ showChallenge guildName challenge

        _ <- react @'GuildMemberRemoveEvt $ \mem -> do
            info @Text $ "Member " <> showtl (mem ^. #id) <> " left/removed"
            P.embed $ deleteChallenge chvar (mem ^. #id)

        _ <- react @'MessageCreateEvt $ \msg -> do
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
                                mguild <- upgrade (challenge ^. #guildID)
                                case mguild of
                                    Nothing -> do
                                        warning @Text "Guild from challenge was nothing"
                                    Just g -> do
                                        void . tell msg $ "Thank you! You are now verified in " <> (g ^. #name) <> "!"
                                        void . invoke $ AddGuildMemberRole (challenge ^. #guildID) (user ^. #id) (cfg ^. #verifiedRole)
                                        P.embed $ deleteChallenge chvar (user ^. #id)
                            else do
                                info @Text "Incorrect response received"
                                let remaining = (challenge ^. #attemptsRemaining) - 1
                                if remaining == 0
                                    then do
                                        info @Text "Challenge failure, attempts exhausted"
                                        void $ tell @Text msg
                                            "Incorrect, you are out of attempts and will now be kicked."
                                        void . invoke $ RemoveGuildMember (challenge ^. #guildID) (user ^. #id)
                                        -- Challenge is removed via GuildMemberRemove event handler
                                    else do
                                        debug @Text "Updating attempts"
                                        void $ tell @Text msg $
                                            "Incorrect, you have "
                                            <> showtl remaining
                                            <> " attempts remaining."
                                        P.embed $ insertChallenge chvar (user ^. #id)
                                            (challenge & #attemptsRemaining .~ remaining)

        addCommands $ do
            let vRole = cfg ^. #verifiedRole
            helpCommand
            command @'[Named "user" (Snowflake User)] "verify" $
                \ctx userID -> case (ctx ^. #guild) of
                    Just guild -> do
                        info @Text $ "Manually verifying user " <> showtl userID
                        void . invoke $ AddGuildMemberRole guild userID vRole
                    Nothing -> do
                        info @Text "Can only verify users in guilds."
                        void $ tell @Text ctx "Can only verify users in guilds."
            command @'[] "verifyAll" $
                \ctx -> case (ctx ^. #guild) of
                    Just guild -> do
                        hasPerm <- canVerify (ctx ^. #member)
                        
                        when (not hasPerm) $ do
                            info @Text "ManageRole permission not found."
                            void $ tell @Text ctx "Permission Denied"

                        when hasPerm $ do
                            info @Text "Manually verifying all users"
                            P.embed $ print $ SM.toList (guild ^. #members)
                            flip mapM_ (SM.elems (guild ^. #members)) $ \mem -> do
                                if vRole `V.notElem` (mem ^. #roles)
                                    then do
                                        debug @Text "Add verified role"
                                        void . invoke $ AddGuildMemberRole guild (mem ^. #id) vRole
                                    else
                                        debug @Text "Already had verified role"
                    Nothing -> do
                        info @Text "Can only verify users in guilds."
                        void $ tell @Text ctx "Can only verify users in guilds."

canVerify :: BotC r => Maybe Member -> P.Sem r Bool
canVerify Nothing = return False
canVerify (Just mem) = do
    perms <- permissionsIn' (mem ^. #guildID) mem
    return $ perms `containsAll` manageRoles