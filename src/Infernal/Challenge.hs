module Infernal.Challenge where

import           Calamity           (BotC, Guild,
                                     GuildRequest (RemoveGuildMember), Member,
                                     Snowflake, User, invoke, tell)
import           Control.Lens       ((^.))
import           Data.Bitraversable (bisequence)
import           Data.Text.Lazy     (Text, pack, unpack)
import           Data.Time.Clock    (UTCTime, addUTCTime, getCurrentTime)
import           System.Random      (Random (randomR), getStdRandom)
import           Text.Read          (readMaybe)

import           Control.Concurrent (MVar, putMVar, takeMVar, threadDelay,
                                     withMVar)
import           Control.Monad      (forM_, void)
import qualified Data.HashTable.IO  as H
import           DiPolysemy         (info)
import           Infernal.Config    (Config)
import           Infernal.Schema    (Challenge (Challenge), Question)
import           Infernal.Utils     (minsToMicroSeconds)
import qualified Polysemy           as P
import           TextShow           (TextShow (showtl))

mkChallenge :: Config
    -> Snowflake User
    -> Snowflake Guild
    -> IO Challenge
mkChallenge config user guild = Challenge user guild
    <$> mkQuestion
    <*> pure (config ^. #challengeAttempts)
    <*> fmap (addUTCTime allowance) getCurrentTime
    where
        allowance = fromInteger $ toInteger (config ^. #challengeEvictMins) * 60

mkQuestion :: IO Question
mkQuestion = bisequence (rand, rand)
    where
        rand = getStdRandom $ randomR (1, 20)

checkResponse :: Text -> Challenge -> Bool
checkResponse msg c = Just True ==
    fmap (\n -> n == x + y) mn
    where
        mn = readMaybe $ unpack msg
        x = fst (c ^. #challengeQuestion)
        y = snd (c ^. #challengeQuestion)

showChallenge :: Text -> Challenge -> Text
showChallenge gName c =
    "Thank you for joining " <> gName <> "!"
    <> "\n\n"
    <> "In order to protect against bots we require you to answer a"
    <> " simple question before you're able to use the server chat."
    <> "\n\n"
    <> "Your question is: " <> ppQuestion (c ^. #challengeQuestion)
    <> "\n\n"
    <> "Please type your answer below..."

ppQuestion :: Question -> Text
ppQuestion (x,y) =
    tshow x <> " + " <> tshow y
    where
        tshow = pack . show

expired :: Challenge -> UTCTime -> Bool
expired challenge now = (challenge ^. #challengeExpiry) >= now

type HashMap k v = H.BasicHashTable k v

type ChallengeMap = HashMap (Snowflake User) Challenge

mkChallengeMap :: IO ChallengeMap
mkChallengeMap = H.new

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
    withMVar chvar $ flip H.lookup userID

deleteChallenge :: MVar ChallengeMap -> Snowflake User -> IO ()
deleteChallenge chvar userID =
    withMVar chvar $ flip H.delete userID

evictLoop :: BotC r => MVar ChallengeMap -> Int -> P.Sem r ()
evictLoop chvar sleepMins = do
    evictExpired chvar
    P.embed $ threadDelay $ minsToMicroSeconds sleepMins
    evictLoop chvar sleepMins

evictExpired :: BotC r => MVar ChallengeMap -> P.Sem r ()
evictExpired chvar = do
    info @Text "Evicting Starting"
    now <- P.embed getCurrentTime
    chs <- P.embed $ withMVar chvar H.toList
    let toEvicts = [ (k,v) | (k,v) <- chs, v ^. #challengeExpiry >= now ]
    chs' <- P.embed $ takeMVar chvar
    forM_ toEvicts $ \(k,v) -> do
        info @Text $ "Evicting " <> showtl k
        P.embed $ H.delete chs' k
        void . tell @Text k $ "You took too long to respond and will now be kicked."
        void . invoke $ RemoveGuildMember (v ^. #challengeGuildID) k
    P.embed $ putMVar chvar chs'
    info @Text "Evicting Complete"
