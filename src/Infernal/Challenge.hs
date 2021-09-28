module Infernal.Challenge where

import           Calamity           (Guild, GuildRequest (RemoveGuildMember),
                                     Member, Snowflake, User, invoke, tell)
import           Control.Lens       ((^.))
import           Data.Bitraversable (bisequence)
import           Data.Text.Lazy     (Text, pack, unpack)
import           Data.Time.Clock    (UTCTime, addUTCTime, getCurrentTime)
import           System.Random      (Random (randomR), getStdRandom)
import           Text.Read          (readMaybe)

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forM_, unless, void, when)
import           Database.Persist   (Entity (entityKey, entityVal), deleteBy,
                                     deleteWhere, getBy, insert, selectList,
                                     update, (<-.), (=.), (==.), (>=.))
import           DiPolysemy         (info)
import           Infernal.Config    (Config)
import           Infernal.Database  (PersistBotC, db, db_)
import           Infernal.Schema    (Challenge (Challenge),
                                     EntityField (ChallengeAttemptsRemaining, ChallengeExpiry, ChallengeId, ChallengeMemberID),
                                     Question, Unique (UniqueMember))
import           Infernal.Utils     (minsToMicroSeconds)
import qualified Polysemy           as P
import           TextShow           (TextShow (showtl))

data ChallengeResult = NotFound | Incorrect Challenge Int | Correct Challenge
    deriving Show

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

-- Database Stuff

newChallenge :: PersistBotC r => Config -> Member -> P.Sem r Challenge
newChallenge cfg mem = newChallenge' cfg (mem ^. #id) (mem ^. #guildID)

newChallenge' :: PersistBotC r => Config -> Snowflake User -> Snowflake Guild -> P.Sem r Challenge
newChallenge' cfg userID guildID = do
    mch <- db $ getBy $ UniqueMember userID
    case mch of
        Nothing -> do
            ch <- P.embed $ mkChallenge cfg userID guildID
            db_ $ insert ch
            return ch
        Just en ->
            return $ entityVal en

lookupChallenge :: PersistBotC r => User -> P.Sem r (Maybe Challenge)
lookupChallenge usr = lookupChallenge' (usr ^. #id)

lookupChallenge' :: PersistBotC r => Snowflake User -> P.Sem r (Maybe Challenge)
lookupChallenge' userID = do
    mEntity <- db $ getBy $ UniqueMember userID
    return $ fmap entityVal mEntity

lookupChallenges :: PersistBotC r => Member -> P.Sem r [Challenge]
lookupChallenges mem = lookupChallenges' (mem ^. #id)

lookupChallenges' :: PersistBotC r => Snowflake User -> P.Sem r [Challenge]
lookupChallenges' userID = do
    cs <- db $ selectList [ ChallengeMemberID ==. userID ] []
    return $ fmap entityVal cs

deleteChallenge :: PersistBotC r => User -> P.Sem r ()
deleteChallenge usr = deleteChallenge' (usr ^. #id)

deleteChallenge' :: PersistBotC r => Snowflake User -> P.Sem r ()
deleteChallenge' userID =
    db_ $ deleteBy $ UniqueMember userID

-- Handlers

checkChallenge :: PersistBotC r => Snowflake User -> Text -> P.Sem r ChallengeResult
checkChallenge userID resp = do
    mch <- db $ getBy $ UniqueMember userID
    case mch of
        Nothing -> do
            info @Text "Challenge not found"
            return NotFound
        Just en -> do
            let ch = entityVal en
                res = checkResponse resp ch
                attempts = (ch ^. #challengeAttemptsRemaining) - 1

            when res $ info @Text "Correct response received"

            unless res $ do
                info @Text "Incorrect response received"
                db_ $ update (entityKey en) [ ChallengeAttemptsRemaining =. attempts ]

            return $ if res
                then Correct ch
                else Incorrect ch attempts

-- Eviction

evictLoop :: PersistBotC r => Int -> P.Sem r ()
evictLoop sleepMins = do
    evictExpired
    P.embed $ threadDelay $ minsToMicroSeconds sleepMins
    evictLoop sleepMins

evictExpired :: PersistBotC r => P.Sem r ()
evictExpired = do
    info @Text "Eviction Starting"
    now <- P.embed getCurrentTime
    (toEvicts :: [Entity Challenge]) <- db $ selectList [ ChallengeExpiry >=. now ] []
    let keys = fmap entityKey toEvicts
        vals = fmap entityVal toEvicts
    forM_ vals $ \chal -> do
        let userID = chal ^. #challengeMemberID
        info @Text $ "Evicting " <> showtl userID
        void . tell @Text userID $ "You took too long to respond and will now be kicked."
        void . invoke $ RemoveGuildMember (chal ^. #challengeGuildID) userID
    db_ $ deleteWhere [ ChallengeId <-. keys ]
    info @Text "Eviction Complete"
