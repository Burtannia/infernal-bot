module Infernal.Challenge where

import Calamity
import           Control.Lens
import Data.Bitraversable
import Data.Text.Lazy
import Data.Time.Clock (UTCTime, getCurrentTime)
import           GHC.Generics
import System.Random

-- IntMap (Snowflake MemberID) [ChallengeStatus]
-- ChallengeStatus = (Snowflake MemberID) (Snowflake GuildID) (Challenge) (AttemptsRemaining) (TimeChallenged)

-- every $evictScanTime minutes iterate the map and kick users that haven't responded to the challenge in $evictTime

type Question = (Int, Int)

data Challenge = Challenge
    { memberId :: Snowflake User
    , guildId :: Snowflake Guild
    , question :: Question
    , attemptsRemaining :: Int
    , timeChallenged :: UTCTime
    } deriving (Show, Generic)

mkChallenge :: (Snowflake User)
    -> (Snowflake Guild)
    -> Int
    -> IO Challenge
mkChallenge user guild attempts = Challenge
    <$> pure user
    <*> pure guild
    <*> mkQuestion
    <*> pure attempts
    <*> getCurrentTime

mkQuestion :: IO Question
mkQuestion = bisequence (rand, rand)
    where
        rand = getStdRandom $ randomR (1, 20)

showChallenge :: Text -> Challenge -> Text
showChallenge gName c =
    "Thank you for joining " <> gName <> "!"
    <> "\n\n"
    <> "In order to protect against bots we require you to answer a"
    <> " simple question before you're able to use the server chat."
    <> "\n\n"
    <> "Your question is: " <> ppQuestion (c ^. #question)
    <> "\n\n"
    <> "Please type your answer below..."

ppQuestion :: Question -> Text
ppQuestion (x,y) =
    tshow x <> " + " <> tshow y
    where
        tshow = pack . show