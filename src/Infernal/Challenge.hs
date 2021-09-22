module Infernal.Challenge where

import           Calamity           (Guild, Snowflake, User)
import           Control.Lens       ((^.))
import           Data.Bitraversable (bisequence)
import           Data.Text.Lazy     (Text, pack, unpack)
import           Data.Time.Clock    (UTCTime, addUTCTime, getCurrentTime)
import           GHC.Generics       (Generic)
import           System.Random      (Random (randomR), getStdRandom)
import           Text.Read          (readMaybe)

import           Infernal.Config    (Config)

type Question = (Int, Int)

data Challenge = Challenge
    { memberID          :: Snowflake User
    , guildID           :: Snowflake Guild
    , question          :: Question
    , attemptsRemaining :: Int
    , expiry            :: UTCTime
    } deriving (Show, Generic)

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
        x = fst (c ^. #question)
        y = snd (c ^. #question)

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
