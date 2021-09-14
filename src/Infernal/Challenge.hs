module Infernal.Challenge where

import Data.Bitraversable
import Data.Text.Lazy
import System.Random

newtype Challenge = Challenge (Int, Int)
    deriving Show

mkChallenge :: IO Challenge
mkChallenge = Challenge
    <$> bisequence (rand, rand)
    where
        rand = getStdRandom $ randomR (1, 20)

showChallenge :: Text -> Challenge -> Text
showChallenge gName c =
    "Thank you for joining " <> gName <> "!"
    <> "\n\n"
    <> "In order to protect against bots we require you to answer a"
    <> " simple question before you're able to use the server chat."
    <> " Please type your answer below."
    <> "\n\n"
    <> "Your question is: " <> formatChallenge c

formatChallenge :: Challenge -> Text
formatChallenge (Challenge (x,y)) =
    tshow x <> " + " <> tshow y
    where
        tshow = pack . show