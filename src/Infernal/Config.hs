module Infernal.Config where

import           Calamity
import           Data.Aeson
import           Data.Generics.Labels ()
import           GHC.Generics
import           Options.Generic

newtype CLIOptions w = Options
    { config :: w ::: Maybe FilePath <?> "The path to the configuration file (must end with .json)"
    } deriving Generic

instance ParseRecord (CLIOptions Wrapped)

data Config = Config
    { botToken :: Text
    , verifiedRole :: Snowflake Role
    , commandPrefix :: Text
    , challengeAttempts :: Int
    , challengeEvictMins :: Int
    , challengeEvictScanMins :: Int
    } deriving (Show, Eq, Generic)
      deriving anyclass (FromJSON, ToJSON)