module Infernal.Config where

import           Calamity             (Role, Snowflake)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Generics.Labels ()
import           GHC.Generics         (Generic)
import           Options.Generic      (ParseRecord, Text, Wrapped, type (:::),
                                       type (<?>))

newtype CLIOptions w = Options
    { config :: w ::: Maybe FilePath <?> "The path to the configuration file (must end with .json)"
    } deriving Generic

instance ParseRecord (CLIOptions Wrapped)

data Config = Config
    { botToken               :: Text
    , verifiedRole           :: Snowflake Role
    , commandPrefix          :: Text
    , challengeAttempts      :: Int
    , challengeEvictMins     :: Int
    , challengeEvictScanMins :: Int
    } deriving (Show, Eq, Generic)
      deriving anyclass (FromJSON, ToJSON)
