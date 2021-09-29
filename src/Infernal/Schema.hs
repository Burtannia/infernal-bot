{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Infernal.Schema where

import           Calamity             (Guild, Snowflake, User)
import           Calamity.Commands    ()
import           Control.Applicative  ()
import           Data.Aeson           ()
import           Data.Maybe           ()
import           Data.Time            (UTCTime)
import           Database.Persist     ()
import           Database.Persist.Sql (BackendKey (SqlBackendKey))
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           GHC.Generics         (Generic)
import           TextShow             ()
import           TextShow.Generic     ()

import           Infernal.Orphans     ()

type Question = (Int, Int)

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
        Challenge
            memberID (Snowflake User)
            guildID (Snowflake Guild)
            question Question
            attemptsRemaining Int
            expiry UTCTime

            UniqueMember memberID

            deriving Show
            deriving Generic
    |]
