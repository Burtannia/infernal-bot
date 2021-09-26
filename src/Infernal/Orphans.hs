{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Infernal.Orphans () where

import           Calamity             (Snowflake (Snowflake))
import           Data.Word            (Word64)
import           Database.Persist     (PersistField)
import           Database.Persist.Sql (PersistFieldSql)

deriving via Word64 instance PersistField (Snowflake a)
deriving via Word64 instance PersistFieldSql (Snowflake a)
