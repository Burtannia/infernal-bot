{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Infernal.Database
    ( Persistable (..)
    , db
    , db_
    , runPersistWith
    , PersistBotC
    , mkConnString) where

import           Calamity                    (BotC)
import           Conduit                     (ResourceT, runResourceT)
import           Control.Monad               (void)
import           Control.Monad.Logger        (LoggingT, runStdoutLoggingT)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS (unwords)
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Persist.Postgresql (SqlPersistT, runSqlConn,
                                              withPostgresqlConn)
import           Infernal.Config             (Config, DatabaseConfig (..))
import qualified Polysemy                    as P
import qualified Polysemy.Reader             as P

type PersistBotC r = (BotC r, P.Members '[ Persistable, P.Reader Config ] r )

type DatabaseAction a = SqlPersistT (LoggingT (ResourceT IO)) a

data Persistable m a where
    Db :: DatabaseAction a -> Persistable m a

P.makeSem  ''Persistable

db_ :: P.Member Persistable r => DatabaseAction a -> P.Sem r ()
db_ = void . db

mkConnString :: DatabaseConfig -> ByteString
mkConnString DatabaseConfig {..} = BS.unwords $
    map encodeUtf8 [host, port, user, pass]
    where
        host = "host=" <> dbHost
        port = "port=" <> dbPort
        user = "user=" <> dbUser
        pass = "password=" <> dbPass

runPersistWith :: P.Member (P.Embed IO) r => ByteString -> P.Sem (Persistable : r) a -> P.Sem r a
runPersistWith conn = P.interpret $ \case
    Db action ->
        P.embed
        . runResourceT
        . runStdoutLoggingT
        . withPostgresqlConn conn
        . runSqlConn
        $ action
