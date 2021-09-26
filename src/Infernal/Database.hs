module Infernal.Database
    ( Persistable (..)
    , db
    , db_
    , runPersistWith
    ) where

import           Conduit                 (ResourceT, runResourceT)
import           Control.Monad           (void)
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Text               (Text)
import           Database.Persist.Sqlite (SqlPersistT, runSqlConn,
                                          withSqliteConn)
import qualified Polysemy                as P

type DatabaseAction a = SqlPersistT (LoggingT (ResourceT IO)) a

data Persistable m a where
    Db :: DatabaseAction a -> Persistable m a

P.makeSem  ''Persistable

db_ :: P.Member Persistable r => DatabaseAction a -> P.Sem r ()
db_ = void . db

runPersistWith :: P.Member (P.Embed IO) r => Text -> P.Sem (Persistable : r) a -> P.Sem r a
runPersistWith conn = P.interpret $ \case
    Db action ->
        P.embed
        . runResourceT
        . runStdoutLoggingT
        . withSqliteConn conn
        . runSqlConn
        $ action
