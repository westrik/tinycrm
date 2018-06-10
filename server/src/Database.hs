{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad (void)
import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT,
                                       LogLevel(..), filterLogger)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Int (Int64)
import           Data.Maybe (listToMaybe)
import           Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc)
import           Database.Persist (get, insert, delete, entityVal, Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn,
                                              runMigration, SqlPersistT)

import           Schema

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=crm dbname=crm"

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

-- This is IO since in a real application we'd want to configure it.
fetchPostgresConnection :: IO PGInfo
fetchPostgresConnection = return localConnString

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchUserPG :: PGInfo -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)

deleteUserPG :: PGInfo -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid