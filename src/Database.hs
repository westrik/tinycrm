{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, LoggingT, LogLevel(..), filterLogger)
import           Control.Monad.Reader (runReaderT)
import           Data.Int (Int64)
import           Data.ByteString.Char8 (pack)
import           Database.Persist (get, insert, delete, selectList, Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, 
                 SqlPersistT)
import           System.Environment (lookupEnv)

import           Schema
import           Logger (logInfo)

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=crm dbname=crm"

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

fetchPostgresConnection :: IO PGInfo
fetchPostgresConnection = do
  connString <- lookupEnv "CRM_DB"
  case connString of
    Just string -> return (pack string)
    Nothing -> do
      logInfo "Falling back to local PG config"
      return localConnString

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchUserPG :: PGInfo -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

fetchAllUsersPG :: PGInfo -> IO [Entity User]
fetchAllUsersPG connString = runAction connString (selectList [] [])

createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)

deleteUserPG :: PGInfo -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid