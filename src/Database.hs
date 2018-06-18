{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, LoggingT, LogLevel(..), filterLogger)
import           Control.Monad.Reader (runReaderT, liftIO)
import           Control.Monad (forM_)
import           Data.Int (Int64)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as Char8
import           Data.Maybe (listToMaybe)
import           Database.Esqueleto (select, deleteCascade, from, where_, (^.), val, (==.), limit)
import           Database.Persist (getBy, insert, selectList, Entity, Entity(..))
import           Database.Persist.Sql (fromSqlKey)
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
    Just string -> return (Char8.pack string)
    Nothing -> do
      logInfo "Falling back to local PG config"
      return localConnString

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

------------- USERS -------------
fetchUserPG :: PGInfo -> String -> IO (Maybe (Entity User))
fetchUserPG connString login = runAction connString (getBy $ UniqueLogin (Text.pack login))

fetchAllUsersPG :: PGInfo -> IO [Entity User]
fetchAllUsersPG connString = runAction connString (selectList [] [])

createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)

deleteUserPG :: PGInfo -> String -> IO ()
deleteUserPG connString login = runAction connString deleteAction
  where
    deleteAction :: SqlPersistT (LoggingT IO) ()
    deleteAction = do
      users <- select $
        from $ \users -> do
        where_ (users ^. UserLogin ==. val (Text.pack login))
        pure users
      forM_ users (deleteCascade . entityKey)

------------- CASES -------------

------------- QUEUES ------------
