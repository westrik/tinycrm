{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Entity, Entity(..))
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchUserPG, fetchAllUsersPG, createUserPG, deleteUserPG, 
                 fetchPostgresConnection, PGInfo)
import           Schema

type FullAPI =
       "users" :> Get '[JSON] [Entity User]
  :<|> "users" :> Capture "userid" String :> Get '[JSON] (Entity User)
  :<|> "users" :> Capture "userid" String :> Delete '[JSON] ()
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

usersAPI :: Proxy FullAPI
usersAPI = Proxy :: Proxy FullAPI

fetchUsersHandler :: PGInfo -> Handler [Entity User]
fetchUsersHandler pgInfo = liftIO $ fetchAllUsersPG pgInfo

fetchUserHandler :: PGInfo -> String -> Handler (Entity User)
fetchUserHandler pgInfo login = do
  maybeUser <- liftIO $ fetchUserPG pgInfo login
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user

deleteUserHandler :: PGInfo -> String -> Handler ()
deleteUserHandler pgInfo login = liftIO $ deleteUserPG pgInfo login

fullAPIServer :: PGInfo -> Server FullAPI
fullAPIServer pgInfo =
  (fetchUsersHandler pgInfo) :<|>
  (fetchUserHandler pgInfo)  :<|>
  (deleteUserHandler pgInfo) :<|>
  (createUserHandler pgInfo)

runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  run 8000 $ cors policy $ serve usersAPI (fullAPIServer pgInfo)
    where 
      policy = const $ Just simpleCorsResourcePolicy 
        { corsRequestHeaders = ["Content-Type"] }

fetchUsersClient :: ClientM [Entity User]
fetchUserClient :: String -> ClientM (Entity User)
deleteUserClient :: String -> ClientM ()
createUserClient :: User -> ClientM Int64
( fetchUsersClient           :<|>
  fetchUserClient            :<|>
  deleteUserClient           :<|>
  createUserClient)  = client (Proxy :: Proxy FullAPI)
