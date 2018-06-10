{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Entity)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchUserPG, fetchAllUsersPG, createUserPG, fetchPostgresConnection, PGInfo)
import           Schema

type FullAPI =
       "users" :> Get '[JSON] [Entity User]
  :<|> "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

usersAPI :: Proxy FullAPI
usersAPI = Proxy :: Proxy FullAPI

fetchUsersHandler :: PGInfo -> Handler [Entity User]
fetchUsersHandler pgInfo = liftIO $ fetchAllUsersPG pgInfo

fetchUserHandler :: PGInfo -> Int64 -> Handler User
fetchUserHandler pgInfo uid = do
  maybeUser <- liftIO $ fetchUserPG pgInfo uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })

createUserHandler :: PGInfo -> User -> Handler Int64
createUserHandler pgInfo user = liftIO $ createUserPG pgInfo user

fullAPIServer :: PGInfo -> Server FullAPI
fullAPIServer pgInfo =
  (fetchUsersHandler pgInfo) :<|>
  (fetchUserHandler pgInfo)  :<|>
  (createUserHandler pgInfo)

runServer :: IO ()
runServer = do
  pgInfo <- fetchPostgresConnection
  run 8000 (serve usersAPI (fullAPIServer pgInfo))

fetchUsersClient :: ClientM [Entity User]
fetchUserClient :: Int64 -> ClientM User
createUserClient :: User -> ClientM Int64
( fetchUsersClient            :<|>
  fetchUserClient             :<|>
  createUserClient)  = client (Proxy :: Proxy FullAPI)
