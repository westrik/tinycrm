{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Schema where

import           Data.Aeson (ToJSON, toJSON, object, (.=), FromJSON, parseJSON, (.:), withObject
                            , Object)
import           Data.Aeson.Types (Parser, Pair)
import           Database.Persist (Entity(..), Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    UniqueEmail email
    deriving Show Read Eq
|]

instance ToJSON (Entity User) where
  toJSON (Entity uid user) = object $
    "id" .= (fromSqlKey uid) : userPairs user

instance ToJSON User where
  toJSON user = object (userPairs user)

userPairs :: User -> [Pair]
userPairs user =
  [ "name" .= userName user
  , "email" .= userEmail user
  ]

instance FromJSON (Entity User) where
  parseJSON = withObject "User Entity" $ \o -> do
    user <- parseUser o
    uid <- o .: "id"
    return $ Entity (toSqlKey uid) user

instance FromJSON User where
  parseJSON = withObject "User" parseUser

parseUser :: Object -> Parser User
parseUser o = do
  uName <- o .: "name"
  uEmail <- o .: "email"
  return User
    { userName = uName
    , userEmail = uEmail
    }