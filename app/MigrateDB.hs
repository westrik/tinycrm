module Main where

import Database (fetchPostgresConnection, migrateDB)

main :: IO ()
main = do
    connection <- fetchPostgresConnection
    migrateDB connection