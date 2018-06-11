module Logger where

logInfo :: String -> IO ()
logInfo msg = putStrLn ("[info] "++msg)

logWarn :: String -> IO ()
logWarn msg = putStrLn ("[warn] "++msg)

logErr :: String -> IO ()
logErr msg = putStrLn ("[error] "++msg)