module Main (main) where

import           App
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Handler.Warp

main :: IO ()
main = do
    putStrLn "http://localhost:3000"
    app >>= run 3000 . logStdoutDev
