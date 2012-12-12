{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified App
import           Network.Wai.Middleware.RequestLogger
import Web.Scotty

main :: IO ()
main = do
    scotty 3000 $ do
      middleware logStdoutDev
      App.app
