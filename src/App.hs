{-# LANGUAGE OverloadedStrings #-}
module App (app) where

import           Network.Wai
import           Web.Scotty hiding (body)

app :: IO Application
app = scottyApp $ do
  get "/foo" $
    text "bar"
