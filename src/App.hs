{-# LANGUAGE OverloadedStrings #-}
module App (app) where

import           Network.Wai
import           Web.Scotty hiding (body)

import           Views

app :: IO Application
app = scottyApp $ do
    get "/" $
        mustache "src/Views/index.mustache" $ indexView "Happy Holidays" "from Fujimura"

    get "/foo" $
        mustache "src/Views/foo.mustache" $ fooView "Foo" "Bar"

    get "/bar" $
        redirect "/foo"
