{-# LANGUAGE OverloadedStrings    #-}

module App
    ( app
    ) where

import           Views
import           Web.Scotty hiding (body)

app :: ScottyM ()
app = do
    get "/" $
        mustache "src/Views/index.mustache" $ indexView "Happy Holidays" "from Fujimura"

    get "/foo" $
        mustache "src/Views/foo.mustache" $ fooView "Foo" "Bar"
