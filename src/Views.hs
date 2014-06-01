{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Views
    ( indexView
    , fooView
    , mustache
    ) where

import           Control.Monad.Trans
import           Data.Data
import           Text.Hastache
import           Text.Hastache.Context
import           Web.Scotty              hiding (body)

mustache :: (Data a, Typeable a) => FilePath -> a -> ActionM ()
mustache path context = do
  body <- liftIO $ hastacheFile defaultConfig path (mkGenericContext context)
  html body

data IndexView = IndexView {
                 _greeting :: String
               , _from     :: String
               } deriving (Data, Typeable)

data FooView = FooView {
                 _heading    :: String
               , _paragraph1 :: String
               , _paragraph2 :: String
               } deriving (Data, Typeable)

indexView :: String -> String -> IndexView
indexView = IndexView

fooView :: String -> String -> FooView
fooView = FooView "This is Foo"
