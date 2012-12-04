{-# LANGUAGE OverloadedStrings #-}

module Helper
  (
    module X
  , get
  , getApp
  , getBody
  , getStatus
  , shouldContains
  ) where

import           Control.Applicative      as X
import           Control.Monad.Trans      as X
import           Test.Hspec               as X

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Network.HTTP.Types       as HT
import qualified Network.Wai              as W
import qualified Network.Wai.Test         as WT
import qualified Web.Scotty               as Scotty
import qualified App

getApp :: IO W.Application
getApp = liftIO $ Scotty.scottyApp App.app

get :: W.Application -> BS.ByteString -> IO WT.SResponse
get app path =
  WT.runSession (WT.srequest (WT.SRequest req "")) app
      where req = WT.setRawPathInfo WT.defaultRequest path

getBody :: WT.SResponse -> LBS.ByteString
getBody res = WT.simpleBody res

getStatus :: WT.SResponse -> Int
getStatus = HT.statusCode . WT.simpleStatus

-- TODO Better message
should :: Show a => (a -> a -> Bool) -> a -> a -> Expectation
should be actual expected = actual `be` expected `shouldBe` True

shouldContains :: LBS.ByteString -> LBS.ByteString -> Expectation
shouldContains subject matcher = should contains matcher subject
    where
      contains m s = any (LBS.isPrefixOf m) $ LBS.tails s
