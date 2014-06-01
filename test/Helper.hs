{-# LANGUAGE OverloadedStrings #-}

module Helper
  (
    module X
  , get
  , getApp
  , getBody
  , getStatus
  , shouldContain
  , shouldRedirectTo
  , shouldRespondWith
  ) where

import           Control.Applicative        as X
import           Control.Monad.Trans        as X
import           Test.Hspec                 as X hiding (shouldContain)
import           Test.HUnit                 (assertBool, assertFailure)

import qualified App
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Lazy       as LBS
import qualified Network.HTTP.Types         as HT
import qualified Network.Wai                as W
import qualified Network.Wai.Test           as WT
import qualified Web.Scotty                 as Scotty

getApp :: IO W.Application
getApp = Scotty.scottyApp App.app

get :: W.Application -> BS.ByteString -> IO WT.SResponse
get app path =
  WT.runSession (WT.srequest (WT.SRequest req "")) app
      where req = WT.setRawPathInfo WT.defaultRequest path

getBody :: WT.SResponse -> LBS.ByteString
getBody = WT.simpleBody

getStatus :: WT.SResponse -> Int
getStatus = HT.statusCode . WT.simpleStatus

orFailWith :: Bool -> String -> Expectation
orFailWith = flip assertBool

failWith :: String -> Expectation
failWith = assertFailure

shouldContain :: LBS.ByteString -> LBS.ByteString -> Expectation
shouldContain subject matcher = assertBool message (subject `contains` matcher)
    where
      s `contains` m = any (LBS.isPrefixOf m) $ LBS.tails s
      message  =
        "Expected \"" ++ LC8.unpack subject ++ "\" to contain \"" ++ LC8.unpack matcher ++ "\", but not"

-- TODO Use Status from http-types
shouldRespondWith :: WT.SResponse -> Int -> Expectation
shouldRespondWith response status =
    (getStatus response == status) `orFailWith` message
    where
      message = "Expected status to be \"" ++ show status ++ "\", but \"" ++ show actual ++ "\""
      actual = getStatus response

shouldRedirectTo :: WT.SResponse -> String -> Expectation
shouldRedirectTo response destination =
    if getStatus response == 302
      then failWith "Expected response to be a redirect but not"
      else case lookup HT.hLocation $ WT.simpleHeaders response of
             Just v -> assertBool
               ("Expected to redirect to \"" ++ destination ++ "\" but \"" ++ C8.unpack v ++ "\"")
               (C8.unpack v == destination)
             Nothing -> failWith "Invalid redirect response header"
