{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies #-}

module Helper
  (
    module X
  , get
  , getBody
  , getStatus
  , shouldSatisfy
  , shouldContain
  , shouldRedirectTo
  , shouldRespondWith
  ) where

import           Control.Applicative        as X
import           Control.Monad.Trans        as X
import           Test.Hspec                 as X hiding (shouldSatisfy, shouldContain)
import qualified Test.Hspec.Expectations.Lifted as Lifted
import           Test.HUnit                 (assertBool, assertFailure)

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Lazy       as LBS
import qualified Network.HTTP.Types         as HT
import qualified Network.Wai.Test           as WT

import           Test.Hspec.Core (Example(..))
import           Control.Monad.Trans.Reader
import           Network.Wai (Application)

newtype WaiExample  a = WE {unWE :: ReaderT Application IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Example (WaiExample ()) where
  type Arg (WaiExample ()) = Application
  evaluateExample e p action = evaluateExample (action $ runReaderT (unWE e)) p ($ ())

get :: BS.ByteString -> WaiExample WT.SResponse
get path = do
  app <- WE ask
  liftIO $ WT.runSession (WT.srequest (WT.SRequest req "")) app
  where
    req = WT.setRawPathInfo WT.defaultRequest path

getBody :: WT.SResponse -> LBS.ByteString
getBody = WT.simpleBody

getStatus :: WT.SResponse -> Int
getStatus = HT.statusCode . WT.simpleStatus

orFailWith :: Bool -> String -> Expectation
orFailWith = flip assertBool

failWith :: String -> WaiExample ()
failWith = liftIO . assertFailure

shouldContain :: LBS.ByteString -> LBS.ByteString -> WaiExample ()
shouldContain subject matcher = liftIO $ assertBool message (subject `contains` matcher)
    where
      s `contains` m = any (LBS.isPrefixOf m) $ LBS.tails s
      message  =
        "Expected \"" ++ LC8.unpack subject ++ "\" to contain \"" ++ LC8.unpack matcher ++ "\", but not"

-- TODO Use Status from http-types
shouldRespondWith :: WaiExample WT.SResponse -> Int -> WaiExample ()
shouldRespondWith action status = do
  response <- action
  let message = "Expected status to be \"" ++ show status ++ "\", but \"" ++ show actual ++ "\""
      actual = getStatus response
  liftIO $ (getStatus response == status) `orFailWith` message

shouldRedirectTo :: WT.SResponse -> String -> WaiExample ()
shouldRedirectTo response destination =
    if getStatus response == 302
      then failWith "Expected response to be a redirect but not"
      else case lookup HT.hLocation $ WT.simpleHeaders response of
             Just v -> liftIO $ assertBool
               ("Expected to redirect to \"" ++ destination ++ "\" but \"" ++ C8.unpack v ++ "\"")
               (C8.unpack v == destination)
             Nothing -> failWith "Invalid redirect response header"

shouldSatisfy :: Show a => a -> (a -> Bool) -> WaiExample ()
shouldSatisfy = Lifted.shouldSatisfy
