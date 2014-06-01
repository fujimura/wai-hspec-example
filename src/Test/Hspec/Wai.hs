{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies #-}
module Test.Hspec.Wai (
  WaiExpectation
, get
, post
, request
, shouldRespondWith
, ResponseMatcher(..)
) where

import           Control.Applicative

import           Test.Hspec
import qualified Test.Hspec.Expectations.Lifted as Lifted

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

import           Network.HTTP.Types

import           Network.Wai.Test hiding (request)
import qualified Network.Wai.Test as Wai

import           Test.Hspec.Core (Example(..))
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Network.Wai (Application)

import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.String
import           Network.Wai (Request(..))
import           Data.Conduit (yield)

type WaiExpectation = WaiSession ()

newtype WaiSession a = WaiSession {runWaiSession :: Session a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Example WaiExpectation where
  type Arg WaiExpectation = Application
  evaluateExample e p action = evaluateExample (action $ runSession (runWaiSession e)) p ($ ())

data ResponseMatcher = ResponseMatcher {
  matchBody :: BodyMatcher
, matchStatus :: StatusMatcher
}

instance IsString ResponseMatcher where
  fromString s = ResponseMatcher (fromString s) AnyStatus

instance Num ResponseMatcher where
  fromInteger n = ResponseMatcher AnyBody (fromInteger n)
  (+) =    error "ResponseMatcher does not support (+)"
  (*) =    error "ResponseMatcher does not support (*)"
  abs =    error "ResponseMatcher does not support `abs`"
  signum = error "ResponseMatcher does not support `signum`"

data BodyMatcher = MatchBody LB.ByteString | AnyBody

instance IsString BodyMatcher where
  fromString = MatchBody . encodeUtf8 . fromString

data StatusMatcher = MatchStatus Int | AnyStatus

instance Num StatusMatcher where
  fromInteger = MatchStatus . fromIntegral
  (+) =    error "StatusMatcher does not support (+)"
  (*) =    error "StatusMatcher does not support (*)"
  abs =    error "StatusMatcher does not support `abs`"
  signum = error "StatusMatcher does not support `signum`"

shouldRespondWith :: WaiSession SResponse -> ResponseMatcher -> WaiExpectation
shouldRespondWith action matcher = do
  r <- action
  case matchStatus matcher of
    AnyStatus -> return ()
    MatchStatus n -> (statusCode . simpleStatus) r `Lifted.shouldBe` n
  case matchBody matcher of
    AnyBody -> return ()
    MatchBody b -> simpleBody r `Lifted.shouldBe` b

get :: ByteString -> WaiSession SResponse
get p = request methodGet p ""

post :: ByteString -> ByteString -> WaiSession SResponse
post = request methodPost

request :: Method -> ByteString -> ByteString -> WaiSession SResponse
request m p b = getApp >>= liftIO . runSession (Wai.request req)
  where
    req = setPath defaultRequest {requestMethod = m, requestBody = yield b} p

getApp :: WaiSession Application
getApp = WaiSession ask
