{-# LANGUAGE OverloadedStrings #-}

module AppSpec ( spec ) where

import           Helper

import qualified Data.ByteString.Lazy as LBS
import           App

spec :: Spec
spec = before app $ do
    describe "GET /" $ do
      it "should contain 'Hello' in response body" $ do
        body <- getBody <$> get ""
        body `shouldSatisfy` \x -> any (LBS.isPrefixOf "Happy Holidays") $ LBS.tails x

    describe "GET /" $ do
      it "should contain 'Guten tag' in response body" $ do
        body <- getBody <$> get ""
        body `shouldContain` "Guten tag"

    describe "GET /bar" $ do
      it "should redirect to /foo" $ do
        res <- get "bar"
        res `shouldRedirectTo` "/foo"

    describe "GET /foobar" $ do
      it "should respond with 404" $ do
        get "foobar" `shouldRespondWith` 404
