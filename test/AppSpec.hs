{-# LANGUAGE OverloadedStrings #-}

module AppSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Wai

import           App

spec :: Spec
spec = before app $ do
  describe "GET /foo" $ do
    it "reponds with 200" $ do
      get "/foo" `shouldRespondWith` 200

    it "reponds with 'bar'" $ do
      get "/foo" `shouldRespondWith` "bar"

    it "reponds with 200 / 'bar'" $ do
      get "/foo" `shouldRespondWith` "bar" {matchStatus = 200}
