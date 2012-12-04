{-# LANGUAGE OverloadedStrings    #-}

module AppSpec ( spec ) where

import Helper

spec :: Spec
spec = describe "GET /" $
    it "should contain 'Hello' in response body" $ do
      app <- getApp
      body <- getBody <$> app `get` ""
      body `shouldContain` "Happy Holidays"
