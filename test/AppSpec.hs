{-# LANGUAGE OverloadedStrings    #-}

module AppSpec ( spec ) where

import Helper

spec :: Spec
spec = do
    describe "GET /" $ do
      it "should contain 'Hello' in response body" $ do
	app <- getApp
	body <- getBody <$> app `get` ""
	body `shouldContain` "Happy Holidays"

    describe "GET /" $ do
      it "should contain 'Guten tag' in response body" $ do
	app <- getApp
	body <- getBody <$> app `get` ""
	body `shouldContain` "Guten tag"

    describe "GET /bar" $ do
      it "should redirect to /foo" $ do
	app <- getApp
	res <- app `get` "bar"
	res `shouldRedirectTo` "/foo"

    describe "GET /foobar" $ do
      it "should respond with 404" $ do
	app <- getApp
	res <- app `get` "foobar"
	res `shouldRespondWith` 404
