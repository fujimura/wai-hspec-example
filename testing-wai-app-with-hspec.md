# Testing wai app with hspec

Recently I wrote some small application with [scotty](http://hackage.haskell.org/package/scotty) with test suite. I got some useful knowledge so I'd like to share it.

Since scotty is based on [wai](http://hackage.haskell.org/package/wai), we can use [wai-test](http://hackage.haskell.org/package/wai-test) for testing. It's not so difficult as you think. Let's grasp some basics of writing test for wai application.

## scotty basics

At first, let's take a glance at basics of scotty. The code below is the application used in this article.

```haskell
app :: ScottyM ()
app = do
    get "/" $
        mustache "src/Views/index.mustache" $ indexView "Happy Holidays" "from Fujimura"

    get "/foo" $
        mustache "src/Views/foo.mustache" $ fooView "Foo" "Bar"

    get "/bar" $
        redirect "/foo"

main :: IO ()
main = scotty 3000 app
```

If you know sinatra or similar kind of web application framework, there could be no introduction required. It serves `index.mustache` object at root, `foo.mustache` at "/foo" and redirect to "/foo" at "/bar"(please ignore detail about View things).

`app` will be served in port 3000 in `main`.

## A bit about Hspec

Behavior Driven Development got popular in recent years. Hspec is a library which supports BDD in haskell, based on the most famous and popular BDD library, RSpec. Hspec is very well designed and easy to use, so if you already have some BDD experience, you'll find how to use it in a short time.

If you don't know about BDD or not tried it yet, it's worth to try Hspec. As a Ruby developer(I'm a Rails developer in the daytime), even I feel BDD and Haskell is much more better than it with Ruby.

## The first step: send request and assert response body

Then let's start writing test code. At first we will assert the text given to the view is contained in the response body or not.

```haskell
import qualified App
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Network.Wai                as W
import qualified Network.Wai.Test           as WT
import qualified Web.Scotty                 as Scotty

main :: IO ()
main = hspec $ do
    describe "GET /" $ do
      it "should contain 'Hello' in response body" $ do
        app <- liftIO $ Scotty.scottyApp App.app
        body <- WT.simpleBody <$> app `get` ""
        body `shouldSatisfy` \x -> any (LBS.isPrefixOf "Happy Holidays") $ LBS.tails x
```

In the example(`it` block), at first we took [application](http://hackage.haskell.org/packages/archive/wai/latest/doc/html/Network-Wai.html#t:Application) from the app above with [`Scotty.scottyApp`](http://hackage.haskell.org/packages/archive/scotty/latest/doc/html/Web-Scotty.html#v:scottyApp).

Sending request to application is a bit complicated, so I made a helper(`get`) to run request. If you want to know details, see document of [wai-test](http://hackage.haskell.org/package/wai-test). Response body can be extracted by [`simpleBody`](http://hackage.haskell.org/packages/archive/wai-test/latest/doc/html/Network-Wai-Test.html#v:simpleBody). [`shouldSatisfy`](http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#v:shouldSatisfy) takes two arguments, the former is value to assert and the latter is predicate.

## Add helper for readable code

I felt example is too much into detail and needs some more helpers. To keep the test code clean is very important for continuous testing. I made few helpers below and the example got much simpler with them.

```haskell

-- test/Helper.hs

import qualified App
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Network.Wai                as W
import qualified Network.Wai.Test           as WT

get :: W.Application -> BS.ByteString -> IO WT.SResponse
get app path =
  WT.runSession (WT.srequest (WT.SRequest req "")) app
      where req = WT.setRawPathInfo WT.defaultRequest path

getBody :: WT.SResponse -> LBS.ByteString
getBody = WT.simpleBody

shouldContain :: LBS.ByteString -> LBS.ByteString -> Expectation
shouldContain subject matcher = assertBool message (subject `contains` matcher)
    where
      s `contains` m = any (LBS.isPrefixOf m) $ LBS.tails s
      message  =
        "Expected \"" ++ LC8.unpack subject ++ "\" to contain \"" ++ LC8.unpack matcher ++ "\", but not"
```

We saw `get` in previous paragraph. It just moved. `getBody` is just an alias for [simpleBody](http://hackage.haskell.org/packages/archive/wai-test/latest/doc/html/Network-Wai-Test.html#v:simpleBody).  `shouldContain` is a matcher, which asserts x is in y or not.

Actual spec got simpler as below.

```haskell
import Helper

main :: IO ()
main = hspec $ do
    describe "GET /" $ do
      it "should contain 'Happy Holidays' in response body" $ do
	app <- getApp
	body <- getBody <$> app `get` ""
	body `shouldContain` "Happy Holidays"
```

## Redirect

Spec to assert 'redirect' response will be like this.

```haskell
-- AppSpec.hs

    describe "GET /bar" $ do
      it "should redirect to /foo" $ do
        app <- getApp
        res <- app `get` "bar"
        res `shouldRedirectTo` "/foo"
```

Yes, I added another matcher `shouldRedirectTo`. I can't say it's simple and clean at this moment, but works fine.

```haskell
-- Helper.hs

shouldRedirectTo :: WT.SResponse -> String -> Expectation
shouldRedirectTo response destination =
    if getStatus response == 302
      then failWith "Expected response to be a redirect but not"
      else case lookup HT.hLocation $ WT.simpleHeaders response of
             Just v -> assertBool
               ("Expected to redirect to \"" ++ destination ++ "\" but \"" ++ C8.unpack v ++ "\"")
               (C8.unpack v == destination)
             Nothing -> failWith "Invalid redirect response header"
```




## Directory structure

## Run specs

## Wrap up
