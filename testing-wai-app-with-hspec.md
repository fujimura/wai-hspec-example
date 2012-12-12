# Testing wai app with hspec

Recently I wrote some small application with [scotty](http://hackage.haskell.org/package/scotty), which is a small web application built upon wai.
We can use wai-test for application written with scotty because it is also a wai apllication. It's not so difficult as you think, so let's grasp some basics of writing test for wai application through scotty.

## scotty basics

At first, lets take a glance at basics of scotty. The code below is the application used in this article.

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

If you know sinatra or similar kind of web application framework, there could be no 説明 required. It serves `index.mustache` object at root, `foo.mustache` at "/foo" and redirect to "/foo" at "/bar".

`app` will be served in port 3000 in `main`.

## A bit about Hspec

Behavior Driven Development got popular in recent years. Hspec is a library that support BDD in haskell, which is based on the most famous and popular BDD library, RSpec. Hspec is great library. It's easy to understand if you already know RSpec or other BDD framework. Even you don't know about BDD or not tried it yet, it's worth to try Hspec. As a Rails developer(I'm a Rails developer in the daytime), I think BDD and Haskell is much more better than it with Ruby.

## The first step: send request and assert response body

Then let's start writing test code. At first we will assert that the text given to the mustache template should be contained to response body.

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

In example(`it` block), we took application instance with [`Scotty.scottyApp`](http://hackage.haskell.org/packages/archive/scotty/latest/doc/html/Web-Scotty.html#v:scottyApp). The example is run upon IO monad, we have to lift app.

Sending request to application is a bit complicated, so I made a helper(`get`) to run request. If you want to know details, see document of [wai-test](http://hackage.haskell.org/package/wai-test). Response body can be extracted by [`simpleBody`](http://hackage.haskell.org/packages/archive/wai-test/latest/doc/html/Network-Wai-Test.html#v:simpleBody). [`shouldSatisfy`](http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#v:shouldSatisfy) takes two arguments, the former is value to assert and the latter is predicate.

## Add helper for readable code

I felt example is too much into detail and need some more helpers. To keep test code clean is very important for continuous testing. I made few helpers below and the example got much simpler.

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

## Redirect and not_found

## Directory structure

## Run specs

## Wrap up
