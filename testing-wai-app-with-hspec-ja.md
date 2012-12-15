# HspecでWAIのアプリケーションをテストしましょう

## イントロ

近頃、私は
[scotty](http://hackage.haskell.org/package/scotty)を使ってアプリケーションをテスト付きで書きました。いくつか便利な情報を得たので、共有したいと思います。

scottyは[WAI](http://hackage.haskell.org/package/wai)ベースなので、テストには[wai-test](http://hackage.haskell.org/package/wai-test)が使えます。wai-testはWAIアプリケーション用のテストフレームワークで、テストに便利な関数が入っています。

文中のコード例の実際に動くものは[ここ](https://github.com/fujimura/wai-hspec-example)にあります。文中のコード例はimportやモジュール定義等を適宜省略していますのでご注意ください。

## scottyの基礎

まずはscottyの基本的な部分を眺めておきましょう。この記事で使われるアプリケーションは下記の通りです。

```haskell
-- src/App.hs

app :: ScottyM ()
app = do
    get "/" $
        mustache "src/Views/index.mustache" $ indexView "Happy Holidays" "from Fujimura"

    get "/foo" $
        mustache "src/Views/foo.mustache" $ fooView "Foo" "Bar"

    get "/bar" $
        redirect "/foo"
```

[sinatra](http://www.sinatrarb.com/)系のシンプルなWebアプリケーションフレームワークを知っている人にはあまり説明はいらないかもしれません。このアプリは"/"で`index.mustache`を、"/foo"で`foo.mustache`の内容を表示して、 "/bar"は"foo"にリダイレクトします。ビューの詳細については省略します。

## Hspecについて少し

ここ数年、振る舞い駆動開発(Behavior Driven Development, BDD)が人気です。HspecはHaskellでBDDをするライブラリで、最も有名なBDDライブラリであるRSpecをベースに作られています。とてもよくデザインされていて、使うのは簡単です。BDDの経験があれば使い方はすぐわかると思います。

もしあなたがBDDについて知らない、もしくはやったことがないなら、Hspecで試してみる価値アリです。HaskellとBDDをの組み合わせは、Rubyプログラマーとして（僕は日中はRailsプログラマーです）、もしかしたらRubyとの組み合わせより良いんじゃないか？って思うくらいです。

## 最初の一歩: リクエストを送って、レスポンスボディを検査しましょう

ではテストコードを書いてみましょう。まずはレスポンスの本文にViewに渡した文字列が含まれてるか検証してみます。

```haskell
-- test/AppSpec.hs

import qualified App
import qualified Data.ByteString.Lazy       as LBS
import           Helper
import qualified Network.Wai                as W
import qualified Network.Wai.Test     as WT
import           Web.Scotty           (scottyApp)

main :: IO ()
main = hspec $ do
    describe "GET /" $ do
      it "should contain 'Hello' in response body" $ do
        app <- liftIO $ scottyApp App.app
        body <- WT.simpleBody <$> app `get` ""
        body `shouldSatisfy` \x -> any (LBS.isPrefixOf "Happy Holidays") $ LBS.tails x
```

example(`it`の部分)の中では、まずはさっきのコードにあった`app`から[scottyApp](http://hackage.haskell.org/packages/archive/scotty/latest/doc/html/Web-Scotty.html#v:scottyApp)を使って、WAIの[application](http://hackage.haskell.org/packages/archive/wai/latest/doc/html/Network-Wai.html#t:Application)を取得しています。

リクエスト送信は少し複雑です。なので、リクエストを送るために`get`というヘルパーを作りました。ヘルパーの詳細を知りたかったら、
[wai-test](http://hackage.haskell.org/package/wai-test)のドキュメントを見てみてください。


レスポンスの本文は[`simpleBody`](http://hackage.haskell.org/packages/archive/wai-test/latest/doc/html/Network-Wai-Test.html#v:simpleBody)で取れます。[`shouldSatisfy`](http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#v:shouldSatisfy)は検証したい値とそれに対する述語をとってアサーションを行います。

## ヘルパーを追加して読みやすくしましょう

この例はちょっと詳細を語りすぎている気がするので、ヘルパーを追加して、綺麗にしたいです。下記のヘルパーを追加して、ちょっとシンプルになりました。

```haskell
-- test/Helper.hs

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

さっき実装した`get`はこちらに動かしました。`getBody`は[simpleBody](http://hackage.haskell.org/packages/archive/wai-test/latest/doc/html/Network-Wai-Test.html#v:simpleBody)に別名をつけただけです。`shouldContain`はマッチャーで、xがyに入ってるかどうか確かめます。

実際のテストコードは下記のようにシンプルになりました。

```haskell
-- test/AppSpec.hs

import qualified Data.ByteString.Lazy as LBS
import           Helper
import qualified Network.Wai.Test     as WT
import           Web.Scotty           (scottyApp)

main :: IO ()
main = hspec $ do
    describe "GET /" $ do
      it "should contain 'Happy Holidays' in response body" $ do
        app <- liftIO $ scottyApp App.app
	body <- getBody <$> app `get` ""
	body `shouldContain` "Happy Holidays"
```

## リダイレクト

リダイレクトを検証するspecは下記のようになります。

```haskell
-- test/AppSpec.hs

    describe "GET /bar" $ do
      it "should redirect to /foo" $ do
        app <- getApp
        res <- app `get` "bar"
        res `shouldRedirectTo` "/foo"
```

はい。`shouldRedirectTo`ってマッチャーを追加しました。実装は綺麗ともシンプルとも言えませんが、まあ動きます。

```haskell
-- test/Helper.hs.hs

import           Test.HUnit                 (assertBool, assertFailure)
import qualified Data.ByteString.Char8      as C8
import qualified Network.HTTP.Types         as HT
import qualified Network.Wai.Test           as WT

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

## テストスイートとアプリケーションコードの配置とか、テストの実行とか…。

この記事で使ったコードを下記にまとめておいたので、それを見てみてください。僕の知っている限りで今のところ一番良いとされている慣行におおよそ従おうとしました。

[https://github.com/fujimura/wai-hspec-example](https://github.com/fujimura/wai-hspec-example)

## 結論

お気づきかもしれませんが、WAIアプリケーションにテストを書くのは、今のところすごく簡単だとは言えない状況です。WAI用のマッチャーを書かないとですね…。


## 追記
`shouldContain`は、wai-testの[assertBodyContains](http://hackage.haskell.org/packages/archive/wai-test/latest/doc/html/Network-Wai-Test.html#v:assertBodyContains)で代用できますね…この記事を書いている途中で気が付きました。
