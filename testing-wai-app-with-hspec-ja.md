# hspecでwaiのアプリケーションをテストしましょう

## イントロ

近頃、私は
[scotty](http://hackage.haskell.org/package/scotty)を使ってアプリケーションをテスト付きで書きました。いくつか便利な情報を得たので、共有したいと思います。


scottyは[wai](http://hackage.haskell.org/package/wai)ベースなので、テストには[wai-test](http://hackage.haskell.org/package/wai-test)が使えます。wai-testはwaiアプリケーション用のテストフレームワークで、テストに便利な関数が入っています。

## scottyの基礎

まずはscottyの基本的な部分を眺めておきましょう。下記のコードがこの記事で使われるアプリケーションです。

```haskell
-- AppSpec.hs
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

あなたがもしsinatraとかのWebアプリケーションフレームワークを知っていたら、あまり説明はいらないかもしれません。このアプリは"/"で`index.mustache`を、"/foo"で`foo.mustache`を表示して、 "/bar"は"foo"にリダイレクトします。ビューの詳細については省略します。

で、`app`は`main`で3000番ポートで実行されます。

## Hspecについて少し

最近振る舞い駆動開発(Behavior Driven Development, BDD)は人気になってきました。HspecはHaskellでBDDをするライブラリで、最も有名なBDDライブラリであるRSpecをベースに作られています。Hspecはとてもよくデザインされていて使うのは簡単です。もしあなたがBDDの経験があれば、使い方はすぐ判ると思います。

もしあなたがBDDについて知らない、もしくはやったことがないなら、Hspecで試してみる価値アリです。HaskellとBDDをの組み合わせは、Rubyプログラマーとして（僕は日中はRailsプログラマーです）的にも、もしかしたらRubyとの組み合わせより良いんじゃないか？って思うくらいです。


## 最初の一歩: リクエストを送って、レスポンスボディを検査しましょう

ではテストコーどを書いてみましょう。まずはレスポンスの本文にViewに渡した文字列が含まれてるか検証してみます。

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

example(`it`の部分)の中では、まずはさっきのコードにあった`app`から[Scotty.scottyApp](http://hackage.haskell.org/packages/archive/scotty/latest/doc/html/Web-Scotty.html#v:scottyApp)を使って、waiの[application](http://hackage.haskell.org/packages/archive/wai/latest/doc/html/Network-Wai.html#t:Application)を取得しています。

リクエスト送信は少し複雑です。なので、リクエストを送るために`get`というヘルパーを作りました。ヘルパーの詳細を知りたかったら、
[wai-test](http://hackage.haskell.org/package/wai-test)のドキュメントを見てみてください。



. Response body can be extracted by 
レスポンスの本文は[`simpleBody`](http://hackage.haskell.org/packages/archive/wai-test/latest/doc/html/Network-Wai-Test.html#v:simpleBody)で取れます。[`shouldSatisfy`](http://hackage.haskell.org/packages/archive/hspec-expectations/latest/doc/html/Test-Hspec-Expectations.html#v:shouldSatisfy)は検証したい値とそれに対する述語をとって検証を行います。

## ヘルパーを追加して読みやすくしましょう

この例はちょっと詳細を語りすぎている気がします。ヘルパーを追加して、綺麗にしたいです。テストコードを綺麗に保つことは、継続的にテストする上で大切です。下記のヘルパーを追加して、ちょっとシンプルになりました。 

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

`get`はさっき書きました。動かしただけです。`getBody`は[simpleBody](http://hackage.haskell.org/packages/archive/wai-test/latest/doc/html/Network-Wai-Test.html#v:simpleBody)の別名です。`shouldContain`はマッチャーで、xがyに入ってるかどうか確かめます。

実際のテストコードは下記のようにシンプルになりました。

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

## リダイレクト

リダイレクトを検証するspecは下記のようになります。

```haskell
-- AppSpec.hs

    describe "GET /bar" $ do
      it "should redirect to /foo" $ do
        app <- getApp
        res <- app `get` "bar"
        res `shouldRedirectTo` "/foo"
```

はい。`shouldRedirectTo`ってマッチャーを追加しました。実装は綺麗ともシンプルとも言えませんが、まあ動きます。

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

## テストスイートとアプリケーションコードの配置とか、テストの実行とか…。

この記事で使ったコードを下記にまとめておいたので、それを見てみてください。
僕の知っている限りで今のところ一番良いとされている慣行に従おうとしたので、参考になるかと思います。

[https://github.com/fujimura/wai-hspec-example](https://github.com/fujimura/wai-hspec-example)

## 結論
見ての通り、waiアプリケーションにテストを書いていくのは、今のところ常に簡単とはいえません。wai用のマッチャーを書かないとですね…。
