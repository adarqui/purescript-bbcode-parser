module Test.Main where


import Data.Either           (Either(..))
import Data.List             (List(..))
import Prelude               (bind, ($), (<$>))
import Test.Unit             (test)
import Test.Unit.Main        (runTest)
import Test.Unit.Assert      as Assert

import Data.BBCode.Parser
import Data.BBCode.Types



main = runTest do



  test "Token Helpers" do

    Assert.equal "str(hi)"
      $ flattenTokens (Cons (BBStr "hi") Nil)

    Assert.equal "open(b),str(hi),closed(b)"
      $ flattenTokens (Cons (BBOpen "b") (Cons (BBStr "hi") (Cons (BBClosed "b") Nil)))

    Assert.equal (Cons (BBStr "ping pong") Nil)
      $ concatTokens (Cons (BBStr "ping") (Cons (BBStr " ") (Cons (BBStr "pong") Nil)))

    Assert.equal (BBStr "ping pong")
      $ concatBBStr (Cons (BBStr "ping") (Cons (BBStr " ") (Cons (BBStr "pong") Nil)))

    Assert.equal (Cons (BBStr "ping") (Cons (BBOpen "b") (Cons (BBStr "pong") Nil)))
      $ concatTokens (Cons (BBStr "ping") (Cons (BBOpen "b") (Cons (BBStr "pong") Nil)))



  test "Token Parser Tests" do

    Assert.equal (Right $ Cons (BBStr "hello") Nil)  $ parseTokens' "hello"
    Assert.equal (Right $ Cons (BBStr "[") Nil)      $ parseTokens' "["
    Assert.equal (Right $ Cons (BBStr "]") Nil)      $ parseTokens' "]"
    Assert.equal (Right $ Cons (BBStr "[ b ]") Nil)  $ parseTokens' "[ b ]"
    Assert.equal (Right $ Cons (BBStr "[/ b ]") Nil) $ parseTokens' "[/ b ]"

    Assert.equal
      (Right $ Cons (BBOpen "b") (Cons (BBStr "hello") (Cons (BBClosed "b") Nil)))
      $ parseTokens' "[b]hello[/b]"

    Assert.equal
      (Right $ Cons (BBOpen "b") (Cons (BBStr "hello") (Cons (BBClosed "b") Nil)))
      $ parseTokens' "[B]hello[/B]"

    Assert.equal
      (Right "open(b),open(u),str(hello),closed(u),closed(b)")
      $ flattenTokens <$> parseTokens' "[b][u]hello[/u][/b]"

    Assert.equal
      (Right "open(b),open(u),closed(u),closed(b)")
      $ flattenTokens <$> parseTokens' "[b][u][/u][/b]"

    Assert.equal
      (Right "open(b),open(u),closed(u)")
      $ flattenTokens <$> parseTokens' "[b][u][/u]"

    Assert.equal
      (Right "open(b),str(a),open(u),str(b),closed(u),str(c)")
      $ flattenTokens <$> parseTokens' "[b]a[u]b[/u]c"



  test "BBCode Parsing Tests" do

    Assert.equal
      (Right $ Cons (DocText $ Text "hello") Nil)
      $ parseBBCode "hello"

    Assert.equal
      (Right $ Cons (DocText $ Text "[") Nil)
      $ parseBBCode "["

    Assert.equal
      (Right $ Cons (DocText $ Text "]") Nil)
      $ parseBBCode "]"

    Assert.equal
      (Right $ Cons (DocText $ Text "[ b ]") Nil)
      $ parseBBCode "[ b ]"

    Assert.equal
      (Right $ Cons (DocText $ Text "[/ b ]") Nil)
      $ parseBBCode "[/ b ]"

    Assert.equal
      (Left "b not pushed")
      $ parseBBCode "[/b]hello"

    Assert.equal
      (Left "b not closed")
      $ parseBBCode "[b]hello"

    Assert.equal
      (Right $ Cons (DocText (Bold (Cons (Text "hello") Nil))) Nil)
      $ parseBBCode "[b]hello[/b]"

{-
    Assert.equal
      (Right $ Cons (DocText (Underline (Cons (Bold (Cons (Text "hello") Nil)) Nil))) Nil)
      $ parseBBCode "[u][b]hello[/b][/u]"

    Assert.equal
      (Right $ Cons (DocSpacing HR) Nil)
      $ parseBBCode "[hr]"
      -}
