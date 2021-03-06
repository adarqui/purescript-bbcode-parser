module Test.Main where


import Control.Monad.Eff               (Eff())
import Control.Monad.Eff.Console       (CONSOLE())
import Data.Either                     (Either(..))
import Data.List                       (List(..))
import Data.Maybe                      (Maybe(..))
import Data.String                     (fromCharArray) as String
import Data.Unfoldable                 (replicate)
import Prelude                         (Unit, bind, ($), (<$>), (<>))
import Test.Unit                       (test)
import Test.Unit.Main                  (runTest)
import Test.Unit.Assert                as Assert
import Test.Unit.Console               (TESTOUTPUT())

import Data.BBCode.Parser              (parseBBCode, parseTokens', concatTokens, concatBBStr)
import Data.BBCode.Types               (BBCode(..), Token(..), flattenTokens)



main :: forall t. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT | t) Unit
main = runTest do



  test "Token Helpers" do

    Assert.equal "str(hi)"
      $ flattenTokens (Cons (BBStr "hi") Nil)

    Assert.equal "open(b),str(hi),closed(b)"
      $ flattenTokens (Cons (BBOpen Nothing "b") (Cons (BBStr "hi") (Cons (BBClosed "b") Nil)))

    Assert.equal (Cons (BBStr "ping pong") Nil)
      $ concatTokens (Cons (BBStr "ping") (Cons (BBStr " ") (Cons (BBStr "pong") Nil)))

    Assert.equal (BBStr "ping pong")
      $ concatBBStr (Cons (BBStr "ping") (Cons (BBStr " ") (Cons (BBStr "pong") Nil)))

    Assert.equal (Cons (BBStr "ping") (Cons (BBOpen Nothing "b") (Cons (BBStr "pong") Nil)))
      $ concatTokens (Cons (BBStr "ping") (Cons (BBOpen Nothing "b") (Cons (BBStr "pong") Nil)))



  test "Token Parser Tests" do

    Assert.equal (Right $ Cons (BBStr "hello") Nil)  $ parseTokens' "hello"

    Assert.equal (Right $ Cons (BBStr "[") Nil)      $ parseTokens' "["
    Assert.equal (Right $ Cons (BBStr "]") Nil)      $ parseTokens' "]"
    Assert.equal (Right $ Cons (BBStr "[ b ]") Nil)  $ parseTokens' "[ b ]"
    Assert.equal (Right $ Cons (BBStr "[/ b ]") Nil) $ parseTokens' "[/ b ]"

    Assert.equal
      (Right $ Cons (BBOpen Nothing "b") (Cons (BBStr "hello") (Cons (BBClosed "b") Nil)))
      $ parseTokens' "[b]hello[/b]"

    Assert.equal
      (Right $ Cons (BBOpen Nothing "b") (Cons (BBStr "hello") (Cons (BBClosed "b") Nil)))
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

    Assert.equal
      (Right "open(url, someurl),str(name),closed(url)")
      $ flattenTokens <$> parseTokens' "[url=someurl]name[/url]"

    Assert.equal
      (Right "open(quote, author=adarqui),str(hello),closed(quote)")
      $ flattenTokens <$> parseTokens' "[quote author=adarqui]hello[/quote]"

    Assert.equal
      (Right "open(youtube),str(https://www.youtube.com/watch?v=video),closed(youtube)")
      $ flattenTokens <$> parseTokens' "[youtube]https://www.youtube.com/watch?v=video[/youtube]"



  test "BBCode Parsing Tests" do

    Assert.equal
      (Right $ Cons (Text "hello") Nil)
      $ parseBBCode "hello"

    Assert.equal
      (Right $ Cons (Text "/:?") Nil)
      $ parseBBCode "/:?"

    Assert.equal
      (Right $ Cons (Text "[") Nil)
      $ parseBBCode "["

    Assert.equal
      (Right $ Cons (Text "]") Nil)
      $ parseBBCode "]"

    Assert.equal
      (Right $ Cons (Text "[ b ]") Nil)
      $ parseBBCode "[ b ]"

    Assert.equal
      (Right $ Cons (Text "[/ b ]") Nil)
      $ parseBBCode "[/ b ]"

    Assert.equal
      (Left "b not pushed")
      $ parseBBCode "[/b]hello"

    Assert.equal
      (Left "b not closed")
      $ parseBBCode "[b]hello"

    Assert.equal
      (Right $ Cons (Bold (Cons (Text "hello") Nil)) Nil)
      $ parseBBCode "[b]hello[/b]"

    Assert.equal
      (Right $ Cons (Bold (Cons (Text "https://adarq.org") Nil)) Nil)
      $ parseBBCode "[b]https://adarq.org[/b]"

    Assert.equal
      (Right $ Cons (Underline (Cons (Bold (Cons (Text "hello") Nil)) Nil)) Nil)
      $ parseBBCode "[u][b]hello[/b][/u]"

    Assert.equal
      (Right $ Cons (Italic (Cons (Underline (Cons (Bold (Cons (Text "hello") Nil)) Nil)) Nil)) Nil)
      $ parseBBCode "[i][u][b]hello[/b][/u][/i]"

    Assert.equal
      (Right $
        Cons
          (Text "ping")
          (Cons
            (Bold (Cons (Text "hello") Nil))
          (Cons (Text "pong") Nil))
      )
      $ parseBBCode "ping[b]hello[/b]pong"

    Assert.equal
      (Right (Cons (Text("zero")) (Cons (Underline(Cons (Text("one")) (Cons (Bold(Cons (Text("two")) (Nil))) (Cons (Text("three")) (Nil))))) (Cons (Text("four")) (Nil)))))
      $ parseBBCode "zero[u]one[b]two[/b]three[/u]four"

    Assert.equal
      (Right (Cons (Youtube "https://www.youtube.com/watch?v=video") Nil))
      $ parseBBCode "[youtube]https://www.youtube.com/watch?v=video[/youtube]"

    Assert.equal
      (Right (Cons HR Nil))
      $ parseBBCode "[hr]"

    Assert.equal
      (Right (Cons NL Nil))
      $ parseBBCode "\n"

    Assert.equal
      (Right (Cons NL (Cons NL Nil)))
      $ parseBBCode "\n\n"

    Assert.equal
      (Right (Cons (Text "hi") (Cons NL Nil)))
      $ parseBBCode "hi\n"

    Assert.equal
      (Right (Cons NL (Cons (Text "hi") Nil)))
      $ parseBBCode "\nhi"

    Assert.equal
      (Right (Cons (Text("I am the ")) (Cons (Bold(Cons (Text("best")) (Nil))) (Cons (Text(" man, I ")) (Cons (Bold(Cons (Text("deed")) (Nil))) (Cons (Text(" it. ")) (Cons (Bold(Cons (Text("yup")) (Nil))) (Nil))))))))
      $ parseBBCode "I am the [b]best[/b] man, I [b]deed[/b] it. [b]yup[/b]"

    Assert.equal
      (Right $ Cons (Link (Just "name") "someUrl") Nil)
      $ parseBBCode "[url=someUrl]name[/url]"

    let
      bigString n      = String.fromCharArray (replicate n 'A')
      big_string_1024  = bigString 1024
      big_string_10024 = bigString 10024

    Assert.equal
      (Right $ Cons (Bold (Cons (Text big_string_1024) Nil)) Nil)
      $ parseBBCode $ "[b]" <> big_string_1024 <> "[/b]"

    Assert.equal
      (Right $ Cons (Bold (Cons (Text big_string_10024) Nil)) Nil)
      $ parseBBCode $ "[b]" <> big_string_10024 <> "[/b]"


--    Assert.equal
--      (Right $ Cons ..
--      $ parseBBCode "[quote author=adarqui]hello[/quote]"




-- TODO: No tests yet .. HTML doesn't have an Eq instance etc.
--
--  test "BBCode To HTML Tests" do
--
--    Assert.equal
--      (H.div_ [H.text "hello"])
--      $ bbcodeToHTML $ fromRight $ parseBBCode "hello"
--
--
--
-- fromRight :: forall a b. Either a b -> b
-- fromRight (Right b) = b
