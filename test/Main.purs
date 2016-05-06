module Test.Main where


import Data.Either           (Either(..))
import Data.List             (List(..))
import Prelude               (bind, ($))
import Test.Unit             (test)
import Test.Unit.Main        (runTest)
import Test.Unit.Assert      as Assert

import Data.BBCode.Parser
import Data.BBCode.Types



main = runTest do

  test "Token Parser Tests" do

    Assert.equal (Right $ Cons (BBStr "hello") Nil)  $ parseTokens "hello" tokens
    Assert.equal (Right $ Cons (BBStr "[") Nil)      $ parseTokens "[" tokens
    Assert.equal (Right $ Cons (BBStr "]") Nil)      $ parseTokens "]" tokens
    Assert.equal (Right $ Cons (BBStr "[ b ]") Nil)  $ parseTokens "[ b ]" tokens
    Assert.equal (Right $ Cons (BBStr "[/ b ]") Nil) $ parseTokens "[/ b ]" tokens

    Assert.equal
      (Right $ Cons (BBOpen "b") (Cons (BBStr "hello") (Cons (BBClosed "b") Nil)))
      $ parseTokens "[b]hello[/b]" tokens
