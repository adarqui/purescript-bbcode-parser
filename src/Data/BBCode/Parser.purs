module Data.BBCode.Parser (
  open,
  closed,
  str,
  token,
  tokens,
  parseTokens
) where



import Control.Alt -- ((<|>))
import Control.Apply
import Control.Lazy -- (fix)
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Data.Either
import Data.Foldable
import Data.Functor
import Data.List hiding (many)
import Data.List as L
import Data.Array (many)
import Data.String
import Prelude -- (Monad, Functor, bind, return, (<$>), (+))
import Test.Assert
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.Language
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token hiding (digit)
import Text.Parsing.Parser.Pos

import Data.BBCode.Types



open :: forall m a. (Monad m) => ParserT String m Token
open = do
  string "["
  c <- letter
  r <- many1Till anyChar (string "]")
  return $ BBOpen (fromCharList $ c : r)



closed :: forall m a. (Monad m) => ParserT String m Token
closed = do
  string "[/"
  c <- letter
  r <- many1Till anyChar (string "]")
  return $ BBClosed (fromCharList $ c : r)



str :: forall m a. (Monad m) => ParserT String m Token
str = do
  r <- some (noneOf ['[', ']'])
  return $ BBStr (fromCharList r)



catchAll :: forall m a. (Monad m) => ParserT String m Token
catchAll = do
  r <- some anyChar
  return $ BBStr (fromCharList r)



token :: forall m a. (Monad m) => ParserT String m Token
token = try closed <|> try open <|> try str <|> try catchAll



tokens :: forall m a. (Monad m) => ParserT String m (List Token)
tokens = L.many token




{-
str :: forall m. (Monad m) => ParserT String m String
str = do
  cs <- many $ satisfy \c -> true
  return $ fromCharArray cs
  -}



fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = foldMap fromChar



parseTokens :: forall s a. s -> Parser s a -> Either String a
parseTokens input p =
  case runParser input p of
    Left err     -> Left $ show err
    Right actual -> Right actual
