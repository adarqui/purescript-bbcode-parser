module Data.BBCode.Parser (
  open,
  closed,
  str,
  token,
  tokens,
  concatTokens,
  concatBBStr,
  parseTokens,
  parseTokens',
  parseBBCodeFromTokens,
  parseBBCode
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
import Data.Map as M
import Data.Maybe
import Data.String hiding (uncons)
import Data.Tuple
import Elm.List as ElmL
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
  r <- manyTill anyChar (string "]")
  return $ BBOpen (fromCharListToLower $ c : r)



closed :: forall m a. (Monad m) => ParserT String m Token
closed = do
  string "[/"
  c <- letter
  r <- manyTill anyChar (string "]")
  return $ BBClosed (fromCharListToLower $ c : r)



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



fromCharListToLower :: forall f. Foldable f => f Char -> String
fromCharListToLower = toLower <<< fromCharList



-- | concat consecutive BBStr's
--
concatTokens :: List Token -> List Token
concatTokens = go Nil
  where
  go accum xs =
    case uncons xs of
      Nothing             -> reverse accum
      Just { head, tail } ->
        let
          heads = L.takeWhile isBBStr tail
          tails = L.dropWhile isBBStr tail
        in
          if isBBStr head
                         then go ((concatBBStr $ head : heads) : accum) tails
                         else go (head : accum) tail



-- | Once we have a list of BBStr's, turn them into one BBStr
--
concatBBStr :: List Token -> Token
concatBBStr = BBStr <$> Elm.String.concat <<< map (\(BBStr s) -> s) <<< filter isBBStr



isBBStr :: Token -> Boolean
isBBStr (BBStr _) = true
isBBStr _         = false



parseTokens :: forall s. s -> Parser s (List Token) -> Either String (List Token)
parseTokens input p =
  case runParser input p of
    Left err     -> Left $ show err
    Right actual -> Right actual



parseTokens' :: String -> Either String (List Token)
parseTokens' s = parseTokens s tokens



runBBCode :: String -> List BBCode -> M.Map String (List BBCode -> Either String BBCode) -> Either String BBCode
runBBCode _ Nil _ = Right None
runBBCode s doc bmap  =
  case M.lookup s bmap of
       Nothing -> Left $ s <> " not found"
       Just v  -> v doc



runBold :: List BBCode -> Either String BBCode
runBold Nil = Right $ Bold Nil
runBold t   = Right $ Bold t
runBold _   = Left "bold error"

runItalic :: List BBCode -> Either String BBCode
runItalic Nil = Right $ Italic Nil
runItalic t   = Right $ Italic t
runItalic _   = Left "bold error"

runUnderline :: List BBCode -> Either String BBCode
runUnderline Nil = Right $ Underline Nil
runUnderline t   = Right $ Underline t
runUnderline _   = Left "bold error"

runHR :: List BBCode -> Either String BBCode
runHR Nil = Right $ HR
runHR _       = Left "hr error"



defaultBBCodeMap :: M.Map String (List BBCode -> Either String BBCode)
defaultBBCodeMap =
  M.fromFoldable [
    Tuple "b" runBold,
    Tuple "i" runItalic,
    Tuple "u" runUnderline
  ]



parseBBCodeFromTokens :: List Token -> Either String BBDoc
parseBBCodeFromTokens = parseBBCodeFromTokens' defaultBBCodeMap



parseBBCodeFromTokens' :: M.Map String (List BBCode -> Either String BBCode) -> List Token -> Either String BBDoc
parseBBCodeFromTokens' bmap toks = go Nil Nil Nil toks
  where
  go accum saccum stack toks' =
    case uncons toks' of
         Nothing ->
          case stack of
               Nil -> Right $ reverse accum
               xs  -> Left $ (Elm.String.concat $ ElmL.intersperse " " xs) <> " not closed"
         Just { head, tail } ->
           case head of
                BBStr s    ->
                  if L.null stack
                     then go ((Text s) : accum) saccum stack tail
                     else go accum (Text s : saccum) stack tail
                BBOpen t   -> go accum saccum (t : stack) tail
                BBClosed t ->
                  case uncons stack of
                       Nothing -> Left $ t <> " not pushed"
                       Just { head : stHead, tail : stTail } -> do
                           new <- runBBCode stHead saccum bmap
                           if L.null stTail
                              then go (new : accum) (L.singleton new) stTail tail
                              else go accum (L.singleton new) stTail tail



parseBBCode :: String -> Either String (List BBCode)
parseBBCode s = parseTokens' s >>= parseBBCodeFromTokens
