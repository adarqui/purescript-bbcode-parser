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
import Control.Monad.RWS
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



runBBCode :: String -> List BBCode -> BBCodeMap -> Either String BBCode
runBBCode s doc bmap  =
  case M.lookup s bmap of
       Nothing -> Left $ s <> " not found"
       Just v  -> v doc



runBold :: List BBCode -> Either String BBCode
runBold t   = Right $ Bold t
runBold _   = Left "bold error"

runItalic :: List BBCode -> Either String BBCode
runItalic t   = Right $ Italic t
runItalic _   = Left "bold error"

runUnderline :: List BBCode -> Either String BBCode
runUnderline t   = Right $ Underline t
runUnderline _   = Left "bold error"

runStrike :: List BBCode -> Either String BBCode
runStrike t   = Right $ Strike t
runStrike _   = Left "bold error"

-- runSize
-- runColor

runCenter :: List BBCode -> Either String BBCode
runCenter t   = Right $ Center t
runCenter _   = Left "bold error"

runCode :: List BBCode -> Either String BBCode
runCode _ = Left "not implemented"

runHR :: List BBCode -> Either String BBCode
runHR Nil = Right $ HR
runHR _       = Left "hr error"

--
-- TODO FIXME: media needs proper url parsing/verification
--

runYoutube :: List BBCode -> Either String BBCode
runYoutube = runMedia Youtube "Youtube"

runVimeo :: List BBCode -> Either String BBCode
runVimeo = runMedia Vimeo "Vimeo"

runFacebook :: List BBCode -> Either String BBCode
runFacebook = runMedia Facebook "Facebook"

runInstagram :: List BBCode -> Either String BBCode
runInstagram = runMedia Instagram "Instagram"

runStreamable :: List BBCode -> Either String BBCode
runStreamable = runMedia Streamable "Streamable"

runImgur :: List BBCode -> Either String BBCode
runImgur = runMedia Imgur "Imgur"

runMedia :: (String -> BBCode) -> String -> List BBCode -> Either String BBCode
runMedia mk _ (Cons (Text url) Nil) = Right $ mk url
runMedia _ tag (Cons _ Nil)          = Left $ tag <> " error: only urls may be wrapped in " <> tag
runMedia _ tag _                     = Left $ tag <> " error"



-- | The default BBCode map specifies all bbcode that works within open & closed tags
--
defaultBBCodeMap :: BBCodeMap
defaultBBCodeMap =
  M.fromFoldable [
    Tuple "b" runBold,
    Tuple "i" runItalic,
    Tuple "u" runUnderline,
    Tuple "s" runStrike,
--    Tuple "size" runSize,
--    Tuple "color" runColor,
    Tuple "center" runCenter,
--    Tuple "quote" runQuote,
--    Tuple "link" runLink,
--    Tuple "list" runList,
--    Tuple "ol" runOrdList,
--    Tuple "ordlist" runOrdList,
--    Tuple "table" runTable,
--    Tuple "img" runImage,
    Tuple "youtube" runYoutube,
    Tuple "vimeo" runVimeo,
    Tuple "facebook" runFacebook,
    Tuple "instagram" runInstagram,
    Tuple "streamable" runStreamable,
    Tuple "imgur" runImgur
  ]



-- | The unary map is for bbcode that doesn't have a closing tag
--
defaultUnaryBBCodeMap :: BBCodeMap
defaultUnaryBBCodeMap =
  M.fromFoldable [
    Tuple "hr" runHR
  ]



-- | The "Consume" map is for bbcode that consumes all other tags up until its own closing tag
--
defaultConsumeBBCodeMap :: BBCodeMap
defaultConsumeBBCodeMap =
  M.fromFoldable [
    Tuple "code" runCode
  ]



parseBBCodeFromTokens :: List Token -> ParseEff (Either String BBDoc)
parseBBCodeFromTokens = parseBBCodeFromTokens' defaultBBCodeMap defaultUnaryBBCodeMap defaultConsumeBBCodeMap



parseBBCodeFromTokens' ::
     BBCodeMap
  -> BBCodeMap
  -> BBCodeMap
  -> List Token
  -> ParseEff (Either String BBDoc)
parseBBCodeFromTokens' bmap umap cmap toks = go toks 0
  where
  go :: List Token -> Int -> ParseEff (Either String BBDoc)
  go toks' level = do

    stack <- gets _.stack
    accum <- gets _.accum
    saccum <- gets _.saccum

    case uncons toks' of
      Nothing -> do
        case stack of
          Nil -> return $ Right $ reverse accum
          xs  -> return $ Left $ (Elm.String.concat $ ElmL.intersperse " " xs) <> " not closed"
      Just { head, tail } ->
        case head of

          BBStr s    -> do
            if L.null stack
               then do
                 modify (\st -> st{ accum = (Text s : st.accum) })
                 go tail level
               else do
                 modify (\st -> st{ saccum = (Tuple level (Text s)) : st.saccum })
                 go tail level

          BBOpen t   -> do
            -- We need to handle things differently based upon whether or not the bbcode is:
            -- 1. a unary operator - no closing tag
            -- 2. a consumer - consumes all other tags until the consumer's closing tag is found
            -- 3. a normal bbcode operator which has an open tag, content, and a closing tag
            if M.member t umap
               then do
                 case (runBBCode t Nil umap) of
                   Left err   -> return $ Left err
                   Right new' -> do
                     modify (\st -> st{ accum = (new' : st.accum) })
                     go tail level
               else do
                 modify (\st -> st{ stack = t : st.stack })
                 go tail (level+1)

          BBClosed t -> do
            case uncons stack of
              Nothing -> return $ Left $ t <> " not pushed"
              Just { head : stHead, tail : stTail } -> do
                let
                  beneath = filter (\(Tuple l v) -> l < level) saccum
                  at_or_above = filter (\(Tuple l v) -> l >= level) saccum
                  new = runBBCode stHead (L.reverse $ map snd at_or_above) bmap
                case new of
                  Left err -> return $ Left err
                  Right new' -> do
                    if L.null stTail
                       then do
                         modify (\st -> st{ accum = new' : st.accum, stack = stTail })
                         go tail (level-1)
                       else do
                         modify (\st -> st{ saccum = (Tuple level new' : beneath), stack = stTail })
                         go tail (level-1)



parseBBCode :: String -> Either String (List BBCode)
parseBBCode s =
  case toks of
       Left s   -> Left s
       Right bb -> fst $ evalRWS (parseBBCodeFromTokens bb) unit defaultParseState
  where
  toks = parseTokens' s
