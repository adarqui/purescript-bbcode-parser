module Data.BBCode.Parser (
  open,
  closed,
  str,
  token,
  tokens,
  concatTokens,
  concatBBStr,
  runBBCode,
  parseTokens,
  parseTokens',
  parseBBCodeFromTokens,
  parseBBCodeFromTokens',
  parseBBCode,
  parseBBCodeWith
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
import Data.String as String
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
  r <- manyTill letter (string "]")
  return $ BBOpen Nothing (fromCharListToLower $ c : r)



openWithParams :: forall m a. (Monad m) => ParserT String m Token
openWithParams = do
  string "["
  c <- letter
  r <- manyTill letter (string " " <|> string "=")
  pc <- anyChar
  pr <- manyTill anyChar (string "]")
  return $ BBOpen (Just (fromCharList $ pc : pr)) (fromCharListToLower $ c : r)



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
token = try closed <|> try openWithParams <|> try open <|> try str <|> try catchAll



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



runBBCode :: Maybe Parameters -> TagName -> List BBCode -> BBCodeMap -> Either ErrorMsg BBCode
runBBCode params tag doc bmap  =
  case M.lookup tag bmap of
       Nothing   -> Left $ tag <> " not found"
       Just bbFn -> bbFn params doc



runBold :: BBCodeFn
runBold = runTextSimple Bold "Bold"

runItalic :: BBCodeFn
runItalic = runTextSimple Italic "Italic"

runUnderline :: BBCodeFn
runUnderline = runTextSimple Underline "Underline"

runStrike :: BBCodeFn
runStrike = runTextSimple Strike "Strike"

-- runSize
-- runColor

runCenter :: BBCodeFn
runCenter = runTextSimple Center "Center"

runAlignLeft :: BBCodeFn
runAlignLeft = runTextSimple AlignLeft "Left"

runAlignRight :: BBCodeFn
runAlignRight = runTextSimple AlignRight "Right"

runQuote :: BBCodeFn
runQuote = runTextSimple (Quote Nothing) "Quote"

runLink :: BBCodeFn
runLink m_params (Cons (Text s) Nil) =
  case m_params of
       Nothing   -> Right $ Link Nothing s
       Just url  -> Right $ Link (Just s) url
runLink _ _ = Left $ "Link" <> " error"

runPre :: BBCodeFn
runPre = runRaw Pre "Pre"

runCode :: BBCodeFn
runCode = runRaw (Code Nothing) "Code"

runMove :: BBCodeFn
runMove = runTextSimple Move "Move"

runNL :: BBCodeFn
runNL _ Nil = Right $ NL
runNL _ _   = Left $ "nl error"

runHR :: BBCodeFn
runHR _ Nil = Right $ HR
runHR _ _   = Left "hr error"

--
-- TODO FIXME: media needs proper url parsing/verification
--

runYoutube :: BBCodeFn
runYoutube = runMedia Youtube "Youtube"

runVimeo :: BBCodeFn
runVimeo = runMedia Vimeo "Vimeo"

runFacebook :: BBCodeFn
runFacebook = runMedia Facebook "Facebook"

runInstagram :: BBCodeFn
runInstagram = runMedia Instagram "Instagram"

runStreamable :: BBCodeFn
runStreamable = runMedia Streamable "Streamable"

runImgur :: BBCodeFn
runImgur = runMedia Imgur "Imgur"

runImage :: BBCodeFn
runImage = runMedia (Image defaultImageOpts) "Image"


-- Helpers
--

runTextSimple :: (List BBCode -> BBCode) -> TagName -> Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode
runTextSimple _ tag _ Nil = Left $ tag <> " error"
runTextSimple mk _ _ t    = Right $ mk t

runRaw :: (String -> BBCode) -> TagName -> Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode
runRaw mk _ _ (Cons (Text raw) Nil) = Right $ mk raw
runRaw _ tag _ _                    = Left $ tag <> " error"

runMedia :: (String -> BBCode) -> TagName -> Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode
runMedia mk _ _ (Cons (Text url) Nil) = Right $ mk url
runMedia _ tag _ (Cons _ Nil)          = Left $ tag <> " error: only urls may be wrapped in " <> tag
runMedia _ tag _ _                     = Left $ tag <> " error"



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
    Tuple "left" runAlignLeft,
    Tuple "right" runAlignRight,
    Tuple "quote" runQuote,
    Tuple "link" runLink,
    Tuple "url" runLink,
--    Tuple "list" runList,
--    Tuple "ol" runOrdList,
--    Tuple "ordlist" runOrdList,
--    Tuple "table" runTable,
    Tuple "move" runMove,
    Tuple "img" runImage,
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
    Tuple "pre" runPre,
    Tuple "code" runCode
  ]



-- | TODO FIXME: worst function ever.. my brain is not working
--
parseTextAndNewlines :: String -> List BBCode
parseTextAndNewlines = go Nil
  where
  go acc "" = acc
  go acc s  =
    let
      str    = String.takeWhile (\c -> c /= '\r' && c /= '\n') s
      nl     = if (String.length str == 0)
                  then String.length $ String.takeWhile (\c -> c == '\r' || c == '\n') s
                  else 0
      rest   = if (nl > 0)
                  then String.drop nl s
                  else String.drop (String.length str) s
    in
      if (nl > 0)
         then
           go (replicate nl NL <> acc) rest
         else
           go (Text str : acc) rest



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

  try_maps params tag =
    case M.lookup tag bmap, M.lookup tag cmap of
         Just bmap_fn, Nothing -> \xs -> runBBCode params tag xs bmap
         Nothing, Just cmap_fn -> \xs -> runBBCode params tag xs cmap
         -- TODO FIXME: need a user supplied FN to handle errors, this is what runBBCode was for; but not anymore
         _, _                  -> \xs -> Right $ Text tag

  go :: List Token -> Int -> ParseEff (Either String BBDoc)
  go toks' level = do

    stack <- gets _.stack
    accum <- gets _.accum
    saccum <- gets _.saccum

    case uncons toks' of
      Nothing -> do
        case stack of
          Nil                    -> return $ Right $ reverse accum
          (Cons (Tuple _ tag) _) -> return $ Left $ tag <> " not closed"
      Just { head, tail } ->
        case head of

          BBStr s           -> do
            let
              text_and_newlines = parseTextAndNewlines s
            if L.null stack
               then do
                 modify (\st -> st{ accum = text_and_newlines <> st.accum })
                 go tail level
               else do
                 modify (\st -> st{ saccum = (map (Tuple level) text_and_newlines) <> st.saccum })
                 go tail level

          BBOpen params tag -> do
            -- We need to handle things differently based upon whether or not the bbcode is:
            -- 1. a unary operator - no closing tag
            -- 2. a consumer - consumes all other tags until the consumer's closing tag is found
            -- 3. a normal bbcode operator which has an open tag, content, and a closing tag
            if M.member tag umap
               then do
                 case (runBBCode params tag Nil umap) of
                   Left err   -> return $ Left err
                   Right new' -> do
                     modify (\st -> st{ accum = (new' : st.accum) })
                     go tail level
               else do
                 modify (\st -> st{ stack = (Tuple params tag) : st.stack })
                 go tail (level+1)

          BBClosed tag      -> do
            case uncons stack of
              Nothing -> return $ Left $ tag <> " not pushed"
              Just { head: Tuple params tag, tail : stTail } -> do
                let
                  beneath = filter (\(Tuple l v) -> l < level) saccum
                  at_or_above = filter (\(Tuple l v) -> l >= level) saccum
                case (try_maps params tag (L.reverse $ map snd at_or_above)) of
                  Left err -> return $ Left err
                  Right new' -> do
                    if L.null stTail
                       then do
                         modify (\st -> st{ accum = new' : st.accum, stack = stTail, saccum = Nil :: (List (Tuple Int BBCode)) })
                         go tail (level-1)
                       else do
                         modify (\st -> st{ saccum = (Tuple level new' : beneath), stack = stTail })
                         go tail (level-1)



parseBBCode :: String -> Either String (List BBCode)
parseBBCode = parseBBCodeWith defaultParseReader



parseBBCodeWith :: ParseReader -> String -> Either String (List BBCode)
parseBBCodeWith parse_reader s =
  case toks of
       Left s   -> Left s
       Right bb -> fst $ evalRWS (parseBBCodeFromTokens bb) parse_reader defaultParseState
  where
  toks = parseTokens' s
