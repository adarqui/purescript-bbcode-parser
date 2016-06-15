module Data.BBCode.HTML (
  runBBCodeToHTML,
  runBBCodeToHTMLWith,
  bbcodeToHTML
) where



import Data.Array                      as A
import Data.Int                        (toNumber)
import Data.List                       as L
import Data.List                       (List(..))
import Data.Maybe                      (Maybe(..))
import Data.Map                        as M
import Data.Tuple                      (fst)

import Control.Monad.RWS
import Control.Monad.Reader
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.CSS.Indexed        as CSS
import CSS.Font                        as CSS
import CSS.Size                        as CSS
import CSS.Text                        as CSS
import CSS.TextAlign                   as CSS
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (id, show, map, bind, pure, (<$>), (<*>), ($), (<>))

import Data.BBCode.Types



runBBCodeToHTML :: List BBCode -> Array (HTML _ _)
runBBCodeToHTML = runBBCodeToHTMLWith defaultParseReader



runBBCodeToHTMLWith :: ParseReader -> List BBCode -> Array (HTML _ _)
runBBCodeToHTMLWith parse_reader codes =
  fst $ evalRWS (bbcodeToHTML codes) parse_reader defaultParseState



-- bbcodeToHTML :: List BBCode -> HTML _ _
bbcodeToHTML :: List BBCode -> ParseEff (Array (HTML _ _))
bbcodeToHTML codes = go [] codes
  where
  go acc xs =
    case L.uncons xs of
      Nothing                 -> pure $ A.reverse acc
      Just {head: h, tail: t} -> do
        html <- codeToHTML h
        go (html `A.cons` acc) t


-- codeToHTML :: 
codeToHTML :: BBCode -> ParseEff (HTML _ _)
codeToHTML tag = do
  case tag of
       Bold xs              -> H.strong_ <$> bbcodeToHTML xs
       Italic xs            -> H.em_ <$> bbcodeToHTML xs
       Underline xs         -> H.span [CSS.style $ CSS.textDecoration CSS.underline] <$> bbcodeToHTML xs
       Strike xs            -> H.del_ <$> bbcodeToHTML xs
       Size (SizePx px) xs  -> H.span [CSS.style $ CSS.fontSize $ CSS.px $ toNumber px] <$> bbcodeToHTML xs
       Color c xs           -> H.span [] <$> bbcodeToHTML xs -- <span style="color: red;">Hello</span>
       Center xs            -> H.center_ <$> bbcodeToHTML xs
       AlignLeft xs         -> H.p [CSS.style $ CSS.textAlign CSS.leftTextAlign] <$> bbcodeToHTML xs
       AlignRight xs        -> H.p [CSS.style $ CSS.textAlign CSS.rightTextAlign] <$> bbcodeToHTML xs
       Quote author xs      -> H.blockquote_ <$> bbcodeToHTML xs
       Link (Just name) url -> pure $ H.a [P.href url, P.target "_blank"] [H.text name]
       Link Nothing url     -> pure $ H.a [P.href url, P.target "_blank"] [H.text url]
--       List list            -- <ul><li>Red <li>Blue <li>Yellow</ul>
--       OrdList list         -- <ol style="list-style-type: decimal"><li>Red <li>Blue <li>Yellow</ol>
--       Table table
       Pre text             -> pure $ H.pre_ [H.text text]
       Code _ code          -> pure $ H.pre_ [H.text code]
       Move xs              -> pure $ H.text "[move] is deprecated"
       Text text            -> pure $ H.text text
       Image opts url       -> runImage opts url -- H.img [P.src url]
       Youtube url          -> pure $ H.iframe [P.src url]
--        Vimeo url
--        Facebook url
--        Instagram url
--        Streamable url
--        Imgur url
       HR                  -> pure $ H.hr_
       NL                  -> pure $ H.br_
       _                   -> pure $ H.p_ [H.text "unknown"]



runImage :: ImageOpts -> MediaURL -> ParseEff (HTML _ _)
runImage opts url = do
  (r :: ParseReader) <- ask
  let props = (case M.lookup "img" r.trfm of
       Nothing -> []
       Just fn -> let opts' = fn (Image opts url) in [P.height $ P.Pixels 10])
  pure (H.img $ props <> [P.src url])
