module Data.BBCode.HTML (
  runBBCodeToHTML,
  runBBCodeToHTMLWith,
  bbcodeToHTML
) where



import Data.Array                      as A
import Data.NonEmpty                   as NonEmpty
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
import CSS.String                      as CSS
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
       Font opts xs         -> runFont opts xs
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
       Image opts url       -> runImage opts url
       Youtube url          -> pure $ H.iframe [P.src url]
--        Vimeo url
--        Facebook url
--        Instagram url
--        Streamable url
--        Imgur url
       HR                  -> pure $ H.hr_
       NL                  -> pure $ H.br_
       _                   -> pure $ H.p_ [H.text "unknown"]



--
-- TODO FIXME: need to cleanup these redundant map lookups + trfm
--

runImage :: ImageOpts -> MediaURL -> ParseEff (HTML _ _)
runImage opts url = do
  (r :: ParseReader) <- ask
  let code = (case M.lookup "img" r.trfm of
             Nothing   -> Image opts url
             Just trfm -> trfm (Image opts url))
  go code
  where
  go (Image (ImageOpts opts) url) = do
    let
      height_props = case opts.imageHeight of
                          Nothing -> []
                          Just (ImagePx n)      -> [P.height $ P.Pixels n]
                          Just (ImagePercent n) -> [P.height $ P.Percent n]
      width_props = case opts.imageWidth of
                         Nothing -> []
                         Just (ImagePx n)      -> [P.width $ P.Pixels n]
                         Just (ImagePercent n) -> [P.width $ P.Percent n]
    let props = height_props <> width_props
    pure (H.img $ props <> [P.src url])



runFont :: FontOpts -> List BBCode -> ParseEff (HTML _ _)
runFont opts xs = do
  (r :: ParseReader) <- ask
  let code = (case M.lookup "font" r.trfm of
              Nothing   -> Font opts xs
              Just trfm -> trfm (Font opts xs))
  go code
  where
  go (Font (FontOpts opts') xs) = do
    html <- bbcodeToHTML xs
    let fam = (case opts'.fontFamily of
              Nothing  -> CSS.sansSerif
              Just fam -> CSS.GenericFontFamily $ CSS.fromString fam)
    pure $ H.span [CSS.style $ CSS.fontFamily opts'.fontFaces (NonEmpty.singleton fam)] html
