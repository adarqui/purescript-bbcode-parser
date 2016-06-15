module Data.BBCode.HTML (
  bbcodeToHTML
) where



import Data.Array as A
import Data.List as L
import Data.Maybe

import Data.BBCode.Types
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.CSS.Indexed        as CSS
import CSS.Text                        as CSS
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (id, show, map, ($), (<>))


-- bbcodeToHTML :: List BBCode -> HTML _ _
bbcodeToHTML codes = go [] codes
  where
  go acc xs =
    case L.uncons xs of
      Nothing                 -> A.reverse acc
      Just {head: h, tail: t} -> go (codeToHTML h `A.cons` acc) t


-- codeToHTML :: 
codeToHTML tag =
  case tag of
       Bold xs         -> H.strong_ $ bbcodeToHTML xs
       Italic xs       -> H.em_ $ bbcodeToHTML xs
       Underline xs    -> H.span [CSS.style $ CSS.textDecoration CSS.underline] $ bbcodeToHTML xs
       Strike xs       -> H.del_ $ bbcodeToHTML xs
       Size sz xs      -> H.span [] $ bbcodeToHTML xs -- <span style="font-size: 9px;">Hello</span>
       Color c xs      -> H.span [] $ bbcodeToHTML xs -- <span style="color: red;">Hello</span>
       Center xs       -> H.center_ $ bbcodeToHTML xs
       AlignLeft xs    -> H.div [] $ bbcodeToHTML xs -- <div style="text-left: center;">centered text</div>
       AlignRight xs   -> H.div [] $ bbcodeToHTML xs
       Quote author xs -> H.blockquote_ $ bbcodeToHTML xs
--       Link name url
--       List list            -- <ul><li>Red <li>Blue <li>Yellow</ul>
--       OrdList list         -- <ol style="list-style-type: decimal"><li>Red <li>Blue <li>Yellow</ol>
--       Table table
       Pre text        -> H.pre_ [H.text text]
       Code code       -> H.pre_ [H.text code]
       Text text       -> H.text text
--        Youtube url
--        Vimeo url
--        Facebook url
--        Instagram url
--        Streamable url
--        Imgur url
       HR              -> H.hr_
       NL              -> H.br_
       _               -> H.p_ [H.text "unknown"]
