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
       Bold xs      -> H.strong_ $ bbcodeToHTML xs
       Italic xs    -> H.em_ $ bbcodeToHTML xs
       Underline xs -> H.span [] $ bbcodeToHTML xs -- <p><span style="text-decoration: underline;">hello</span></p>
       Text text    -> H.text text
       NL           -> H.br_
       _            -> H.p_ [H.text "unknown"]
