module Data.BBCode.Types (
  ParseEff,
  ParseState,
  defaultParseState,
  Token (..),
  flattenTokens,
  BBCodeMap,
  BBDoc (..),
  BBCode (..),
  BBSize (..),
  BBList (..),
  BBTable (..),
  BBColor (..),
  ImageSize (..),
  QuoteAuthor,
  LinkURL,
  LinkName,
  MediaURL,
  ImageHeight,
  ImageWidth
) where



import Control.Monad.RWS (RWS)
import Data.Either       (Either)
import Data.Foldable     (foldl)
import Data.List         (List(..))
import Data.Map          as M
import Data.Maybe        (Maybe)
import Data.String       (toLower)
import Data.Tuple        (Tuple)
import Elm.List          (intersperse)
import Prelude           (Unit, class Show, show, class Eq, map, (<>), (==), (<<<), (&&), (+), (-))



type ParseState = {
  accum :: List BBCode,
  stack :: List String,
  saccum :: List (Tuple Int BBCode)
}

defaultParseState :: ParseState
defaultParseState = { accum: Nil, stack: Nil, saccum: Nil }

type ParseEff = RWS Unit Unit ParseState



data Token
  = BBOpen   String
  | BBClosed String
  | BBStr    String

instance tokenShow :: Show Token where
  show (BBOpen s)   = "open("<>toLower s<>")"
  show (BBClosed s) = "closed("<>toLower s<>")"
  show (BBStr s)    = "str("<>s<>")"

instance tokenEq :: Eq Token where
  eq (BBOpen t1)   (BBOpen t2)   = t1 == t2
  eq (BBClosed t1) (BBClosed t2) = t1 == t2
  eq (BBStr t1)    (BBStr t2)    = t1 == t2
  eq _             _             = false

flattenTokens :: List Token -> String
flattenTokens = foldl (<>) "" <<< intersperse "," <<< map show



type BBCodeMap = M.Map String (List BBCode -> Either String BBCode)



type BBDoc = List BBCode



data BBCode
  = Bold       (List BBCode)
  | Italic     (List BBCode)
  | Underline  (List BBCode)
  | Strike     (List BBCode)
  | Size       BBSize  (List BBCode)
  | Color      BBColor (List BBCode)
  | Center     (List BBCode)
  | Quote      (Maybe QuoteAuthor) (List BBCode)
  | Link       (Maybe LinkName) LinkURL
  | List       BBList
  | OrdList    BBList
  | Table      BBTable
  | Code       String
  | Text       String
  | Image      (Maybe ImageHeight) (Maybe ImageWidth) MediaURL
  | Youtube    MediaURL
  | Vimeo      MediaURL
  | Facebook   MediaURL
  | Instagram  MediaURL
  | Streamable MediaURL
  | Imgur      MediaURL
  | HR
  | NL
  | None

instance bbcodeShow :: Show BBCode where
  show (Bold t)      = "Bold("<>show t<>")"
  show (Italic t)    = "Italic("<>show t<>")"
  show (Underline t) = "Underline("<>show t<>")"
  show (Strike t)    = "Strike("<>show t<>")"
  show (Size _ t)    = "Size("<>show t<>")"
  show (Color _ t)   = "Color("<>show t<>")"
  show (Center _)    = "Center"
  show (Quote _ _)   = "Quote"
  show (Link _ _)    = "Link"
  show (List _)      = "List"
  show (OrdList _)   = "OrdList"
  show (Table _)     = "Table"
  show (Code t)      = "Code("<>show t<>")"
  show (Text t)      = "Text("<>t<>")"

  show (Image mh mw url) = "Image"
  show (Youtube url)     = "Youtube("<>url<>")"
  show (Vimeo url)       = "Vimeo("<>url<>")"
  show (Facebook url)    = "Facebook("<>url<>")"
  show (Instagram url)   = "Instagram("<>url<>")"
  show (Streamable url)  = "Streamable("<>url<>")"
  show (Imgur url)       = "Imgur("<>url<>")"

  show HR            = "HR"
  show NL            = "NL"
  show None          = "None"
  show _             = "Unknown"

instance bbcodeEq :: Eq BBCode where
  eq (Bold t1)      (Bold t2)      = t1 == t2
  eq (Italic t1)    (Italic t2)    = t1 == t2
  eq (Underline t1) (Underline t2) = t1 == t2
  eq (Strike t1)    (Strike t2)    = t1 == t2
  eq (Size s1 t1)   (Size s2 t2)   = s1 == s2 && t1 == t2
  eq (Color c1 t1)  (Color c2 t2)  = c1 == c2 && t1 == t2
  eq (Center t1)    (Center t2)    = t1 == t2
  eq (Quote a1 t1)  (Quote a2 t2)  = a1 == a2 && t1 == t2
  eq (Link n1 t1)   (Link n2 t2)   = n1 == n2 && t1 == t2
  eq (List t1)      (List t2)      = t1 == t2
  eq (OrdList t1)   (OrdList t2)   = t1 == t2
  eq (Table t1)     (Table t2)     = t1 == t2
  eq (Code t1)      (Code t2)      = t1 == t2
  eq (Text t1)      (Text t2)      = t1 == t2

  eq (Image mh1 mw1 url1 ) (Image mh2 mw2 url2) = mh1 == mh2 && mw1 == mw2 && url1 == url2
  eq (Youtube url1) (Youtube url2)              = url1 == url2
  eq (Vimeo url1) (Vimeo url2)                  = url1 == url2
  eq (Facebook url1) (Facebook url2)            = url1 == url2
  eq (Instagram url1) (Instagram url2)          = url1 == url2
  eq (Streamable url1) (Streamable url2)        = url1 == url2
  eq (Imgur url1) (Imgur url2)                  = url1 == url2

  eq HR             HR             = true
  eq NL             NL             = true
  eq None           None           = true
  eq _              _              = false



data BBSize
  = SizePx Int

instance bbsizeEq :: Eq BBSize where
  eq (SizePx t1) (SizePx t2) = t1 == t2
  eq _           _           = false



data BBList
  = ListItem BBCode

instance bblistEq :: Eq BBList where
  eq (ListItem t1) (ListItem t2) = t1 == t2
  eq _             _             = false



data BBTable
  = TableRow BBCode

instance bbtableEq :: Eq BBTable where
  eq (TableRow t1) (TableRow t2) = t1 == t2
  eq _             _             = false



data ImageSize
  = ImagePx Int
  | ImagePercent Number

instance imageSizeEq :: Eq ImageSize where
  eq (ImagePx t1)      (ImagePx t2)      = t1 == t2
  eq (ImagePercent t1) (ImagePercent t2) = t1 == t2
  eq _                 _                 = false



data BBColor
  = Red
  | White
  | Blue

instance bbcolorEq :: Eq BBColor where
  eq Red   Red   = true
  eq White White = true
  eq Blue  Blue  = true
  eq _     _     = false



type QuoteAuthor = String
type LinkURL     = String
type LinkName    = String
type MediaURL    = String
type ImageHeight = ImageSize
type ImageWidth  = ImageSize
