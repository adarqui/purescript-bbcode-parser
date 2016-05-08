module Data.BBCode.Types (
  Token (..),
  flattenTokens,
  BBDocument,
  BBDoc (..),
  BBText (..),
  BBMedia (..),
  BBSize (..),
  BBList (..),
  BBTable (..),
  BBColor (..),
  BBSpacing (..),
  ImageSize (..),
  QuoteAuthor,
  LinkURL,
  LinkName,
  MediaURL,
  ImageHeight,
  ImageWidth
) where



import Data.Foldable (foldl)
import Data.List     (List)
import Data.Maybe    (Maybe)
import Data.String   (toLower)
import Elm.List      (intersperse)
import Prelude       (class Show, show, class Eq, map, (<>), (==), (<<<), (&&))



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



type BBDocument = List BBDoc



data BBDoc
  = DocText    BBText
  | DocMedia   BBMedia
  | DocSpacing BBSpacing
  | DocNone

instance bbdocShow :: Show BBDoc where
  show (DocText t)    = "DocText("<>show t<>")"
  show (DocMedia _)   = "DocMedia"
  show (DocSpacing _) = "DocSpacing"
  show DocNone        = "DocNone"

instance bbdocEq :: Eq BBDoc where
  eq (DocText t1)    (DocText t2)    = t1 == t2
  eq (DocMedia t1)   (DocMedia t2)   = t1 == t2
  eq (DocSpacing t1) (DocSpacing t2) = t1 == t2
  eq DocNone         DocNone         = true
  eq _                _              = false



data BBText
  = Bold      (List BBText)
  | Italic    (List BBText)
  | Underline (List BBText)
  | Strike    (List BBText)
  | Size      BBSize  (List BBText)
  | Color     BBColor (List BBText)
  | Center    (List BBText)
  | Quote     (Maybe QuoteAuthor) (List BBText)
  | Link      (Maybe LinkName) LinkURL
  | List      BBList
  | OrdList   BBList
  | Table     BBTable
  | Code      String
  | Text      String

instance bbtextShow :: Show BBText where
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

instance bbtextEq :: Eq BBText where
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
  eq _              _              = false



data BBMedia
  = Image      (Maybe ImageHeight) (Maybe ImageWidth) MediaURL
  | Youtube    MediaURL
  | Vimedo     MediaURL
  | Facebook   MediaURL
  | Instagram  MediaURL
  | Streamable MediaURL
  | Imgur      MediaURL

instance bbmediaShow :: Show BBMedia where
  show _ = "BBMedia"

instance bbmediaEq :: Eq BBMedia where
  eq (Image ih1 iw1 u1) (Image ih2 iw2 u2) = ih1 == ih2 && iw1 == iw2 && u1 == u2
  eq _                  _                  = false



data BBSize
  = SizePx Int

instance bbsizeEq :: Eq BBSize where
  eq (SizePx t1) (SizePx t2) = t1 == t2
  eq _           _           = false



data BBList
  = ListItem BBText

instance bblistEq :: Eq BBList where
  eq (ListItem t1) (ListItem t2) = t1 == t2
  eq _             _             = false



data BBTable
  = TableRow BBText

instance bbtableEq :: Eq BBTable where
  eq (TableRow t1) (TableRow t2) = t1 == t2
  eq _             _             = false



data ImageSize
  = ImagePx Int
  | ImagePercent Number

instance imageSizeEq :: Eq ImageSize where
  eq (ImagePx t1) (ImagePx t2)           = t1 == t2
  eq (ImagePercent t1) (ImagePercent t2) = t1 == t2
  eq _             _                     = false



data BBColor
  = Red
  | White
  | Blue

instance bbcolorEq :: Eq BBColor where
  eq Red   Red   = true
  eq White White = true
  eq Blue  Blue  = true
  eq _     _     = false



data BBSpacing
  = NL
  | HR

instance bbspacingShow :: Show BBSpacing where
  show _ = "BBSpacing"

instance bbspacingEq :: Eq BBSpacing where
  eq NL NL = true
  eq HR HR = true
  eq _  _  = false



type QuoteAuthor = String
type LinkURL     = String
type LinkName    = String
type MediaURL    = String
type ImageHeight = ImageSize
type ImageWidth  = ImageSize
