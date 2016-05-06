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
import Elm.List      (intersperse)
import Prelude       (class Show, show, class Eq, map, (<>), (==), (<<<))



data Token
  = BBOpen   String
  | BBClosed String
  | BBStr    String

instance tokenShow :: Show Token where
  show (BBOpen s)   = "open("<>s<>")"
  show (BBClosed s) = "closed("<>s<>")"
  show (BBStr s)    = "str("<>s<>")"

instance tokenEq :: Eq Token where
  eq (BBOpen s1)   (BBOpen s2)   = s1 == s2
  eq (BBClosed s1) (BBClosed s2) = s1 == s2
  eq (BBStr s1)    (BBStr s2)    = s1 == s2
  eq _             _             = false

flattenTokens :: List Token -> String
flattenTokens = foldl (<>) "" <<< intersperse "," <<< map show



type BBDocument = List BBDoc



data BBDoc
  = Text    BBText
  | Media   BBMedia
  | Spacing BBSpacing



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



data BBMedia
  = Image      (Maybe ImageHeight) (Maybe ImageWidth) MediaURL
  | Youtube    MediaURL
  | Vimedo     MediaURL
  | Facebook   MediaURL
  | Instagram  MediaURL
  | Streamable MediaURL
  | Imgur      MediaURL



data BBSize
  = SizePx Int



data BBList
  = ListItem BBText



data BBTable
  = TableRow BBText



data ImageSize
  = ImagePx Int
  | ImagePercent Number



data BBColor
  = Red
  | White
  | Blue



data BBSpacing
  = NL
  | HR



type QuoteAuthor = String
type LinkURL     = String
type LinkName    = String
type MediaURL    = String
type ImageHeight = ImageSize
type ImageWidth  = ImageSize
