module Data.BBCode.Types (
  Token (..),
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



import Data.List  (List)
import Data.Maybe (Maybe)
import Prelude    (class Show, class Eq, (<>), (==))



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



type BBDocument = List BBDoc



data BBDoc
  = Text    BBText
  | Media   BBMedia
  | Spacing BBSpacing



data BBText
  = Bold      BBText
  | Italic    BBText
  | Underline BBText
  | Strike    BBText
  | Size      BBSize  BBText
  | Color     BBColor BBText
  | Center    BBText
  | Quote     (Maybe QuoteAuthor) BBText
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
