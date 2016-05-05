module Data.BBCode.Types (
  BBDocument,
  BBDoc (..),
  BBText (..),
  BBMedia (..),
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



import Data.List  (List)
import Data.Maybe (Maybe)



type BBDocument = List BBDoc



data BBDoc
  = BBDocText  BBText
  | BBDocMedia BBMedia


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



type QuoteAuthor = String
type LinkURL     = String
type LinkName    = String
type MediaURL    = String
type ImageHeight = ImageSize
type ImageWidth  = ImageSize
