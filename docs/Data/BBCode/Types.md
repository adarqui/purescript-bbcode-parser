## Module Data.BBCode.Types

#### `ParseState`

``` purescript
type ParseState = { accum :: List BBCode, stack :: List (Tuple (Maybe Parameters) TagName), saccum :: List (Tuple Int BBCode) }
```

#### `defaultParseState`

``` purescript
defaultParseState :: ParseState
```

#### `ParseReader`

``` purescript
type ParseReader = { linkOpts :: LinkOpts, imageOpts :: ImageOpts, trfm :: Map String (BBCode -> BBCode) }
```

#### `defaultParseReader`

``` purescript
defaultParseReader :: ParseReader
```

#### `ParseEff`

``` purescript
type ParseEff = RWS ParseReader Unit ParseState
```

#### `Token`

``` purescript
data Token
  = BBOpen (Maybe Parameters) TagName
  | BBClosed TagName
  | BBStr String
```

##### Instances
``` purescript
Show Token
Eq Token
```

#### `flattenTokens`

``` purescript
flattenTokens :: List Token -> String
```

#### `BBCodeMap`

``` purescript
type BBCodeMap = Map TagName BBCodeFn
```

#### `BBCodeFn`

``` purescript
type BBCodeFn = Maybe Parameters -> List BBCode -> Either ErrorMsg BBCode
```

#### `TagName`

``` purescript
type TagName = String
```

#### `Parameters`

``` purescript
type Parameters = String
```

#### `ErrorMsg`

``` purescript
type ErrorMsg = String
```

#### `BBDoc`

``` purescript
type BBDoc = List BBCode
```

#### `BBCode`

``` purescript
data BBCode
  = Bold (List BBCode)
  | Italic (List BBCode)
  | Underline (List BBCode)
  | Strike (List BBCode)
  | Font FontOpts (List BBCode)
  | Size SizeOpts (List BBCode)
  | Color ColorOpts (List BBCode)
  | Center (List BBCode)
  | AlignLeft (List BBCode)
  | AlignRight (List BBCode)
  | Quote (Maybe QuoteAuthor) (List BBCode)
  | Link (Maybe LinkName) LinkURL
  | List BBList
  | OrdList BBList
  | Table BBTable
  | Pre String
  | Code (Maybe String) String
  | Move (List BBCode)
  | Text String
  | Image ImageOpts MediaURL
  | Youtube MediaURL
  | Vimeo MediaURL
  | Facebook MediaURL
  | Instagram MediaURL
  | Streamable MediaURL
  | Imgur MediaURL
  | HR
  | NL
  | None
```

##### Instances
``` purescript
Show BBCode
Eq BBCode
```

#### `BBSize`

``` purescript
data BBSize
  = SizePx Int
  | SizePt Int
  | SizeEm Int
```

##### Instances
``` purescript
Eq BBSize
```

#### `BBList`

``` purescript
data BBList
  = ListItem BBCode
```

##### Instances
``` purescript
Eq BBList
```

#### `BBTable`

``` purescript
data BBTable
  = TableRow BBCode
```

##### Instances
``` purescript
Eq BBTable
```

#### `ImageSize`

``` purescript
data ImageSize
  = ImagePx Int
  | ImagePercent Number
```

##### Instances
``` purescript
Eq ImageSize
```

#### `BBColor`

``` purescript
data BBColor
  = ColorName String
  | ColorHex String
```

##### Instances
``` purescript
Eq BBColor
```

#### `QuoteAuthor`

``` purescript
type QuoteAuthor = String
```

#### `LinkURL`

``` purescript
type LinkURL = String
```

#### `LinkName`

``` purescript
type LinkName = String
```

#### `MediaURL`

``` purescript
type MediaURL = String
```

#### `ImageHeight`

``` purescript
type ImageHeight = ImageSize
```

#### `ImageWidth`

``` purescript
type ImageWidth = ImageSize
```

#### `LinkOpts`

``` purescript
newtype LinkOpts
  = LinkOpts { linkName :: Maybe String }
```

##### Instances
``` purescript
Eq LinkOpts
```

#### `defaultLinkOpts`

``` purescript
defaultLinkOpts :: LinkOpts
```

#### `ImageOpts`

``` purescript
newtype ImageOpts
  = ImageOpts { imageHeight :: Maybe ImageHeight, imageWidth :: Maybe ImageWidth }
```

##### Instances
``` purescript
Eq ImageOpts
```

#### `defaultImageOpts`

``` purescript
defaultImageOpts :: ImageOpts
```

#### `FontOpts`

``` purescript
newtype FontOpts
  = FontOpts { fontFamily :: Maybe String, fontFaces :: Array String }
```

##### Instances
``` purescript
Eq FontOpts
```

#### `defaultFontOpts`

``` purescript
defaultFontOpts :: FontOpts
```

#### `SizeOpts`

``` purescript
newtype SizeOpts
  = SizeOpts { sizeValue :: Maybe BBSize }
```

##### Instances
``` purescript
Eq SizeOpts
```

#### `defaultSizeOpts`

``` purescript
defaultSizeOpts :: SizeOpts
```

#### `ColorOpts`

``` purescript
newtype ColorOpts
  = ColorOpts { colorValue :: Maybe BBColor }
```

##### Instances
``` purescript
Eq ColorOpts
```

#### `defaultColorOpts`

``` purescript
defaultColorOpts :: ColorOpts
```


