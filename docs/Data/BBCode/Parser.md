## Module Data.BBCode.Parser

#### `open`

``` purescript
open :: forall m a. Monad m => ParserT String m Token
```

#### `closed`

``` purescript
closed :: forall m a. Monad m => ParserT String m Token
```

#### `str`

``` purescript
str :: forall m a. Monad m => ParserT String m Token
```

#### `token`

``` purescript
token :: forall m a. Monad m => ParserT String m Token
```

#### `tokens`

``` purescript
tokens :: forall m a. Monad m => ParserT String m (List Token)
```

#### `concatTokens`

``` purescript
concatTokens :: List Token -> List Token
```

concat consecutive BBStr's

#### `concatBBStr`

``` purescript
concatBBStr :: List Token -> Token
```

Once we have a list of BBStr's, turn them into one BBStr

#### `parseTokens`

``` purescript
parseTokens :: forall s. s -> Parser s (List Token) -> Either String (List Token)
```

#### `parseTokens'`

``` purescript
parseTokens' :: String -> Either String (List Token)
```

#### `runBBCode`

``` purescript
runBBCode :: Maybe Parameters -> TagName -> List BBCode -> BBCodeMap -> Either ErrorMsg BBCode
```

#### `parseBBCodeFromTokens`

``` purescript
parseBBCodeFromTokens :: List Token -> ParseEff (Either String BBDoc)
```

#### `parseBBCodeFromTokens'`

``` purescript
parseBBCodeFromTokens' :: BBCodeMap -> BBCodeMap -> BBCodeMap -> List Token -> ParseEff (Either String BBDoc)
```

#### `parseBBCode`

``` purescript
parseBBCode :: String -> Either String (List BBCode)
```

#### `parseBBCodeWith`

``` purescript
parseBBCodeWith :: ParseReader -> String -> Either String (List BBCode)
```


