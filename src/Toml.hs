{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Toml
  ( -- * Reading files
    load

    -- * Decoders
  , Decoder
  , key
  , optionalKey
  , keys
  , table
  , tableArray

    -- ** Value decoders
  , ValueDecoder
  , alt
  , bool
  , string
  , text
  , pstring
  , datetime
  , utcTime
  , list
  , record
  , RecordDecoder
  , recordKey
  , value

    -- * Error types
  , TomlError (..)
  , Sage.ParseError (..)
  , Located (..)

    -- * TOML syntax

    -- ** Parsing
  , parse
  , tomlParser
  , keyParser
  , ValueContext (..)
  , valueParser

    -- ** Decoding
  , decode
  , Toml (..)
  , TomlKeyEntry (..)
  , TomlValue (..)
  , Datetime (..)
  , TomlItem (..)

    -- ** Printing
  , keyPrinter
  , valuePrinter
  , datetimePrinter
  )
where

import Control.Applicative (Alternative (..), many, optional, some)
import Control.Monad (unless)
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader.Class (ask)
import Control.Monad.State (State, StateT, get, lift, put, runState, runStateT)

{- [Note: `runWriterT` from `transformers`]

This really should come from `mtl`, but `runWriterT` wasn't re-exported until `mtl-2.3.2`.
At the time of writing (2026-09-20) `mtl-2.3.2` is not in nixpkgs, so we
import from `transformers` here for convenience.
-}
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Writer.Class (tell)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import Data.Either (partitionEithers)
import Data.Fixed (Pico)
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.Functor (void)
import Data.List (deleteBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Any (..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate, isLeapYear)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeOfDay (..), timeOfDayToTime)
import Numeric (readDec)
import qualified Text.Sage as Sage

load :: FilePath -> Decoder a -> IO (Either TomlError a)
load path decoder = do
  contents <- ByteString.readFile path
  case parse contents of
    Left err -> pure $ Left err
    Right toml ->
      case decode toml decoder of
        Left err -> pure $ Left err
        Right a -> pure $ Right a

data TomlError
  = ParseError
      !Sage.ParseError
  | -- | Decoder failed due to 'empty'
    DecodeFail
      -- | Offset of contex in which the failure occurred.
      !Int
  | -- | A required key was not found.
    MissingKey
      -- | Offset
      !Int
      -- | Key name
      !Text
  | -- | A key was repeated.
    DuplicateKey
      -- | Offset
      !Int
      -- | Key name
      !Text
  | -- | A required table was not found.
    MissingTable
      -- | Offset
      !Int
      -- | Table name
      !Text
  | -- | A table was repeated.
    DuplicateTables
      -- | Offsets
      ![Int]
      -- | Table name
      !Text
  | -- | There were leftover items in the TOML input.
    UnexpectedEntries
      -- | Key-value pairs
      ![TomlKeyEntry]
      -- | Offsets of non-key-value items (tables, arrays of tables)
      ![Int]
  | -- | A value was something other than a string.
    ExpectedString
      -- | Offset
      !Int
  | StringParseError
      -- | Offset of string
      !Int
      -- | String's value
      !ByteString
      !Sage.ParseError
  | -- | A value was something other than a record.
    ExpectedRecord
      -- | Offset
      !Int
  | -- A record contains unexpected fields.
    UnexpectedFields
      -- | Offsets of field names
      ![Int]
  | -- A record was missing a required field.
    MissingField
      -- | Offset of record
      !Int
      -- | Missing field name
      !Text
  | -- | A value was something other than a datetime.
    ExpectedDatetime
      -- | Offset
      !Int
  deriving (Show, Eq)

parse :: ByteString -> Either TomlError Toml
parse input =
  case Sage.parse (tomlParser <* Sage.eof) input of
    Left err -> Left $ ParseError err
    Right a -> pure a

located :: Sage.Parser a -> Sage.Parser (Located a)
located ma = Located <$> Sage.getOffset <*> ma

token :: Sage.Parser a -> Sage.Parser a
token ma = ma <* Sage.skipMany (Sage.satisfy (`elem` " \t"))

newlines :: Sage.Parser ()
newlines = Sage.skipSome (void (Sage.char '\n') <|> void (Sage.string $ fromString "\r\n"))

sepEndBy :: Alternative f => f a -> f sep -> f [a]
sepEndBy ma sep =
  (:)
    <$> ma
    <*> loop
      <|> pure []
  where
    loop =
      sep
        *> ((:) <$> ma <*> loop <|> pure [])
          <|> pure []

sepBy1 :: Sage.Parser a -> Sage.Parser sep -> Sage.Parser [a]
sepBy1 ma sep =
  (:) <$> ma <*> (sep *> Sage.sepBy ma sep <|> pure [])

-- | A TOML document
tomlParser :: Sage.Parser Toml
tomlParser =
  Toml
    <$> located (sepEndBy keyParser newlines)
    <*> many (located itemParser)

nameParser :: Sage.Parser Text
nameParser = fmap Text.pack (some . Sage.satisfy $ (||) <$> Char.isAlphaNum <*> (`elem` "_-"))

-- | A key-value entry
keyParser :: Sage.Parser (Text, TomlKeyEntry)
keyParser =
  (\(Located keyOffset name) -> (,) name . TomlKeyEntry keyOffset)
    <$> located (token nameParser)
    <* token (Sage.char '=')
    <*> located (valueParser TopLevel)

data ValueContext
  = -- | A value that comes after a key's @=@ sign.
    TopLevel
  | -- | A value that is contained by another (e.g. array items)
    Nested

valueParser ::
  {-| * 'TopLevel': only the space character is considered whitespace
  * 'Nested': every space-like ('Char.isSpace') character is considered whitespace
  -}
  ValueContext ->
  Sage.Parser TomlValue
valueParser ctx =
  valueToken $
    boolParser
      <|> stringParser
      <|> datetimeParser
      <|> multilineStringParser
      <|> arrayParser
      <|> recordParser
  where
    nestedToken p = p <* Sage.skipMany (Sage.satisfy Char.isSpace)
    valueToken =
      case ctx of
        TopLevel -> token
        Nested -> nestedToken

    boolParser =
      (VTrue <$ Sage.string (fromString "true"))
        <|> (VFalse <$ Sage.string (fromString "false"))

    quoted :: String
    quoted = "\\\""

    stringParser =
      VString . Text.pack
        <$ Sage.label
          (Sage.Char '"')
          (Sage.try $ Sage.char '"' <* Sage.notFollowedBy (Sage.string $ fromString "\"\""))
        <*> many (Sage.satisfy (`notElem` quoted) <|> Sage.char '\\' *> Sage.satisfy (`elem` quoted))
        <* Sage.char '"'

    datetimeParser =
      fmap VDatetime $
        Datetime
          <$> fourDigitParser
          <* Sage.char '-'
          <*> twoDigitParser
          <* Sage.char '-'
          <*> twoDigitParser
          <* Sage.char 'T'
          <*> twoDigitParser
          <* Sage.char ':'
          <*> twoDigitParser
          <* Sage.char ':'
          <*> twoDigitParser
          <* Sage.char 'Z'
      where
        fourDigitParser :: Integral n => Sage.Parser n
        fourDigitParser =
          ( \a b c d -> case readDec [a, b, c, d] of
              [(n, "")] -> n
              _ -> undefined
          )
            <$> Sage.satisfy Char.isDigit
            <*> Sage.satisfy Char.isDigit
            <*> Sage.satisfy Char.isDigit
            <*> Sage.satisfy Char.isDigit

        twoDigitParser :: Integral n => Sage.Parser n
        twoDigitParser =
          ( \a b -> case readDec [a, b] of
              [(n, "")] -> n
              _ -> undefined
          )
            <$> Sage.satisfy Char.isDigit
            <*> Sage.satisfy Char.isDigit

    multilineStringParser =
      VString . Text.pack
        <$ Sage.string (fromString "\"\"\"")
        <* optional (Sage.char '\n')
        <*> many
          ( Sage.satisfy (`notElem` quoted)
              <|> Sage.try (Sage.char '"' <* Sage.notFollowedBy (Sage.string $ fromString "\"\""))
              <|> (Sage.char '\\' *> Sage.satisfy (`elem` quoted))
          )
        <* Sage.string (fromString "\"\"\"")

    arrayParser =
      VArray
        <$ nestedToken (Sage.char '[')
        <*> Sage.sepBy (located $ valueParser Nested) (nestedToken $ Sage.char ',')
        <* Sage.char ']'

    recordParser =
      VRecord
        <$ nestedToken (Sage.char '{')
        <*> Sage.sepBy
          ( (,)
              <$> located (nestedToken nameParser)
              <* nestedToken (Sage.char '=')
              <*> located (valueParser Nested)
          )
          (nestedToken $ Sage.char ',')
        <* Sage.char '}'

itemParser :: Sage.Parser TomlItem
itemParser =
  tableArrayParser
    <|> tableParser

tableArrayParser :: Sage.Parser TomlItem
tableArrayParser =
  TomlTableArray
    <$ Sage.string (fromString "[[")
    <*> sepBy1 nameParser (Sage.char '.')
    <* Sage.string (fromString "]]")
    <* newlines
    <*> sepEndBy keyParser newlines

tableParser :: Sage.Parser TomlItem
tableParser =
  TomlTable
    <$ Sage.char '['
    <*> sepBy1 nameParser (Sage.char '.')
    <* Sage.char ']'
    <* newlines
    <*> sepEndBy keyParser newlines

data Located a = Located {locatedOffset :: !Int, locatedValue :: !a}
  deriving (Show, Eq)

data Toml
  = Toml
      -- | Top-level key-value pairs
      !(Located [(Text, TomlKeyEntry)])
      -- | Non-key-value items (tables, arrays of tables)
      [Located TomlItem]
  deriving (Show)

data TomlKeyEntry
  = TomlKeyEntry
      -- | Offset of key name
      !Int
      -- | Value
      !(Located TomlValue)
  deriving (Show, Eq)

data TomlItem
  = TomlTable
      -- | Dot-separated header parts
      ![Text]
      -- | Entries
      ![(Text, TomlKeyEntry)]
  | TomlTableArray
      -- | Dot-separated header parts
      ![Text]
      -- | Entries
      ![(Text, TomlKeyEntry)]
  deriving (Show, Eq)

data TomlValue
  = VTrue
  | VFalse
  | VString !Text
  | VInt !Int
  | VArray ![Located TomlValue]
  | VRecord ![(Located Text, Located TomlValue)]
  | VDatetime !Datetime
  deriving (Show, Eq)

data Datetime
  = Datetime
  { dtYear :: !Integer
  , dtMonth :: !Int
  , dtDay :: !Int
  , dtHour :: !Int
  , dtMinute :: !Int
  , dtSecond :: !Int
  }
  deriving (Show, Eq)

newtype Decoder a = Decoder (StateT Toml (Either TomlError) a)
  deriving (Functor, Applicative)

decode :: Toml -> Decoder a -> Either TomlError a
decode toml (Decoder decoder) = do
  (a, Toml (Located _offset keys) entries) <- runStateT decoder toml
  unless (null keys && null entries) . throwError $
    UnexpectedEntries (fmap snd keys) (fmap locatedOffset entries)
  pure a

-- | @key = value@
key :: Text -> ValueDecoder a -> Decoder a
key name valueDecoder = Decoder $ do
  Toml (Located offset keys) entries <- get
  case lookup name keys of
    Just (TomlKeyEntry _keyOffset value') -> do
      a <- lift $ valueDecode value' valueDecoder
      let keys' = deleteBy ((==) `on` fst) (name, undefined) keys
      put $ Toml (Located offset keys') entries
      pure a
    Nothing ->
      throwError $ MissingKey offset name

-- | @key = value@
optionalKey :: Text -> ValueDecoder a -> Decoder (Maybe a)
optionalKey name valueDecoder = Decoder $ do
  Toml (Located offset keys) entries <- get
  case lookup name keys of
    Just (TomlKeyEntry _keyOffset value') -> do
      a <- lift $ valueDecode value' valueDecoder
      let keys' = deleteBy ((==) `on` fst) (name, undefined) keys
      put $ Toml (Located offset keys') entries
      pure $ Just a
    Nothing ->
      pure Nothing

-- | Decode all remaining keys.
keys :: ValueDecoder a -> Decoder (Map Text a)
keys decoder = Decoder $ do
  Toml keys entries <- get

  keys' <-
    foldlM
      ( \acc (key, TomlKeyEntry offset value) ->
          if Map.member key acc
            then
              throwError $ DuplicateKey offset key
            else do
              value' <- liftEither $ valueDecode value decoder
              pure $ Map.insert key value' acc
      )
      mempty
      (locatedValue keys)

  put $ Toml keys{locatedValue = mempty} entries

  pure keys'

{-|
@
[header]
key_0 = value_0
key_1 = value_1
key_2 = value_2
@
-}
table :: Text -> Decoder a -> Decoder a
table name (Decoder decoder) = Decoder $ do
  Toml (Located offset keys) entries <- get

  let
    matchingOrNonMatching item =
      case locatedValue item of
        TomlTable (part : parts) entries' | part == name -> Left (locatedOffset item, parts, entries')
        _ -> Right item
  let (matching, nonMatching) = partitionEithers $ fmap matchingOrNonMatching entries

  case matching of
    [] -> throwError $ MissingTable offset name
    _item : rest@(_ : _) -> throwError $ DuplicateTables (fmap (\(offset', _parts, _entries) -> offset') rest) name
    [(offset', [], entries')] -> do
      (a, Toml (Located _offset keys'') entries'') <-
        lift $ runStateT decoder (Toml (Located offset' entries') [])
      unless (null keys'' && null entries'') . throwError $
        UnexpectedEntries (fmap snd keys'') (fmap locatedOffset entries'')
      put $ Toml (Located offset keys) nonMatching
      pure a
    [(_offset', _ : _, _entries')] ->
      error "TODO: nested tables"

{-|
@
[[header]]
key_0 = value_0
key_1 = value_1
key_2 = value_2

[[header]]
key_0 = value_3
key_1 = value_4
key_2 = value_5

...
@
-}
tableArray :: Text -> Decoder a -> Decoder [a]
tableArray name (Decoder decoder) = Decoder $ do
  Toml keys entries <- get

  let
    matchingOrNonMatching =
      foldr
        ( \(Located offset entry) rest ->
            case entry of
              TomlTableArray (part : parts) entries'
                | part == name ->
                    Left (Located offset $ TomlTableArray parts entries') : rest
              _ ->
                Right (Located offset entry) : rest
        )
        []
        entries

    (matching, nonMatching) = partitionEithers matchingOrNonMatching

  let
    loop [] =
      pure []
    loop (Located offset (TomlTableArray [] entries') : rest) = do
      let (prefix, suffix) = break (\case (Located _offset (TomlTableArray [] _)) -> True; _ -> False) rest
      case runStateT decoder $ Toml (Located offset entries') prefix of
        Left err -> Left err
        Right (a, Toml (Located _offset keys'') entries'') -> do
          unless (null keys'' && null entries'') . throwError $
            UnexpectedEntries (fmap snd keys'') (fmap locatedOffset entries'')
          (a :) <$> loop suffix
    loop entry@(Located _ (TomlTableArray (_ : _) _) : _) = do
      error $ "impossible: " ++ show entry
    loop entry@(Located _ (TomlTable _ _) : _) =
      error $ "impossible: " ++ show entry

  as <- lift $ loop matching

  put $ Toml keys nonMatching
  pure as

newtype ValueDecoder a = ValueDecoder (Located TomlValue -> Either TomlError a)
  deriving (Functor) via (ReaderT (Located TomlValue) (Either TomlError))

valueDecode :: Located TomlValue -> ValueDecoder a -> Either TomlError a
valueDecode value (ValueDecoder decoder) = decoder value

{-| Fall through to the second decoder if the first fails.

Note: 'ValueDecoder' has no 'Alternative' instance because it can't have an 'Applicative' instance.
-}
alt ::
  -- | First decoder
  ValueDecoder a ->
  -- | Fallback
  ValueDecoder a ->
  ValueDecoder a
alt (ValueDecoder ma) (ValueDecoder mb) =
  ValueDecoder $ \value ->
    case ma value of
      Left{} -> mb value
      Right a -> pure a

infixl 3 `alt`

-- | Decode a string literal as 'Text'.
text :: ValueDecoder Text
text =
  ValueDecoder $
    \(Located offset value) ->
      case value of
        VString s -> Right s
        _ -> Left $ ExpectedString offset

-- | Decode a boolean as a 'Bool'.
bool :: ValueDecoder Bool
bool =
  ValueDecoder $
    \(Located offset value) ->
      case value of
        VTrue -> Right True
        VFalse -> Right False
        _ -> Left $ ExpectedString offset

-- | Decode a string literal as a 'String'.
string :: ValueDecoder String
string = Text.unpack <$> text

-- | Parse the value of a string literal.
pstring :: Sage.Parser a -> ValueDecoder a
pstring p =
  ValueDecoder $
    \(Located offset value) ->
      case value of
        VString s -> do
          let input = Text.Encoding.encodeUtf8 s
          first (StringParseError offset input) $ Sage.parse (p <* Sage.eof) input
        _ -> Left $ ExpectedString offset

datetime :: ValueDecoder Datetime
datetime =
  ValueDecoder $
    \(Located offset value) ->
      case value of
        VDatetime dt -> Right dt
        _ -> Left $ ExpectedDatetime offset

utcTime :: ValueDecoder UTCTime
utcTime =
  fmap (\(Datetime y m d hour minute second) -> mkUTCTime y m d hour minute second) datetime
  where
    mkUTCTime y m d hour minute second =
      UTCTime
        (fromOrdinalDate y $ monthAndDayToDayOfYear (isLeapYear y) m d)
        (timeOfDayToTime $ TimeOfDay hour minute (fromIntegral second))

-- | Decode an array.
list :: ValueDecoder a -> ValueDecoder [a]
list decoder =
  ValueDecoder $
    \(Located offset value) ->
      case value of
        VArray xs -> traverse (`valueDecode` decoder) xs
        _ -> Left $ ExpectedString offset

-- | Decode an arbitrary 'TomlValue'.
value :: ValueDecoder TomlValue
value = ValueDecoder $ \(Located _offset value) -> Right value

newtype RecordDecoder a
  = RecordDecoder
      ( ExceptT
          TomlError
          (WriterT Any (ReaderT Int (State (Map Text (Located Text, Located TomlValue)))))
          a
      )
  deriving (Functor, Applicative)

instance Alternative RecordDecoder where
  empty = RecordDecoder $ throwError . DecodeFail =<< ask
  RecordDecoder ma <|> RecordDecoder mb =
    RecordDecoder $ do
      (result, Any consumed) <- lift . lift . runWriterT . runExceptT $ ma
      case result of
        Left err ->
          if consumed
            then throwError err
            else mb
        Right a -> pure a

-- | Decode an inline table.
record :: RecordDecoder a -> ValueDecoder a
record (RecordDecoder decoder) =
  ValueDecoder $ \(Located offset value) ->
    case value of
      VRecord fields -> do
        let
          ((result, _consumed), state) =
            flip runState (Map.fromList [(locatedValue k, (k, v)) | (k, v) <- fields])
              . flip runReaderT offset
              . runWriterT
              . runExceptT
              $ decoder
        a <- liftEither result
        unless (Map.null state) . throwError $
          UnexpectedFields [locatedOffset k | (k, _v) <- Map.elems state]
        pure a
      _ -> Left $ ExpectedRecord offset

recordKey :: Text -> ValueDecoder a -> RecordDecoder a
recordKey key decoder =
  RecordDecoder $ do
    fields <- get
    case Map.lookup key fields of
      Nothing -> do
        offset <- ask
        throwError $ MissingField offset key
      Just (_key, val) -> do
        a <- liftEither $ valueDecode val decoder
        put $ Map.delete key fields
        tell $ Any True
        pure a

keyPrinter :: Text -> TomlValue -> Builder
keyPrinter key value =
  Builder.fromText key
    <> fromString " = "
    <> valuePrinter value

valuePrinter :: TomlValue -> Builder
valuePrinter value =
  case value of
    VTrue -> fromString "true"
    VFalse -> fromString "false"
    VString s -> fromString "\"" <> foldMap escapeChar (Text.unpack s) <> fromString "\""
    VInt n -> fromString (show n)
    VDatetime dt -> datetimePrinter dt
    VArray items ->
      fromString "["
        <> sepBy (fromString ", ") (fmap (valuePrinter . locatedValue) items)
        <> fromString "]"
    VRecord fields -> fromString "{" <> sepBy (fromString ", ") (fmap (uncurry fieldPrinter) fields) <> fromString "}"
  where
    sepBy :: Monoid m => m -> [m] -> m
    sepBy _sep [] = mempty
    sepBy _sep [x] = x
    sepBy sep (x : xs@(_ : _)) = x <> sep <> sepBy sep xs

    fieldPrinter :: Located Text -> Located TomlValue -> Builder
    fieldPrinter (Located _offset key) (Located _offset' value) = keyPrinter key value

    escapeChar :: Char -> Builder
    escapeChar '"' = fromString "\\\""
    escapeChar c = Builder.fromText $ Text.singleton c

datetimePrinter :: Datetime -> Builder
datetimePrinter (Datetime y m d hour minute second) =
  fromString (padZero 4 $ show y)
    <> fromString "-"
    <> fromString (padZero 2 $ show m)
    <> fromString "-"
    <> fromString (padZero 2 $ show d)
    <> fromString "T"
    <> fromString (padZero 2 $ show hour)
    <> fromString ":"
    <> fromString (padZero 2 $ show minute)
    <> fromString ":"
    <> fromString (padZero 2 $ show second)
    <> fromString "Z"
  where
    padZero n str = replicate (max 0 $ n - length str) '0' ++ str
