{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

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
  , string
  , text
  , pstring

    -- * Error types
  , TomlError (..)
  , Sage.ParseError (..)
  , Located (..)

    -- * TOML syntax

    -- ** Parsing
  , parse
  , tomlParser

    -- ** Decoding
  , decode
  , Toml (..)
  , TomlKeyEntry (..)
  , TomlValue (..)
  , TomlItem (..)
  )
where

import Control.Applicative (Alternative, many, some, (<|>))
import Control.Monad (unless)
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT, get, lift, put, runStateT)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import Data.Either (partitionEithers)
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.Functor (void)
import Data.List (deleteBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
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
  deriving (Show, Eq)

parse :: ByteString -> Either TomlError Toml
parse input =
  case Sage.parse (tomlParser <* Sage.eof) input of
    Left err -> Left $ ParseError err
    Right a -> pure a

located :: Sage.Parser a -> Sage.Parser (Located a)
located ma = Located <$> Sage.getOffset <*> ma

token :: Sage.Parser a -> Sage.Parser a
token ma = ma <* Sage.skipSome (Sage.satisfy (`elem` " \t"))

newlines :: Sage.Parser ()
newlines = Sage.skipSome (void (Sage.char '\n') <|> void (Sage.string $ fromString "\r\n"))

sepEndBy :: Alternative f => f a -> f sep -> f [a]
sepEndBy ma sep =
  (:) <$> ma <*> loop
    <|> pure []
  where
    loop =
      sep *> ((:) <$> ma <*> loop <|> pure [])
        <|> pure []

sepBy1 :: Sage.Parser a -> Sage.Parser sep -> Sage.Parser [a]
sepBy1 ma sep =
  (:) <$> ma <*> (sep *> Sage.sepBy ma sep <|> pure [])

tomlParser :: Sage.Parser Toml
tomlParser =
  Toml
    <$> located (sepEndBy keyParser newlines)
    <*> many (located itemParser)

nameParser :: Sage.Parser Text
nameParser = fmap Text.pack (some . Sage.satisfy $ (||) <$> Char.isAlphaNum <*> (`elem` "_-"))

keyParser :: Sage.Parser (Text, TomlKeyEntry)
keyParser =
  (\(Located keyOffset name) -> (,) name . TomlKeyEntry keyOffset)
    <$> located (token nameParser)
    <* token (Sage.char '=')
    <*> located valueParser

quoted :: String
quoted = "\\\""

valueParser :: Sage.Parser TomlValue
valueParser =
  VString . Text.pack
    <$ Sage.char '"'
    <*> many (Sage.satisfy (`notElem` quoted) <|> Sage.char '\\' *> Sage.satisfy (`elem` quoted))
    <* Sage.char '"'

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
  = VString !Text
  | VInt !Int
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

-- | Decode a string literal as 'Text'.
text :: ValueDecoder Text
text =
  ValueDecoder $
    \(Located offset value) ->
      case value of
        VString s -> Right s
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
