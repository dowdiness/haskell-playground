{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
    ( alternatives
    , parseUri
    , mySequence
    , pScheme
    , pUri
    , Parser
    , Uri(..)
    , createUri
    , Scheme(..)
    , Authority(..)
    , createAutority
    , Path(..)
    , createPath
    , Query(..)
    , createQuery
    ) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

mySequence :: Parser (Char, Char, Char)
mySequence = do (,,)
    <$> char 'a'
    <*> char 'b'
    <*> char 'c'

-- scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]

data Scheme
    = SchemeData
    | SchemeFile
    | SchemeFtp
    | SchemeHttp
    | SchemeHttps
    | SchemeIrc
    | SchemeMailto
    deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
    [ SchemeData   <$ string "data"
    , SchemeFile   <$ string "file"
    , SchemeFtp    <$ string "ftp"
    , SchemeHttps  <$ string "https"
    , SchemeHttp   <$ string "http"
    , SchemeIrc    <$ string "irc"
    , SchemeMailto <$ string "mailto" ]

data Uri = Uri
    { uriScheme :: Scheme
    , uriAuthority :: Maybe Authority
    , uriPath :: Maybe Path
    } deriving (Eq, Show)

createUri :: Scheme -> Maybe Authority -> Maybe Path -> Uri
createUri uriScheme uriAuthority uriPath = Uri {..}

data Authority = Authority
    { authUser :: Maybe (Text, Text) -- (user, password)
    , authHost :: Text
    , authPort :: Maybe Int
    } deriving (Eq, Show)

createAutority :: Maybe (Text, Text) -> Text -> Maybe Int -> Authority
createAutority authUser authHost authPort = Authority {..}

data Path = Path
    { pathName :: Maybe Text
    , pathQuery :: Maybe Query
    , pathHash :: Maybe Text
    } deriving (Eq, Show)

createPath :: Maybe Text -> Maybe Query -> Maybe Text -> Path
createPath pathName pathQuery pathHash = Path {..}

data Query = Query
    { queryKey :: Text
    , queryValue :: Text
    } deriving (Eq, Show)

createQuery :: Text -> Text -> Query
createQuery queryKey queryValue = Query {..}

pUri :: Parser Uri
pUri = do
    uriScheme <- pScheme <?> "valid scheme"
    void (char ':')
    uriAuthority <- optional $ do
        void (string "//")
        authUser <- optional . try $ do
            user <- T.pack <$> some alphaNumChar <?> "username"
            void (char ':')
            password <- T.pack <$> some alphaNumChar  <?> "password"
            void (char '@')
            return (user, password)
        authHost <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
        authPort <- optional (char ':' *> label "port number" L.decimal)
        return (Authority authUser authHost authPort)
    path <- optional $ do
        pathName <- optional (T.pack <$> some (char '/' <|> alphaNumChar) <?> "path")
        pathQuery <- optional . try $ do
            void (char '?')
            queryKey <- T.pack <$> some alphaNumChar <?> "query key"
            void (char '=')
            queryValue <- T.pack <$> some alphaNumChar <?> "query value"
            return (Query queryKey queryValue)
        pathHash <- optional $ do
            void (char '#')
            T.pack <$> some alphaNumChar <?> "fragment"
        return (Path pathName pathQuery pathHash)
    return (Uri uriScheme uriAuthority path)

alternatives :: Parser (Char, Char)
alternatives = try foo <|> bar
    where
        foo = (,) <$> char 'a' <*> char 'b'
        bar = (,) <$> char 'a' <*> char 'c'

parseUri :: Text -> Either (ParseErrorBundle Text Void) Uri
parseUri = runParser (pUri <* eof) ""
