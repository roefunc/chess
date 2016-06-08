module HttpTypes (
    Header(..),
    Sender,
    Config(..),
    Handler(..),
    HttpCommand(..),
    packStr
) where

import Data.Int
import System.Posix.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

data HttpCommand = Empty | Get { url::String } | Head { url::String} | Post { url::String, content::String } deriving (Show,Eq)

type Sender = Header->B.ByteString->IO Int64

data Handler = Handler { sendData::Sender->Config->HttpCommand->String->IO Int64}

data Config = Config { rootDir::String, notFoundPage::String, urlHandler::(String->Maybe (String,Handler)) }

data Header = OK String String FileOffset | NotFound String deriving (Show)

packStr::String->B.ByteString
packStr = C.pack
