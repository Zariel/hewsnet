module NNTP.Types
( NNTPResponse(..)
, NNTPServerT
, NNTPServer(..)
, CommandLine
) where

import qualified Data.ByteString as B

import System.IO.Streams (InputStream, OutputStream)
import Control.Monad.Reader
import Network.Socket (Socket)

import Config

type NNTPServerT = ReaderT NNTPServer IO
data NNTPServer = NNTPServer
	{ nntpInput :: InputStream B.ByteString
	, nntpOutput :: OutputStream B.ByteString
	, nntpSocket :: Socket
	, nntpConfig :: ServerConfig
	}

type CommandLine = B.ByteString

data NNTPResponse = NNTPSuccess (Int, B.ByteString)
				  | NNTPClientError (Int, B.ByteString)
				  | NNTPServerError (Int, B.ByteString)
				  deriving (Show)
