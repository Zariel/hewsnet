module NNTP.Types where

import qualified Data.ByteString as B

import System.IO.Streams (InputStream, OutputStream)
import Control.Monad.Reader
import Network.Socket (Socket)

import Control.Concurrent.STM.TQueue

import Nzb

import Config

-- A queue which holds Nzbs
type NzbQueue = TQueue NzbDownloadThing

type WriterQueue = TQueue (NNTPResponse, String)

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

type NzbDownloadThing = (String, NzbSegment, [ NzbGroup ])

nzbToDownload :: Nzb -> [ NzbDownloadThing ]
nzbToDownload nzb = concat $ map nzbMapper (nzbFiles nzb)
	where
		nzbMapper :: NzbFile -> [ NzbDownloadThing ]
		nzbMapper (NzbFile poster date subject segments groups) = map (segMapper subject groups) segments

		segMapper :: String -> [NzbGroup] -> NzbSegment -> NzbDownloadThing
		segMapper subject groups segment = (subject, segment, groups)