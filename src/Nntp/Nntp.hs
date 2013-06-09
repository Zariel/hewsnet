{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module NNTP.Server
( NNTPServer
) where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad.Reader
import Data.Maybe
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Bc

import Nzb.NzbFile

type NNTPServerT = ReaderT NNTPServer IO
data NNTPServer = NNTPServer
	{ nntpInput :: InputStream B.ByteString
	, nntpOutput :: OutputStream B.ByteString
	, nntpSocket :: Socket
	}

data UsenetServerC = UsenetSSLConf
	{ host :: String
	, port :: Int
	}

nntpMain :: UsenetServerC -> IO ()
nntpMain conf = bracket (nntpConnect conf) (disconnect) (loop)
	where
		disconnect = sClose . nntpSocket
		loop :: NNTPServer -> IO ()
		loop st = runReaderT run st

run :: NNTPServerT ()
run = undefined

nntpConnect :: UsenetServerC -> IO NNTPServer
nntpConnect (UsenetSSLConf host port) = do
	addr <- fmap head (getAddrInfo Nothing (Just host) Nothing)
	sock <- socket (addrFamily addr) Stream (defaultProtocol)
	connect sock (addrAddress addr)
	(is, os) <- S.socketToStreams sock

	return $ NNTPServer is os sock


--connect (UsenetConf host port) = do

nntpSend :: B.ByteString -> NNTPServerT (Maybe B.ByteString)
nntpSend cmd = do
	os <- asks nntpOutput
	is <- asks nntpInput

	liftIO $ S.write (Just cmd) os
	liftIO $ S.read is

setGroup :: B.ByteString -> NNTPServerT (Maybe B.ByteString)
setGroup group = nntpSend (B.append "GROUP " group)

getArticle :: B.ByteString -> NNTPServerT (Maybe B.ByteString)
getArticle article = nntpSend (B.append "ARTICLE " article)

fetchArticle :: [NzbGroup] -> NzbSegment -> NNTPServerT (Maybe B.ByteString)
fetchArticle groups (NzbSegment size number article) = getArticle (Bc.pack article)
	where
		group = (Bc.pack . head) groups

