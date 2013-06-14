{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module NNTP.Client
( fetchArticle
, nntpMain
) where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad.Reader
import Data.Maybe

import Network.Socket
--import Network.Socket hiding (send, sendTo, recv, recvFrom)
--import Network.Socket.ByteString

import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S
import System.IO.Streams.Attoparsec

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Nzb.NzbFile

import NNTP.Types
import NNTP.Parser

import Config

nntpMain :: Nzb -> ServerConfig -> IO NNTPResponse
nntpMain nzb conf = bracket (nntpConnect conf) nntpShutdown loop
	where
		loop :: NNTPServer -> IO NNTPResponse
		loop st = runReaderT (run nzb) st

nntpShutdown :: NNTPServer -> IO ()
nntpShutdown = sClose . nntpSocket

run :: Nzb -> NNTPServerT NNTPResponse
run nzb = do
	auth >>= (liftIO . print)
	fetchArticle (nzbFileGroups files) (head . nzbFileSegments $ files)
	where files = head $ nzbFiles nzb

nntpConnect :: ServerConfig -> IO NNTPServer
nntpConnect config = do
	addr <- fmap head (getAddrInfo Nothing (Just $ serverHost config) (Just . show $ serverPort config))
	print addr
	sock <- socket (addrFamily addr) Stream (defaultProtocol)
	connect sock (addrAddress addr)
	(is, os) <- S.socketToStreams sock

	return $ NNTPServer is os sock config

auth :: NNTPServerT NNTPResponse
auth = do
	user <- asks (serverUserName . nntpConfig)
	nntpSend $ mkCmd "AUTHINFO USER" (BC.pack user)

	pass <- asks (serverPassword . nntpConfig)
	nntpSend $ mkCmd "AUTHINFO PASS" (BC.pack pass)

nntpSend :: CommandLine -> NNTPServerT NNTPResponse
nntpSend cmd = do
	os <- asks nntpOutput
	is <- asks nntpInput

	liftIO $ S.write (Just cmd) os
	liftIO $ parseFromStream responseParser is

setGroup :: B.ByteString -> NNTPServerT NNTPResponse
setGroup group = nntpSend (mkCmd "GROUP" group)

getArticle :: B.ByteString -> NNTPServerT NNTPResponse
getArticle article = nntpSend (mkCmd "STAT" article)

fetchArticle :: [NzbGroup] -> NzbSegment -> NNTPServerT NNTPResponse
fetchArticle groups (NzbSegment size number article) = do
	setGroup group >>= (liftIO . print)
	getArticle (BC.pack article)
	where
		group = (BC.pack . head) groups

type CommandLine = B.ByteString

-- Command lines have a command space arg crlf form
-- TODO: Make the args an array to handle more than one arg
mkCmd :: B.ByteString -> B.ByteString -> CommandLine
mkCmd cmd arg = B.concat [cmd, space, arg, crlf]
	where
		space = " "
		crlf = "\r\n"
