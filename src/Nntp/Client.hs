{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module NNTP.Internal
( fetchArticle
, nntpMain
) where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad.Reader
import Data.Maybe

import Network.Socket

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
nntpMain nzb conf = bracket (nntpConnect conf) (sClose . nntpSocket) loop
	where
		loop :: NNTPServer -> IO NNTPResponse
		loop st = runReaderT (run nzb) st

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

fetchArticle :: [NzbGroup] -> NzbSegment -> NNTPServerT NNTPResponse
fetchArticle groups (NzbSegment size number article) = do
	nntpGroup group >>= (liftIO . print)
	nntpArticle (BC.pack article)
	where
		group = (BC.pack . head) groups
