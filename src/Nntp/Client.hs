{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module NNTP.Client
( fetchArticle
, nntpMain
) where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad.Reader

import Network.Socket

import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Nzb

import NNTP.Types
import NNTP.Parser
import NNTP.Internal
import NNTP.Commands

import Config

nntpMain :: Nzb -> ServerConfig -> IO NNTPResponse
nntpMain nzb conf = bracket (nntpConnect conf) (sClose . nntpSocket) loop
	where
		loop :: NNTPServer -> IO NNTPResponse
		loop st = runReaderT (nntpRun nzb) st

nntpRun :: Nzb -> NNTPServerT NNTPResponse
nntpRun nzb = do
	nntpAuth
	res <- fetchArticle (nzbFileGroups files) (head . nzbFileSegments $ files)
	nntpQuit
	return res
	where files = head $ nzbFiles nzb

fetchArticle :: [NzbGroup] -> NzbSegment -> NNTPServerT NNTPResponse
fetchArticle groups (NzbSegment size number article) = do
	nntpGroup group >>= (liftIO . print)
	nntpArticle (BC.pack article)
	where
		group = (BC.pack . head) groups
