{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module NNTP.Client
( fetchArticle
, nntpMain
) where

import Prelude hiding (catch)

import Control.Concurrent.STM.TQueue
import Control.Exception

import Control.Monad.Reader
import Control.Monad.STM

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

nntpMain :: NzbQueue -> WriterQueue -> ServerConfig -> IO NNTPResponse
nntpMain inQ outQ conf = bracket (nntpConnect conf) (sClose . nntpSocket) run
	where
		run :: NNTPServer -> IO NNTPResponse
		run st = runReaderT (nntpRun inQ outQ) st

nntpRun :: NzbQueue -> WriterQueue -> NNTPServerT NNTPResponse
nntpRun inQ outQ = do
	nntpAuth
	-- Probably need to put a command channel in here too =\
	loop inQ outQ
	nntpQuit

fetchArticle :: [NzbGroup] -> NzbSegment -> NNTPServerT NNTPResponse
fetchArticle groups (NzbSegment size number article) = do
	nntpGroup group >>= (liftIO . print)
	nntpArticle (BC.pack article)
	where
		group = (BC.pack . head) groups
