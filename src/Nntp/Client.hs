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

nntpMain :: NzbQueue -> WriterQueue -> ServerConfig -> IO ()
nntpMain inQ outQ conf = bracket (nntpConnect conf) (sClose . nntpSocket) run
	where
		run :: NNTPServer -> IO ()
		run st = runReaderT (nntpRun inQ outQ) st

nntpRun :: NzbQueue -> WriterQueue -> NNTPServerT ()
nntpRun inQ outQ = do
	nntpAuth
	-- Probably need to put a command channel in here too =\
	forever $ loop inQ outQ
	nntpQuit
	return ()

loop :: NzbQueue -> WriterQueue -> NNTPServerT NNTPResponse
loop inQ outQ = do
	segment <- atomIO $ readTQueue inQ
	fetchArticle segment

atomIO :: STM a -> NNTPServerT a
atomIO = liftIO . atomically

fetchArticle :: NzbDownloadThing -> NNTPServerT NNTPResponse
fetchArticle (subject, segment, groups) = do
	nntpGroup' group
	nntpArticle $ segmentToArticle segment
	where
		group = head groups