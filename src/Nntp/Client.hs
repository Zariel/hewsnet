{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Nntp.Client
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

import Nntp.Types
import Nntp.Parser
import Nntp.Internal
import Nntp.Commands

import Config

nntpMain :: NzbQueue -> WriterQueue -> ServerConfig -> IO ()
nntpMain inQ outQ conf = bracket (nntpConnect conf) (sClose . nntpSocket) run
	where
		run :: NNTPServer -> IO ()
		run st = runReaderT (nntpRun inQ outQ) st

nntpRun :: NzbQueue -> WriterQueue -> NNTPServerT ()
nntpRun inQ outQ = do
	nntpAuth
	loop inQ outQ
	nntpQuit
	return ()

-- Given if a Nothing comes into the queue then the loop stops. This isnt ideal and may have to be replaced later
loop :: NzbQueue -> WriterQueue -> NNTPServerT ()
loop inQ outQ = do
	work <- atomIO $ readTQueue inQ

	case work of
		Just segment -> fetchArticle segment >>= (writer inQ outQ) >> loop inQ outQ
		Nothing -> return ()

atomIO :: STM a -> NNTPServerT a
atomIO = liftIO . atomically

-- Handles sending downloaded data to the writer queue, if the download failed, or it has not enough blocks. What do we do?
writer :: NzbQueue -> WriterQueue -> NNTPResponse -> NNTPServerT ()
writer inQ outQ res = undefined

fetchArticle :: NzbDownloadThing -> NNTPServerT NNTPResponse
fetchArticle (subject, segment, groups) = do
	nntpGroup' group
	nntpArticle $ segmentToArticle segment
	where
		group = head groups