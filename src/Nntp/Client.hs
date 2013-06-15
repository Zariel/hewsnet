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

nntpMain :: NzbQueue -> ServerConfig -> IO NNTPResponse
nntpMain queue conf = bracket (nntpConnect conf) (sClose . nntpSocket) loop
	where
		loop :: NNTPServer -> IO NNTPResponse
		loop st = runReaderT (nntpRun queue) st

nntpRun :: NzbQueue -> NNTPServerT NNTPResponse
nntpRun queue = do
	nntpAuth
	nntpQuit

fetchArticle :: [NzbGroup] -> NzbSegment -> NNTPServerT NNTPResponse
fetchArticle groups (NzbSegment size number article) = do
	nntpGroup group >>= (liftIO . print)
	nntpArticle (BC.pack article)
	where
		group = (BC.pack . head) groups
