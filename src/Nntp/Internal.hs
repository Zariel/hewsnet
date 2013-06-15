{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module NNTP.Internal
( nntpSend
, nntpConnect
) where

import NNTP.Types
import NNTP.Parser

import Nzb

import Config

import Control.Monad.Reader

import Data.Maybe

import Network.Socket

import System.IO
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.Attoparsec
import qualified System.IO.Streams as S

-- Sends a formatted NNTP request to the sever and parses the repsonse
nntpSend :: CommandLine -> NNTPServerT NNTPResponse
nntpSend cmd = do
	os <- asks nntpOutput
	is <- asks nntpInput

	liftIO $ print cmd
	liftIO $ S.write (Just cmd) os
	liftIO $ parseFromStream responseParser is

-- Connect to an NNTP Server with a given config and create the streams
nntpConnect :: ServerConfig -> IO NNTPServer
nntpConnect config = do
	addr <- fmap head (getAddrInfo Nothing (Just $ serverHost config) (Just . show $ serverPort config))
	print addr
	sock <- socket (addrFamily addr) Stream (defaultProtocol)
	connect sock (addrAddress addr)
	(is, os) <- S.socketToStreams sock

	return $ NNTPServer is os sock config

