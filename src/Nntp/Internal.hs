{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Nntp.Internal
( nntpSend
, nntpConnect
) where

import Nntp.Types
import Nntp.Parser

import Nzb

import Config

import Control.Monad.Reader

import Data.Maybe

import Network.Socket

import System.IO
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.Attoparsec
import qualified System.IO.Streams as S

nntpRead = parseFromStream responseParser

-- Sends a formatted NNTP request to the sever and parses the repsonse
nntpSend :: CommandLine -> NNTPServerT NNTPResponse
nntpSend cmd = do
	os <- asks nntpOutput
	is <- asks nntpInput

	logM cmd
	liftIO $ S.write (Just cmd) os
	(liftIO $ nntpRead is) >>= logM

logM :: (Show a) => a -> NNTPServerT a
logM msg = do
	(liftIO . print) msg
	return msg

-- Connect to an NNTP Server with a given config and create the streams
nntpConnect :: ServerConfig -> IO NNTPServer
nntpConnect config = do
	addr <- fmap head (getAddrInfo Nothing (Just $ serverHost config) (Just . show $ serverPort config))
	sock <- socket (addrFamily addr) Stream (defaultProtocol)
	connect sock (addrAddress addr)
	(is, os) <- S.socketToStreams sock

	-- Need to expect the 200 hello from the server
	nntpRead is

	return $ NNTPServer is os sock config

-- Combinator, given a response execute the given command if the first succeeds
cont :: CommandLine -> NNTPResponse -> NNTPServerT NNTPResponse
cont cmd (NNTPSuccess _) = nntpSend cmd
cont _ res = return res

(>@>) :: NNTPServerT NNTPResponse -> CommandLine -> NNTPServerT NNTPResponse
(>@>) res cmd = res >>= (cont cmd)
