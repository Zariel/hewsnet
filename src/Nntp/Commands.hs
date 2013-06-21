{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Nntp.Commands
( nntpAuth
, nntpQuit
, nntpGroup
, nntpArticle
, nntpGroup'
) where

import Nntp.Internal
import Nntp.Types

import Config

import Nzb

import Control.Monad.Reader

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

nntpAuth :: NNTPServerT NNTPResponse
nntpAuth = do
	user <- asks (serverUserName . nntpConfig)
	pass <- asks (serverPassword . nntpConfig)

	(nntpSend $ mkCmd "AUTHINFO USER" (BC.pack user)) >|> mkCmd "AUTHINFO PASS" (BC.pack pass)

nntpQuit :: NNTPServerT NNTPResponse
nntpQuit = nntpSend $ mkCmd0 "QUIT"

nntpGroup :: B.ByteString -> NNTPServerT NNTPResponse
nntpGroup group = nntpSend (mkCmd "GROUP" group)

nntpGroup' :: String -> NNTPServerT NNTPResponse
nntpGroup' = nntpGroup . BC.pack

nntpArticle :: NzbArticle -> NNTPServerT NNTPResponse
nntpArticle article = nntpSend (mkCmd "STAT" article)

-- Command lines have a command space arg crlf form
-- TODO: Make the args an array to handle more than one arg
mkCmd :: B.ByteString -> B.ByteString -> CommandLine
mkCmd cmd arg = B.concat [cmd, space, arg, crlf]
	where
		space = " "
		crlf = "\r\n"

mkCmd0 :: B.ByteString -> CommandLine
mkCmd0 cmd = B.concat [cmd, crlf]
	where
		crlf = "\r\n"
