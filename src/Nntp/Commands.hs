module NNTP.Commands
( nntpAuth
, nntpQuit
, nntpGroup
, nntpArticle
) where

-- Internal function to send an NNTP command
nntpSend :: CommandLine -> NNTPServerT NNTPResponse
nntpSend cmd = do
	os <- asks nntpOutput
	is <- asks nntpInput

	liftIO $ print cmd
	liftIO $ S.write (Just cmd) os
	liftIO $ parseFromStream responseParser is

nntpAuth :: NNTPServerT NNTPResponse
nntpAuth = do
	user <- asks (serverUserName . nntpConfig)
	nntpSend $ mkCmd "AUTHINFO USER" (BC.pack user)

	pass <- asks (serverPassword . nntpConfig)
	nntpSend $ mkCmd "AUTHINFO PASS" (BC.pack pass)

nntpQuit :: NNTPServerT NNTPResponse
nntpQuit = nntpSend $ mkCmd0 "QUIT"

nntpGroup :: B.ByteString -> NNTPServerT NNTPResponse
nntpGroup group = nntpSend (mkCmd "GROUP" group)

nntpArticle :: B.ByteString -> NNTPServerT NNTPResponse
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
