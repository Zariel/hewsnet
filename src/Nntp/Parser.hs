module Nntp.Parser
( responseParser
) where

import Nntp.Types

import Control.Applicative

import qualified Data.ByteString.Char8 as B

import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8

import Data.Text
import Data.Text.Encoding

-- Currently this only parses single line responses
-- TODO: Handle multi-line
responseParser :: P.Parser NNTPResponse
responseParser = do
	code <- P.takeWhile P8.isDigit_w8 <* P8.char8 ' '
	body <- P.takeTill P8.isEndOfLine <* P8.endOfLine

	return $ getResponse (read . unpack $ decodeUtf8 code) body

getResponse :: Int -> B.ByteString -> NNTPResponse
getResponse code body
	| code >= 500 = NNTPServerError (code, body)
	| code >= 400 = NNTPClientError (code, body)
	| code >= 200 = NNTPSuccess (code, body)
	| otherwise   =  NNTPClientError (code, body)
