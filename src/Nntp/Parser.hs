module NNTP.Parser
( responseParser
) where

import NNTP.Types

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

	return $ (read . unpack $ decodeUtf8 code, body)
