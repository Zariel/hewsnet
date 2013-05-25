module Nzb.Parser
( parseNzb
) where

import Nzb.NzbFile

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Token
import Text.Parsec.Perm
import Data.ByteString

import Data.HashMap
import Control.Monad

import Debug.Trace(trace)

parseNzb :: ByteString -> Either String Nzb
parseNzb xml = case runParser parser () "" xml of
				 Left err -> Left $ show err
				 Right nzb -> Right nzb


parser :: Parser Nzb
parser = do
	preamble
	emptyTag "nzb"
	nzb <- parseXmlBody
	closeTag "nzb"
	manyTill anyChar (try eof)

	return nzb

parseXmlBody :: Parser Nzb
parseXmlBody = permute (Nzb <$$> (try header) <||> (try allFiles))
	where
		allFiles = sepEndBy1 files (try $ closeTag "file")
		comments :: Parser String
		comments = do
			string "<--"
			manyTill anyChar (try $ string "-->")

		handle head files  _ = Nzb head files

preamble :: Parser ()
preamble = xmlDec >> dtdDec

xmlDec :: Parser ()
xmlDec = do
	optional spaces
	string "<?xml"
	optional spaces
	manyTill anyChar (try $ char '>')
	optional spaces

	return ()

dtdDec :: Parser ()
dtdDec = do
	optional spaces
	string "<!DOCTYPE"

	endTag

	return ()

comments :: Parser String
comments = do
	string "<--"
	manyTill anyChar (try $ string "-->")

--  Applies the parser, parsing optional comments
com :: Parser a -> Parser a
com p = optional comments >> p

endTag :: Parser ()
endTag = do
	manyTill anyChar (try $ char '>')
	optional spaces

	return ()

closeTag :: String -> Parser ()
closeTag name = do
	optional spaces
	string "</"
	optional spaces
	string name
	optional spaces
	char '>'

	return ()

emptyTag :: String -> Parser ()
emptyTag name = tag name >>	endTag

tag :: String -> Parser ()
tag name = do
	optional spaces
	try (do
		char '<'
		notFollowedBy (char '/')
		)
	optional spaces
	string name
	optional spaces

	return ()

value :: Parser String
value = between (char '"') (char '"') (many $ noneOf "\"")

attribute :: String -> Parser String
attribute name = do
	optional spaces
	string name
	optional spaces
	char '='
	optional spaces
	value

attributes :: Parser [(String, String)]
attributes =  manyTill kvp (char '>')
	where
		kvp :: Parser (String, String)
		kvp = do
			key <- endBy anyChar (char '=')
			char '='
			val <- value

			return (key, val)

nzbRoot :: Parser ()
nzbRoot = emptyTag "nzb"

{-
xmlTag :: String -> Parser a -> Parser b -> Parser (a, b)
xmlTag name attrib value = do
	tag name
	attribs <- attrib
	endTag

	val <- endBy value (lookAhead (try $ closeTag name))
	closeTag name

	return (attribs, val)

	where
		valBody :: String -> Parser String
		valBody tag = manyTill anyChar (try (string tag))
-}

-- Nzb header is currently a HashMap
header :: Parser NzbHeader
header = do
	emptyTag "head"
	metas <- manyTill meta (lookAhead (try $ closeTag "head"))
	closeTag "head"

	return $ fromList metas
	where
		meta :: Parser (String, String)
		meta = do
			tag "meta"
			key <- attribute "type"
			endTag
			val <- manyTill anyChar (lookAhead $ try (closeTag "meta"))
			closeTag "meta"

			return (key, val)

files :: Parser NzbFile
files = do
	try $ tag "file"
	nzbFile <- parseFile
	endTag

	(groups, segments) <- parseBody
	lookAhead (closeTag "file")

	return nzbFile

	where
		parseFile = permute (NzbFile <$$> (try $ attribute "poster") <||> (try $ attribute "date") <||> (try $ attribute "subject"))
		parseBody = permute ((\a b -> (a, b)) <$$> (try $ parseGroups) <||> (try parseSegments))

parseGroups :: Parser [ String ]
parseGroups = do
	emptyTag "groups"
	groups <- manyTill group (lookAhead $ try (closeTag "groups"))
	closeTag "groups"

	return groups

	where
		group = do
			emptyTag "group"
			group <- manyTill anyChar (lookAhead $ try (closeTag "group"))
			closeTag "group"

			return group

parseSegments :: Parser [ NzbSegment ]
parseSegments = do
	emptyTag "segments"
	segments <- manyTill segment (lookAhead $ try (closeTag "segments"))
	closeTag "segments"

	return segments

	where
		segment = do
			tag "segment"
			(size, num) <- attr
			endTag

			article <- manyTill anyChar (lookAhead $ try (closeTag "segment"))
			closeTag "segment"

			return $ NzbSegment size num article

		attr = permute ((\a b -> (int a, int b)) <$$> (try $ attribute "bytes") <||> (try $ attribute "number"))
		int x = (read x) :: Integer
