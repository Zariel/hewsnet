{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Nzb.Parser
( parseNzb
) where

import Nzb.NzbFile

import Text.XML.HXT.Core

-- Need to filter out the dtd so HxT doesnt try to fetch it, it doesnt exist.
parseNzb :: String -> IO Nzb
parseNzb xml = runX (parseXML (removeDTD xml) >>> getNzb) >>= (return . head)

removeDTD :: String -> String
removeDTD = unlines . filter (\x -> take 9 x /= "<!DOCTYPE") . lines

parseXML = readString [ withValidate no
					  , withRemoveWS yes
					  ]

getNzb :: ArrowXml a => a XmlTree Nzb
getNzb = atTag "nzb" >>> proc l -> do
	head <- atTag "head" -< l
	header <- listA getHeader -< head

	files <- listA getFile -< l

	returnA -< Nzb header files

getHeader :: ArrowXml a => a XmlTree NzbHeader
getHeader = atTag "meta" >>> proc l -> do
	key <- getAttrValue "type" -< l
	val <- text -< l

	returnA -< NzbHeader key val

getFile :: ArrowXml a => a XmlTree NzbFile
getFile = atTag "file" >>> proc l -> do
	poster <- getAttrValue "poster" -< l
	date <- getAttrValue "date" -< l
	subject <- getAttrValue "subject" -< l

	group <- atTag "groups" -< l
	groups <- listA getGroups -< group

	segment <- atTag "segments" -< l
	segments <- listA getSegments -< segment

	returnA -< NzbFile poster date subject segments groups

getGroups :: ArrowXml a => a XmlTree NzbGroup
getGroups = atTag "group" >>> proc l -> do
	group <- text -< l
	returnA -< group

getSegments :: ArrowXml a => a XmlTree NzbSegment
getSegments = atTag "segment" >>> proc l -> do
	bytes <- getAttrValue "bytes" -< l
	number <- getAttrValue "number" -< l
	article <- text -< l

	returnA -< NzbSegment (read bytes) (read number) article

atTag tag = deep (isElem >>> hasName tag)

text = getChildren >>> getText
