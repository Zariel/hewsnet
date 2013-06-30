{-# LANGUAGE OverloadedStrings #-}
module Nzb
( NzbFile(..)
, Nzb(..)
, NzbHeader(..)
, NzbSegment(..)
, NzbGroup
, NzbArticle
, segmentToArticle
) where

import qualified Data.ByteString.Char8 as B

data Nzb = Nzb
	{ nzbHeader :: [ NzbHeader ]
	, nzbFiles :: [ NzbFile ]
	} deriving (Show)

data NzbHeader = NzbHeader
	{ nzbHeaderType :: String
	, nzbHeaderValue :: String
	} deriving (Show)

data NzbFile = NzbFile
	{ nzbFilePoster :: String
	, nzbFileDate :: String
	, nzbFileSubject :: String
	, nzbFileSegments :: [ NzbSegment ]
	, nzbFileGroups :: [ NzbGroup ]
	} deriving (Show)

type NzbGroup = String

data NzbSegment = NzbSegment
	{ nzbSegmentSize :: Integer
	, nzbSegmentNumber :: Integer
	, nzbSegmentArticle :: String
	} deriving (Show)

type NzbArticle = B.ByteString

segmentToArticle :: NzbSegment -> NzbArticle
segmentToArticle (NzbSegment _ _ article) = B.concat ["<", (B.pack article), ">"]