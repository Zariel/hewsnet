module Nzb.NzbFile
( NzbFile(..)
, Nzb(..)
, NzbHeader(..)
, NzbSegment(..)
) where

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
