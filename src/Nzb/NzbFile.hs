module Nzb.NzbFile
( NzbFile(..)
, Nzb(..)
, NzbHeader
, NzbSegment(..)
) where

import Data.HashMap

data Nzb = Nzb
	{ nzbHeader :: NzbHeader
	, nzbFiles :: [ NzbFile ]
	} deriving (Show)

{-
data NzbHeader = NzbHeader
	{ headerTitle :: String
	} deriving (Show)
-}
type NzbHeader = Map String String

data NzbFile = NzbFile
	{ nzbFilePoster :: String
	, nzbFileDate :: String
	, nzbFileSubject :: String
	} deriving (Show)

data NzbSegment = NzbSegment
	{ nzbSegmentSize :: Integer
	, nzbSegmentNumber :: Integer
	, nzbSegmentArticle :: String
	} deriving (Show)
