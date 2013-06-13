module Config
( NNTPServerConf(..)
, Config(..)
, openConfig
) where

import System.IO

data NNTPServerConf = NNTPServerConf
	{ serverName :: String
	, serverHost :: String
	, serverPort :: Int
	, serverUserName :: String
	, serverPassword :: String
	, serverSSL :: Bool
	, serverConections :: Int
	, serverRetention :: Int
	} deriving (Show)

data Config = Config
	{ configServers :: [ NNTPServerConf ]
	, configTempDir :: String
	, configFinishDir :: String
	, configApiPort :: Integer
	} deriving (Show)

openConfig :: FilePath -> IO Config
openConfig path = return $ Config [] "" "" 0
