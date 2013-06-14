{-# LANGUAGE OverloadedStrings #-}
module Config
( ServerConfig(..)
, Config(..)
, openConfig
) where

import System.IO

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B

data ServerConfig = ServerConfig
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
	{ configServers :: [ ServerConfig ]
	, configTempDir :: String
	, configFinishDir :: String
	, configApiPort :: Integer
	} deriving (Show)

instance FromJSON Config where
	parseJSON = parseConfig

instance FromJSON ServerConfig where
	parseJSON = parseServer

parseConfig :: Value -> Parser Config
parseConfig (Object v) = do
	servers <- (v .: "servers") >>= parseJSON
	tempDir <- (v .: "tempDir")
	completeDir <- (v .: "completeDir")
	apiPort <- (v .: "apiPort")

	return $ Config servers tempDir completeDir apiPort

parseServer :: Value -> Parser ServerConfig
parseServer (Object v) =
	ServerConfig <$>
	(v .: "name") <*>
	(v .: "host") <*>
	(v .: "port") <*>
	(v .: "username") <*>
	(v .: "password") <*>
	(v .: "ssl") <*>
	(v .: "connections") <*>
	(v .: "retention")

-- Currently the config is a JSON file but in the future this may change
openConfig :: B.ByteString -> Maybe Config
openConfig = decode
