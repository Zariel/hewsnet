
import Control.Concurrent.STM.TQueue

import Control.Monad
import Control.Monad.STM

import Control.Concurrent

import Network.Socket.Internal

import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs.Implicit

import Nntp.Client
import Nntp.Types

import Nzb.Parser

import Config

data CmdLine = CmdLine { configFile :: String
                       , nzbFile :: String
                       }
	deriving (Show, Data, Typeable)

pipeQueue :: [a] -> TQueue a -> IO ()
pipeQueue vals queue = atomically $ mapM_ (writeTQueue queue) vals

run :: Maybe Config -> IO (Maybe ThreadId)
run Nothing = return Nothing
run (Just conf) = do
	-- A queue which Nzb's can be sent down
	inQ <- atomically newTQueue :: IO NzbQueue
	-- Output writing queue is seperate from the main one
	outQ <- atomically newTQueue :: IO WriterQueue

	tid <- forkIO $ nntpMain inQ outQ (head $ configServers conf)
	return $ Just tid

main :: IO (Maybe ThreadId)
main = withSocketsDo $ do
	args <- cmdArgs CmdLine
		{ configFile = def
		, nzbFile = def
		}

	file <- B.readFile (configFile args)
	run $ openConfig file