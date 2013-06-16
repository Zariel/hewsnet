
import Control.Concurrent.STM.TQueue

import Control.Monad
import Control.Monad.STM

import Network.Socket.Internal

import qualified Data.ByteString.Lazy as B
import System.Console.CmdArgs.Implicit

import NNTP.Client
import NNTP.Types

import Nzb
import Nzb.Parser

import Config

data CmdLine = CmdLine { configFile :: String
                       , nzbFile :: String
                       }
	deriving (Show, Data, Typeable)

pipeQueue :: [a] -> TQueue a -> IO ()
pipeQueue vals queue = atomically $ mapM_ (writeTQueue queue) vals

main :: IO ()
main = withSocketsDo $ do
	args <- cmdArgs CmdLine
		{ configFile = def
		, nzbFile = def
		}
	--openConfig args >>= startServer
	-- A queue which Nzb's can be sent down
	inQ <- atomically newTQueue :: IO NzbQueue
	-- Output writing queue is seperate from the main one
	outQ <- atomically newTQueue :: IO WriterQueue

	nzb <- readFile (nzbFile args) >>= parseNzb
	json <- B.readFile (configFile args)

	pipeQueue (take 2 $ nzbToDownload nzb) inQ

	case openConfig json of
	  Just conf -> nntpMain inQ outQ (head $ configServers conf) >>= print
	  Nothing -> print "Cant parse"

	return ()
