

import Control.Concurrent.STM.TMVar
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

data CmdLine = CmdLine
	{ configFile :: String
    , nzbFile :: String
	} deriving (Show, Data, Typeable)

data NNTPThread = NNTPThread
	{ nntpInputQueue :: NzbQueue
	, nntpOutpuQueue :: WriterQueue
	, nntpThreadId :: ThreadId
	}

instance Show NNTPThread where
	show = show . nntpThreadId

pipeQueue :: [a] -> TQueue a -> IO ()
pipeQueue vals queue = atomically $ mapM_ (writeTQueue queue) vals

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = (liftM concat) $ mapM f xs

startThreadDispather :: Config -> IO (TMVar Bool)
startThreadDispather config = do
	outQ <- atomically newTQueue :: IO WriterQueue
	-- Going to need a state or something to hold the queue and threadIDs
	concatMapM (run outQ) (configServers config) >>= print

	atomically newEmptyTMVar

-- Start a thread for each connection
run :: WriterQueue -> ServerConfig -> IO [NNTPThread]
run outQ serverConfig = do
	-- A queue which Nzb's can be sent down
	inQ <- atomically newTQueue :: IO NzbQueue

	threads <- replicateM (serverConections serverConfig) (startThread inQ)

	return threads
	where
		startThread :: NzbQueue -> IO NNTPThread
		startThread inQ = do
			threadId <- forkIO $ nntpMain inQ outQ serverConfig

			return $ NNTPThread inQ outQ threadId

hewsnet :: Config -> IO ()
hewsnet config = do
	var <- startThreadDispather config
	atomically $ takeTMVar var
	return ()

main :: IO ()
main = withSocketsDo $ do
	args <- cmdArgs CmdLine
		{ configFile = def
		, nzbFile = def
		}

	file <- B.readFile (configFile args)
	case (openConfig file) of
		Just conf -> hewsnet conf >>= print
		Nothing -> print "Unable to openConfig"

	return ()