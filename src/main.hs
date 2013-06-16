
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


main :: IO ()
main = withSocketsDo $ do
	args <- cmdArgs CmdLine
		{ configFile = def
		, nzbFile = def
		}
	--openConfig args >>= startServer
	-- A queue which Nzb's can be sent down
	queue <- atomically newTQueue :: IO (NzbQueue)
	nzb <- readFile (nzbFile args) >>= parseNzb
	json <- B.readFile (configFile args)
	case openConfig json of
	  Just conf -> nntpMain queue (head $ configServers conf) >>= print
	  Nothing -> print "Cant parse"

	return ()
