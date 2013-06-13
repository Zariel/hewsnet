import qualified Data.ByteString as B
import System.Console.CmdArgs.Implicit

import NNTP.Client
import Nzb.Parser
import Config

data CmdLine = CmdLine { configFile :: String
                       , nzbFile :: String
                       }
	deriving (Show, Data, Typeable)

startServer :: Int -> IO ()
startServer = undefined

main :: IO ()
main = do
	args <- cmdArgs CmdLine
		{ configFile = def
		, nzbFile = def
		}
	--openConfig args >>= startServer
	nzb <- readFile (nzbFile args) >>= parseNzb

	return ()
