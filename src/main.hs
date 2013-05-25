import qualified Data.ByteString as B
import System.Console.CmdArgs.Implicit
import Nzb.Parser

data Server = Server

data Config = Config { _servers :: [ Server ]
					 , _tempDir :: String
					 , _finishDir :: String
					 , _apiPort :: Integer
					 }

data CmdLine = CmdLine { configFile :: String
                       , nzbFile :: String
                       }
	deriving (Show, Data, Typeable)

openConfig :: CmdLine -> IO Config
openConfig (CmdLine configFile _) = do 
	putStrLn configFile

	return $ Config [] "" "" 0

startServer :: Config -> IO ()
startServer = undefined

main :: IO ()
main = do
	args <- cmdArgs CmdLine { configFile = def, nzbFile = def }
	--openConfig args >>= startServer
	B.readFile (nzbFile args) >>= print . parseNzb

	return ()
