import Pipes
import Pipes.Bgzf
import System.Environment
import System.IO
import qualified Data.ByteString as B

main = do
    r:_ <- getArgs
    hdl <- openFile r ReadMode
    runEffect $ for (bgzfPipe hdl) $ \bs -> liftIO $ B.hPut stdout bs
