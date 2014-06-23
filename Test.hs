import Pipes
import Pipes.Bgzf
import System.Environment
import System.IO
import qualified Data.ByteString as B

main = do
    files <- getArgs
    hdls <- mapM (\r -> openFile r ReadMode) files
    runEffect $ for (bgzfMultiPipe hdls) $ \bs -> liftIO $ mapM_ (B.hPut stdout) bs
