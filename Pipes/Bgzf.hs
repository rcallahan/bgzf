{-# LANGUAGE ForeignFunctionInterface, RankNTypes, OverloadedStrings #-}
module Pipes.Bgzf (
    inflateBlock,
    bgzfPipe ) where

import Data.Streaming.Zlib.Lowlevel
import Data.Streaming.Zlib (ZlibException (..), WindowBits(..))
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.ByteString.Internal
import System.IO
import System.IO.Unsafe
import qualified Data.ByteString as B
import Data.ByteString.Unsafe
import Pipes

foreign import ccall unsafe "streaming_commons_free_z_stream_inflate"
    freeZstream :: ZStream' -> IO ()

bgzfBlockSize = 65536

inflateBlock :: ByteString -> ByteString
inflateBlock bs = unsafePerformIO $ do
    zstr <- zstreamNew
    inflateInit2 zstr (WindowBits (-15))
    fp <- mallocByteString bgzfBlockSize
    withForeignPtr fp $ \p -> do
        let buff = castPtr p
        c_set_avail_out zstr buff $ fromIntegral bgzfBlockSize
        unsafeUseAsCStringLen bs $ \(cstr, len) -> do
            c_set_avail_in zstr cstr $ fromIntegral len
            res <- c_call_inflate_noflush zstr
            avail <- c_get_avail_out zstr
            freeZstream zstr
            let size = bgzfBlockSize - fromIntegral avail
            return $ PS fp 0 size

parseHeader :: ByteString -> Int
parseHeader bs = unsafePerformIO $ unsafeUseAsCString bs $ \p -> do
    let usptr :: Ptr CUShort
        usptr = castPtr (p `plusPtr` 16)
    blklen <- peek usptr
    return $ fromIntegral $ blklen + 1

bgzfPipe :: MonadIO m => Handle -> Producer' ByteString m ()
bgzfPipe hdl = go where
    go = do
        header <- liftIO $ B.hGet hdl 18
        case parseHeader header of
            28 -> return ()
            blocklen -> do
                chunk <- liftIO $ B.hGet hdl (blocklen - 18)
                yield $ inflateBlock chunk
                go
