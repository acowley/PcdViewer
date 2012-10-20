module RawPoints where
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Linear.V3
import System.IO (withBinaryFile, IOMode(ReadMode), hFileSize, hGetBuf)

load3DVerts :: FilePath -> IO (Vector (V3 Float))
load3DVerts f = withBinaryFile f ReadMode $ \h ->
                  do sz <- fromIntegral `fmap` hFileSize h
                     fp <- mallocForeignPtrBytes sz
                     withForeignPtr fp $ \ptr ->
                       hGetBuf h ptr sz
                     return $ V.unsafeFromForeignPtr0 fp (sz `quot` 12)
