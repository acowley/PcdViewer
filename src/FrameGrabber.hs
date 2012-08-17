module FrameGrabber where
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy as B
import Data.Monoid ((<>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word (Word8)
import Codec.Picture
import Graphics.Rendering.OpenGL

saveFrame :: Int -> Int -> FilePath -> IO ()
saveFrame w h f = do v <- VM.new (w*h*3) :: IO (VM.IOVector Word8)
                     VM.unsafeWith v $
                       readPixels (Position 0 0) sz
                       . PixelData RGB UnsignedByte
                     v' <- V.freeze v
                     writeTGA w h f v'
                     -- writePng f (Image w h v'::Image PixelRGB8)
  where sz = Size (fromIntegral w) (fromIntegral h)

writeHeader isColor w h = do putWord8 0
                             putWord8 0
                             putWord8 (if isColor then 2 else 3)
                             putWord16host 0
                             putWord16host 0
                             putWord8 0
                             putWord16le 0 -- X origin
                             putWord16le $ fromIntegral h - 1 -- Y origin
                             putWord16le (fromIntegral w)
                             putWord16le (fromIntegral h)
                             putWord8 (if isColor then 24 else 8)
                             putWord8 32 -- Screen origin in upper-left

tgaHeader :: ByteString
tgaHeader = runPut $ writeHeader True 640 480
                     
writeTGA :: Int -> Int -> FilePath -> V.Vector Word8 -> IO ()
writeTGA w h f v = B.writeFile f (tgaHeader <> runPut (V.mapM_ putWord8 v))
