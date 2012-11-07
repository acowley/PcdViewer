module FrameGrabber (saveFloatFrame, saveFrame) where
import Data.Binary.Put
import Data.Bits ((.|.), shiftL)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Vector.Storable ((!), Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word (Word8)
import Linear.V3
import Graphics.Rendering.OpenGL
import System.IO (openBinaryFile, hPutBuf, hClose, IOMode(WriteMode))
import System.FilePath (replaceExtension)

flipCol :: V3 a -> V3 a
flipCol (V3 x y z) = V3 z y x

flipVert :: VM.Storable a => Int -> Vector a -> Vector a
flipVert w v = V.create $ do v' <- VM.new n
                             let dst i = VM.slice ((h - i - 1)*w) w v'
                                 src i = V.slice (i*w) w v
                                 go i | i == h = return v'
                                      | otherwise = V.copy (dst i) (src i) >>
                                                    go (i+1)
                             go 0
  where n = V.length v
        h = n `quot` w

-- Read pixels from the color buffer into a 'Vector'.
readPixelVector :: Int -> Int -> IO (Vector (V3 Word8))
readPixelVector w h = do v <- VM.new (fromIntegral $ w*h)
                         VM.unsafeWith v $
                           readPixels (Position 0 0) sz
                           . PixelData RGB UnsignedByte
                         V.freeze v
  where sz = Size (fromIntegral w) (fromIntegral h)

getFrameSize :: IO (Int,Int)
getFrameSize = do (_, Size w h) <- get viewport
                  return (fromIntegral w, fromIntegral h)

-- Given a vector of colors, construct a function that returns the
-- distance (in the range [0,1]) associated with a particular color.
reverseHeatLookup :: Vector (V3 Word8) -> V3 Word8 -> Float
reverseHeatLookup heat = fromMaybe 0 . (`M.lookup` m) . hash
--reverseHeatLookup heat = fromMaybe 0 . searchCol . hash
  where m = V.ifoldl' aux M.empty heat
        n = fromIntegral $ V.length heat
        aux m' i col = M.insert (hash col) (fromIntegral i / n) m'
        hash (V3 r g b) = fromIntegral r 
                       .|. (fromIntegral g `shiftL` 8)
                       .|. (fromIntegral b `shiftL` 16)
        -- searchCol i = getFirst . mconcat $ 
        --               map (First . (`M.lookup` m)) 
        --                   [i, i+1, i-1, i+256, i-256, i+65536, i-65536]

saveFloatFrame :: Vector (V3 Word8) -> FilePath -> IO ()
saveFloatFrame t f = do (w,h) <- getFrameSize
                        v <- flipVert w `fmap` readPixelVector w h
                        let toFloat i = toDist (v ! i)
                            -- Convert normalized [0,1] depths
                            -- to metric distances by using the same
                            -- scaling (normalization) factor used by
                            -- @cloud.frag@. Currently 15.
                            v' = V.map (*15) . V.map toFloat 
                               $ V.enumFromN 0 (w*h)
                        hdl <- openBinaryFile f WriteMode
                        V.unsafeWith v' $
                          flip (hPutBuf hdl) (w*h*4)
                        hClose hdl
                        putStr $ "Depths range from "++show (V.minimum v')
                        putStrLn $ " to "++show (V.maximum v')
                        writeTGA w h (replaceExtension f "tga") 
                                 (V.unsafeCast $ V.map flipCol v)
                        putStrLn $ "Saved depths to "++f
  where toDist = reverseHeatLookup t

saveFrame :: FilePath -> IO ()
saveFrame f = do (w,h) <- getFrameSize
                 v <- readPixelVector w h
                 writeTGA w h f (V.unsafeCast . V.map flipCol $ flipVert w v)
                 -- writePng f (Image w h v::Image PixelRGB8)

writeHeader :: Integral a => Bool -> a -> a -> PutM ()
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

tgaHeader :: Int -> Int -> ByteString
tgaHeader w h = runPut $ writeHeader True w h
                     
writeTGA :: Int -> Int -> FilePath -> V.Vector Word8 -> IO ()
writeTGA w h f v = B.writeFile f (tgaHeader w h <> runPut (V.mapM_ putWord8 v))
