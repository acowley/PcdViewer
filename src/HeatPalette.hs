module HeatPalette (heatTexture) where
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import CommonTypes
import Foreign.Ptr (castPtr)
import Graphics.Rendering.OpenGL

fmod :: (Ord a, RealFrac a) => a -> a -> a
fmod n m | n > m = n - fromIntegral (floor (n / m)) * m
         | otherwise = n

buildPalette :: Int -> V.Vector (V3 Word8)
buildPalette n = V.unfoldrN n aux 0
  where step = 360 / fromIntegral n
        saturation = 0.85
        value = 0.9
        chroma = saturation * value
        m = value - chroma :: Float
        colZero = floor $ m * 255
        colC = floor $ (chroma + m) * 255
        aux hue = Just (col, hue+step)
          where h = hue / 60
                x = chroma * (1 - abs (fmod h 2 - 1))
                colX = floor $ (x + m) * 255
                col | h < 1 = V3 colC colX colZero
                    | h < 2 = V3 colX colC colZero
                    | h < 3 = V3 colZero colC colX
                    | h < 4 = V3 colZero colX colC
                    | h < 5 = V3 colX colZero colC
                    | h < 6 = V3 colC colZero colX

heatTexture :: Int -> IO (V.Vector (V3 Word8), TextureObject)
heatTexture n = do [obj] <- genObjectNames 1
                   textureBinding Texture1D $= Just obj
                   textureFilter Texture1D $= ((Nearest, Nothing), Nearest)
                   textureWrapMode Texture1D S $= (Mirrored, Clamp)
                   let v = buildPalette n
                   V.length v `seq`
                     (V.unsafeWith v $ 
                       --texImage2D Nothing NoProxy 0 RGBA' sz 0
                       texImage1D NoProxy 0 RGBA' sz' 0
                       . PixelData RGB UnsignedByte
                       . castPtr)
                   return (v, obj)
  where sz = TextureSize2D (fromIntegral n) 1
        sz' = TextureSize1D (fromIntegral n)
