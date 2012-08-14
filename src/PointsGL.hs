module PointsGL where
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word (Word32)
import Foreign.Storable (sizeOf)
import Graphics.GLUtil
import Graphics.Rendering.OpenGL

import CommonTypes

prepPoints :: Vector (V3 Float) -> AttribLocation -> IO (IO ())
prepPoints v vertexPos = 
  do vb <- fromVector ArrayBuffer v
     let iv = V.enumFromN 0 (V.length v) :: V.Vector Word32
     ib <- fromVector ArrayBuffer iv
     return $ do bindBuffer ArrayBuffer $= Just vb
                 vertexAttribPointer vertexPos $= 
                   (ToFloat, VertexArrayDescriptor 3 Float sz offset0)
                 vertexAttribArray vertexPos $= Enabled
                 bindBuffer ElementArrayBuffer $= Just ib
                 drawElements Points (fromIntegral $ V.length v)
                              UnsignedInt offset0
  where sz :: Integral a => a
        sz = fromIntegral $ sizeOf (undefined :: V3 Float)
        
