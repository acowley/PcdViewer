module PointsGL where
import qualified Data.Vector.Storable as V
import Data.Word (Word32)
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import CommonTypes

prepPoints :: Vector (V3 Float) -> AttribLocation -> IO (IO ())
prepPoints v vertexPos = 
  do vb <- fromVector ArrayBuffer v
     let iv = V.enumFromN 0 (V.length v) :: V.Vector Word32
     ib <- fromVector ArrayBuffer iv
     bindBuffer ArrayBuffer $= Just vb
     vertexAttribArray vertexPos $= Enabled
     vertexAttribPointer vertexPos $=
       (ToFloat, VertexArrayDescriptor 3 Float 0 offset0)
     return $ do bindBuffer ElementArrayBuffer $= Just ib
                 drawElements Points (fromIntegral $ V.length v)
                              UnsignedInt offset0
