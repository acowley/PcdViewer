module PointsGL where
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word (Word32)
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Linear.V3

prepPoints :: Vector (V3 Float) -> AttribLocation -> IO (IO ())
prepPoints v vertexPos = 
  do vb <- fromVector ArrayBuffer v
     let iv = V.enumFromN 0 (V.length v) :: V.Vector Word32
     ib <- fromVector ArrayBuffer iv
     bindBuffer ArrayBuffer $= Just vb
     bindBuffer ElementArrayBuffer $= Just ib
     vertexAttribArray vertexPos $= Enabled
     vertexAttribPointer vertexPos $=
       (ToFloat, VertexArrayDescriptor 3 Float 0 offset0)
     return $ do bindBuffer ArrayBuffer $= Just vb
                 --bindBuffer ElementArrayBuffer $= Just ib
                 vertexAttribPointer vertexPos $=
                   (ToFloat, VertexArrayDescriptor 3 Float 0 offset0)
                 drawElements Points (fromIntegral $ V.length v)
                              UnsignedInt offset0
