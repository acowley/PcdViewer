{-# LANGUAGE ScopedTypeVariables #-}
-- |Common types for dealing with PCD files.
module CommonTypes (module LinAlg.V2, module LinAlg.Vector, 
                    module LinAlg.V3, module LinAlg.Quaternion,
                    Vector, Word8) where
import Control.Applicative
import Data.Vector.Storable (Vector)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import LinAlg.V2
import LinAlg.V3
import LinAlg.Vector
import LinAlg.Quaternion

instance forall a. Storable a => Storable (V3 a) where
  sizeOf _ = sizeOf (undefined::a) * 3
  alignment _ = alignment (undefined::a)
  peek ptr = let sz = sizeOf (undefined::a)
                 ptr' = castPtr ptr
             in V3 <$> peek ptr' <*> peek (plusPtr ptr' sz)
                   <*> peek (plusPtr ptr' (2*sz))
  poke ptr (V3 x y z) = let sz = sizeOf (undefined::a)
                            ptr' = castPtr ptr
                        in poke ptr' x >> poke (plusPtr ptr' sz) y >>
                           poke (plusPtr ptr' (2*sz)) z

