{-# LANGUAGE ScopedTypeVariables #-}
-- |Common types for dealing with PCD files.
module CommonTypes where
import Control.Applicative
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable(..))

data V3 a = V3 a a a deriving (Eq,Show,Ord)
data Quaternion a = Q a a a a deriving (Eq,Show,Ord)

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

