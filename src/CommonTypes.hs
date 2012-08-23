{-# LANGUAGE ScopedTypeVariables #-}
-- |Common types for dealing with PCD files.
module CommonTypes (V2(..), module Linear.Vector, 
                    V3(..), Quaternion(..),
                    Vector, Word8) where

-- module CommonTypes (module LinAlg.V2, module LinAlg.Vector, 
--                     module LinAlg.V3, module LinAlg.Quaternion,
--                     Vector, Word8) where
import Data.Vector.Storable (Vector)
import Data.Word (Word8)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.Vector
import Linear.Quaternion (Quaternion(..))
