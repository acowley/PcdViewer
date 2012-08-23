{-# LANGUAGE TemplateHaskell #-}
module Camera where
import Control.Lens
import qualified Data.Foldable as F
import Linear.Conjugate
import Linear.Epsilon
import Linear.Quaternion
import Linear.V3
import Linear.V4
import Linear.Vector
import Linear.Matrix hiding (translation)
import Linear.Metric

type M33 a = V3 (V3 a)

data Camera = Camera { _rotation    :: Quaternion Float
                     , _translation :: V3 Float
                     , _velocity    :: V3 Float }
            deriving Show
makeLenses ''Camera

-- Create a human-readable text output of a camera's rotation and
-- translation.
writePose :: Camera -> String
writePose cam = show (_rotation cam) ++ "\n" ++ show (_translation cam) ++ "\n"

-- Read back a camera pose saved in a format compatible with
-- 'writePose'.
readPose :: String -> Maybe Camera
readPose = aux . lines
  where aux [r,t] = Just $ Camera (read r) (read t) 0
        aux _ = Nothing

-- |Axes identified in the camera's local coordinate frame expressed
-- as vectors in the global coordinate frame.
forward,right,up :: Camera -> V3 Float
forward = flip rotate (V3 0 0 (-1)) . conjugate . view rotation
right = flip rotate (V3 1 0 0) . conjugate . view rotation
up = flip rotate (V3 0 1 0) . view rotation

defaultCamera :: Camera
defaultCamera = Camera 1 0 0

qToM :: Num a => Quaternion a -> M33 a
qToM (Quaternion a (V3 b c d)) = 
  V3 (V3 (a*a+b*b-c*c-d*d) (2*b*c-2*a*d) (2*b*d+2*a*c))
     (V3 (2*b*c+2*a*d) (a*a-b*b+c*c-d*d) (2*c*d-2*a*b))
     (V3 (2*b*d-2*a*c) (2*c*d+2*a*b) (a*a-b*b-c*c+d*d))

snoc3 :: V3 a -> a -> V4 a
snoc3 (V3 a b c) d = V4 a b c d

mkTransformationMat :: Num a => M33 a -> V3 a -> M44 a
mkTransformationMat (V3 row1 row2 row3) (V3 x y z) = 
  V4 (snoc3 row1 x) (snoc3 row2 y) (snoc3 row3 z) (_w.~1$0)

mkTransformation :: Num a => Quaternion a -> V3 a -> M44 a
mkTransformation = mkTransformationMat . qToM

m33_to_m44 :: Num a => M33 a -> M44 a
m33_to_m44 (V3 r1 r2 r3) = V4 (snoc3 r1 0) (snoc3 r2 0) (snoc3 r3 0) (_w.~1$0)

translationToM :: Num a => V3 a -> M44 a
translationToM (V3 x y z) = V4 (V4 1 0 0 x)
                               (V4 0 1 0 y)
                               (V4 0 0 1 z)
                               (V4 0 0 0 1)

toMatrix :: Camera -> M44 Float
toMatrix (Camera r t _) = m33_to_m44 (qToM r) !*! translationToM (negate t)

toLists :: (F.Foldable t, Functor t, F.Foldable r) => t (r a) -> [[a]]
toLists = F.toList . fmap F.toList

axisAngle :: (Epsilon a, Floating a) => V3 a -> a -> Quaternion a
axisAngle axis theta = normalize $ Quaternion (cos half) $ (sin half) *^ axis
  where half = theta / 2

pan :: Float -> Camera -> Camera
pan theta c@(Camera r t v) = Camera r' t v
--  where r' = normalize $ axisAngle (V3 0 (-1) 0) theta * r
  where r' = normalize $ axisAngle (up c) theta * r
  
tilt :: Float -> Camera -> Camera
tilt theta c@(Camera r t v) = Camera r' t v
  where r' = normalize $ axisAngle (V3 1 0 0) theta * r
  -- where r' = normalize $ axisAngle (right c) theta * r

roll :: Float -> Camera -> Camera
roll theta c@(Camera r t v) = Camera r' t v
  where r' = normalize $ axisAngle (forward c) theta * r

-- |Add a vector to a 'Camera''s current velocity.
deltaV :: V3 Float -> Camera -> Camera
deltaV = (velocity +~)

-- Note that signorm in the Metric module does this, but doesn't check
-- if the input vector is zero or unit.
normalize :: (Floating a, Metric f, Epsilon a) => f a -> f a
normalize v = let l = quadrance v 
              in if nearZero l || nearZero (1-l) then v else fmap (/sqrt l) v

clampSpeed :: Float -> Camera -> Camera
clampSpeed maxSpeed = velocity %~ aux
  where aux v = let q = quadrance v
                in if q >= maxSpeed*maxSpeed 
                   then maxSpeed *^ fmap (/ sqrt q) v
                   else v

slow :: Float -> Camera -> Camera
slow rate = velocity %~ (rate *^)

-- Zero out velocity in a particular direction.
stopAxis :: V3 Float -> Camera -> Camera
stopAxis axis (Camera r t v) = Camera r t $ v ^-^ dot (rotate r axis) v *^ axis

-- Update a camera's position based on its velocity and a time step
-- (in seconds).
update :: Double -> Camera -> Camera
update dt (Camera r t v) = Camera r (t + v ^* realToFrac dt) v
