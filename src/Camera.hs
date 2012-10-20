{-# LANGUAGE TemplateHaskell #-}
module Camera where
import SmallLens
import Linear.Conjugate
import Linear.Quaternion
import Linear.V3
import Linear.Vector
import Linear.Matrix hiding (translation)
import Linear.Metric

data Camera = Camera { _rotation    :: Quaternion Float
                     , _translation :: V3 Float
                     , _velocity    :: V3 Float 
                     , _cameraUp    :: V3 Float}
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
  where aux [r,t] = Just $ Camera (read r) (read t) 0 (V3 0 1 0)
        aux _ = Nothing

-- |Axes identified in the camera's local coordinate frame expressed
-- as vectors in the global coordinate frame.
forward,right,up :: Camera -> V3 Float
forward = flip rotate (V3 0 0 (-1)) . conjugate . view rotation
right = flip rotate (V3 1 0 0) . conjugate . view rotation
--up = flip rotate (V3 0 1 0) . view rotation
up c = rotate (_rotation c) (_cameraUp c)

defaultCamera :: Camera
defaultCamera = Camera 1 0 0 (V3 0 1 0)

toMatrix :: Camera -> M44 Float
toMatrix (Camera r t _ _) = mkTransformation r (rotate r (negate t))

pan :: Float -> Camera -> Camera
pan theta c@(Camera r _ _ _) = rotation .~ r' $ c
--pan theta c@(Camera r t v _) = Camera r' t v
--  where r' = normalize $ axisAngle (V3 0 (-1) 0) theta * r
  where r' = normalize $ axisAngle (up c) theta * r
  
tilt :: Float -> Camera -> Camera
tilt theta c@(Camera r _ _ _) = rotation .~ r' $ c
  where r' = normalize $ axisAngle (V3 1 0 0) theta * r
  -- where r' = normalize $ axisAngle (right c) theta * r

roll :: Float -> Camera -> Camera
roll theta c@(Camera r _ _ _) = cameraUp .~ up' $ rotation .~ r' $ c
  where r' = normalize $ axisAngle (V3 0 0 (-1)) theta * r
        up' = axisAngle (rotate (conjugate r) $ V3 0 0 (-1)) theta `rotate` _cameraUp c
-- roll theta c@(Camera r _ _ _) = rotation .~ r' $ c
--   where r' = normalize $ axisAngle (forward c) theta * r

-- |Add a vector to a 'Camera''s current velocity.
deltaV :: V3 Float -> Camera -> Camera
deltaV = (velocity +~)

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
stopAxis axis (Camera r t v u) = flip (Camera r t) u $ 
                                 v ^-^ dot (rotate r axis) v *^ axis

-- Update a camera's position based on its velocity and a time step
-- (in seconds).
update :: Double -> Camera -> Camera
update dt (Camera r t v u) = Camera r (t + v ^* realToFrac dt) v u
