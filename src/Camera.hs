{-# LANGUAGE TemplateHaskell #-}
module Camera where
import Control.Applicative
import Control.Lens
import qualified Data.Foldable as F
import Data.Functor.Identity
import LinAlg.Epsilon
import LinAlg.Involutive
import LinAlg.Quaternion
import LinAlg.V3
import LinAlg.V4
import LinAlg.Vector
import LinAlg.Matrix hiding (translation)
import LinAlg.Metric

type M33 a = V3 (V3 a)

data Camera = Camera { _rotation    :: Quaternion Float
                     , _translation :: V3 Float
                     , _velocity    :: V3 Float }
            deriving Show
makeLenses ''Camera

defaultCamera :: Camera
defaultCamera = Camera 1 0 0

qToM :: Num a => Quaternion a -> M33 a
qToM (Quaternion a b c d) = V3  
                            (V3 (a*a+b*b-c*c-d*d) (2*b*c-2*a*d) (2*b*d+2*a*c))
                            (V3 (2*b*c+2*a*d) (a*a-b*b+c*c-d*d) (2*c*d-2*a*b))
                            (V3 (2*b*d-2*a*c) (2*c*d+2*a*b) (a*a-b*b-c*c+d*d))

mkTransformationMat :: Num a => M33 a -> V3 a -> M44 a
mkTransformationMat (V3 (V3 a b c)
                        (V3 d e f)
                        (V3 g h i))
                    (V3 x y z) = V4 (V4 a b c x)
                                    (V4 d e f y)
                                    (V4 g h i z)
                                    (V4 0 0 0 1)

mkTransformation :: Num a => Quaternion a -> V3 a -> M44 a
mkTransformation = mkTransformationMat . qToM

m33_to_m44 :: Num a => M33 a -> M44 a
m33_to_m44 (V3 (V3 a b c)
               (V3 d e f)
               (V3 g h i)) = V4 (V4 a b c 0)
                                (V4 d e f 0)
                                (V4 g h i 0)
                                (V4 0 0 0 1)

translationToM :: Num a => V3 a -> M44 a
translationToM (V3 x y z) = V4 (V4 1 0 0 x)
                               (V4 0 1 0 y)
                               (V4 0 0 1 z)
                               (V4 0 0 0 1)

toMatrix :: Camera -> M44 Float
--toMatrix (Camera r t _) = mkTransformation (conjugate r) (negate t)
toMatrix (Camera r t _) = m33_to_m44 (qToM r) !*! translationToM (negate t)

toLists :: (F.Foldable t, Functor t, F.Foldable r) => t (r a) -> [[a]]
toLists = F.toList . fmap F.toList

mkQ :: a -> V3 a -> Quaternion a
mkQ a (V3 b c d) = Quaternion a b c d

axisAngle :: Floating a => V3 a -> a -> Quaternion a
axisAngle axis theta = mkQ (cos half) $ (sin half) *^ axis
  where half = theta / 2

pan :: Float -> Camera -> Camera
--pan delta (Camera r t v) = Camera (axisAngle (rotate r yAxis) delta * r) t v
pan delta (Camera r t v) = Camera (axisAngle yAxis delta * r) t v
  where yAxis = V3 0 1 0

tilt :: Float -> Camera -> Camera
tilt delta (Camera r t v) = Camera (axisAngle (rotate r xAxis) delta * r) t v
  where xAxis = V3 1 0 0

moveForward :: Float -> Camera -> Camera
moveForward delta (Camera r t v) = Camera r t (v + (rotate r zAxis ^* delta))
  where zAxis = V3 0 0 1

normalize :: (Floating a, Metric f, Epsilon a) => f a -> f a
normalize v = let l = sqrt $ quadrance v 
              in if nearZero l then v else fmap (/l) v

clampSpeed :: Float -> Camera -> Camera
clampSpeed maxSpeed = velocity %~ aux
  where aux v = let q = quadrance v
                in if q >= maxSpeed*maxSpeed 
                   then maxSpeed *^ fmap (/ sqrt q) v
                   else v

slow :: Float -> Camera -> Camera
slow rate = velocity %~ (rate *^)

-- Zero out forward/backward velocity.
stopForward :: Camera -> Camera
stopForward = stopAxis $ V3 0 0 1

stopSideways :: Camera -> Camera
stopSideways = stopAxis $ V3 1 0 0

stopAxis :: V3 Float -> Camera -> Camera
stopAxis axis (Camera r t v) = Camera r t $ v ^-^ dot (rotate r axis) v *^ axis

moveSideways :: Float -> Camera -> Camera
moveSideways delta (Camera r t v) = Camera r t (v + (rotate r xAxis ^* delta))
  where xAxis = V3 1 0 0

-- Update a camera's position based on its velocity and a time step
-- (in seconds).
update :: Double -> Camera -> Camera
update dt (Camera r t v) = Camera r (t + v ^* realToFrac dt) v

-- Write 'pan' using lens combinators
-- wat delta = rotation %~ \r -> axisAngle (rotate r yAxis) delta * r
-- wat delta = rotation %~ ((*) <$> flip axisAngle delta . flip rotate yAxis <*> id)
--   where yAxis = V3 0 1 0