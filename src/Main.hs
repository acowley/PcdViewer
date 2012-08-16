{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Applicative
import Control.Lens
import Data.IORef (readIORef, writeIORef, newIORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (transpose)
import qualified Data.Set as S
import qualified Renderer as R
import qualified Data.Vector.Storable as V
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import System.Exit (exitSuccess)
import Camera
import LinAlg.V2
import LinAlg.V3
import LinAlg.Vector
import PCD
import PointsGL
import MyPaths

data AppState = AppState { cam       :: Camera 
                         , prevMouse :: Maybe (V2 Int) }

handler :: AppState -> Double -> R.UIEvents -> (Bool, AppState)
handler (AppState c prev) dt (R.UIEvents {..}) = (stop, AppState c' prev')
  where stop = R.KeyEsc `elem` map fst (fst keys)
        c' = auxKey (go (inc*^forward c)) R.KeyUp
           . auxKey (go ((-inc)*^forward c)) R.KeyDown
           . auxKey (go ((-inc)*^right c)) R.KeyLeft
           . auxKey (go (inc*^right c)) R.KeyRight
           . maybe id (pan . (^.x)) dMouse
           . maybe id (tilt . negate . (^.y)) dMouse
           . slow 0.9 
           $ update dt c 
        s = 15.0  -- max speed
        inc = 1.0 -- 0.1
        go = (clampSpeed s .) . deltaV
        auxKey f k = if S.member k (snd keys) then f else id
        dMouse = (\old -> (fromIntegral <$> mousePos ^-^ old) ^* 0.01) <$> prev
        prev' = maybe (const mousePos <$> prev) 
                      (bool (Just mousePos) Nothing)
                      (lookup R.MouseButton0 mouseButtons)

bool :: a -> a -> Bool -> a
bool t _ True = t
bool _ f False = f

data ShaderArgs = ShaderArgs { camMat    :: UniformLocation
                             , vertexPos :: AttribLocation }

initShader :: IO ShaderArgs
initShader = do vs <- loadShader =<< getDataFileName "etc/cloud.vert"
                fs <- loadShader =<< getDataFileName "etc/cloud.frag"
                p <- linkShaderProgram [vs] [fs]
                currentProgram $= Just p
                ShaderArgs <$> get (uniformLocation p "cam")
                           <*> get (attribLocation p "vertexCoord")

buildMat :: GLfloat -> GLfloat -> [[GLfloat]]
buildMat near far = [ [1, 0, 0, 0]
                    , [0, 1, 0, 0]
                    , [0, 0, -2 / (far - near), (near - far) / (far - near)]
                    , [0, 0, -1, 0] ]

matMul :: [[GLfloat]] -> [[GLfloat]] -> [[GLfloat]]
matMul x y = map (\row -> map (\col -> sum (zipWith (*) row col)) y') x
  where y' = transpose y

setup :: IO (Camera -> IO ())
setup = do clearColor $= Color4 (115/255) (124/255) (161/255) 0
           -- depthFunc $= Just Always
           -- depthFunc $= Just Greater
           depthFunc $= Just Lequal
           -- pointSize $= 3.0
           pointSizeRange $= (0,20)
           vertexProgramPointSize $= Enabled
           pointSmooth $= Enabled
           s <- initShader
           v <- loadTest
           -- putStrLn $ "Lowest point: " ++ show (lowestPoint v)
           let m = uniformMat (camMat s)
               proj = buildMat 0.01 100.0
           drawPoints <- prepPoints v (vertexPos s)
           return $ \c -> m $= matMul proj (map (map realToFrac) (toLists (toMatrix c))) 
                          >> drawPoints

draw :: IO ()
draw = clear [ColorBuffer, DepthBuffer]

onlyEvery n = \m -> do old <- readIORef tmp
                       if old == n
                          then m >> writeIORef tmp 0
                          else writeIORef tmp (old+1)
  where tmp = unsafePerformIO $ newIORef 0
        {-# NOINLINE tmp #-}

main :: IO ()
main = do R.setup
          drawCloud <- setup
          let renderLoop = R.loop (((return .) .) . handler)
                                  (\s -> draw >> drawCloud (cam s))
              occasionally = onlyEvery 10
              go c = do (shouldExit,c') <- renderLoop c
                        occasionally $ print (cam c'^.translation) >>
                                       putStrLn ("Forward = " ++ show (forward (cam c')))
                        if shouldExit
                          then (R.shutdown >> exitSuccess)
                          else go c'
              startCam = (translation.y .~ 3)
                       . roll pi . pan pi
                       $ defaultCamera
          go $ AppState startCam Nothing
