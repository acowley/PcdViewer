{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Applicative
import Control.Lens
import Data.List (transpose)
import qualified Renderer as R
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import System.Exit (exitSuccess)
import Camera
import LinAlg.V2
import LinAlg.Vector
import PCD
import PointsGL
import MyPaths

data AppState = AppState { cam       :: Camera 
                         , prevMouse :: Maybe (V2 Int) }

handler :: AppState -> Double -> R.UIEvents -> (Bool, AppState)
handler (AppState c prev) dt (R.UIEvents {..}) = (stop, AppState (update dt c') prev')
  where stop = R.KeyEsc `elem` map fst keys
        c' = auxKey (go (moveForward inc) stopForward) R.KeyUp
           . auxKey (go (moveForward (-inc)) stopForward) R.KeyDown
           . auxKey (go (moveSideways inc) stopSideways) R.KeyLeft
           . auxKey (go (moveSideways (-inc)) stopSideways) R.KeyRight
           . maybe id (pan . (^.x)) dMouse
           . maybe id (tilt . (^.y)) dMouse
           $ slow 0.9 c
        s = 150.0 -- max speed
        inc = 10.0 -- 0.1
        go yes _ True = clampSpeed s . yes
        go _ no False = no
        auxKey f k = maybe id f $ lookup k keys
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
           -- pointSize $= 3.0
           s <- initShader
           v <- loadTest
           let m = uniformMat (camMat s)
               proj = buildMat 0.01 100.0
           drawPoints <- prepPoints v (vertexPos s)
           return $ \c -> m $= matMul proj (map (map realToFrac) (toLists (toMatrix c))) 
                          >> drawPoints

draw :: IO ()
draw = clear [ColorBuffer, DepthBuffer]

main :: IO ()
main = do R.setup
          drawCloud <- setup
          let renderLoop = R.loop (((return .) .) . handler)
                                  (\s -> draw >> drawCloud (cam s))
              go c = do (shouldExit,c') <- renderLoop c
                        if shouldExit
                          then (R.shutdown >> exitSuccess)
                          else go c'
          go $ AppState defaultCamera Nothing
