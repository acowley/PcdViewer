{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Applicative
import Control.Lens
import Data.List (transpose)
import qualified Data.Set as S
import qualified Renderer as R
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Camera
import LinAlg.V2
import LinAlg.Vector
import PCD
import PointsGL
import MyPaths
import HeatPalette
--import FrameGrabber
--import Text.Printf

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
                             , heatTex   :: UniformLocation
                             , vertexPos :: AttribLocation }

initShader :: IO ShaderArgs
initShader = do vs <- loadShader =<< getDataFileName "etc/cloud.vert"
                fs <- loadShader =<< getDataFileName "etc/cloud.frag"
                p <- linkShaderProgram [vs] [fs]
                currentProgram $= Just p
                ShaderArgs <$> get (uniformLocation p "cam")
                           <*> get (uniformLocation p "heat")
                           <*> get (attribLocation p "vertexCoord")

buildMat :: GLfloat -> GLfloat -> [[GLfloat]]
buildMat near far = [ [1, 0, 0, 0]
                    , [0, 1, 0, 0]
                    , [0, 0, -2 / (far - near), (near - far) / (far - near)]
                    , [0, 0, -1, 0] ]

matMul :: [[GLfloat]] -> [[GLfloat]] -> [[GLfloat]]
matMul a b = map (\row -> map (\col -> sum (zipWith (*) row col)) b') a
  where b' = transpose b

setup :: IO (Camera -> IO ())
setup = do clearColor $= Color4 (115/255) (124/255) (161/255) 0
           depthFunc $= Just Lequal
           -- pointSize $= 3.0
           -- pointSizeRange $= (0,3)
           vertexProgramPointSize $= Enabled
           pointSmooth $= Enabled
           textureFunction $= Decal
           lighting $= Disabled
           s <- initShader
           activeTexture $= TextureUnit 0
           uniform (heatTex s) $= Index1 (0::GLuint)
           t <- heatTexture 1024
           v <- loadTest
           let m = uniformMat (camMat s)
               proj = buildMat 0.01 100.0
               cmat = map (map realToFrac) . toLists . toMatrix
           drawPoints <- prepPoints v (vertexPos s)
           return $ \c -> do m $= matMul proj (cmat c)
                             activeTexture $= TextureUnit 0
                             uniform (heatTex s) $= Index1 (0::GLuint)
                             textureBinding Texture1D $= Just t
                             drawPoints

draw :: IO ()
draw = clear [ColorBuffer, DepthBuffer]

main :: IO ()
main = do loop <- R.setup
          drawCloud <- setup
          occasionally <- R.onlyEvery 3
          rate <- R.rateLimitHz 60
          (incFrame,getFPS) <- R.fps
          let renderLoop = loop (((return .) .) . handler)
                                (\s -> draw >> drawCloud (cam s))
              -- frameFile = printf "/tmp/frames/frame%05d.tga"
              -- saveFrame' = saveFrame 640 480 . frameFile
              go frame c = 
                do incFrame
                   (shouldExit,c') <- renderLoop c
                   -- occasionally $ saveFrame' frame
                   occasionally $ putStr "FPS: " >> getFPS >>= print
                   if shouldExit
                     then R.shutdown
                     else rate >> go (frame+1) c'
              startCam = (translation.y .~ 3)
                       . roll pi . pan pi
                       $ defaultCamera
          go (0::Int) $ AppState startCam Nothing
