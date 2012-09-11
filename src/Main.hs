{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Main where
import Control.Applicative
import Control.Lens
import Data.Foldable (toList)
import Data.IORef (newIORef, writeIORef, readIORef)
import Data.List (transpose)
import qualified Data.Set as S
import qualified Data.Vector.Storable as V
import qualified Renderer as R
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Camera
import CommonTypes
import Linear.V2
import PCD
import PointsGL
import MyPaths
import HeatPalette
import FrameGrabber
import System.FilePath ((</>))

data AppState = AppState { _cam          :: Camera 
                         , _prevMouse    :: Maybe (V2 Int)
                         , _saveDepthmap :: AppState -> IO () }
makeLenses ''AppState

keyActions :: AppState -> [(R.Key, Bool)] -> IO AppState
keyActions s keys 
  | (R.CharKey 'F', True) `elem` keys = (s^.saveDepthmap) s >> return s
  | (R.CharKey 'C', True) `elem` keys = print (s^.cam) >> return s
  | otherwise = return s

cameraControl :: Double -> R.UIEvents -> AppState -> (Bool, AppState)
cameraControl dt (R.UIEvents{..}) st = (stop, ((cam.~c').(prevMouse.~prev')) $ st)
  where c = st^.cam 
        prev = st^.prevMouse
        stop = R.KeyEsc `elem` map fst (fst keys)
        c' = auxKey (go (inc*^forward c)) R.KeyUp
           . auxKey (go ((-inc)*^forward c)) R.KeyDown
           . auxKey (go ((-inc)*^right c)) R.KeyLeft
           . auxKey (go (inc*^right c)) R.KeyRight
           . maybe id (pan . (^._x)) dMouse
           . maybe id (tilt . negate . (^._y)) dMouse
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

handler :: AppState -> Double -> R.UIEvents -> IO (Bool, AppState)
handler s dt ui = keyActions s (fst (R.keys ui)) >>= 
                  return . cameraControl dt ui


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

-- Configures OpenGL and returns a drawing function.
setup :: IO (FilePath -> IO (), Camera -> IO ())
setup = do clearColor $= Color4 (115/255) (124/255) (161/255) 0
           --clearColor $= Color4 0 0 0 0
           clearColor $= Color4 1 1 1 0
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
           (heatVec, t) <- heatTexture 1024
           v <- fixup <$> loadTest
           let m = uniformMat (camMat s)
               proj = buildMat 0.01 100.0
               cmat = toList . fmap (toList . fmap realToFrac) . toMatrix
           drawPoints <- prepPoints v (vertexPos s)
           let draw c = do m $= matMul proj (cmat c)
                           activeTexture $= TextureUnit 0
                           uniform (heatTex s) $= Index1 (0::GLuint)
                           textureBinding Texture1D $= Just t
                           drawPoints
           return (saveFloatFrame heatVec, draw)
  where fixup = V.map (\(V3 x y z) -> V3 x (-z) y)

preDraw :: IO ()
preDraw = clear [ColorBuffer, DepthBuffer]

makeFrameSaver :: (FilePath -> IO ()) -> IO (AppState -> IO ())
makeFrameSaver dump = do cnt <- newIORef (1::Int)
                         let f s = do n <- readIORef cnt
                                      writeIORef cnt (n+1)
                                      dump $ baseName++show n++".bin"
                                      writeFile (baseName++show n++"pose.txt")
                                                (writePose (_cam s))
                         return f
  where baseName = projRoot </> "depthmaps" </> "depths"

main :: IO ()
main = do loop <- R.setup
          (dumpDepth, drawCloud) <- setup
          dumper <- makeFrameSaver dumpDepth
          occasionally <- R.onlyEvery 3
          rate <- R.rateLimitHz 60
          (incFrame,getFPS) <- R.fps
          let renderLoop = loop handler
                                (\s -> preDraw >> drawCloud (s^.cam))
              go frame c = 
                do incFrame
                   (shouldExit,c') <- renderLoop c
                   occasionally $ putStr "FPS: " >> getFPS >>= print
                   if shouldExit
                     then R.shutdown
                     else rate >> go (frame+1) c'
              startCam = (translation._y .~ 3)
                       . roll pi . pan pi
                       $ defaultCamera
          go (0::Int) $ AppState startCam Nothing dumper
