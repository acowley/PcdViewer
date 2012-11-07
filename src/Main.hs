{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Main where
import Control.Applicative
import SmallLens
import Control.Monad (when)
import Data.Foldable (toList)
import Data.IORef (newIORef, writeIORef, readIORef)
import qualified Data.Set as S
import qualified Data.Vector.Storable as V
import qualified Renderer as R
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Camera
import Linear.Matrix ((!*!), M44)
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import qualified PCD.Data as PCD
--import PCDCleaner
import PointsGL
import VizMarkers
import MyPaths
import HeatPalette
import FrameGrabber
import PointLoader
import System.Directory (canonicalizePath, createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory, takeExtension)

data AppState = AppState { _cam          :: Camera 
                         , _prevMouse    :: Maybe (V2 Int)
                         , _saveDepthmap :: AppState -> IO () }
makeLenses ''AppState

keyActions :: AppState -> [(R.Key, Bool)] -> IO AppState
keyActions s keys 
  | (R.CharKey 'F', True) `elem` keys = (s^.saveDepthmap) s >> return s
  | (R.CharKey 'C', True) `elem` keys = print (s^.cam) >> return s
  | otherwise = return s

cameraControl :: Float -> Double -> R.UIEvents -> AppState -> (Bool, AppState)
cameraControl scale dt (R.UIEvents{..}) st = 
  (stop, ((cam.~c').(prevMouse.~prev')) $ st)
  where c = st^.cam 
        prev = st^.prevMouse
        stop = R.KeyEsc `elem` map fst (fst keys)
        c' = auxKey (go (inc*^forward c)) R.KeyUp
           . auxKey (go ((-inc)*^forward c)) R.KeyDown
           . auxKey (go ((-inc)*^right c)) R.KeyLeft
           . auxKey (go (inc*^right c)) R.KeyRight
           -- . auxKeyOnce (roll (pi*0.5)) R.KeyPageup
           -- . auxKeyOnce (roll (-pi * 0.5)) R.KeyPagedown
           . auxKey (roll 0.01) R.KeyPageup
           . auxKey (roll (-0.01)) R.KeyPagedown
           . maybe id (pan . negate . (^._x)) dMouse
           . maybe id (tilt . negate . (^._y)) dMouse
           . slow 0.9 
           $ update dt c 
        s = 15.0 / scale  -- max speed
        inc = 1.0 / scale -- 0.1
        go = (clampSpeed s .) . deltaV
        auxKey f k = if S.member k (snd keys) then f else id
        auxKeyOnce f k = if (k, True) `elem` fst keys then f else id
        dMouse = (\old -> (fromIntegral <$> mousePos ^-^ old) ^* 0.01) <$> prev
        prev' = maybe (const mousePos <$> prev) 
                      (bool (Just mousePos) Nothing)
                      (lookup R.MouseButton0 mouseButtons)

handler :: Float -> AppState -> Double -> R.UIEvents -> IO (Bool, AppState)
handler scale s dt ui = keyActions s (fst (R.keys ui)) >>= 
                        return . cameraControl scale dt ui

bool :: a -> a -> Bool -> a
bool t _ True = t
bool _ f False = f

data ShaderArgs = ShaderArgs { camMat    :: UniformLocation
                             , heatTex   :: UniformLocation
                             , vertexPos :: AttribLocation
                             , cloudProg :: Program }

initShader :: IO ShaderArgs
initShader = do vs <- loadShader =<< getDataFileName "etc/cloud.vert"
                fs <- loadShader =<< getDataFileName "etc/cloud.frag"
                p <- linkShaderProgram [vs] [fs]
                currentProgram $= Just p
                ShaderArgs <$> get (uniformLocation p "cam")
                           <*> get (uniformLocation p "heat")
                           <*> get (attribLocation p "vertexCoord")
                           <*> pure p

buildMat :: Float -> Float -> Float -> M44 Float
buildMat s near far = V4 (set _x s 0)
                         (set _y s 0)
                         (V4 0 0 (-2/(far-near)) ((near-far)/(far-near)))
                         (set _z (-s) 0)

-- Configures OpenGL and returns a drawing function.
setup :: Float -> FilePath -> IO (FilePath -> IO (), Camera -> IO ())
setup scale ptFile = do clearColor $= Color4 1 1 1 0
                        depthFunc $= Just Lequal
                        vertexProgramPointSize $= Enabled
                        pointSmooth $= Enabled
                        --textureFunction $= Decal
                        lighting $= Disabled
                        s <- initShader
                        activeTexture $= TextureUnit 0
                        uniform (heatTex s) $= Index1 (0::GLint)
                        (heatVec, t) <- heatTexture 1024
                        let ext = takeExtension ptFile
                        gp <- groundPlane 5 0.1
                        v <- case () of
                               _ | ext == ".pcd" -> 
                                   PCD.loadXyz ptFile >>= \v' ->
                                     if V.null v'
                                     then V.map (view _xyz) <$> 
                                          PCD.loadXyzw ptFile
                                     else return v'
                               _ | ext == ".conf" -> loadConf ptFile
                               _ | ext == ".ply" -> 
                                   V.map (\(SurfacePoint p _ _) -> p) <$>
                                   loadPLYfancy ptFile
                               _ | otherwise -> load3DVerts ptFile
                        let m = uniformMat (camMat s)
                            proj = buildMat scale 0.01 100.0
                            v' = v --V.filter ((< 0.009) . quadrance . view _xy) v
                        -- saveCleanPCD ptFile v'
                        drawPoints <- prepPoints v' (vertexPos s)
                        let draw c = do gp Z (V3 1 0 0) (fmap (fmap realToFrac) $
                                                         proj !*! toMatrix c)
                                        currentProgram $= Just (cloudProg s)
                                        m $= (toList . fmap (toList . fmap realToFrac) $
                                              proj !*! toMatrix c)
                                        activeTexture $= TextureUnit 0
                                        uniform (heatTex s) $= Index1 (0::GLuint)
                                        textureBinding Texture1D $= Just t
                                        drawPoints
                        return (saveFloatFrame heatVec, draw)

preDraw :: IO ()
preDraw = clear [ColorBuffer, DepthBuffer]

makeFrameSaver :: FilePath -> (FilePath -> IO ()) -> IO (AppState -> IO ())
makeFrameSaver pcdRoot dump = 
  do cnt <- newIORef (1::Int)
     let dir = pcdRoot </> "depthmaps"
         baseName = dir </> "depths"
     dirExists <- doesDirectoryExist dir 
     when (not dirExists)
          (createDirectory dir)
     let f s = do n <- readIORef cnt
                  writeIORef cnt (n+1)
                  dump $ baseName++show n++".bin"
                  writeFile (baseName++show n++"pose.txt")
                            (writePose (_cam s))
     return f

runDisplay :: Float -> FilePath -> IO ()
runDisplay scale pcdFile = 
  do loop <- R.setup
     (dumpDepth, drawCloud) <- setup scale pcdFile
     dumper <- makeFrameSaver (takeDirectory pcdFile) dumpDepth
     occasionally <- R.onlyEvery 3
     rate <- R.rateLimitHz 60
     (incFrame,getFPS) <- R.fps
     let renderLoop = loop (handler scale)
                           (\s -> preDraw >> drawCloud (s^.cam))
         go frame c = 
           do incFrame
              (shouldExit,c') <- renderLoop c
              occasionally $ putStr "FPS: " >> getFPS >>= print
              if shouldExit
                then R.shutdown
                else rate >> go (frame+1) c'
         startCam = (translation._y .~ -1)
                  . (translation._z .~ 0.2) 
                  $ fpsDefault
     go (0::Int) $ AppState startCam Nothing dumper

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [x] = []
pairs (x:y:xs) = (x,y) : pairs xs

main :: IO ()
main = getArgs >>= aux
  where aux [pcdFile] = canonicalizePath pcdFile >>= runDisplay 1
        aux (pcdFile:args@(_:_)) = canonicalizePath pcdFile >>=
                                   runDisplay (maybe 1 read (lookup "-s" (pairs args)))
        aux _ = do putStrLn "Usage: PcdViewer PointCloudFile [-s X]"
                   putStrLn $ "- To view a PCD, PLY, or .conf file, supply "++
                              "the file name as a parameter. The \"-s\""++
                              " option may be used to apply a scale factor "++
                              "to the geometry."
