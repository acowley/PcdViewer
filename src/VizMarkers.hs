{-# OPTIONS_GHC -Wall #-}
module VizMarkers where
import Control.Exception (SomeException, catch)
import Data.Foldable (toList)
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Linear.V2
import Linear.V3
import Linear.Matrix
import MyPaths
import System.Exit (exitWith, ExitCode(..))

data EuclideanGround = X | Y | Z deriving (Eq,Ord,Show,Enum)

enumToGL :: Enum a => a -> GLint
enumToGL = fromIntegral . fromEnum

toGLMat :: Real a => M44 a -> [[GLfloat]]
toGLMat = toList . fmap (toList . fmap realToFrac)

initGroundShader :: IO (EuclideanGround -> V3 GLfloat -> M44 GLfloat -> IO ())
initGroundShader = do let giveUp :: SomeException -> IO a
                          giveUp _ = {-shutdown >>-} exitWith (ExitFailure 1)
                      bindBuffer ArrayBuffer $= Nothing
                      currentProgram $= Nothing
                      vs <- (loadShader =<< getDataFileName "etc/GroundPlane.vert")
                            `catch` giveUp
                      fs <- (loadShader =<< getDataFileName "etc/GroundPlane.frag")
                            `catch` giveUp
                      p <- linkShaderProgram [vs] [fs] `catch` giveUp
                      pos <- get $ attribLocation p "vertexCoord"
                      cam <- get $ uniformLocation p "cam"
                      plane <- get $ uniformLocation p "euclideanGround"
                      wireColor <- get $ uniformLocation p "wireColor"
                      let mat = uniformMat cam
                          vec = uniformVec wireColor
                          vad = VertexArrayDescriptor 2 Float 0 offset0
                      vertexAttribArray pos   $= Enabled
                      vertexAttribPointer pos $= (ToFloat, vad)
                      return $ \whichPlane col proj ->
                        do currentProgram $= Just p
                           vertexAttribPointer pos $= (ToFloat, vad)
                           uniform plane $= Index1 (enumToGL whichPlane)
                           mat $= toList (fmap toList proj)
                           vec $= toList col

checkErrors :: IO ()
checkErrors = get errors >>= aux
  where aux [] = return ()
        aux x = print x >> exitWith (ExitFailure 1)

-- |@groundPlane gridSize cellSize@ prepares an action for drawing a
-- wireframe ground plane, centered at the origin, consisting of
-- @gridSize@ squares with side length @cellSize@ extending off in
-- each direction.
groundPlane :: Int -> GLfloat -> 
               IO (EuclideanGround -> V3 GLfloat -> M44 GLfloat -> IO ())
groundPlane n sz = do vb <- makeBuffer ArrayBuffer (horizontals ++ verts)
                      putStrLn $ (show (length (horizontals++verts))) ++ " vs "++
                                 (show (8*(n+1)))++" vs "++show (length indices)
                      print (horizontals++verts)
                      bindBuffer ArrayBuffer $= Just vb
                      shader <- initGroundShader
                      eb <- makeBuffer ElementArrayBuffer indices
                      bindBuffer ElementArrayBuffer $= Just eb
                      let go whichPlane col proj = 
                            do bindBuffer ArrayBuffer $= Just vb
                               --bindBuffer ElementArrayBuffer $= Just eb
                               shader whichPlane col proj
                               drawElements Lines (8*(fromIntegral n+1)) 
                                            UnsignedInt offset0
                      return go
  where extent = fromIntegral n * sz
        horizontals = concatMap (\z -> let z' = fromIntegral z * sz
                                       in [ V2 (-extent) z'
                                          , V2 extent z'
                                          , V2 (-extent) (-z')
                                          , V2 extent (-z') ])
                                [0 .. n]
        verts = concatMap (\x -> let x' = fromIntegral x * sz
                                 in [ V2 x' (-extent)
                                    , V2 x' extent
                                    , V2 (-x') (-extent)
                                    , V2 (-x') extent ])
                          [0 .. n]
        indices = [(0::GLuint)..8 * (fromIntegral n + 1)-1]
