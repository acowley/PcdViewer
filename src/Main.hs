{-# LANGUAGE NamedFieldPuns #-}
module Main where
import Control.Applicative
import qualified Renderer as R
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import System.Exit (exitSuccess)

import PCD
import PointsGL
import MyPaths

handler :: R.UIEvents -> Bool
handler R.UIEvents {R.keys} = R.KeyEsc `elem` map fst keys

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

setup :: IO (IO ())
setup = do clearColor $= Color4 (115/255) (124/255) (161/255) 0
           depthFunc $= Just Always
           -- pointSize $= 3.0
           s <- initShader
           v <- loadTest
           let m = uniformMat (camMat s)
               proj = buildMat 0.01 100.0
           prepPoints v (vertexPos s) >>= return . ((m $= proj) >>)

draw :: IO ()
draw = clear [ColorBuffer, DepthBuffer]

main :: IO ()
main = do R.setup
          drawCloud <- setup
          let renderLoop = R.loop (const (return . handler)) 
                                  (const (draw >> drawCloud))
              go = do shouldExit <- renderLoop False
                      if shouldExit
                        then (R.shutdown >> exitSuccess)
                        else go
          go
