module Renderer (UIEvents(..), loop, setup, shutdown, 
                 module Graphics.UI.GLFW) where
import Control.Applicative
import Control.Concurrent.MVar
import Graphics.UI.GLFW
import System.IO.Unsafe
import Control.Monad.IO.Class

data UIEvents = UIEvents { keys         :: [(Key,Bool)]
                         , mouseButtons :: [(MouseButton,Bool)]
                         , mousePos     :: (Int,Int) }

-- We provide callback functions to GLFW that buffer keyboard and
-- mouse button presses that the client of 'loop' eventually receives.

{-# NOINLINE keyBuffer #-}
keyBuffer :: MVar [(Key,Bool)]
keyBuffer = unsafePerformIO $ newMVar []

{-# NOINLINE mbBuffer #-}
mbBuffer :: MVar [(MouseButton,Bool)]
mbBuffer = unsafePerformIO $ newMVar []

bufferKey :: Key -> Bool -> IO ()
bufferKey k p = modifyMVar_ keyBuffer (return . ((k,p):))

bufferMB :: MouseButton -> Bool -> IO ()
bufferMB m p = modifyMVar_ mbBuffer (return . ((m,p):))

loop :: MonadIO m => (s -> UIEvents -> m s) -> (s -> m ()) -> s -> m s
loop eventHandler draw = go 
  where go s = do ui <- liftIO $ pollEvents >>
                                 UIEvents <$> swapMVar keyBuffer []
                                          <*> swapMVar mbBuffer []
                                          <*> getMousePosition
                  s' <- eventHandler s ui
                  draw s'
                  liftIO swapBuffers
                  return s'

setup :: IO ()
setup = do _ <- initialize
           _ <- openWindow opts
           setWindowTitle "PCD Viewer"
           setKeyCallback bufferKey
           setMouseButtonCallback bufferMB
  where opts = defaultDisplayOptions { displayOptions_width = 640
                                     , displayOptions_height = 480 }

shutdown :: IO ()
shutdown = terminate