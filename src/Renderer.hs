module Renderer (UIEvents(..), loop, setup, shutdown, 
                 module Graphics.UI.GLFW) where
import Control.Applicative
import Control.Arrow ((***))
import Control.Concurrent.MVar
import Graphics.UI.GLFW
import System.IO.Unsafe
import Control.Monad.IO.Class
import qualified Data.Set as S

import LinAlg.V2

data UIEvents = UIEvents { keys         :: ([(Key,Bool)], S.Set Key)
                         , mouseButtons :: [(MouseButton,Bool)]
                         , mousePos     :: V2 Int }

-- We provide callback functions to GLFW that buffer keyboard and
-- mouse button presses that the client of 'loop' eventually receives.

{-# NOINLINE keyBuffer #-}
-- We track presses and toggle states (in the form of a list of
-- currently depressed keys). The client can decide which is more
-- useful.
keyBuffer :: MVar ([(Key,Bool)], S.Set Key)
keyBuffer = unsafePerformIO $ newMVar ([], S.empty)

{-# NOINLINE mbBuffer #-}
mbBuffer :: MVar [(MouseButton,Bool)]
mbBuffer = unsafePerformIO $ newMVar []

bufferKey :: Key -> Bool -> IO ()
bufferKey k p = modifyMVar_ keyBuffer (return . (((k,p):)***toggle))
  where toggle | p = S.insert k
               | otherwise = S.delete k

bufferMB :: MouseButton -> Bool -> IO ()
bufferMB m p = modifyMVar_ mbBuffer (return . ((m,p):))

loop :: MonadIO m => (a -> Double -> UIEvents -> m b) -> (a -> m ()) -> a -> m b
loop eventHandler draw = go 
  where updateKeys = modifyMVar keyBuffer 
                                (\(p,s) -> return (([],s), (p, s)))
        go s = do draw s
                  liftIO swapBuffers
                  ui <- liftIO $ pollEvents >>
                                 UIEvents <$> updateKeys
                                          <*> swapMVar mbBuffer []
                                          <*> (uncurry V2 <$> getMousePosition)
                  dt <- liftIO $ do t' <- getTime
                                    dt <- (t' -) <$> takeMVar lastTime
                                    putMVar lastTime t'
                                    return dt
                  eventHandler s dt ui
        {-# NOINLINE lastTime #-}
        lastTime = unsafePerformIO $ getTime >>= newMVar

setup :: IO ()
setup = do _ <- initialize
           _ <- openWindow opts
           setWindowTitle "PCD Viewer"
           setKeyCallback bufferKey
           setMouseButtonCallback bufferMB
  where opts = defaultDisplayOptions { displayOptions_width = 640
                                     , displayOptions_height = 480
                                     , displayOptions_numDepthBits = 24 }

shutdown :: IO ()
shutdown = terminate