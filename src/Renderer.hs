module Renderer (UIEvents(..), setup, shutdown, onlyEvery, fps, frameCounter,
                 onlyEveryN, module Graphics.UI.GLFW, rateLimitHz) where
import Control.Applicative
import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Graphics.UI.GLFW

import qualified Data.Set as S

import LinAlg.V2

data UIEvents = UIEvents { keys         :: ([(Key,Bool)], S.Set Key)
                         , mouseButtons :: [(MouseButton,Bool)]
                         , mousePos     :: V2 Int }

-- We provide callback functions to GLFW that buffer keyboard and
-- mouse button presses that the client of 'loop' eventually receives.

-- We track presses and toggle states (in the form of a list of
-- currently depressed keys). The client can decide which is more
-- useful.

type KeyBuffer = MVar ([(Key,Bool)], S.Set Key)
type MouseBuffer = MVar [(MouseButton,Bool)]
type UIBuffer = (KeyBuffer, MouseBuffer)

bufferKey :: KeyBuffer -> Key -> Bool -> IO ()
bufferKey keyBuffer k p = modifyMVar_ keyBuffer (return . (((k,p):)***toggle))
  where toggle | p = S.insert k
               | otherwise = S.delete k

bufferMB :: MouseBuffer -> MouseButton -> Bool -> IO ()
bufferMB mbBuffer m p = modifyMVar_ mbBuffer (return . ((m,p):))

loop :: MonadIO m => UIBuffer -> MVar Double -> 
        (a -> Double -> UIEvents -> m b) -> (a -> m ()) -> a -> m b
loop (keyBuffer, mbBuffer) lastTime eventHandler draw = go 
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

setup :: MonadIO m => 
         IO ((a -> Double -> UIEvents -> m b) -> (a -> m ()) -> a -> m b)
setup = do _ <- initialize
           _ <- openWindow opts
           setWindowTitle "PCD Viewer"
           kb <- newMVar ([],S.empty)
           mb <- newMVar []
           setKeyCallback $ bufferKey kb
           setMouseButtonCallback $ bufferMB mb
           setWindowBufferSwapInterval 0
           loop (kb,mb) <$> (getTime >>= newMVar)
  where opts = defaultDisplayOptions { displayOptions_width = 640
                                     , displayOptions_height = 480
                                     , displayOptions_numDepthBits = 24
                                     , displayOptions_refreshRate = Just 100 }

shutdown :: IO ()
shutdown = terminate

-- Some utility functions for people using a renderer

-- |@onlyEveryN n@: Return a function that only runs the given action
-- every @n@ calls.
onlyEveryN :: Int -> IO(IO () -> IO ())
onlyEveryN n = do tmp <- newIORef 0
                  return $ \m -> do old <- readIORef tmp
                                    if old == n
                                      then m >> writeIORef tmp 0
                                      else writeIORef tmp (old+1)

-- |@onlyEvery t@: Return a function that only runs the given action
-- every @t@ seconds.
onlyEvery :: Double -> IO (IO () -> IO ())
onlyEvery dt = do tmp <- getTime >>= newIORef
                  return $ \m -> do t <- getTime
                                    dt' <- (t -) <$> readIORef tmp
                                    when (dt' >= dt)
                                         (writeIORef tmp t >> m)

-- |Returns an action that sleeps the calling thread until at least
-- the specified inter-call time span (in seconds) has lapsed.
rateLimit :: Double -> IO (IO ())
rateLimit period = do prev <- getTime >>= newIORef 
                      return $ do t <- readIORef prev
                                  t' <- getTime
                                  writeIORef prev t'
                                  let dt = floor $ 1000000 * (t' - t)
                                  when (dt < period') 
                                       (threadDelay (period' - dt))
  where period' = floor $ period * 1000000
-- Re. the above: GLFW-b's 'sleep' function doesn't seem to work?

-- |Returns an action that sleeps the calling thread in order to
-- prevent calls to the action from occurring at higher than the given
-- maximum rate (in Hz).
rateLimitHz :: Double -> IO (IO ())
rateLimitHz = rateLimit . (1 /)

strictInc :: Int -> Int
strictInc n = n `seq` n + 1

-- |Returns two actions: one to increment a frame count, and a second
-- to get the current frame count and reset that count to zero.
frameCounter :: IO (IO (), IO Int)
frameCounter = do tmp <- newIORef 0
                  let getCount = do c <- readIORef tmp
                                    writeIORef tmp 0
                                    return c
                  return (modifyIORef tmp (+ 1), getCount)

fps :: IO (IO (), IO Double)
fps = do startTime <- getTime >>= newIORef
         frameCount <- newIORef 0
         let getRate = do t <- getTime
                          dt <- (t -) <$> readIORef startTime
                          n <- readIORef frameCount
                          writeIORef frameCount 0
                          writeIORef startTime t
                          return $ fromIntegral n / dt
         return (modifyIORef frameCount strictInc, getRate)