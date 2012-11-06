{-# LANGUAGE OverloadedStrings #-}
module PointLoader where
import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Linear.V3
import qualified PLY.Data as PLY
import System.IO (withBinaryFile, IOMode(ReadMode), hFileSize, hGetBuf)

load3DVerts :: FilePath -> IO (Vector (V3 Float))
load3DVerts f = withBinaryFile f ReadMode $ \h ->
                  do sz <- fromIntegral `fmap` hFileSize h
                     fp <- mallocForeignPtrBytes sz
                     _ <- withForeignPtr fp $ \ptr ->
                            hGetBuf h ptr sz
                     return $ V.unsafeFromForeignPtr0 fp (sz `quot` 12)

loadConf :: FilePath -> IO (Vector (V3 Float))
loadConf = fmap (either (error.show) id) . flip PLY.loadMeshesV3 "vertex"

loadPLY :: FilePath -> IO (Vector (V3 Float))
loadPLY = fmap (either error id . (PLY.loadPLY >=> PLY.loadElementsV3 "vertex")) 
        . BS.readFile
