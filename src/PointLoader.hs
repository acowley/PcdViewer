{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module PointLoader where
import Control.Applicative
import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import qualified Data.Vector as B
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (castPtr)
import Foreign.Storable
import Linear.V3
import qualified PLY.Data as PLY
import qualified PLY.Types as PLY
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

-- Support for PLY vertices with position, normal, and color.
data SurfacePoint = SurfacePoint (V3 Float) (V3 Float) (V3 Word8)

instance Storable SurfacePoint where
  sizeOf _ = 27
  alignment _ = 27
  peek ptr = SurfacePoint <$> peek (castPtr ptr)
                          <*> peekByteOff (castPtr ptr) 12
                          <*> peekByteOff (castPtr ptr) 24
  poke ptr (SurfacePoint pos nor col) = do poke (castPtr ptr) pos
                                           pokeByteOff (castPtr ptr) 12 nor
                                           pokeByteOff (castPtr ptr) 24 col

loadPLYfancy :: FilePath -> IO (Vector SurfacePoint)
loadPLYfancy f = do Right h <- PLY.loadHeader f
                    putStrLn $ "Loaded PLY header: "
                    print h
                    let Right vs = PLY.loadElements "vertex" h
                    return $ aux vs
  where aux :: B.Vector (B.Vector PLY.Scalar) -> Vector SurfacePoint
        aux = V.convert . B.map (componentsToPt . B.toList)
        componentsToPt :: [PLY.Scalar] -> SurfacePoint
        componentsToPt cpts = 
          let [!x,!y,!z,!nx,!ny,!nz] = map PLY.unsafeUnwrap (take 6 cpts)
              [!r,!g,!b] = map PLY.unsafeUnwrap (drop 6 cpts)
          in SurfacePoint (V3 x y z) (V3 nx ny nz) (V3 r g b)