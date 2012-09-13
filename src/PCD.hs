{-# LANGUAGE ScopedTypeVariables #-}
-- |Parser for PCD (point cloud data) files. Also provides a facility
-- for converting from ASCII to binary formatted point data.
module PCD where
import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad (when)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy as ATL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.Storable (Storable, sizeOf)
import System.IO (Handle, openFile, hClose, 
                  IOMode(..), withBinaryFile, hPutBuf, hGetBuf)
import CommonTypes
import Header

-- testFile,testFileB :: FilePath
-- testFile = "/Users/acowley/Documents/Projects/PcdViewer/etc/LAB.pcd"
-- testFileB = "/Users/acowley/Documents/Projects/PcdViewer/etc/LAB_bin.pcd"
-- testFile = "/Users/acowley/Documents/Projects/PcdViewer/etc/levine_4th_floor-1.pcd"
-- testFileB = "/Users/acowley/Documents/Projects/PcdViewer/etc/levine_4th_floor-1_bin.pcd"

readAsciiPoints :: Storable a => Header -> Handle -> ATL.Parser a -> 
                   IO (Vector a)
readAsciiPoints pcd h p = aux <$> TL.hGetContents h
  where n = {- min 100000 $ -}fromIntegral $ pcd^.points
        aux t0 = V.create $
                 do v <- VM.new n
                    let write = VM.write v
                        go i t
                          | i == n = return v
                          | otherwise = case ATL.parse p t of
                                          ATL.Done t' pt -> write i pt >> 
                                                            go (i+1) t'
                                          ATL.Fail _ _ msg -> error msg
                    go 0 t0

{-
-- This isn't work for me on OS X
readBinPoints pcd f offset = unsafeMMapVector f $
                             Just (offset, fromIntegral $ pcd^.points)
-}
readBinPoints :: forall a. Storable a => Header -> Handle -> IO (Vector a)
readBinPoints pcd h = do vm <- VM.new (fromIntegral $ pcd^.points)
                         _ <- VM.unsafeWith vm (flip (hGetBuf h) numBytes)
                         V.freeze vm
  where numBytes = fromIntegral (pcd^.points) * sizeOf (undefined::a)

-- Read point data given an ascii and a binary parser for the point
-- data type.
readPointData :: Storable a => 
                 Header -> Handle -> ATL.Parser a -> 
                 IO (Either String (Vector a))
readPointData hd h pa 
  | hd^.format == ASCII = readAsciiPoints hd h pa >>= return . Right
  | otherwise = Right <$> readBinPoints hd h

readXYZ_ascii :: ATL.Parser (V3 Float)
readXYZ_ascii = (\[x,y,z] -> V3 x y z) <$> 
                count 3 ((realToFrac <$> double) <* skipSpace)

-- readXYZ_bin :: AB.Parser (V3 Float)
-- readXYZ_bin = (\[x,y,z] -> V3 x y z) <$> count 3 float
--   where float :: AB.Parser Float
--         float = unsafeCoerce <$> anyWord32le

lowestPoint :: Vector (V3 Float) -> Float
lowestPoint = V.minimum . V.map (\(V3 _ y _) -> y)

asciiToBinary :: FilePath -> FilePath -> IO ()
asciiToBinary i o = do h <- openFile i ReadMode
                       (pcdh,_) <- readHeader h
                       pcdh `deepseq` print pcdh
                       when ((pcdh^.format) /= ASCII)
                            (error "Input PCD is already binary!")
                       v <- readAsciiPoints pcdh h readXYZ_ascii
                       putStrLn $ "Got points: " ++ show (V.length v)
                       hClose h
                       let pcdh' = format.~Binary $ pcdh
                           sz = sizeOf (undefined::V3 Float) * V.length v
                       print pcdh'
                       T.writeFile o (writeHeader pcdh')
                       withBinaryFile o AppendMode $ \h' ->
                         V.unsafeWith v (flip (hPutBuf h') sz)

loadPCD :: FilePath -> IO (Vector (V3 Float))
loadPCD pcdFile = do h <- openFile pcdFile ReadMode
                     (pcdh,_) <- readHeader h
                     r <- pcdh `deepseq` 
                          readPointData pcdh h readXYZ_ascii
                     either putStrLn (print . V.length) r
                     hClose h
                     print pcdh
                     case r of 
                       Right v -> let zs = V.map (\(V3 _ _ z) -> z) v
                                  in do putStrLn $ "Z ranges from "++
                                                    show (V.minimum zs)++
                                                   " to "++show (V.maximum zs)
                                        return v
                       Left _ -> return V.empty
