module PCD (main) where
import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.ByteString as AB
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy as ATL
import Data.Attoparsec.Binary
import qualified Data.ByteString as B
import qualified Data.Text.Lazy.IO as TL
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.Storable (Storable)
import System.IO (Handle, openFile, hClose, IOMode(..))
import Unsafe.Coerce

import CommonTypes
import Header

testFile :: FilePath
testFile = "etc/LAB.pcd"

readAsciiPoints :: Storable a => Header -> Handle -> ATL.Parser a -> 
                   IO (Vector a)
readAsciiPoints pcd h p = aux <$> TL.hGetContents h
  where n = fromIntegral $ pcd^.points
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

-- Read point data given an ascii and a binary parser for the point
-- data type.
readPointData :: Storable a => 
                 Header -> Handle -> (ATL.Parser a, AB.Parser a) -> 
                 IO (Either String (Vector a))
readPointData hd h (pa,pb) 
  | hd^.format == ASCII = readAsciiPoints hd h pa >>= return . Right
  | otherwise = aux . AB.parse (V.fromList <$> count 5 pb) <$> B.hGetContents h
  where aux (AB.Done _ v) = Right v
        aux (AB.Partial _) = Left "Partial"
        aux (AB.Fail _ _ msg) = Left msg

readXYZ_ascii :: ATL.Parser (V3 Float)
readXYZ_ascii = (\[x,y,z] -> V3 x y z) <$> 
                count 3 ((realToFrac <$> double) <* skipSpace)

readXYZ_bin :: AB.Parser (V3 Float)
readXYZ_bin = (\[x,y,z] -> V3 x y z) <$> count 3 float
  where float :: AB.Parser Float
        float = unsafeCoerce <$> anyWord32le

test :: IO ()
test = do h <- openFile testFile ReadMode
          pcdh <- readHeader h
          r <- readPointData (fst pcdh) h (readXYZ_ascii, readXYZ_bin)
          either putStrLn (print . V.length) r
          hClose h
          print pcdh

main :: IO ()
main = test
