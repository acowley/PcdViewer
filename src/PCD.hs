{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
module PCD where
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Lens
import qualified Data.Attoparsec.Types as A
import qualified Data.Attoparsec.ByteString as AB
import Data.Attoparsec.Text hiding (I)
import qualified Data.Attoparsec.Text.Lazy as ATL
import Data.Attoparsec.Binary
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import System.IO (Handle, openFile, hClose, IOMode(..))
import Control.Monad.State
import Unsafe.Coerce

data V3 a = V3 a a a deriving (Eq,Show,Ord)
data Quaternion a = Q a a a a deriving (Eq,Show,Ord)
data DimType = I | U | F deriving (Eq,Show,Ord)
data DataFormat = ASCII | Binary deriving (Eq,Show,Ord)

data Header = Header { _version :: Text 
                     , _fields :: [Text]
                     , _sizes :: [Int]
                     , _dimTypes :: [DimType]
                     , _counts :: [Int]
                     , _width :: Integer
                     , _height :: Int
                     , _viewpoint :: (V3 Double, Quaternion Double)
                     , _points :: Integer
                     , _format :: DataFormat } deriving Show
makeLenses ''Header

defaultHeader :: Header
defaultHeader = Header "" [] [] [] [] 0 0 (V3 0 0 0, Q 1 0 0 0) 0 ASCII

testFile :: FilePath
testFile = "etc/LAB.pcd"

readVersion :: Parser Text
readVersion = "VERSION" .*> space *> takeText

readFields :: Parser [Text]
readFields = "FIELDS" .*> space *> (fmap T.words takeText)

readSizes :: Parser [Int]
readSizes = "SIZE" .*> space *> sepBy decimal space

readTypes :: Parser [DimType]
readTypes = "TYPE" .*> space *> sepBy t space
  where t = "I" .*> return I <|> "U" .*> return U <|> "F" .*> return F

readCounts :: Parser [Int]
readCounts = "COUNT" .*> space *> sepBy decimal space

readWidth :: Parser Integer
readWidth = "WIDTH" .*> space *> decimal

readHeight :: Parser Int
readHeight = "HEIGHT" .*> space *> decimal

readViewpoint :: Parser (V3 Double, Quaternion Double)
readViewpoint = "VIEWPOINT" .*> space *> ((,) <$> v <*> q)
  where v = fmap (\[x,y,z] -> V3 x y z) $ count 3 (double <* skipSpace)
        q = fmap (\[w,i,j,k] -> Q w i j k) $ count 4 (double <* skipSpace)

readPoints :: Parser Integer
readPoints = "POINTS" .*> space *> decimal

readFormat :: Parser DataFormat
readFormat = "DATA" .*> space *> 
             ("ascii" .*> pure ASCII) <|> ("binary" .*> pure Binary)

nextLine :: Handle -> IO Text
nextLine h = do t <- T.hGetLine h 
                either (const $ return t) (const $ nextLine h) $ 
                       parseOnly isComment t
  where isComment = string "#"

readHeader :: Handle -> IO (Header, Maybe Text)
readHeader h = flip execStateT (defaultHeader, Nothing) $ 
                  sequence_ [ entry readVersion version
                            , entry readFields fields
                            , entry readSizes sizes
                            , entry readTypes dimTypes
                            , entry readCounts counts
                            , entry readWidth width
                            , entry readHeight height
                            , entry readViewpoint viewpoint 
                            , entry readPoints points
                            , entry readFormat format ]
  where nxt :: (MonadState (a,Maybe Text) m, MonadIO m) => m ()
        nxt = liftIO (nextLine h) >>= (_2.=) . Just
        entry :: (MonadState (s,Maybe Text) m, MonadIO m, Functor m) => 
                 Parser a -> Setting s s a a -> m ()
        entry parser field = do use _2 >>= maybe nxt (const (return ()))
                                Just ln <- use _2
                                case parseOnly parser ln of
                                  Left _ -> return ()
                                  Right v -> _1 . field .= v >> _2.=Nothing

-- Read point data given an ascii and a binary parser for the point data type.
readPointData :: Header -> Handle -> (ATL.Parser a, AB.Parser a) -> 
                 IO (Either String (Vector a))
readPointData hd h (pa,pb) 
  | hd^.format == ASCII = aux2 . ATL.parse (V.fromList <$> count 5 pa) <$> TL.hGetContents h
  | otherwise = aux . AB.parse (V.fromList <$> count 5 pb) <$> B.hGetContents h
  -- | hd^.format == ASCII = aux2 . ATL.parse (V.fromList <$> count w pa) <$> TL.hGetContents h
  -- | otherwise = aux . AB.parse (V.fromList <$> count w pb) <$> B.hGetContents h
  where aux (Done _ v) = Right v
        aux (Partial _) = Left "Partial"
        aux (Fail _ _ msg) = Left msg
        aux2 (ATL.Done _ v) = Right v
        aux2 (ATL.Fail _ _ msg) = Left msg
        -- w = fromIntegral $ hd^.width

readXYZ_ascii :: ATL.Parser (V3 Double)
readXYZ_ascii = (\[x,y,z] -> V3 x y z) <$> count 3 (double <* skipSpace)

readXYZ_bin :: A.Parser ByteString (V3 Double)
readXYZ_bin = (\[x,y,z] -> V3 x y z) <$> count 3 (realToFrac <$> float)
  where float :: AB.Parser Float
        float = unsafeCoerce <$> anyWord32le

test :: IO ()
test = do h <- openFile testFile ReadMode
          pcdh <- readHeader h
          r <- readPointData (fst pcdh) h (readXYZ_ascii, readXYZ_bin)
          either putStrLn (print . V.length) r
          hClose h
          print pcdh
