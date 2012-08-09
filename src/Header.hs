{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
-- Define a data structure for a PCD file header and an associated
-- parser.
module Header where
import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO
import Data.Attoparsec.Text hiding (I)
import System.IO (Handle)
import CommonTypes

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

readVersion :: Parser Text
readVersion = "VERSION" .*> space *> takeText

readFields :: Parser [Text]
readFields = "FIELDS" .*> space *> fmap T.words takeText

readTypes :: Parser [DimType]
readTypes = "TYPE" .*> space *> sepBy t space
  where t = "I" .*> return I <|> "U" .*> return U <|> "F" .*> return F

namedIntegral :: Integral a => Text -> Parser a
namedIntegral n = n .*> space *> decimal

namedIntegrals :: Integral a => Text -> Parser [a]
namedIntegrals n = n .*> space *> sepBy decimal space

readViewpoint :: Parser (V3 Double, Quaternion Double)
readViewpoint = "VIEWPOINT" .*> space *> ((,) <$> v <*> q)
  where v = fmap (\[x,y,z] -> V3 x y z) $ count 3 (double <* skipSpace)
        q = fmap (\[w,i,j,k] -> Q w i j k) $ count 4 (double <* skipSpace)

readFormat :: Parser DataFormat
readFormat = "DATA" .*> space *> 
             ("ascii" .*> pure ASCII) <|> ("binary" .*> pure Binary)

nextLine :: Handle -> IO Text
nextLine h = do t <- hGetLine h 
                either (const $ return t) (const $ nextLine h) $ 
                       parseOnly isComment t
  where isComment = string "#"

readHeader :: Handle -> IO (Header, Maybe Text)
readHeader h = flip execStateT (defaultHeader, Nothing) $ 
                  sequence_ [ entry readVersion version
                            , entry readFields fields
                            , entry (namedIntegrals "SIZE") sizes
                            , entry readTypes dimTypes
                            , entry (namedIntegrals "COUNT") counts
                            , entry (namedIntegral "WIDTH") width
                            , entry (namedIntegral "HEIGHT") height
                            , entry readViewpoint viewpoint 
                            , entry (namedIntegral "POINTS") points
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
