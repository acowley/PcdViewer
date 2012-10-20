{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts, 
             BangPatterns #-}
-- |Define a data structure for a PCD file header and an associated
-- parser.
module Header where
import Control.Applicative
import Control.Arrow ((***))
import SmallLens
import Control.Monad.State
import Data.Foldable (Foldable, foldMap)
import Data.List (intersperse)
import Data.Monoid (mconcat, (<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO
import Data.Attoparsec.Text hiding (I)
import System.IO (Handle)
import Control.DeepSeq
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

instance NFData Header where
  rnf (Header !_v !_f !_s !_d !_c !_w !_h !(!_t,!_r) !_p !_fmt) = ()

defaultHeader :: Header
defaultHeader = Header "" [] [] [] [] 0 0 (0, Quaternion 1 0) 0 ASCII

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
        q = fmap (\[w,i,j,k] -> Quaternion w (V3 i j k)) $ 
            count 4 (double <* skipSpace)

readFormat :: Parser DataFormat
readFormat = "DATA" .*> space *> 
             (("ascii" .*> pure ASCII) <|> ("binary" .*> pure Binary))

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

writeHeader :: Header -> Text
writeHeader h = (<> "\n") . mconcat . intersperse "\n" $
                [ "VERSION " <> (h^.version)
                , "FIELDS " <> joinFields (h^.fields)
                , "SIZE " <> joinFields (map tshow (h^.sizes))
                , "TYPE " <> joinFields (map tshow (h^.dimTypes))
                , "COUNT " <> joinFields (map tshow (h^.counts))
                , "WIDTH " <> tshow (h^.width)
                , "HEIGHT " <> tshow (h^.height)
                , "VIEWPOINT " <> (uncurry (<>) . (ftshow***((" "<>).ftshow)) $
                                   (h^.viewpoint))
                , "POINTS " <> tshow (h^.points)
                , "DATA " <> T.toLower (tshow (h^.format)) ]
  where joinFields = mconcat . intersperse " "
        tshow :: Show a => a -> Text
        tshow = T.pack . show
        ftshow :: (Foldable t, Show a) => t a -> Text
        ftshow = mconcat . intersperse " " . foldMap ((:[]) . tshow)