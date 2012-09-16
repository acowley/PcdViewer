{-# LANGUAGE CPP #-}
module MyPaths (getDataFileName) where
#ifdef CABAL
import qualified Paths_PcdViewer as P
getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
#else
import System.FilePath

projRoot :: FilePath
projRoot = "/Users/acowley/Documents/Projects/PcdViewer"

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (projRoot </>)
#endif


