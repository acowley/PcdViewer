{-# LANGUAGE CPP #-}
module MyPaths (projRoot, getDataFileName) where
#ifdef CABALFOO
import qualified Paths_PcdViewer as P
getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
#else
import System.FilePath
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (projRoot </>)
#endif

projRoot :: FilePath
projRoot = "/Users/acowley/Documents/Projects/PcdViewer"

