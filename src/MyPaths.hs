{-# LANGUAGE CPP #-}
module MyPaths (getDataFileName) where
#ifdef CABALFOO
import qualified Paths_PcdViewer as P
getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
#else
import System.FilePath
root :: FilePath
root = "/Users/acowley/Documents/Projects/PcdViewer"

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (root </>)
#endif
