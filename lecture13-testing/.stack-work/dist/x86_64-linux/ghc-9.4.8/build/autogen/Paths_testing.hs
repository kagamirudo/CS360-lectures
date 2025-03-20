{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_testing (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/kagamirudo/CS360/lectures/lecture13-testing/.stack-work/install/x86_64-linux/1051c306eb4ca83350c729eebdcbf38f7cc5e13b09e6bc9bb0b090875a831c42/9.4.8/bin"
libdir     = "/home/kagamirudo/CS360/lectures/lecture13-testing/.stack-work/install/x86_64-linux/1051c306eb4ca83350c729eebdcbf38f7cc5e13b09e6bc9bb0b090875a831c42/9.4.8/lib/x86_64-linux-ghc-9.4.8/testing-0.1-HD5s5jE3deuDrSHybT10J9"
dynlibdir  = "/home/kagamirudo/CS360/lectures/lecture13-testing/.stack-work/install/x86_64-linux/1051c306eb4ca83350c729eebdcbf38f7cc5e13b09e6bc9bb0b090875a831c42/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/home/kagamirudo/CS360/lectures/lecture13-testing/.stack-work/install/x86_64-linux/1051c306eb4ca83350c729eebdcbf38f7cc5e13b09e6bc9bb0b090875a831c42/9.4.8/share/x86_64-linux-ghc-9.4.8/testing-0.1"
libexecdir = "/home/kagamirudo/CS360/lectures/lecture13-testing/.stack-work/install/x86_64-linux/1051c306eb4ca83350c729eebdcbf38f7cc5e13b09e6bc9bb0b090875a831c42/9.4.8/libexec/x86_64-linux-ghc-9.4.8/testing-0.1"
sysconfdir = "/home/kagamirudo/CS360/lectures/lecture13-testing/.stack-work/install/x86_64-linux/1051c306eb4ca83350c729eebdcbf38f7cc5e13b09e6bc9bb0b090875a831c42/9.4.8/etc"

getBinDir     = catchIO (getEnv "testing_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "testing_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "testing_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "testing_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "testing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "testing_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
