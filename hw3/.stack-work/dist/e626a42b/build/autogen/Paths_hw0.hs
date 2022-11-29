{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hw0 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Noteb\\Downloads\\haskell-hw0\\.stack-work\\install\\bf8f7f09\\bin"
libdir     = "C:\\Users\\Noteb\\Downloads\\haskell-hw0\\.stack-work\\install\\bf8f7f09\\lib\\x86_64-windows-ghc-8.6.5\\hw0-0.1.0.0-KAKb1FLYPLj7uVm8ybZCK"
dynlibdir  = "C:\\Users\\Noteb\\Downloads\\haskell-hw0\\.stack-work\\install\\bf8f7f09\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\Noteb\\Downloads\\haskell-hw0\\.stack-work\\install\\bf8f7f09\\share\\x86_64-windows-ghc-8.6.5\\hw0-0.1.0.0"
libexecdir = "C:\\Users\\Noteb\\Downloads\\haskell-hw0\\.stack-work\\install\\bf8f7f09\\libexec\\x86_64-windows-ghc-8.6.5\\hw0-0.1.0.0"
sysconfdir = "C:\\Users\\Noteb\\Downloads\\haskell-hw0\\.stack-work\\install\\bf8f7f09\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw0_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw0_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw0_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw0_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw0_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw0_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
