{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_projetoPFAzul (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/livia/projetoPFAzul/.stack-work/install/x86_64-linux/a0a82ec2db97203e96fe87fda4dcd551c3023c623834402ee5aeb383fabd9646/9.6.6/bin"
libdir     = "/home/livia/projetoPFAzul/.stack-work/install/x86_64-linux/a0a82ec2db97203e96fe87fda4dcd551c3023c623834402ee5aeb383fabd9646/9.6.6/lib/x86_64-linux-ghc-9.6.6/projetoPFAzul-0.1.0.0-7Sz07ku46XAEVYd8dDRvkw-projetoPFAzul"
dynlibdir  = "/home/livia/projetoPFAzul/.stack-work/install/x86_64-linux/a0a82ec2db97203e96fe87fda4dcd551c3023c623834402ee5aeb383fabd9646/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/livia/projetoPFAzul/.stack-work/install/x86_64-linux/a0a82ec2db97203e96fe87fda4dcd551c3023c623834402ee5aeb383fabd9646/9.6.6/share/x86_64-linux-ghc-9.6.6/projetoPFAzul-0.1.0.0"
libexecdir = "/home/livia/projetoPFAzul/.stack-work/install/x86_64-linux/a0a82ec2db97203e96fe87fda4dcd551c3023c623834402ee5aeb383fabd9646/9.6.6/libexec/x86_64-linux-ghc-9.6.6/projetoPFAzul-0.1.0.0"
sysconfdir = "/home/livia/projetoPFAzul/.stack-work/install/x86_64-linux/a0a82ec2db97203e96fe87fda4dcd551c3023c623834402ee5aeb383fabd9646/9.6.6/etc"

getBinDir     = catchIO (getEnv "projetoPFAzul_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "projetoPFAzul_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "projetoPFAzul_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "projetoPFAzul_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "projetoPFAzul_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "projetoPFAzul_sysconfdir") (\_ -> return sysconfdir)



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
