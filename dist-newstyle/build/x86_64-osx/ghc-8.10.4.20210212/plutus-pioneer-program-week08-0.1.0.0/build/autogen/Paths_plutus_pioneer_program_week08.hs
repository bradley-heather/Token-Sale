{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_plutus_pioneer_program_week08 (
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

bindir     = "/Users/bradleyheather/.cabal/bin"
libdir     = "/Users/bradleyheather/.cabal/lib/x86_64-osx-ghc-8.10.4.20210212/plutus-pioneer-program-week08-0.1.0.0-inplace"
dynlibdir  = "/Users/bradleyheather/.cabal/lib/x86_64-osx-ghc-8.10.4.20210212"
datadir    = "/Users/bradleyheather/.cabal/share/x86_64-osx-ghc-8.10.4.20210212/plutus-pioneer-program-week08-0.1.0.0"
libexecdir = "/Users/bradleyheather/.cabal/libexec/x86_64-osx-ghc-8.10.4.20210212/plutus-pioneer-program-week08-0.1.0.0"
sysconfdir = "/Users/bradleyheather/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "plutus_pioneer_program_week08_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "plutus_pioneer_program_week08_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "plutus_pioneer_program_week08_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "plutus_pioneer_program_week08_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "plutus_pioneer_program_week08_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "plutus_pioneer_program_week08_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
