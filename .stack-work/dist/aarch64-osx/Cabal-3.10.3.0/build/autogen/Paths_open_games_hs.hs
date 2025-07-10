{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_open_games_hs (
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
bindir     = "/Users/silasgrimm/BachelorThesis/code/OpenGames/open-games-engine/.stack-work/install/aarch64-osx/232fd9b97ddcfb142488b675d39bad3cb2bfa18763a9ec8268e59659c54cd413/9.6.5/bin"
libdir     = "/Users/silasgrimm/BachelorThesis/code/OpenGames/open-games-engine/.stack-work/install/aarch64-osx/232fd9b97ddcfb142488b675d39bad3cb2bfa18763a9ec8268e59659c54cd413/9.6.5/lib/aarch64-osx-ghc-9.6.5/open-games-hs-0.1.0.0-5oX1In7pQ12JzTN6JmjCBe"
dynlibdir  = "/Users/silasgrimm/BachelorThesis/code/OpenGames/open-games-engine/.stack-work/install/aarch64-osx/232fd9b97ddcfb142488b675d39bad3cb2bfa18763a9ec8268e59659c54cd413/9.6.5/lib/aarch64-osx-ghc-9.6.5"
datadir    = "/Users/silasgrimm/BachelorThesis/code/OpenGames/open-games-engine/.stack-work/install/aarch64-osx/232fd9b97ddcfb142488b675d39bad3cb2bfa18763a9ec8268e59659c54cd413/9.6.5/share/aarch64-osx-ghc-9.6.5/open-games-hs-0.1.0.0"
libexecdir = "/Users/silasgrimm/BachelorThesis/code/OpenGames/open-games-engine/.stack-work/install/aarch64-osx/232fd9b97ddcfb142488b675d39bad3cb2bfa18763a9ec8268e59659c54cd413/9.6.5/libexec/aarch64-osx-ghc-9.6.5/open-games-hs-0.1.0.0"
sysconfdir = "/Users/silasgrimm/BachelorThesis/code/OpenGames/open-games-engine/.stack-work/install/aarch64-osx/232fd9b97ddcfb142488b675d39bad3cb2bfa18763a9ec8268e59659c54cd413/9.6.5/etc"

getBinDir     = catchIO (getEnv "open_games_hs_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "open_games_hs_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "open_games_hs_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "open_games_hs_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "open_games_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "open_games_hs_sysconfdir") (\_ -> return sysconfdir)



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
