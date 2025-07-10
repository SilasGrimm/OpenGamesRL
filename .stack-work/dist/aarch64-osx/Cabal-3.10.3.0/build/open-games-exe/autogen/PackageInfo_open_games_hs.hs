{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_open_games_hs (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "open_games_hs"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Haskell implementation of open games"
copyright :: String
copyright = "Jules Hedges, Andr\233 Videla, Philipp Zahn & other contributors"
homepage :: String
homepage = ""
