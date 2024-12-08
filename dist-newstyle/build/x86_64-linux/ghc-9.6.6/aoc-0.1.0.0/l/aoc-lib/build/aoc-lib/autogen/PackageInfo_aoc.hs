{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_aoc (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "aoc"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Advent Of Code 2024\nREADME.md"
copyright :: String
copyright = "2024  Trent Turner"
homepage :: String
homepage = ""
