-- louse - distributed bugtracker
-- Copyright (C) 2015 Peter Harpending
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at
-- your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | 
-- Module      : Spec
-- Description : Runs the tests
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- Here is a longer description of this module, containing some
-- commentary with @some markup@.

module Main where

import Control.Monad (forM_)
import Data.Louse.Schemata
import Safe
import Test.Hspec

main :: IO ()
main =
  hspec (do describe "schemata"
                     (do specify "schemata dir should be an absolute path"
                                 (do sd <- schemataDir
                                     shouldBe (headMay sd)
                                              (Just '/'))
                         specify "all schemata files should be absolute paths"
                                 (do sfs <- schemataFiles
                                     forM_ sfs
                                           (\fp ->
                                              shouldBe (headMay fp)
                                                       (Just '/')))
                         specify "none of the schemata names should have a '/' in them"
                                 (do schemas <- schemata
                                     forM_ schemas
                                           (\scm ->
                                              shouldBe False (elem '/' scm)))))
