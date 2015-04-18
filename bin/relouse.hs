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
-- Module      : Main
-- Description : Runs louse
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This is some of the most horrible code I have ever written. Please
-- don't judge me.

module Main where

import           Data.Default
import           Data.Foldable
import           Data.IORef
import           Data.Monoid
import           Data.Traversable
import           System.Posix.Env.ByteString

data ReLouse = ReLouse { _quiet :: Bool, _to :: String }

instance Default HelloOptions where
  def = HelloOptions False False

type OptionPart a = [ByteString] -> (IORef a) -> IO ()
type OptionMorph a = (a -> a)

runArgs :: OptionPart -> IO ()

matchAny :: [ByteString] -> OptionMorph a -> OptionPart a
matchAny 

main :: IO ()
main = runArgs $ do 
  on anyOf ["-q", "--quiet"] $ switch quiet
  on anyOf ["-t", "--to"] $ _to `setTo` theMetavar
  -- matchAny ["-t", "--to"] $ do
  --   with aMetavar $ set _to
  --   without aMetavar $ fail mzero
  
