-- decamp-bugtrack - distributed bugtracker
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
-- Description : Runs decamp-bugtrack
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Main where

import Control.Applicative
import Data.Monoid
import Options.Applicative
import Options.Applicative.Arrows

data Args =
  Init {bare :: Bool
       ,interactive :: Bool}
  deriving (Show)

greet :: Args -> IO ()
greet = print

infoHelp :: Parser a -> InfoMod a -> ParserInfo a
infoHelp a = info (helper <*> a)

initCommand :: ParserInfo Args
initCommand = infoHelp initParser fullDesc

initParser :: Parser Args
initParser =
  hsubparser $
  command "init" initOptionsInfo

initOptionsInfo :: ParserInfo Args
initOptionsInfo = infoHelp initOptions fullDesc

initOptions :: Parser Args
initOptions =
  Init <$>
  switch (long "bare" <>
          help "bare") <*>
  switch (short 'i' <>
          help "interactive")


main :: IO ()
main = execParser initCommand >>= greet
