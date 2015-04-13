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
-- Module      : Data.Louse.Trivia
-- Description : Trivia about louse,
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This has stuff like the license and copyright

module Data.Louse.Trivia where

import           Control.Monad ((<=<))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Louse.Internal
import           Data.Version (showVersion)
import           Paths_louse (getDataFileName, version)
import           System.IO (hSetBinaryMode, stdout)

-- |The copyright notice
louseCopyright :: IO ByteString
louseCopyright =  readDataFile "res/copyright.txt"

-- |The tutorial
louseTutorial :: IO ByteString
louseTutorial =  readDataFile "TUTORIAL.md"

-- |The license (GPLv3)
louseLicense :: IO ByteString
louseLicense = readDataFile "LICENSE"

-- |The readme
louseReadme :: IO ByteString
louseReadme = readDataFile "README.md"

-- |The version
louseVersion :: String
louseVersion = showVersion version

-- |Print one of the 'ByteString's from above
printOut :: IO ByteString -> IO ()
printOut b = do
  hSetBinaryMode stdout True
  B.hPut stdout =<< b

-- |Print the version
printVersion :: IO ()
printVersion = putStrLn $ louseVersion

