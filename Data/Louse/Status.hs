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
-- Module      : Data.Louse.Status
-- Description : Stuff for the "louse status" command
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
module Data.Louse.Status where

import Data.Ratio
import Data.Louse.Read
import Data.Louse.Types
import qualified Data.Map as M
import System.Directory (getCurrentDirectory)
import System.Exit
import System.IO
import System.IO.Error

-- |Get the status
statusStr :: FilePath -> IO String
statusStr dir =
  do let errprint = hPutStrLn stderr
     louse <-
       (>>=) (tryIOError (readLouseFromErr dir))
             (\case
                Left err
                  | isDoesNotExistError err ->
                    do errprint (mappend "Oops! You don't appear to have a louse repository in " dir)
                       errprint "Hint: Try running `louse init`."
                       exitFailure
                  | isPermissionError err ->
                    do errprint (mappend "I got a permission error when trying to read the louse repo in "
                                         dir)
                       errprint "Do you have permission to read this directory?"
                       exitFailure
                  | isAlreadyInUseError err ->
                    do fail (mconcat ["Another process is using the louse repo in "
                                     ,dir
                                     ,". I don't know what to do about that, so I'm just going to quit."])
                  | otherwise -> ioError err
                Right l -> pure l)
     let bugs = louseBugs louse
         nTotalBugs = M.size bugs
         nOpenBugs =
           length (M.filter bugOpen bugs)
         ratioOf a b = round (a % b)
         closureRateStr
           | nTotalBugs == 0 = mempty
           | otherwise =
             mconcat ["Closure rate: "
                     ,show ((*) 100 (ratioOf (nTotalBugs - nOpenBugs) nTotalBugs))
                     ,"%."]
     pure (unlines [mappend "Louse directory: " dir
                   ,mappend "Open bugs: " (show nOpenBugs)
                   ,closureRateStr])


-- |Print the status to the screen
-- 
-- > status = putStr =<< statusStr =<< getCurrentDirectory
-- 
status :: IO ()
status = putStr =<< statusStr =<< getCurrentDirectory
