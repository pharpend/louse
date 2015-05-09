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
-- Module      : Data.Louse.IO
-- Description : I/O operations for Louse
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Louse.IO
       (module Data.Louse.IO, module Data.Louse.IO.Bugs,
        module Data.Louse.IO.DataFiles, module Data.Louse.IO.Read)
       where

import           Control.Monad
import           Control.Monad.Trans.Resource (runResourceT)
import           Crypto.Random
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Base16 as Bs16
import           Data.Conduit (connect)
import           Data.Conduit.Binary (sinkFile)
import           Data.Louse.IO.DataFiles
import           Data.Louse.IO.Read
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           System.Directory
import           System.IO.Error

-- |Print the status to the screen
-- 
-- > status = putStr <=< statusStr
-- 
status :: FilePath -> IO ()
status = putStr <=< statusStr

initInDir :: FilePath -> Bool -> IO ()
initInDir dr force =
  do let dir = dr <> "/"
         rpath = mappend dir
     louseDirExists <- doesDirectoryExist $ rpath _louse_dir
     -- Try to read a louse
     eitherLouse <-
       tryIOError (readLouseFromErr dir)
     case eitherLouse of
       -- If we can read one, something has gone terribly wrong
       Right louse ->
         unless force
                (fail (mconcat ["Louse is already initialized in ",dir,"."]))
       Left err
       -- If we can't read one, that's good
         | isDoesNotExistError err -> pure ()
         | otherwise -> ioError err
     -- If all is well, carry on
     --
     -- Create the louse directory
     createDirectoryIfMissing True $
       rpath _louse_dir
     -- Write the "new project" template
     runResourceT
       (connect (readDataFile _templ_new_project)
                (sinkFile (mappend (rpath _project_json) ".sample")))
     -- Create the bugs directory and the people directory
     createDirectoryIfMissing True $
       rpath _bugs_dir
     createDirectoryIfMissing True $
       rpath _people_dir
