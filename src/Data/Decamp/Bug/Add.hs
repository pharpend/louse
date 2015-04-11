-- decamp - distributed bugtracker
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
-- Module      : Data.Decamp.Bug.Add
-- Description : Add a bug.
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This is a module for all things having to do with adding a bug to a
-- project.

module Data.Decamp.Bug.Add where

import           Crypto.Hash.Whirlpool
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Base16 (encode)
import           Data.Decamp.Internal
import           Data.Decamp.Project
import           Data.Decamp.Types
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Time
import           Data.Yaml hiding (encode)

-- |If you are running an executable, this is probably what you want to use.
-- 
addBugToCurrentProject :: IO ()
addBugToCurrentProject = 
  writeProject =<< 
  addBug <$> getCurrentProjectErr <*> mkBug

-- |Pure function to add a 'Bug' to a 'Project'
addBug :: Project -> Bug -> Project
addBug p b = p { projectBugs = b : (projectBugs p) }

-- |Make a bug with @$EDITOR@
mkBug :: IO Bug
mkBug =
  editTemplate _templ_bug_path >>=
  \bugBytes -> case decodeEither bugBytes of
    Left err  -> fail err
    Right nbg -> parseNewBug nbg
