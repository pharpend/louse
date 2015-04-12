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
-- Module      : Data.Decamp.Internal.DataFiles
-- Description : Using Decamp's data files
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Decamp.Internal.DataFiles where

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Decamp.Internal.Aeson
import           Data.Decamp.Internal.MagicValues
import           Data.Decamp.Types
import           Paths_decamp
import           System.Directory
import           System.IO
import           Text.Editor

-- |Read a data file
-- 
-- @
-- readDataFile = B.readFile <=< getDataFileName
-- @
readDataFile :: FilePath -> IO ByteString
readDataFile = B.readFile <=< getDataFileName


-- |Given the path to a template, give the template to the user, allow
-- him to edit it, and then return the edited template
-- 
-- @
-- editTemplate fp =
--   readDataFile fp >>=
--   runUserEditorDWIM yamlTemplate
-- @
editTemplate :: FilePath -> IO ByteString
editTemplate fp =
  readDataFile fp >>=
  runUserEditorDWIM yamlTemplate


-- |Write the project to @$(pwd)\/.decamp\/project.json@
writeProject :: Project -> IO ()
writeProject p = do
  hSetBinaryMode stdout True
  createDirectory _decamp_dir
  encodeProjectFile _project_json p
