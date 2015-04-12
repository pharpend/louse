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
-- Module      : Data.Decamp.Initialize
-- Description : Module to initialize Decamp in directory.
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX
-- 

module Data.Decamp.Initialize where

import           Control.Monad (mzero)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Decamp.Internal
import           Data.Decamp.Types
import           Data.List.Utils
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml
import           Paths_decamp
import           System.Directory
import           System.IO
import           Text.Editor


-- |Get the project from the user, and then write it
-- 
-- @
-- initialize = getProject >>= writeProject
-- @
initialize :: IO ()
initialize = getProject >>= writeProject

-- |Get the project from the user
getProject :: IO Project
getProject =
  getProjectBS >>=
  \pbs -> case decodeEither pbs of
    Left err -> fail err
    Right (NewProject nom mtrs hp descr) -> do
      newNom <- if | nom == _repl_working_dir -> T.pack . last . split "/" <$> getCurrentDirectory
                   | otherwise -> pure nom
      pure $ Project newNom (mtnrToPerson <$> mtrs) hp descr []

-- |Get the YAML representation of the project from the user
-- 
-- @
-- getProjectBS =
--   editTemplate _templ_prj_path >>=
--   runUserEditorDWIM yamlTemplate
-- @
getProjectBS :: IO ByteString
getProjectBS =
  editTemplate _templ_prj_path >>=
  runUserEditorDWIM yamlTemplate

