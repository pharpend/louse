{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
-- Module      : Data.Louse.DataFiles
-- Description : Paths and interface to Louse's data files
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Louse.DataFiles where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as Bs
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as Bl
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.Text
import           Data.Monoid
import           Paths_louse
import           System.Directory (getAppUserDataDirectory)
import           System.Exit
import           Text.Editor

-- |These are all magic values of the paths in the louse hierarchy.
_app_name :: FilePath
_app_name = "louse"

_louse_dir         = mconcat ["/.", _app_name, "/"]
_project_json      = _louse_dir <> "project.yaml"
_bugs_dir          = _louse_dir <> "bugs/"
_people_dir        = _louse_dir <> "people/"

_config_path :: IO FilePath
_config_path =
  do dataDir <- getAppUserDataDirectory _app_name
     return (mappend dataDir "/config.yaml")

-- |Read a file lazily but efficiently
produceFile :: FilePath -> Producer (ResourceT IO) Bs.ByteString
produceFile fp =
  do x <- liftIO (getDataFileName fp)
     sourceFile x

-- |Alias for 'produceFile'
readDataFile :: FilePath -> Producer (ResourceT IO) Bs.ByteString
readDataFile = produceFile

type TemplatePath = String

-- |Path to template for new 'ProjectInfo'
_templ_new_project :: IO TemplatePath
_templ_new_project = getDataFileName "res/templates/new-project-info.yaml"

-- |Path to template for new 'Bug'
_templ_new_bug :: IO TemplatePath
_templ_new_bug = getDataFileName "res/templates/new-bug.yaml"
