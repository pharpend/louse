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
import           System.Exit
import           Text.Editor

-- |These are all magic values of the paths in the louse hierarchy.
_louse_dir         = "/.louse/"
_project_json      = _louse_dir <> "project.yaml"
_bugs_dir          = _louse_dir <> "bugs/"
_people_dir        = _louse_dir <> "people/"
_templ_new_project = "res/templates/new-project.yaml"

-- |Read a data file
readDataFile :: FilePath -> Producer (ResourceT IO) Bs.ByteString
readDataFile fp = do
  x <- lift (lift (getDataFileName fp))
  sourceFile x

-- |Given the path to a template, give the template to the user, allow
-- him to edit it, and then return the edited template
-- 
-- >>> editTemplate _templ_new_project
-- 
-- This will open up the "new project" template in the user's $EDITOR.
editTemplate :: FilePath -> IO Bl.ByteString
editTemplate fp =
  do (exitcode,bytes) <-
       runResourceT
         (bracketConduit yamlTemplate
                         (readDataFile fp)
                         (toConsumer sinkLbs))
     case exitcode of
       ExitSuccess -> pure bytes
       x@(ExitFailure _) ->
         fail (mappend "Editor process failed with " (show x))
