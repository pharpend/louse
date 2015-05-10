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
-- Module      : Data.Louse.Templates
-- Description : Templates
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- Louse has a number of template YAML files, which are sent to the
-- user's $EDITOR to be edited by the user. This module contains
-- functions to access and modify the templates.

module Data.Louse.Templates where

import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Louse.DataFiles
import Data.Louse.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Yaml
import Paths_louse
import System.Exit
import Text.Editor

-- |Alias for 'produceFile'
produceTemplate :: TemplatePath -> Producer (ResourceT IO) ByteString
produceTemplate = produceFile

-- |Given the path to a template, give the template to the user, allow
-- them to edit it, and then return the edited template
-- 
-- >>> editTemplate _templ_new_project
-- 
-- This will open up the "new project" template in the user's $EDITOR.
editTemplate :: FilePath -> Consumer ByteString (ResourceT IO) a -> IO a
editTemplate fp consumer =
  do (exitcode,bytes) <-
       runResourceT
         (bracketConduit yamlTemplate
                         (readDataFile fp)
                         consumer)
     case exitcode of
       ExitSuccess -> pure bytes
       x@(ExitFailure _) ->
         fail (mappend "Editor process failed with " (show x))
