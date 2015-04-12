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
-- Module      : Data.Decamp.Internal.MagicValues
-- Description : Magic values for Decamp
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- These are constant values which are used throughout the program. I
-- don't like putting raw strings in function calls, because then it's
-- a pain in the ass to debug. Instead, I just put variables in this
-- module, and use the variables throughout.

module Data.Decamp.Internal.MagicValues where

import           Data.Monoid
import           Data.Text (Text)

-- |The path to ".decamp"
_decamp_dir :: FilePath
_decamp_dir = ".decamp"

-- |The path to the project.json file
_project_json :: FilePath
_project_json = _decamp_dir <> "/project.json"

-- |Magic value for "replace with working directory"
_repl_working_dir :: Text
_repl_working_dir = "REPL_WORKING_DIR"

-- |Template project path
_templ_prj_path :: FilePath
_templ_prj_path = "res/templates/new-project.yaml"

-- |Template bug path
_templ_bug_path :: FilePath
_templ_bug_path = "res/templates/new-bug.yaml"

-- |Length of random ident values
_ident_length :: Int
_ident_length = 20
