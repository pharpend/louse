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
-- Module      : Data.Louse.Project
-- Description : Louse Projects
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Louse.Project where

import           Data.Louse.Internal
import           Data.Louse.Types
import           Data.Monoid
import           System.Directory

-- |Try to parse @./.louse/project.json@. Returns @Left "error
-- message"@ if it fails, and @Right Project@ if it succeeds.
getCurrentProject :: IO (Either String Project)
getCurrentProject =
  doesFileExist _project_json >>=
  \case
    True -> decodeProjectFileEither _project_json
    False -> pure . Left $ "File not found: " <> _project_json

-- |Try parse to @./.louse/project.json@. Fails if there is an error.
-- 
-- @
-- getCurrentProjectErr =
--   getCurrentProject >>= \case
--     Left err  -> fail err
--     Right prj -> pure prj
-- @
getCurrentProjectErr :: IO Project
getCurrentProjectErr =
  getCurrentProject >>= \case
    Left err  -> fail err
    Right prj -> pure prj

-- |Try parse to @./.louse/project.json@.
-- 
-- @
-- getCurrentProjectMaybe =
--   flip fmap getCurrentProject $ \case
--     Left _    -> Nothing
--     Right prj -> Just prj
-- @
getCurrentProjectMaybe :: IO (Maybe Project)
getCurrentProjectMaybe =
  flip fmap getCurrentProject $ \case
    Left _    -> Nothing
    Right prj -> Just prj
