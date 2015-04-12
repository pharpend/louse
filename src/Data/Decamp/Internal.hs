-- -*- hindent-style: chris-done -*-

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
-- Module      : Data.Decamp.Internal
-- Description : Internal variables for Decamp
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- Stuff that's really only of use inside Decamp. You are welcome to
-- use this stuff, but I can't see why you would want to.

module Data.Decamp.Internal
       (module Data.Decamp.Internal.Aeson,
        module Data.Decamp.Internal.DataFiles,
        module Data.Decamp.Internal.MagicValues,
        module Data.Decamp.Internal.Types)
       where

import Data.Decamp.Internal.Aeson
import Data.Decamp.Internal.DataFiles
import Data.Decamp.Internal.MagicValues
import Data.Decamp.Internal.Types
