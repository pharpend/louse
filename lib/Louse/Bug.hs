-- louse - distributed bugtracker
-- Copyright (c) 2015, Peter Harpending.
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
-- Module      : Louse.Bug
-- Description : The type for a Louse Bug
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- The type for a louse bug.

module Louse.Bug where

import Louse.Comment
import Louse.Description
import Louse.Person
import Louse.Title

import Data.Time

-- |The type for a bug
-- 
-- Since: 0.1.0.0
data Bug =
  Bug {bugTitle :: Title
      ,bugDescription :: Description
      ,bugAuthor :: Author
      ,bugTime :: UTCTime
      ,bugComments :: [Comment]}
  deriving (Eq,Show)

-- |'Bug' is trivially an instance of 'FromBug'
-- 
-- Since: 0.1.0.0
instance FromBug Bug where
  fromBug = id

-- |'Bug' is trivially an instance of 'ToBug'
instance ToBug Bug where
  toBug = id

-- |Typeclass to convert something to a 'Bug'
class ToBug a  where
  toBug :: a -> Bug
  
-- |Convert something from a 'Bug'
-- 
-- Since: 0.1.0.0
class FromBug a where
  fromBug :: Bug -> a
