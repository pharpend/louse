{-# LANGUAGE CPP #-}

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
-- Module      : Louse
-- Description : The louse library
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This is the top-level module for the louse library. You only need to
-- import this module, everything else will automatically be
-- re-exported.
-- 
-- Since: 0.1.0.0

module Louse
  (-- *** Convenience re-exports
   module Control.Exceptional
   -- * Creating pure-ish bugs
  ,Bug(..)
   -- *** Bug titles
  ,Title
  ,mkTitle
  ,unTitle
   -- *** Bug descriptions
  ,Description
  ,mkDescription
  ,unDescription
   -- ** People
  ,Person(..)
  ,Author
  ,Reporter
   -- ** Comments
  ,Comment(..)
   -- *** Comment text
  ,CommentText
  ,mkCommentText
  ,unCommentText
   -- * Converting to & from bugs
  ,ToBug(..)
  ,FromBug(..)
   -- * Converting to & from trees
  ,ToTree(..)
  ,FromTree(..)
   -- ** Forests are just lists of trees
  ,ToForest(..)
  ,FromForest(..))
  where
  
import Louse.Bug
import Louse.Comment
import Louse.Person
import Louse.Trees

import Control.Exceptional
