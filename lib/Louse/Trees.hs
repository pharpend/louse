{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
-- Module      : Louse.Trees
-- Description : Short description
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC

module Louse.Trees where

import Data.Foldable (Foldable(..))
import Data.Tree

-- |Convert something of type @foo@ to a 'Tree' of type @bar@.
-- 
-- Since: 0.1.0.0
class ToTree foo bar where
  toTree :: foo -> Tree bar

-- |Convert a 'Tree' of type @bar@s to something of type @foo@.
-- 
-- Since: 0.1.0.0
class FromTree bar foo where
  fromTree :: Tree bar -> foo

-- |Convert something of type @foo@ to a 'Forest' of @bar@s.
-- 
-- Since: 0.1.0.0
class ToForest foo bar where
  toForest :: foo -> Forest bar

-- |Since: 0.1.0.0
instance (ToTree foo bar,Foldable t) => ToForest (t foo) bar where
  toForest = foldMap (\baz -> [toTree baz])

-- |Convert a 'Forest' of type @bar@ to something of type @foo@.
-- 
-- Since: 0.1.0.0
class FromForest bar foo where
  fromForest :: Forest bar -> foo

-- |Since: 0.1.0.0
instance (FromTree bar foo) => FromForest bar [foo] where
  fromForest = map fromTree
