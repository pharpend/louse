{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

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
-- Module      : Louse.Person
-- Description : The type for a person in louse
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC

module Louse.Person where

#if !(MIN_VERSION_base (4,8,0))
import Data.Monoid
#endif
import Data.Text (Text)
import qualified Data.Text as T

-- |Type for a person. Just has email and name
-- 
-- Since: 0.1.0.0
data Person =
  Person {personName :: Text
         ,personEmail :: Text}
  deriving (Eq)

-- | 
-- >>> Person "Joe Q. Public" "jqp@foo.bar.baz"
-- Joe Q. Public <jqp@foo.bar.baz>
-- it :: Person
-- 
-- Since: 0.1.0.0
instance Show Person where
  show (Person n e) = T.unpack (mconcat [n," <",e,">"])

-- |Alias for 'Person'
-- 
-- Since: 0.1.0.0
type Author = Person

-- |Alias for 'Person'
-- 
-- Since: 0.1.0.0
type Reporter = Person
