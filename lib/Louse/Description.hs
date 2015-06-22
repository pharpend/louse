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
-- Module      : Louse.Description
-- Description : The type for description fields in louse.
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC

module Louse.Description where

import           Control.Exceptional
import           Data.Ord (comparing)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T

-- |Yet another newtype over 'Text'. This is to make sure the
-- description is less than (or equal to) 8192 characters.
-- 
-- Use 'mkDescription' to make a description. This is an instance of
-- 'IsString', too, so, in pure code, you can just write plain strings,
-- and turn on the OverloadedStrings extension.
-- 
-- >>> :set -XOverloadedStrings 
-- >>> "hello" :: Description
-- Description {unDescription = "hello"}
-- it :: Description
-- 
-- If you give invalid input, then there will be an error:
-- 
-- >>> "" :: Description
-- *** Exception: Description mustn't be empty.
-- 
-- Since: 0.1.0.0
newtype Description =
  Description {unDescription :: Text}
  deriving (Eq)

-- |Compares by the value of 'unDescription'.
-- 
-- Since: 0.1.0.0
instance Ord Description where
  compare = comparing unDescription

-- |Since: 0.1.0.0
instance Show Description where
  show = T.unpack . unDescription

-- |Note that this will throw an error if given invalid input.
-- 
-- Since: 0.1.0.0
instance IsString Description where
  fromString s =
    case mkDescription (T.pack s) of
      Failure foo -> error foo
      Success bar -> bar

-- |Attempt to make a description from a pure 'Text' value. This returns
-- an error if the description is empty.
-- 
-- Since: 0.1.0.0
mkDescription :: Text -> Exceptional Description
mkDescription t
  | T.null t = fail "Description mustn't be empty."
  | otherwise = return (Description t)
