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
-- Module      : Louse.Title
-- Description : The type for a louse title.
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- The type for a louse title

module Louse.Title where

import Control.Exceptional
import Data.Ord (comparing)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T

-- |A newtype over 'Text'. Haskell doesn't have dependent types, so I
-- have to use a hack called "smart constructors" to make sure 
-- 
-- > 0 < title_length <= 64
-- 
-- Use 'mkTitle' to make a title. Alternatively, you could turn on
-- OverloadedStrings, and use 'Title''s 'IsString' instance:
-- 
-- >>> :set -XOverloadedStrings 
-- >>> "hello" :: Title
-- Title {unTitle = "hello"}
-- it :: Title
-- 
-- Note that if you give invalid input, then there will be an error:
-- 
-- >>> "" :: Title
-- *** Exception: Title mustn't be empty.
-- >>> fromString (mconcat (replicate 50 "foo")) :: Title
-- *** Exception: Title mustn't be >64 characters long.
-- 
-- Since: 0.1.0.0
newtype Title =
  Title {unTitle :: Text}
  deriving (Eq)

-- |Compares by the value of @unTitle@.
-- 
-- Since: 0.1.0.0
instance Ord Title where
  compare = comparing unTitle

-- |Since: 0.1.0.0
instance Show Title where
  show = T.unpack . unTitle

-- |Note that this will throw an error if you give it an invalid value.
-- 
-- Since: 0.1.0.0
instance IsString Title where
  fromString s =
    case mkTitle (T.pack s) of
      Failure err -> error err
      Success s -> s

-- |Attempt to make a title, returning an error message if the length is
-- longer than 64 characters, or if the title is empty.
-- 
-- Since: 0.1.0.0
mkTitle :: Text -> Exceptional Title
mkTitle t
  | T.null t = fail "Title mustn't be empty."
  | 64 < T.length t = fail "Title mustn't be >64 characters long."
  | otherwise = return (Title t)
