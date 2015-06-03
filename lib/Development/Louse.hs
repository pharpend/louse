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
-- Module      : Development.Louse
-- Description : The louse library
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Development.Louse
  ( -- * Creating pure-ish bugs
   Bug(..)
  ,Title
  ,mkTitle
  ,unTitle
  ,Description
  ,mkDescription
  ,unDescription
   -- * Converting to & from bugs
  ,ToBug(..)
  ,FromBug(..))
  where

import Control.Exceptional
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif
import Data.String (IsString(..))
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Tree

-- |The type for a bug
data Bug =
  Bug {bugTitle :: Title
      ,bugDescription :: Description
      ,bugAuthor :: Author
      ,bugTime :: UTCTime
      ,bugComments :: Forest Comment}
  deriving (Eq,Show)

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
newtype Title =
  Title {unTitle :: Text}
  deriving (Eq)

-- |Compares by the value of @unTitle@.
instance Ord Title where
  compare = comparing unTitle

instance Show Title where
  show = T.unpack . unTitle

-- |Note that this will throw an error if you give it an invalid value.
instance IsString Title where
  fromString s =
    case mkTitle (T.pack s) of
      Failure err -> error err
      Success s -> s

-- |Attempt to make a title, returning an error message if the length is
-- longer than 64 characters, or if the title is empty.
mkTitle :: Text -> Exceptional Title
mkTitle t
  | T.null t = fail "Title mustn't be empty."
  | 64 < T.length t = fail "Title mustn't be >64 characters long."
  | otherwise = return (Title t)
  
  
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
newtype Description =
  Description {unDescription :: Text}
  deriving (Eq)

-- |Compares by the value of 'unDescription'.
instance Ord Description where
  compare = comparing unDescription

instance Show Description where
  show = T.unpack . unDescription

-- |Note that this will throw an error if given invalid input.
instance IsString Description where
  fromString s =
    case mkDescription (T.pack s) of
      Failure foo -> error foo
      Success bar -> bar

-- |Attempt to make a description from a pure 'Text' value. This returns
-- an error if the description is empty.
mkDescription :: Text -> Exceptional Description
mkDescription t
  | T.null t = fail "Description mustn't be empty."
  | otherwise = return (Description t)

-- |Type for a person. Just has email and name
data Person =
  Person {personName :: Text
         ,personEmail :: Text}
  deriving (Eq)

-- | 
-- >>> Person "Joe Q. Public" "jqp@foo.bar.baz"
-- Joe Q. Public <jqp@foo.bar.baz>
-- it :: Person
instance Show Person where
  show (Person n e) = T.unpack (mconcat [n," <",e,">"])

-- |Alias for 'Person'
type Author = Person
-- |Alias for 'Person'
type Reporter = Person

-- |The type for a comment
data Comment =
  Comment {commentAuthor :: Author
          ,commentText :: CommentText}
  deriving (Eq,Show)

-- |Comment text has the same requirements as a 'Description', so alias
-- the two:
type CommentText = Description

-- |Typeclass to convert something to a 'Bug'
class ToBug a  where
  toBug :: a -> Bug
  
-- |'Bug' is trivially an instance of 'ToBug'
instance ToBug Bug where
  toBug = id

-- |Convert something from a 'Bug'
class FromBug a where
  fromBug :: Bug -> a

-- |'Bug' is trivially an instance of 'FromBug'
instance FromBug Bug where
  fromBug = id
