{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
   -- *** Comment trees
  ,CommentTree
  ,unCommentTree
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

import Control.Exceptional
import Crypto.Hash.SHA1
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as BH
import Data.Foldable (Foldable(..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif
import Data.String (IsString(..))
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Tree

-- |The type for a bug
-- 
-- Since: 0.1.0.0
data Bug =
  Bug {bugTitle :: Title
      ,bugDescription :: Description
      ,bugAuthor :: Author
      ,bugTime :: UTCTime
      ,bugComments :: CommentTree}
  deriving (Eq,Show)

-- |'Bug' is trivially an instance of 'FromBug'
-- 
-- Since: 0.1.0.0
instance FromBug Bug where
  fromBug = id

-- |'Bug' is trivially an instance of 'ToBug'
instance ToBug Bug where
  toBug = id
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

-- |The type for a comment
-- 
-- Since: 0.1.0.0
data Comment =
  Comment 
  {commentAuthor :: Author
  ,commentText :: CommentText
  ,subComments :: CommentTree}
  deriving (Eq,Show)

-- |Comment text has the same requirements as a 'Description', so alias
-- the two
-- 
-- Since: 0.1.0.0
type CommentText = Description
   
-- |Alias for 'mkDescription'
-- 
-- Since: 0.1.0.0
mkCommentText :: Text -> Exceptional CommentText
mkCommentText = mkDescription

-- |Alias for 'unDescription'
-- 
-- Since: 0.1.0.0
unCommentText :: CommentText -> Text
unCommentText = unDescription

-- |This is similar to a Tree from containers, except it's implemented
-- using lazy 'HashMap's.
-- 
-- Specifically, this is a newtype over 'HashMap' 'ByteString' 'Comment'. The idea being that the key 
-- 
-- Since: 0.1.0.0
newtype CommentTree =
  CommentTree {unCommentTree :: HashMap ByteString Comment}
  deriving (Eq,Show)
  
-- |Since: 0.1.0.0
instance ToForest CommentTree (Author,CommentText) where
  toForest commentTree =
    do (_,Comment auth txt subcomments) <-
         H.toList (unCommentTree commentTree)
       return (Node (auth,txt)
                    (toForest subcomments))

-- |Since: 0.1.0.0
instance FromForest (Author,CommentText) CommentTree where
  fromForest forest_ =
    CommentTree
      (mconcat (do Node (auth,commentTxt) subcomments <- forest_
                   let subcommentTree = fromForest subcomments
                       commentHash =
                         B16.decode (hash (TE.encodeUtf8 (unCommentText commentTxt)))
                   return (H.singleton commentHash
                                       (Comment auth commentTxt subcommentTree))))

-- |Typeclass to convert something to a 'Bug'
class ToBug a  where
  toBug :: a -> Bug
  
-- |Convert something from a 'Bug'
-- 
-- Since: 0.1.0.0
class FromBug a where
  fromBug :: Bug -> a

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
