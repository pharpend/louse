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
-- Module      : Louse.Comment
-- Description : The type for a louse comment.
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC

module Louse.Comment where

import           Control.Exceptional
import           Crypto.Hash.SHA1 (hash)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as BH
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Tree as RT
import           Louse.Description
import           Louse.Person
import           Louse.Trees

-- |The type for a comment
-- 
-- Since: 0.1.0.0
data Comment =
  Comment 
  {commentAuthor :: Author
  ,commentText :: CommentText
  ,subComments :: [Comment]}
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

-- |Since: 0.1.0.0
instance ToTree Comment (Author,CommentText) where
  toTree comment_ =
    RT.Node (commentAuthor comment_, commentText comment_)
            (toForest $ subComments comment_)

-- |Since: 0.1.0.0
instance FromTree (Author,CommentText) Comment where
  fromTree (RT.Node (auth, commentTxt) subcomments) = 
    Comment auth commentTxt (fromForest subcomments)

-- |Take the sha1sum of some UTF-8 encoded text
sha1 :: Text -> Text
sha1 = TE.decodeUtf8 . BH.encode . hash . TE.encodeUtf8
