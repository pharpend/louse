-- decamp-bugtrack - distributed bugtracker
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
-- Module      : Data.Decamp.Types
-- Description : Decamp's bug tracker
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : Linux/GHC
-- 

module Data.Decamp.Types where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Maybe
import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (Text, pack)
import           Data.Time

data Person = Person { personName :: Text
                     , personEmail :: Text
                     }
  deriving Show

data Comment = Comment { commentPerson :: Maybe Person
                       , commentText :: Text
                       }
  deriving Show

data Bug = Bug { bugId :: Text
               , bugReporter :: Maybe Person
               , bugCreationDate :: UTCTime
               , bugTitle :: Text
               , bugDescription :: Maybe Text
               , bugOpen :: Bool
               , bugComments :: [Comment]
               }
  deriving Show

-- |Synonym for @bugReporter@
bugPerson :: Bug -> Maybe Person
bugPerson = bugReporter

data Project =
  Project {projectName :: Text
          ,projectMaintainer :: Maybe Person
          ,projectHomepage :: Maybe Text
          ,projectDescription :: Maybe Text
          ,projectBugs :: [Bug]}


