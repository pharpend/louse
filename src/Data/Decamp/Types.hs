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

import           Data.Text (Text)
import           Data.Time

-- |The most important data type: the data type for projects.
data Project =
       Project
         {
         -- |The project's name
         projectName :: Text
         -- |The 'Person' who maintains the project. (Can be anonymous).
         , projectMaintainer :: Maybe Person
         -- |Optional project home page
         , projectHomepage :: Maybe Text
         -- |Optional project description
         , projectDescription :: Maybe Text
         -- |List of 'Bug's associated with this project
         , projectBugs :: [Bug]
         }
  deriving Show

-- |Type for bugs
data Bug =
       Bug
         {
         -- |A unique id for the bug. I haven't decided how this is to be created.
         bugId :: Text
         -- |The person who reported the bug
         , bugReporter :: Maybe Person
         -- |The non-optional time at which the bug was created.
         , bugCreationDate :: UTCTime
         -- |The title of the bug
         , bugTitle :: Text
         -- |An optional description of the bug
         , bugDescription :: Maybe Text
         -- |Whether or not the bug is open
         , bugOpen :: Bool
         -- |'Comment's on the bug
         , bugComments :: [Comment]
         }
  deriving Show

-- |Type for a Person
data Person = Person { personName :: Text -- ^The person's name
                     , personEmail :: Text -- ^Their email
                     }
  deriving Show

-- |This is the type for a comment, usually on a 'Bug'. It can really
-- be a comment on anything.
data Comment = Comment {
-- |The person who made the comment (it can be anonymous)
commentPerson :: Maybe Person 
-- |The actual comment itself
                       , commentText :: Text           
                       }
  deriving Show


-- |Synonym for 'bugReporter'
bugPerson :: Bug -> Maybe Person
bugPerson = bugReporter

