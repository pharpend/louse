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

import           Control.Monad (mzero)
import           Data.Aeson
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


-- The rest of the file contains orphan instances of 'Project',
-- 'Person', 'Comment', and 'Bug'.

instance FromJSON Project where
  parseJSON (Object v) = Project <$> v .: "project-name"
                                 <*> v .:? "project-maintainer"
                                 <*> v .:? "project-homepage"
                                 <*> v .:? "project-description"
                                 <*> v .: "project-bugs"
  parseJSON _ = mzero

instance ToJSON Project where
  toJSON p = object
               [ "project-name" .= projectName p
               , "project-maintainer" .= projectMaintainer p
               , "project-homepage" .= projectHomepage p
               , "project-description" .= projectDescription p
               , "project-bugs" .= projectBugs p
               ]
               

instance FromJSON Person where
  parseJSON (Object v) = Person <$> v .: "person-name" <*> v .: "person-email"
  parseJSON _ = mzero

instance ToJSON Person where
  toJSON (Person nom em) = object ["person-name" .= nom, "person-email" .= em]

instance FromJSON Comment where
  parseJSON (Object v) = Comment <$> v .:? "comment-person" <*> v .: "comment-text"
  parseJSON (String v) = pure $ Comment Nothing v
  parseJSON _ = mzero

instance ToJSON Comment where
  toJSON (Comment ps txt) = object ["comment-person" .= ps, "comment-text" .= txt]

instance FromJSON Bug where
  parseJSON (Object v) = Bug <$> v .: "bug-id"
                             <*> v .:? "bug-reporter"
                             <*> v .: "bug-creation-date"
                             <*> v .: "bug-title"
                             <*> v .: "bug-description"
                             <*> v .: "bug-open"
                             <*> v .: "bug-comments"
  parseJSON _ = mzero

instance ToJSON Bug where
  toJSON bug = object
                 [ "bug-id" .= bugId bug
                 , "bug-reporter" .= bugReporter bug
                 , "bug-creation-date" .= bugCreationDate bug
                 , "bug-title" .= bugTitle bug
                 , "bug-description" .= bugDescription bug
                 , "bug-open" .= bugOpen bug
                 , "bug-comments" .= bugComments bug
                 ]
