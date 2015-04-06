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
-- Module      : Data.Git.Decamp.BugTracker.Projectn
-- Description : Decamp's bug tracker
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : Linux/GHC
-- 

module Data.Git.Decamp.BugTracker.Project where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Time
import           Data.Yaml

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
          ,projectAuthor :: Maybe Person
          ,projectHomepage :: Maybe Text
          ,projectDescription :: Maybe Text
          ,projectBugs :: [Bug]}

encodeProject :: Project -> ByteString
encodeProject = encode

encodeProjectFile :: FilePath -> Project -> IO ()
encodeProjectFile = encodeFile

decodeProject :: ByteString -> Maybe Project
decodeProject = decode

decodeProjectFile :: FilePath -> IO (Maybe Project)
decodeProjectFile = decodeFile

decodeProjectEither :: ByteString -> Either String Project
decodeProjectEither = decodeEither

decodeProjectFileEither :: FilePath -> IO (Either String Project)
decodeProjectFileEither = decodeFile

instance FromJSON Project where
  parseJSON (Object v) = Project <$> v .: "project-name"
                                 <*> v .:? "project-author"
                                 <*> v .:? "project-homepage"
                                 <*> v .:? "project-description"
                                 <*> v .: "project-bugs"
  parseJSON _ = mzero

instance ToJSON Project where
  toJSON p = [ "project-name" .= projectName p
             , "project-author" .= projectAuthor p
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
