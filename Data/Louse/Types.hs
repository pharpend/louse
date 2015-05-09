-- louse - distributed bugtracker
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
-- Module      : Data.Louse.Types
-- Description : The types for Louse's bug tracker
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : Linux/GHC
-- 

module Data.Louse.Types where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Aeson
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as Tse
import           Data.Time

type BugId = T.Text
type IdMap = M.Map T.Text

-- |Sort of an umbrella type for the various types.
data Louse =
       Louse
         { workingDirectory :: FilePath
         , louseProjectInfo :: !(Maybe ProjectInfo)
         , louseBugs :: IdMap Bug
         }
  deriving (Eq, Show)

-- |Information about the project.
data ProjectInfo =
  ProjectInfo {projectName :: Maybe T.Text
              ,projectMaintainers :: Maybe [Person]
              ,projectHomepage :: Maybe T.Text
              ,projectDescription :: Maybe T.Text}
  deriving (Eq,Show)

instance FromJSON ProjectInfo where
  parseJSON (Object v) = ProjectInfo <$> v .:? _project_name
                                     <*> v .:? _project_maintainers
                                     <*> v .:? _project_homepage
                                     <*> v .:? _project_description
  parseJSON _ =
    fail "Project information must be in the form of a JSON object"

instance ToJSON ProjectInfo where
  toJSON (ProjectInfo nom mtrs hp dscr) = object
                                            [ _project_name .= nom
                                            , _project_maintainers .=  mtrs
                                            , _project_homepage .= hp
                                            , _project_description .= dscr
                                            ]
  
-- |A bug.
data Bug =
  Bug {bugReporter :: Person
      ,bugCreationDate :: UTCTime
      ,bugTitle :: T.Text
      ,bugDescription :: T.Text
      ,bugOpen :: Bool
      ,bugComments :: [Comment]}
  deriving (Eq,Show)

instance FromJSON Bug where
  parseJSON (Object v) = Bug <$> v .: _bug_reporter
                             <*> v .: _bug_creation_date
                             <*> v .: _bug_title
                             <*> v .: _bug_description
                             <*> v .: _bug_open
                             <*> v .: _bug_comments
  parseJSON _ = fail "Bug information must be in the form of a JSON object"

instance ToJSON Bug where
  toJSON bg = object
                [ _bug_reporter .= bugReporter bg
                , _bug_creation_date .= bugCreationDate bg
                , _bug_title .= bugTitle bg
                , _bug_description .= bugDescription bg
                , _bug_open .= bugOpen bg
                , _bug_comments .= bugComments bg
                ]
  
-- |This is a person.
data Person
  = Person {personName :: T.Text -- ^The person's name
           ,personEmail :: T.Text -- ^Their email
           }
  | Anonymous
  deriving (Eq,Show)

instance FromJSON Person where
  parseJSON (Object v) =
    (<|>) (ap (fmap Person (v .: _person_name))
              (v .: _person_email))
          (pure Anonymous)
  parseJSON Null = pure Anonymous
  parseJSON _ = mzero

instance ToJSON Person where
  toJSON (Person n e) = 
    object [_person_name .= n,_person_email .= e]

data Comment =
  Comment {commentPerson :: Person
          ,commentDate :: UTCTime
          ,commentText :: T.Text}
  deriving (Eq,Show)

instance FromJSON Comment where
  parseJSON (Object v) =
    do person <- v .: _comment_person
       date <- v .: _comment_date
       txt <- v .: _comment_text
       pure (Comment person date txt)
  parseJSON _ = mzero

instance ToJSON Comment where
  toJSON (Comment p d t) = object
    [ _comment_person .= p
    , _comment_date   .= d
    , _comment_text   .= t
    ]

-- |These are all magic values used in JSON serialization.
-- 
-- > _bug_title = "bug-title"
-- 
-- It's to prevent mistakes. (Or rather, it's so GHC catches the
-- mistakes)
_bug_reporter        = "bug-reporter"
_bug_creation_date   = "bug-creation-date"
_bug_title           = "bug-title"
_bug_description     = "bug-description"
_bug_open            = "bug-open"
_bug_comments        = "bug-comments"
_comment_person      = "comment-person"
_comment_date        = "comment-date"
_comment_text        = "comment-text"
_person_name         = "person-name"
_person_email        = "person-email"
_project_name        = "project-name"
_project_maintainers = "project-maintainers"
_project_homepage    = "project-homepage"
_project_description = "project-description"
