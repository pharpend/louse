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

-- | Module      : Data.Louse
--   Description : Umbrella module for louse library
--   Copyright   : Copyright (C) 2015 Peter Harpending
--   License     : GPL-3
--   Maintainer  : Peter Harpending <peter@harpending.org>
--   Stability   : experimental
--   Portability : UNIX/GHC
module Data.Louse where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bl
import qualified Data.Foldable as F
import qualified Data.HashMap.Lazy as H
import Data.List.Utils
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Traversable as Tr
import Data.Time
import Safe
import System.Directory

type TextMap = H.HashMap T.Text

-- |This is a function to read the Louse instance from a directory.
readLouseFromErr 
  :: FilePath -- ^The path to the project directory (i.e. NOT .louse)
  -> IO Louse -- ^The resulting 'Louse'
readLouseFromErr fp =
  let prjInfo = Bl.readFile (fp <> _project_json)
                  >>= \x -> case eitherDecode x of
                              Left err -> fail err
                              Right pi -> pure pi
  in Louse <$> prjInfo 
           <*> readBugsFromErr fp 
           <*> readPeopleFromErr fp


-- |Sort of an umbrella type for the various types.
data Louse =
  Louse {louseProjectInfo :: !(Maybe ProjectInfo)
        ,louseBugs :: H.HashMap T.Text Bug
        ,lousePeople :: H.HashMap T.Text Person}
  deriving (Eq,Show)

-- |Information about the project.
data ProjectInfo = ProjectInfo { projectName :: Maybe T.Text
                               , projectMaintainers :: Maybe [Person]
                               , projectHomepage :: Maybe T.Text
                               , projectDescription :: Maybe T.Text
                               }
  deriving (Eq, Show)

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
                                            , _project_maintainers .= mtrs
                                            , _project_homepage .= hp
                                            , _project_description .= dscr
                                            ]
  
-- |A bug.
data Bug = Bug { bugReporter :: Maybe Person
               , bugCreationDate :: UTCTime
               , bugTitle :: T.Text
               , bugDescription :: T.Text
               , bugOpen :: Bool
               , bugComments :: [Comment]
               }
  deriving (Eq, Show)

instance FromJSON Bug where
  parseJSON (Object v) = Bug <$> v .:? _bug_reporter
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
data Person = Person { personName  :: T.Text -- ^The person's name
                     , personEmail :: T.Text -- ^Their email
                     }
  deriving (Eq,Show)

instance FromJSON Person where
  parseJSON (Object v) = Person <$> v .: _person_name 
                                <*> v .: _person_email
  parseJSON _ = mzero

instance ToJSON Person where
  toJSON (Person n e) = 
    object [_person_name .= n,_person_email .= e]

data Comment =
       Comment
         { commentPerson :: Maybe Person
         , commentDate :: UTCTime
         , commentText :: T.Text
         }
  deriving (Eq, Show)

instance FromJSON Comment where
  parseJSON (Object v) = do
    person <- v .:? _comment_person
    date   <- v .:  _comment_date
    txt    <- v .:  _comment_text
    if | T.length txt > 512 -> fail 
          "Comment length may not be longer than 512 characters."
    pure $ Comment person date txt
  parseJSON _ = mzero

instance ToJSON Comment where
  toJSON (Comment p d t) = object
    [ _comment_person .= p
    , _comment_date   .= d
    , _comment_text   .= t
    ]

-- |These are all magic values of the paths in the louse hierarchy.
_louse_dir    = ".louse/"
_project_json = _louse_dir <> "project.yaml"
_bugs_dir     = _louse_dir <> "bugs"
_people_dir   = _louse_dir <> "people"

-- ^These are all magic values used in JSON serialization.
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

-- |Lazily reads the bugs.
readBugsFromErr 
  :: FilePath                  -- ^The path to the project directory
  -> IO (H.HashMap T.Text Bug) -- ^The resulting HashMap
readBugsFromErr fp = 
  readFromErr $ mappend fp _bugs_dir

-- |Lazily reads the bugs.
readPeopleFromErr 
  :: FilePath                  -- ^The path to the project directory
  -> IO (H.HashMap T.Text Person) -- ^The resulting HashMap
readPeopleFromErr fp = 
  readFromErr $ mappend fp _people_dir

-- |Lazily reads files in a directory, returns a 'H.HashMap' of the name
-- of the file, along with the decoded value.
readFromErr 
  :: FromJSON t 
  => FilePath                   -- ^The directory holding the files
  -> IO (TextMap t)             -- ^The resulting HashMap
readFromErr directoryPath =
  H.fromList <$> (mapM mkHashMapMember =<< files)
  where
    files :: IO [FilePath]
    files = getDirectoryContents directoryPath

    -- This function constructs an individual element of the HashMap
    mkHashMapMember :: FromJSON t => FilePath -> IO (T.Text, t)
    mkHashMapMember filePath = do
      fcontents <- Bl.readFile filePath
      decodedValue <- case eitherDecode fcontents of
                        Left err -> fail err
                        Right x  -> pure x
      pure (deCanonicalize filePath, decodedValue)
    -- quux.yaml -> quux
    removeDot :: T.Text -> T.Text
    removeDot = T.dropEnd 5

    -- Split a string on "/"
    splitSlashes :: T.Text -> [T.Text]
    splitSlashes fp = T.splitOn "/" fp

    -- Takes a canonical filename: /foo/bar/baz/quux.yaml -> quux . Also converts to Text while it's at
    -- it.
    deCanonicalize :: FilePath -> T.Text
    deCanonicalize fp =
      case removeDot <$> lastMay (splitSlashes (T.pack fp)) of
        Just x  -> x
        Nothing -> T.pack fp
