-- decamp - distributed bugtracker
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
-- Module      : Data.Decamp.Aeson
-- Description : Aeson stuff for decamp
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Decamp.Aeson where

import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types (Pair, Value)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Decamp.Types
import           Data.Maybe
import           Data.Text (Text, pack)
import           Data.Time

encodeProject :: Project -> ByteString
encodeProject = toStrict . encode

encodeProjectFile :: FilePath -> Project -> IO ()
encodeProjectFile fp = B.writeFile fp . encodeProject

decodeProject :: ByteString -> Maybe Project
decodeProject = decodeStrict

decodeProjectFile :: FilePath -> IO (Maybe Project)
decodeProjectFile = fmap decodeStrict . B.readFile

eitherDecodeProject :: ByteString -> Either String Project
eitherDecodeProject = eitherDecodeStrict

decodeProjectFileEither :: FilePath -> IO (Either String Project)
decodeProjectFileEither = fmap eitherDecodeStrict . B.readFile

instance FromJSON Project where
  parseJSON (Object v) = Project <$> v .: "project-name"
                                 <*> v .:? "project-maintainer"
                                 <*> v .:? "project-homepage"
                                 <*> v .:? "project-description"
                                 <*> v .: "project-bugs"
  parseJSON _ = mzero

(.=?) :: ToJSON x => Text -> Maybe x -> Maybe Pair
_ .=? Nothing = Nothing
t .=? (Just x) = Just $ t .= x

objectMaybe :: [Maybe Pair] -> Value
objectMaybe = object . catMaybes

instance ToJSON Project where
  toJSON p = objectMaybe
               [ Just $ "project-name" .= projectName p
               , "project-maintainer" .=? projectMaintainer p
               , "project-homepage" .=? projectHomepage p
               , "project-description" .=? projectDescription p
               , Just $ "project-bugs" .= projectBugs p
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
  toJSON (Comment ps txt) = objectMaybe ["comment-person" .=? ps, Just $ "comment-text" .= txt]

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
  toJSON bug = objectMaybe
                 [ Just $ "bug-id" .= bugId bug
                 , "bug-reporter" .=? bugReporter bug
                 , Just $ "bug-creation-date" .= bugCreationDate bug
                 , Just $ "bug-title" .= bugTitle bug
                 , Just $ "bug-description" .= bugDescription bug
                 , Just $ "bug-open" .= bugOpen bug
                 , Just $ "bug-comments" .= bugComments bug
                 ]
