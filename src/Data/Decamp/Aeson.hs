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
-- This module isn't very interesting. It just contains "Data.Aeson"
-- instances for the types in "Data.Decamp.Types".
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

prettyEncode :: ToJSON a => a -> ByteString
prettyEncode = toStrict . encodePretty' defConfig { confIndent = 2 }

-- ^This is a wrapper around 'encodePretty' and 'toStrict'
-- 
-- @
-- prettyEncode = toStrict . encodePretty' defConfig { confIndent = 2 }
-- @
-- 
-- These next two functions are sort of funky. Let's look at a
-- typical aeson input/output.
-- 
-- @
-- data Foo = Foo { bar :: Text, baz :: Maybe Text, quux :: Maybe UTCTime }
-- 
-- ...
-- 
-- instance FromJSON Foo where
--   fromJSON (Object v) = Foo \<$\> v .: "bar" \<*\> v .:? "baz" \<*\> v .:? "quux"
-- 
-- instance ToJSON Foo where
--   toJSON x = object ["bar" .= bar x, "baz" .= baz x, "quux" .= quux x]
-- 
-- ...
-- 
-- main :: IO ()
-- main = B.hPut stdout $ prettyEncode f
--   where
--     f = Foo "hi" Nothing Nothing
-- @
-- 
-- The JSON generated will look something like
-- 
-- @
-- {
--   "bar": "hi",
--   "baz": null,
--   "quux": null
-- }
-- @
-- 
-- If @"baz"@ and @"quux"@ are optional values, making them @null@ only
-- confuses people. In this case, @"quux"@ and @"baz"@ are optional,
-- and have the Haskell value of @Nothing@. Instead of being @null@
-- when converted to JSON, they should just be ignored.
--
-- Enter '.=?' and 'objectMaybe'. If you replace the 'ToJSON' instance above with
-- 
-- @
-- instance ToJSON Foo where
--   toJSON x = objectMaybe [Just $ "bar" .= bar x, "baz" .=? baz x, "quux" .=? quux x]
-- @
--
-- Then the JSON output will be
-- 
-- @
-- {
--   "bar": "hi"
-- }
-- @
-- 
-- Et voila!
(.=?) :: ToJSON x => Text -> Maybe x -> Maybe Pair
_ .=? Nothing = Nothing
t .=? (Just x) = Just $ t .= x

objectMaybe :: [Maybe Pair] -> Value
objectMaybe = object . catMaybes

-- |This is just an alias for 'prettyEncode', but non-generic
encodeProject :: Project -> ByteString
encodeProject = prettyEncode

-- |Encode a project to a file.
-- 
-- @
-- encodeProjectFile fp = B.writeFile fp . encodeProject
-- @
encodeProjectFile :: FilePath                   -- ^The file to which to write
                  -> Project                    -- ^The project to write
                  -> IO ()
encodeProjectFile fp = B.writeFile fp . encodeProject

-- |This is an alias for 'decodeStrict' from Aeson. It takes a strict
-- 'ByteString' and tries to convert it to a 'Project'.
decodeProject :: ByteString -> Maybe Project
decodeProject = decodeStrict

-- |This tries to decode a project from a file. Returns @Nothing@ if
-- it fails, @Just Project@ if it succeeds.
-- 
-- @
-- decodeProjectFile = fmap decodeStrict . B.readFile
-- @
decodeProjectFile :: FilePath  -- ^File to decode
                  -> IO (Maybe Project)
decodeProjectFile = fmap decodeStrict . B.readFile

-- |Alias for 'eitherDecodeStrict' from Aeson.
eitherDecodeProject :: ByteString -> Either String Project
eitherDecodeProject = eitherDecodeStrict

-- |Tries to decode project from file. Returns @Right Project@ if it
-- succeeds, @Left String@ if it fails. The @String@ part is the error
-- message.
-- 
-- @
-- decodeProjectFileEither = fmap eitherDecodeStrict . B.readFile
-- @
decodeProjectFileEither :: FilePath -> IO (Either String Project)
decodeProjectFileEither = fmap eitherDecodeStrict . B.readFile

-- |The rest of the file contains orphan instances of 'Project',
-- 'Person', 'Comment', and 'Bug'.

instance FromJSON Project where
  parseJSON (Object v) = Project <$> v .: "project-name"
                                 <*> v .:? "project-maintainer"
                                 <*> v .:? "project-homepage"
                                 <*> v .:? "project-description"
                                 <*> v .: "project-bugs"
  parseJSON _ = mzero

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
