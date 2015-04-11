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
-- Module      : Data.Decamp.Internal
-- Description : Internal variables for Decamp
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Decamp.Internal where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types (Pair)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Maybe
import           Data.Decamp.Types
import           Paths_decamp
import           Data.Text (Text)

_project_json :: FilePath
_project_json = ".decamp/project.json"

-- |Read a data file
-- 
-- @
-- readDataFile = B.readFile <=< getDataFileName
-- @
readDataFile :: FilePath -> IO ByteString
readDataFile = B.readFile <=< getDataFileName

-- |Magic value for "replace with working directory"
_repl_working_dir :: Text
_repl_working_dir = "REPL_WORKING_DIR"

-- |Template project path
_templ_prj_path :: FilePath
_templ_prj_path = "res/templates/new-project.yaml"

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

-- |This tries to decode a project from a file. Returns @Nothing@ if
-- it fails, @Just Project@ if it succeeds.
-- 
-- @
-- eitherDecodeProjectFile = fmap eitherDecodeStrict . B.readFile
-- @
eitherDecodeProjectFile :: FilePath  -- ^File to decode
                        -> IO (Either String Project)
eitherDecodeProjectFile = fmap eitherDecodeStrict . B.readFile

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
