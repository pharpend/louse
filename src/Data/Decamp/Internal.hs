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

import           Crypto.Random
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types (Pair)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Bc8
import           Data.ByteString.Base16 as Bs16
import           Data.ByteString.Lazy (toStrict)
import           Data.Decamp.Types
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time
import           Paths_decamp
import           System.Directory
import           System.IO
import           Text.Editor

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

-- |Template bug path
_templ_bug_path :: FilePath
_templ_bug_path = "res/templates/new-bug.yaml"

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

-- |Given the path to a template, give the template to the user, allow
-- him to edit it, and then return the edited template
-- 
-- @
-- editTemplate fp =
--   readDataFile fp >>=
--   runUserEditorDWIM yamlTemplate
-- @
editTemplate :: FilePath -> IO ByteString
editTemplate fp =
  readDataFile fp >>=
  runUserEditorDWIM yamlTemplate

data NewProject =
       NewProject
         { prjName :: Text
         , prjMaintainer :: Mtnr
         , prjHomePage :: Maybe Text
         , prjDescr :: Maybe Text
         }
  deriving (Show, Eq)

data Mtnr = Mtnr { mtnrName :: Text, mtnrEmail :: Text } | Anon
  deriving (Show, Eq)

mtnrToPerson :: Mtnr -> Maybe Person
mtnrToPerson Anon = Nothing
mtnrToPerson (Mtnr n e) = Just $ Person n e

parseNewBug :: NewBug -> IO Bug
parseNewBug (NewBug reporter synopsis description) = do
  currentTime <- getCurrentTime
  -- The id should be a base16 encoding of 512 random bytes
  bugid <- decodeUtf8 . Bs16.encode <$> createRandomBytes 512
  pure $ Bug bugid (mtnrToPerson reporter) currentTime synopsis description True []

instance FromJSON NewProject where
  parseJSON (Object v) = NewProject <$> v .:? "name" .!= _repl_working_dir
                                    <*> v .: "maintainer"
                                    <*> v .: "homepage"
                                    <*> v .: "description"
  parseJSON _ = mzero

instance FromJSON Mtnr where
  parseJSON (Object v) = Mtnr <$> v .: "name" <*> v .: "email"
  parseJSON Null = pure Anon
  parseJSON _ = mzero

data NewBug = NewBug { nbrptr :: Mtnr, nbttl :: Text, nbdescr :: Maybe Text }

instance FromJSON NewBug where
  parseJSON (Object v) =
    NewBug <$> v .: "reporter" <*> v .: "synopsis" <*> v .: "description"
  parseJSON _ = mzero


-- -- |Type for bugs
-- data Bug =
--        Bug
--          {
--          -- |A unique id for the bug. I haven't decided how this is to be created.
--          bugId :: Text
--          -- |The person who reported the bug
--          , bugReporter :: Maybe Person
--          -- |The non-optional time at which the bug was created.
--          , bugCreationDate :: UTCTime
--          -- |The title of the bug
--          , bugTitle :: Text
--          -- |An optional description of the bug
--          , bugDescription :: Maybe Text
--          -- |Whether or not the bug is open
--          , bugOpen :: Bool
--          -- |'Comment's on the bug
--          , bugComments :: [Comment]
--          }
--   deriving Show

-- |Write the project to @$(pwd)\/.decamp\/project.json@
writeProject :: Project -> IO ()
writeProject p = do
    hSetBinaryMode stdout True
    cwd <- getCurrentDirectory
    let decampDir = cwd <> "/.decamp"
        populate dir = do
          let prjpth = dir <> "/project.json"
          encodeProjectFile prjpth p
    createDirectory decampDir
    populate decampDir

createRandomBytes :: Int -> IO ByteString
createRandomBytes i =
  cprgCreate <$> createEntropyPool >>=
  \(gen :: SystemRNG) -> let (bs, _) = cprgGenerate i gen
                         in pure bs
