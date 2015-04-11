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
-- Module      : Data.Decamp.Initialize
-- Description : Module to initialize Decamp in directory.
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX
-- 

module Data.Decamp.Initialize where

import           Control.Monad (mzero)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Decamp.Aeson
import           Data.Decamp.Types
import           Data.List.Utils
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           Data.Yaml
import           Paths_decamp
import           System.Directory
import           System.IO
import           Text.Editor

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

-- |Get the project from the user, and then write it
-- 
-- @
-- initialize = getProject >>= writeProject
-- @
initialize :: IO ()
initialize = getProject >>= writeProject

-- |Get the project from the user
getProject :: IO Project
getProject =
  getProjectBS >>=
  \pbs -> case decodeEither pbs of
    Left err -> fail err
    Right (NewProject nom mtr hp descr) -> do
      newNom <- if | nom == _repl_working_dir -> T.pack . last . split "/" <$> getCurrentDirectory
                   | otherwise -> pure nom
      let newMtr =
            case mtr of
              Anon     -> Nothing
              Mtnr n e -> Just $ Person n e
      pure $ Project newNom newMtr hp descr []

-- |Get the YAML representation of the project from the user
getProjectBS :: IO ByteString
getProjectBS =
  getDataFileName _templ_prj_path >>=
  B.readFile >>=
  runUserEditorDWIM yamlTemplate

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

-- = Internal things =


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

-- |Magic value for "replace with working directory"
_repl_working_dir :: Text
_repl_working_dir = "REPL_WORKING_DIR"

_templ_prj_path :: FilePath
_templ_prj_path = "res/templates/new-project.yaml"
