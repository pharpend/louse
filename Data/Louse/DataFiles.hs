{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
-- Module      : Data.Louse.DataFiles
-- Description : Paths and interface to Louse's data files
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Louse.DataFiles where

import           Control.Exceptional
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as Bs
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Text as CT
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Data.Yaml
import           Paths_louse
import           System.Directory (getAppUserDataDirectory)

-- |These are all magic values of the paths in the louse hierarchy.
_app_name :: FilePath
_app_name = "louse"

_louse_dir         = mconcat ["/.", _app_name, "/"]
_project_yaml      = _louse_dir <> "project.yaml"
_bugs_dir          = _louse_dir <> "bugs/"
_people_dir        = _louse_dir <> "people/"

_config_path :: IO FilePath
_config_path =
  do dataDir <- getAppUserDataDirectory _app_name
     return (mappend dataDir "/config.yaml")

-- |Read a file lazily but efficiently
produceFile :: FilePath -> Producer (ResourceT IO) Bs.ByteString
produceFile = sourceFile

-- |Alias for 'produceFile'
readDataFile :: FilePath -> Producer (ResourceT IO) Bs.ByteString
readDataFile = produceFile

-- |This runs 'getDataFileName' on the argument, and then sends back the text from that file..
readDataFileText :: FilePath -> IO Text
readDataFileText fp =
  do filePath <- getDataFileName fp
     TIO.readFile filePath


type TemplatePath = String

-- |Path to template for new 'ProjectInfo'
_templ_new_project :: IO TemplatePath
_templ_new_project =
  getDataFileName "res/templates/new-project.yaml"

-- |Path to template for new 'Bug'
_templ_new_bug :: IO TemplatePath
_templ_new_bug = getDataFileName "res/templates/new-bug.yaml"


-- |Extra functions
errDecodeFile :: FromJSON a => FilePath -> IO a
errDecodeFile filePath =
  ((>>=) (decodeFileEither filePath)
         (\case
            Left err -> ppError err
            Right x -> pure x))

decodeFileExceptional :: FromJSON a => FilePath -> IO (Exceptional a)
decodeFileExceptional filePath =
  (flip fmap
        (decodeFileEither filePath)
        (\case
           Left err -> ppError err
           Right x -> pure x))

errDecode :: FromJSON a
          => Bs.ByteString -> IO a
errDecode bytes =
  case decodeEither' bytes of
    Left err -> ppError err
    Right x -> pure x

decodeExceptional :: FromJSON a
                   => Bs.ByteString -> Exceptional a
decodeExceptional bytes =
  case decodeEither' bytes of
    Left err -> ppError err
    Right x -> pure x

ppError :: Monad m => ParseException -> m a
ppError = fail . prettyPrintParseException

bindl :: Monad m => (a -> m b) -> m a -> m b
bindl = (=<<)

bindr :: Monad m => m a -> (a -> m b) -> m b
bindr = (>>=)
