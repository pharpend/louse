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
-- Module      : Data.Louse.IO.Read
-- Description : 'readLouse' and Friends
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Louse.IO.Read where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bl
import           Data.Louse.IO.DataFiles
import           Data.Monoid
import qualified Data.Map as M
import           Data.List.Utils (split)
import           Data.Louse.Types
import qualified Data.Text as T
import           Safe
import           System.Directory
import           Text.Editor

-- |Read the 'Louse' from the current directory
-- 
-- > readLouse = readLouseFrom =<< getCurrentDirectory
-- 
readLouse :: IO (Either String Louse)
readLouse = readLouseFrom =<< getCurrentDirectory

-- |Read the 'Louse' from the current directory
-- 
-- > readLouseMay = readLouseFromMay =<< getCurrentDirectory
-- 
readLouseMay :: IO (Maybe Louse)
readLouseMay = readLouseFromMay =<< getCurrentDirectory

-- |Read the 'Louse' from the current directory
-- 
-- > readLouseErr = readLouseFromErr =<< getCurrentDirectory
-- 
readLouseErr :: IO Louse
readLouseErr = readLouseFromErr =<< getCurrentDirectory

-- |Wrapper around 'readLouseFromErr', which catches errors, and returns
-- a 'Left' if there is an error.
readLouseFrom 
  :: FilePath                   -- ^The working directory
  -> IO (Either String Louse)
readLouseFrom fp = (try (readLouseFromErr fp) :: IO (Either SomeException Louse)) >>= \case
                     Left err -> pure (Left (show err))
                     Right x  -> pure (Right x)

-- |Wrapper around 'readLouseFromErr', which returns 'Nothing' if there
-- is an error.
readLouseFromMay 
  :: FilePath         -- ^The working directory
  -> IO (Maybe Louse)
readLouseFromMay = readLouseFrom >=> \case
                     Left _  -> pure Nothing
                     Right x -> pure (Just x)

-- |This is a function to read the Louse instance from a directory.
readLouseFromErr 
  :: FilePath -- ^The path to the project directory (i.e. NOT .louse)
  -> IO Louse -- ^The resulting 'Louse'
readLouseFromErr fp =
  let prjInfo = Bl.readFile (fp <> _project_json)
                  >>= \x -> case eitherDecode x of
                              Left err -> fail err
                              Right pi -> pure pi
  in Louse fp <$> prjInfo 
              <*> readBugsFromErr fp 
              <*> readPeopleFromErr fp

-- |Lazily reads the bugs.
readBugsFromErr 
  :: FilePath             -- ^The path to the project directory
  -> IO (M.Map BugId Bug) -- ^The resulting Map
readBugsFromErr fp = 
  readFilesFromErr $ mappend fp _bugs_dir

-- |Lazily reads the bugs.
readPeopleFromErr 
  :: FilePath          -- ^The path to the project directory
  -> IO (M.Map PersonId Person) -- ^The resulting Map
readPeopleFromErr fp = 
  readFilesFromErr $ mappend fp _people_dir

-- |Lazily reads files in a directory, returns a 'M.Map' of the name
-- of the file, along with the decoded value.
readFilesFromErr 
  :: FromJSON t 
  => FilePath     -- ^The directory holding the files
  -> IO (IdMap t) -- ^The resulting Map
readFilesFromErr directoryPath =
  M.fromList <$> (mapM mkMapMember =<< files)
  where
    files :: IO [FilePath]
    files = getDirectoryContents directoryPath

    -- This function constructs an individual element of the Map
    mkMapMember :: FromJSON t => FilePath -> IO (T.Text, t)
    mkMapMember filePath = do
      fcontents <- Bl.readFile filePath
      decodedValue <- case eitherDecode fcontents of
                        Left err -> fail err
                        Right x  -> pure x
      pure (T.pack (deCanonicalize filePath), decodedValue)
    -- quux.yaml -> quux
    removeDot :: FilePath -> FilePath
    removeDot = reverse . drop 5 . reverse

    -- Split a string on "/"
    splitSlashes :: FilePath -> [FilePath]
    splitSlashes fp = split "/" fp

    -- Takes a canonical filename: /foo/bar/baz/quux.yaml -> quux . Also converts to Text while it's at
    -- it.
    deCanonicalize :: FilePath -> FilePath
    deCanonicalize fp =
      case removeDot <$> lastMay (splitSlashes fp) of
        Just x  -> x
        Nothing -> fp
