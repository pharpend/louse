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
-- Module      : Data.Louse.Read
-- Description : 'readLouse' and Friends
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Louse.Read where

import           Control.Exception
import           Control.Exceptional
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bl
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary hiding (drop)
import           Data.Louse.DataFiles
import           Data.Monoid
import qualified Data.Map as M
import           Data.List.Utils (split,startswith)
import           Data.Louse.Types
import           Data.Ratio ((%))
import qualified Data.Text as T
import           Data.Yaml
import           Safe
import           System.Directory
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error
import           Text.Editor

-- |Read the 'Louse' from the current directory
-- 
-- > readLouse = readLouseFrom =<< getCurrentDirectory
-- 
readLouse :: IO (Exceptional Louse)
readLouse = readLouseFrom =<< getCurrentDirectory

-- |Read the 'Louse' from the current directory
-- 
-- > readLouseErr = readLouseFromErr =<< getCurrentDirectory
-- 
readLouseErr :: IO Louse
readLouseErr = readLouseFromErr =<< getCurrentDirectory

-- |Wrapper around 'readLouseFromErr', which catches errors, and returns
-- a 'Left' if there is an error.
readLouseFrom :: FilePath                   -- ^The working directory
              -> IO (Exceptional Louse)
readLouseFrom fp =
  (try (readLouseFromErr fp) :: IO (Either SomeException Louse)) >>=
  \case
    Left err -> pure (fail (show err))
    Right x -> pure (pure x)

-- |This is a function to read the Louse instance from a directory.
readLouseFromErr 
  :: FilePath -- ^The path to the project directory (i.e. NOT .louse)
  -> IO Louse -- ^The resulting 'Louse'
readLouseFromErr fp =
  do let pryaml = mappend fp _project_yaml
     prjInfoExists <- doesFileExist pryaml
     prjInfo <-
       if (not prjInfoExists)
          then pure Nothing
          else fmap Just (errDecodeFile pryaml)
     ap (fmap (Louse fp prjInfo)
              (readBugsFromErr fp))
        (fmap whoami readLouseConfig)

-- |Lazily reads the bugs.
readBugsFromErr 
  :: FilePath             -- ^The path to the project directory
  -> IO (M.Map BugId Bug) -- ^The resulting Map
readBugsFromErr fp = 
  readFilesFromErr (mappend fp _bugs_dir)

-- |Lazily reads files in a directory, returns a 'M.Map' of the name
-- of the file, along with the decoded value.
readFilesFromErr :: FromJSON t
                 => FilePath -> IO (IdMap t)
readFilesFromErr directoryPath =
  -- Get the files in the directory
  do filePaths <- getDirectoryContents directoryPath
     -- For each filePath
     fmap
       M.fromList
       (forM (filter (not .
                      startswith ".")
                     filePaths)
             (\fp ->
                do decodedContents <-
                     errDecodeFile (mconcat [directoryPath,"/",fp])
                   let fileName =
                         T.pack (reverse (drop 5 (reverse fp)))
                   return (fileName,decodedContents)))

-- |Look up a bug by its 'BugId'
lookupBug :: Louse -> BugId -> Maybe Bug
lookupBug louse bugid =
  M.lookup bugid (louseBugs louse)

-- |Read the louse config
readLouseConfig :: IO LouseConfig
readLouseConfig =
  do configPath <- _config_path
     configPathExists <- doesFileExist configPath
     if configPathExists
        then errDecodeFile configPath
        else pure (LouseConfig Anonymous)

-- |Write the louse config back
writeLouseConfig :: LouseConfig -> IO ()
writeLouseConfig cfg =
  do dataDir <- getAppUserDataDirectory "louse"
     createDirectoryIfMissing True dataDir
     configPath <- _config_path
     encodeFile configPath cfg
