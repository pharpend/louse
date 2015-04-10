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
-- Module      : Data.Decamp.Schemata
-- Description : Print schemata of the JSON files.
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- Decamp stores its data in JSON files. This file contains functions
-- to access the schemata of said files.

module Data.Decamp.Schemata where

import           Control.Exception (try)
import           Control.Monad ((>=>))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Monoid ((<>))
import           Data.List (sort)
import           Data.List.Utils (endswith)
import           Paths_decamp (getDataDir)
import           System.Directory (getDirectoryContents)
import           System.IO (stdout)
import           System.IO.Error (isDoesNotExistError)

-- |The list of available schemata
--
-- @
-- schemata = sort . fmap (reverse . drop 5 . reverse) . filter (endswith ".json") <$> schemataFiles
-- @
schemata :: IO [String]
schemata = sort . fmap (reverse . drop 5 . reverse) . filter (endswith ".json") <$> schemataFiles

-- |Print the output of 'schemata' to the console
-- 
-- @
-- listSchemata = mapM_ putStrLn =<< schemata
-- @
listSchemata :: IO ()
listSchemata = mapM_ putStrLn =<< schemata
  
-- |Get a specific schema. You can use 'schemata' to see the
--  list of available schemata.
getSchema :: String -> IO ByteString
getSchema s = do
  d <- schemataDir
  let fp = mconcat [d, "/", s, ".json"]
  try (B.readFile fp) >>= \case
    Left err ->
      if | isDoesNotExistError err -> fail $ mconcat
                                               [ "Invalid schema: "
                                               , s
                                               , ". Run `decamp list-schema' for a list of valid schema."
                                               ]
         | otherwise -> ioError err
    Right txt -> pure txt

-- |Get a specific schema and print it to the console.
-- 
-- @
-- showSchema = getSchema >=> B.hPut stdout
-- @
showSchema :: String -> IO ()
showSchema = getSchema >=> B.hPut stdout

-- |The directory in which the schemata are stored
-- 
-- @
-- schemataDir = (<> "/res/schemata") <$> getDataDir
-- @
schemataDir :: IO FilePath
schemataDir = (<> "/res/schemata") <$> getDataDir

-- |The list of schemata files
-- 
-- @
-- schemataFiles = getDirectoryContents =<< schemataDir
-- @
schemataFiles :: IO [FilePath]
schemataFiles = getDirectoryContents =<< schemataDir
