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
-- Module      : Data.Louse.Schemata
-- Description : Print schemata of the JSON files.
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- Louse stores its data in JSON files. This file contains functions
-- to access the schemata of said files.

module Data.Louse.Schemata where

import           Control.Exception (try)
import           Control.Monad ((>=>))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Monoid ((<>))
import           Data.List (sort)
import           Data.List.Utils (endswith)
import           Paths_louse (getDataDir)
import           System.Directory (getDirectoryContents)
import           System.IO (stdout)
import           System.IO.Error (isDoesNotExistError)

-- |The list of available schemata
--
schemata :: IO [String]
schemata =
  do jsonFiles <-
       fmap (filter (endswith ".json")) schemataFiles
     let jsonFileNames =
           fmap (reverse .
                 drop 5 .
                 reverse)
                jsonFiles
     pure (sort jsonFileNames)

-- |Print the output of 'schemata' to the console
-- 
-- @
-- listSchemata = mapM_ putStrLn =<< schemata
-- @
listSchemata :: IO ()
listSchemata =
  let bindl = (=<<)
  in bindl (mapM_ putStrLn) schemata

-- |Get a specific schema. You can use 'schemata' to see the
--  list of available schemata.
getSchema :: String -> IO ByteString
getSchema s =
  do d <- schemataDir
     let fp = mconcat [d,"/",s,".json"]
     eitherFileText <- try (B.readFile fp)
     case eitherFileText of
       Left err ->
         if |  isDoesNotExistError err ->
              fail (mconcat ["Invalid schema: "
                            ,s
                            ,". Run `louse list-schema' for a list of valid schema."])
            |  otherwise -> ioError err
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
schemataDir =
  fmap (\x -> mappend x "/res/schemata") getDataDir

-- |Print 'schemataDir' to the console
-- 
-- @
-- showSchemaDir = putStrLn =<< schemataDir
-- @
showSchemaDir :: IO ()
showSchemaDir = putStrLn =<< schemataDir

-- |The list of schemata files
-- 
-- @
-- schemataFiles = getDirectoryContents =<< schemataDir
-- @
schemataFiles :: IO [FilePath]
schemataFiles = getDirectoryContents =<< schemataDir
