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

-- |Module      : Data.Louse
-- Description : Umbrella module for louse library
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This is an umbrella module for the louse library. This is the library
-- you should import. However, all this library really does is re-export
-- some other modules.


module Data.Louse (
    module Data.Louse,
    -- $use
    module Data.Louse.Bug.Add,
    module Data.Louse.Initialize,
    module Data.Louse.Project,
    module Data.Louse.Schemata,
    module Data.Louse.Types
    ) where

import Data.Louse.Bug.Add
import Data.Louse.Initialize
import Data.Louse.Project
import Data.Louse.Schemata
import Data.Louse.Trivia
import Data.Louse.Types

-- $use
-- 
-- = How to use this library =
-- 
-- This document is split in 3 sections:
-- 
-- * Louse's types
-- * Reading an existing louse project
-- * Initializing a new louse project
-- * Interacting with the pure data
-- 
-- == Louse's types ==
-- 
-- This library centers around the 'Louse' data type.
-- 
-- > data Louse =
-- >   Louse {louseProjectInfo :: Maybe ProjectInfo
-- >         ,louseBugs :: HashMap ByteString Bug
-- >         ,lousePeople :: HashMap ByteString Person}
-- >   deriving (Eq, Show)
-- 
-- which in turn relies on 3 other data types:
-- 
-- > data ProjectInfo =
-- >   ProjectInfo {projectName :: Maybe Text
-- >               ,projectMaintainers :: Maybe (Vector Person)
-- >               ,projectHomepage :: Maybe Text
-- >               ,projectDescription :: Maybe Text}
-- >   deriving (Eq,Show)
-- >   
-- > data Bug =
-- >   Bug {bugReporter :: Maybe Person
-- >       ,bugCreationDate :: UTCTime
-- >       ,bugTitle :: Text
-- >       ,bugDescription :: Text
-- >       ,bugOpen :: Bool
-- >       ,bugComments :: [Comment]}
-- >   deriving (Eq,Show)
-- >   
-- > data Person =
-- >   Person {personName :: Text -- ^The person's name
-- >          ,personEmail :: Text -- ^Their email
-- >          }
-- >   deriving (Eq, Show)
-- > 
-- > data Comment =
-- >   Comment {commentPerson :: Maybe Person
-- >           ,commentDate :: UTCTime
-- >           ,commentText :: Text}
-- >   deriving (Eq,Show)
-- 
-- The rest of the types you'll encounter are ordinary @ByteString@s,
-- @Text@s, @FilePath@s, and the like.
-- 
-- == Reading an existing louse project ==
-- 
-- The first function you'll want to call is 'readLouse', from
-- "Data.Louse.Initialize". 
-- 
-- > readLouse :: IO (Either String Louse)
-- 
-- This tries to read a 'Louse' from the current directory. If it
-- succeeds, it returns @Right Louse@. If it fails, it returns @Left
-- String@, containing the error.
-- 
-- Here's a simple minimal example:
-- 
-- >>> import Data.Louse
-- >>> readLouse >>= print
-- 
-- And here's a slightly less minimal example
-- 
-- > {-# LANGUAGE LambdaCase #-}
-- > 
-- > module Main where
-- > 
-- > import Data.Louse
-- > import System.IO (hPut, stderr)
-- > 
-- > main :: IO ()
-- > main =
-- >   readLouse >>=
-- >   \case
-- >     Left err -> hPut stderr err
-- >     Right lse -> print lse
-- 
-- If you want to get the 'Louse' from a different directory, use
-- 'readLouseFrom'
-- 
-- > readLouseFrom :: FilePath -> IO (Either String Louse)
-- 
-- There are many variants of 'readLouse':
-- 
-- > readLouseMaybe:: IO (Maybe Louse)
-- > readLouseErr:: IO Louse
-- > readLouseFromMaybe:: FilePath -> IO (Maybe Louse)
-- > readLouseFromErr:: FilePath -> IO Louse
-- 
-- Their name and data type should give a good indication of what they
-- do. If you're still unsure, see the documentation in
-- "Data.Louse.Initialize"
-- 
-- == Initializing a new louse project ==
-- 
-- If you don't already have a louse project to work with, you'll want
-- to call 'newLouse' from "Data.Louse.Initialize".
-- 
-- > newLouse :: Louse
-- > newLouse = Louse Nothing mempty mempty
