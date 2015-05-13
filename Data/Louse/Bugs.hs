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
-- Module      : Data.Louse.Bugs
-- Description : Stuff for reading & writing bugs
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Louse.Bugs where

import Control.Monad (mzero)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Attoparsec (parseOnly, IResult(..))
import qualified Data.ByteString.Char8 as Bsc
import qualified Data.ByteString.Lazy as Bl
import Data.Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Binary
import Data.Conduit.Combinators (sinkLazy)
import Data.Louse.Config
import Data.Louse.DataFiles
import Data.Louse.Templates
import Data.Louse.Trivia (randomIdent)
import Data.Louse.Types
import Data.Time (getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as L
import System.Directory (getCurrentDirectory, removeFile)
import System.IO (openFile, IOMode(..))
import System.Exit
import Text.Editor

-- For reference:
-- 
-- -- |A bug.
-- data Bug = Bug { bugReporter :: Maybe Person
--                , bugCreationDate :: UTCTime
--                , bugTitle :: T.Text
--                , bugDescription :: T.Text
--                , bugOpen :: Bool
--                , bugComments :: [Comment]
--                }
--   deriving (Eq, Show)

-- |Interactively add a bug


-- |Add a bug to the current project. This doesn't return a bug. It
-- instead writes the bug to a file, and returns the 'BugId' pertaining
-- to the file.
addBug :: Person         -- ^The reporter
       -> T.Text         -- ^Title of the bug
       -> T.Text         -- ^Description of the bug
       -> IO BugId       -- ^Resulting 'BugId'
addBug person title description =
  do reportTime <- getCurrentTime
     let bugIsOpen = False
         comments = mempty
         nb =
           Bug person reportTime title description bugIsOpen comments
     bugid <- fmap Bsc.unpack randomIdent
     pwd <- getCurrentDirectory
     runResourceT
       (connect (sourceLbs (encode nb))
                (sinkFile (mconcat [pwd,_bugs_dir,bugid,".json"])))
     return (T.pack bugid)

-- |Close a bug. This actually edits the files, so be careful.
closeBug :: BugId -> IO ()
closeBug bugid =
  do let bugsPath =
           (mconcat [_bugs_dir,T.unpack bugid,".json"])
     bugAST <- runResourceT (connect (sourceFile bugsPath) (sinkParser json))
     bug <- parseMonad bugAST
     runResourceT
       (connect (sourceLbs (encode (bug {bugOpen = False})))
                (sinkFile bugsPath))

-- |Comment on a bug. This actually edits the data files, so be careful!
commentOnBug :: BugId                      -- ^The bug on which to comment
             -> Person             -- ^The commenter
             -> T.Text                     -- ^The actual comment text
             -> IO ()
commentOnBug bugid personid comment =
  do let bugsPath =
           (mconcat [_bugs_dir,T.unpack bugid,".json"])
     bugAST <-
       runResourceT (connect (sourceFile bugsPath) (sinkParser json))
     bug <- parseMonad bugAST
     commentTime <- getCurrentTime
     let nc =
           Comment personid commentTime comment
     runResourceT
       (connect (sourceLbs (encode (bug {bugComments =
                                           (mappend (bugComments bug)
                                                    [nc])})))
                (sinkFile bugsPath))

-- ^Delete a bug from the list of bugs. 
deleteBug :: BugId -> IO ()
deleteBug bugid =
  let bugPath =
        (mconcat [_bugs_dir,T.unpack bugid,".json"])
  in removeFile bugPath >>
     putStrLn (mappend "Deleted bug " (show bugid))

-- |Intermediate type for new bugs
data NewBug =
  NewBug {nbSynopsis :: Text
         ,nbDescription :: Text}
  deriving (Show,Eq)

instance FromJSON NewBug where
  parseJSON (Object v) =
    NewBug <$>
    (v .: "synopsis") <*>
    (v .: "description")
  parseJSON _ = mzero

-- |Make a new bug
newBug :: IO ()
newBug =
  do maybeLC <- readLouseConfig
     let reporter =
           case maybeLC of
             Just (LouseConfig r) -> r
             Nothing -> Anonymous
     nbTemplate <- _templ_new_bug
     nb <-
       fmap eitherDecodeStrict (editTemplate nbTemplate) >>=
       \case
         Left err -> fail err
         Right x -> pure x
     bugId <-
       addBug reporter
              (nbSynopsis nb)
              (nbDescription nb)
     putStrLn (mappend "Added new bug with id " (T.unpack bugId))

-- |Make a new bug
newComment :: BugId -> IO ()
newComment bid =
  do maybeLC <- readLouseConfig
     let reporter =
           case maybeLC of
             Just (LouseConfig r) -> r
             Nothing -> Anonymous
     comment <-
       fmap decodeUtf8 (runUserEditorDWIM plainTemplate "Enter your comment here")
     commentOnBug bid reporter comment
     putStrLn (mappend "Added comment to bug " (T.unpack bid))
