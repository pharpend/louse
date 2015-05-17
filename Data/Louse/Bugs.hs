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

import Control.Exceptional
import Control.Monad (forM_, mzero)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as Bsc
import qualified Data.ByteString.Lazy as Bl
import Data.Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Binary
import Data.Conduit.Combinators (sinkLazy)
import Data.Louse.DataFiles
import Data.Louse.Read
import Data.Louse.Templates
import Data.Louse.Trivia (randomIdent)
import Data.Louse.Types
import qualified Data.Map as M
import Data.Time (getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as L
import Data.Traversable (for)
import Data.Yaml
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


-- |Add a bug to the current project. This doesn't return a bug. It
-- instead writes the bug to a file, and returns the 'BugId' pertaining
-- to the file.
addBug :: FilePath
       -> Person         -- ^The reporter
       -> T.Text         -- ^Title of the bug
       -> T.Text         -- ^Description of the bug
       -> IO BugId       -- ^Resulting 'BugId'
addBug pwd person title description =
  do reportTime <- getCurrentTime
     let bugIsOpen = True
         comments = mempty
         nb =
           Bug person reportTime title description bugIsOpen comments
     bugid <- fmap Bsc.unpack randomIdent
     encodeFile (mconcat [pwd,_bugs_dir,bugid,".yaml"])
                nb
     return (T.pack bugid)

-- |Close a bug. This actually edits the files, so be careful.
closeBug :: FilePath -> BugId -> IO ()
closeBug pwd bugid =
  do let bugsPath =
           (mconcat [_bugs_dir,T.unpack bugid,".yaml"])
     bug <- errDecodeFile bugsPath
     encodeFile bugsPath
                (bug {bugOpen = False})

-- |Comment on a bug. This actually edits the data files, so be careful!
commentOnBug :: FilePath
             -> BugId                      -- ^The bug on which to comment
             -> Person             -- ^The commenter
             -> T.Text                     -- ^The actual comment text
             -> IO ()
commentOnBug pwd bugid personid comment =
  do let bugsPath =
           (mconcat [_bugs_dir,T.unpack bugid,".yaml"])
     bug <- errDecodeFile bugsPath
     commentTime <- getCurrentTime
     let nc =
           Comment personid commentTime comment
     (encodeFile
        bugsPath
        (bug {bugComments =
                (mappend (bugComments bug)
                         [nc])}))
-- |Edit a bug manually
editBug :: FilePath -> BugId -> IO ()
editBug pwd bugid =
  do let bugsPath =
           (mconcat [pwd,_bugs_dir,T.unpack bugid,".yaml"])
     bug <- (errDecodeFile bugsPath) :: IO Bug
     newBug <-
       (errDecode =<<
        runUserEditorDWIM yamlTemplate
                          (encode bug)) :: IO Bug
     encodeFile bugsPath newBug

-- |Delete a bug from the list of bugs. 
deleteBug :: FilePath -> BugId -> IO ()
deleteBug pwd bugid =
  do let bugPath =
           (mconcat [pwd,_bugs_dir,T.unpack bugid,".yaml"])
     removeFile bugPath
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
newBug :: FilePath -> IO ()
newBug fp =
  do reporter <- fmap whoami readLouseConfig
     nbTemplate <- _templ_new_bug
     nb <-
       (=<<) errDecode (editTemplate nbTemplate)
     bugId <-
       addBug fp
              reporter
              (nbSynopsis nb)
              (nbDescription nb)
     putStrLn (mappend "Added new bug with id " (T.unpack bugId))

-- |Make a new bug
newComment :: FilePath -> BugId -> Maybe Text -> IO ()
newComment fp bid maybeComment =
  do reporter <- fmap whoami readLouseConfig
     comment <-
       case maybeComment of
         Just c -> pure c
         Nothing ->
           fmap decodeUtf8 (runUserEditorDWIM plainTemplate "Enter your comment here")
     commentOnBug fp bid reporter comment
     putStrLn (mappend "Added comment to bug " (T.unpack bid))


-- |Print out information about a bug to the terminal.
showBug :: T.Text -- ^The optionally abbreviated bug id
        -> IO ()
showBug ident =
  lookupShortKey ident >>=
  \case
    Nothing ->
      fail (mconcat ["I can't find a bug whose ident starts with:\n\t"
                    ,(T.unpack ident)])
    Just x -> print x

data State = Open | Closed | All
-- |Print out a list in the form ident\ttitle to the terminal.
listBugs :: State -> IO ()
listBugs state =
  do louse <- readLouseErr
     let openBugs =
           let allBugs = louseBugs louse
           in case state of
                Open -> M.filter bugOpen allBugs
                All -> allBugs
                Closed ->
                  M.filter (not . bugOpen) allBugs
         keys_ = M.keys openBugs
         titles_ =
           map bugTitle (M.elems openBugs)
         keysTitles = zip keys_ titles_
     forM_ keysTitles
           (\(key,title) ->
              TIO.putStrLn
                (mconcat [T.take 8 key,T.replicate 4 " ",ellipsize title]))
  where ellipsize t
          | T.length t > 65 =
            mappend (T.take 65 t) "..."
          | otherwise = t
