{-# LANGUAGE CPP #-}

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
import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as Bsc
import Data.Louse.DataFiles
import Data.Louse.Read
import Data.Louse.Templates
import Data.Louse.Trivia (randomIdent)
import Data.Louse.Types
import Data.Time (getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml
import System.Directory (removeFile)
import System.Exit
import System.IO
import Text.Editor

#if (!MIN_VERSION_base (4,8,0))
import Control.Applicative
import Data.Monoid
#endif

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

data BoolPair = BP Bool Text

-- |Make sure the bug synopsis is valid
-- 
-- If it is, just send back the text. Otherwise, return an error message.
validateSynopsis :: Text -> Exceptional Text
validateSynopsis t =
  let conds =
        [BP (t == "A short synopsis goes here") "You have to change the synopsis."
        ,BP (T.strip t == mempty) "You can't leave the synopsis empty"
        ,BP (T.isInfixOf "\r" t) "You can't use stupid Windows newline characters (\\r)."
        ,BP (T.isInfixOf "\n" t) "You can't use Linux newline characters either (\\n)."
        ,BP (T.length t > 64) "The synopsis can't be longer than 64 characters."]
      errorMessages =
        do BP condition error <- conds
           if condition
              then return error
              else mzero
  in case errorMessages of
       [] -> return t
       messages ->
         fail (T.unpack (T.unlines messages))
       

-- |Make sure the bug description is valid
-- 
-- If it is, just send back the text. Otherwise, return an error message.
validateDescription :: Text -> Exceptional Text
validateDescription t =
  let defaultDescription =
        T.strip (T.unlines ["A much longer description of the bug goes here."
                           ,""
                           ,"This portion of the bug is usually parsed with Markdown syntax -"
                           ,"<https://daringfireball.net/projects/markdown/syntax>."
                           ,""
                           ,"This entire document is parsed using YAML syntax -"
                           ,"<https://en.wikipedia.org/wiki/Yaml>."
                           ,""
                           ,"This portion of the text is limited to 8192 characters, including spaces and"
                           ,"line breaks. These here lines are broken at 80 characeters for"
                           ,"readability. You should do the same thing. If every line is 80 characters"
                           ,"long, then you can have about 100 lines before you run out of space. So, you"
                           ,"have plenty of space. Note that most lines are not 80 characters long, it's"
                           ,"just that 80 characters is the **maximum**."])
      conds =
        [BP (T.strip t == defaultDescription) "You have to change the description."
        ,BP (T.strip t == mempty) "You can't have an empty description."
        ,BP (T.isInfixOf "\r" t) "You can't use stupid Windows newline characters (\\r)."
        ,BP ((T.length t < 64) ||
             (T.length t > 8192))
            "The synopsis can't be less than 64 characters or less than 8192."]
      errorMessages =
        do BP condition error <- conds
           if condition
              then return error
              else mzero
  in case errorMessages of
       [] -> return t
       messages ->
         fail (T.unpack (T.unlines messages))

-- |Make sure a comment is valid
-- 
-- If it is, just send back the text. Otherwise, return an error message.
validateComment :: Text -> Exceptional Text
validateComment t =
  let conds =
        [BP (T.strip t == mempty) "You have to add a comment"
        ,BP (T.isInfixOf "\r" t) "You can't use non-standard Windows newlines (\\r)."]
      errorMessages =
        do BP condition error <- conds
           if condition
              then return error
              else mzero
  in case errorMessages of
       [] -> return t
       messages ->
         fail (T.unpack (T.unlines messages))

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
     title_ <-
       runExceptional (validateSynopsis title)
     description_ <-
       runExceptional (validateDescription title)
     let bugIsOpen = True
         comments = mempty
         nb =
           Bug person reportTime title_ description_ bugIsOpen comments
     bugid <- fmap Bsc.unpack randomIdent
     encodeFile (mconcat [pwd,_bugs_dir,bugid,".yaml"])
                nb
     return (T.pack bugid)

-- |Comment on a bug. This actually edits the data files, so be careful!
commentOnBug :: FilePath
             -> BugId                      -- ^The bug on which to comment
             -> Person             -- ^The commenter
             -> T.Text                     -- ^The actual comment text
             -> IO ()
commentOnBug pwd bugid personid comment =
  do comment_ <-
       runExceptional (validateComment comment)
     bugs_ <- readBugsFromErr pwd
     (key,bug) <-
       case lookupAbbreviatedKeyInMap bugs_ bugid of
         Just x -> return x
         Nothing ->
           fail "I couldn't find any bug with that key."
     let bugsPath =
           (mconcat [pwd,_bugs_dir,T.unpack key,".yaml"])
     bug <- errDecodeFile bugsPath
     commentTime <- getCurrentTime
     let nc =
           Comment personid commentTime comment_
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
     newBug_ <-
       (errDecode =<<
        runUserEditorDWIM yamlTemplate
                          (encode bug)) :: IO Bug
     encodeFile bugsPath newBug_

-- |Delete a bug from the list of bugs. 
deleteBug :: FilePath -> BugId -> IO ()
deleteBug pwd bugid =
  do bugs <- readBugsFromErr pwd
     (key,bug) <-
       case lookupAbbreviatedKeyInMap bugs bugid of
         Just x -> return x
         Nothing ->
           fail "I couldn't find any bug with that key."
     hSetBuffering stdout NoBuffering
     putStr (mconcat ["Are you sure you want to delete bug "
                     ,(T.unpack key)
                     ,"?\nAnswer with all-caps \"YES\": "])
      
     response <- getLine
     if |  response /= "YES" -> exitSuccess
        |  otherwise ->
          do let bugPath =
                   (mconcat [pwd,_bugs_dir,T.unpack key,".yaml"])
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

