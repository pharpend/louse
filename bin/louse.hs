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
-- Module      : Main
-- Description : Runs louse program
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Main where

import           Control.Applicative
import           Control.Exceptional
import           Control.Monad.Trans.Reader
import           Data.Louse
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Version hiding (Version)
import           Options.Applicative
import           Options.Applicative.Types (ReadM(..))
import           Paths_louse
import           System.Directory

runArgs :: Args -> IO ()
runArgs (Args workdir stdin cmd) =
  let failNotImplemented =
        (fail "FIXME: Feature not yet implemented") :: IO a
  in do workingDirectory <-
          case workdir of
            Nothing -> getCurrentDirectory
            Just x -> pure x
        case cmd of
          Initialize ->
            initInDir workingDirectory False
          Query y ->
            case y of
              Get a ->
                do selection <-
                     runExceptional (select (T.pack a)) :: IO Query
                   decoded <-
                     (=<<) runExceptional (selectGet selection) :: IO T.Text
                   TIO.putStr decoded
              Set a b ->
                do selection <-
                     runExceptional (select (T.pack a)) :: IO Query
                   selectSet selection (T.pack (head b))
              Lookup _ _ -> failNotImplemented
          Status -> status

data Args =
  Args {workingDirectory :: Maybe FilePath
       ,getStuffFromStdin :: Bool
       ,argsCommand :: SubCommand}
  deriving (Eq,Show)

data SubCommand
  = Initialize
  | Query QueryAction
  | Status
  | AddBug
  | CommentOnBug String (Maybe String)
  | DeleteBug String
  deriving (Eq,Show)

data QueryAction
  = Set String
        [String]
  | Get String
  | Lookup String
           [String]
  deriving (Eq,Show)

argsParserInfo :: ParserInfo Args
argsParserInfo =
  info ((<*>) helper
              (Args <$>
               (option maybeReader
                       (mconcat [help "The working directory"
                                ,short 'w'
                                ,long "workdir"
                                ,short 'd'
                                ,long "directory"])) <*>
               (switch (mconcat [help "Get input from stdin instead of opening up your $EDITOR."
                                ,long "stdin"])) <*>
               (alt [alt [subparser (command "initialize"
                                             (flip info
                                                   (mappend briefDesc
                                                            (progDesc "Initialize louse in the (optionally specified) directory."))
                                                   (pure Initialize)))
                         ,subparser (command "init"
                                             (flip info
                                                   (mappend briefDesc
                                                            (progDesc "Alias for \"initialize\"."))
                                                   (pure Initialize)))
                         ,subparser (command "status"
                                             (flip info
                                                   (mappend briefDesc
                                                            (progDesc "Get the status of the louse instance in the (optionally specified) directory"))
                                                   (pure Status)))
                         ,subparser (command "st"
                                             (flip info
                                                   (mappend briefDesc
                                                            (progDesc "Alias for \"status\"."))
                                                   (pure Status)))]
                    ,queryCommands
                    ,bugCommands])))
       (mconcat [fullDesc
                ,header ("louse v." <> showVersion version)
                ,progDesc "A distributed bug tracker."
                ,footer "For information on a specific command, run `louse COMMAND --help`, where COMMAND is one of the commands listed above."])
          

bugCommands :: Parser SubCommand
bugCommands =
  let addBug = pure AddBug
      commentOnBug =
        (pure CommentOnBug) <*>
        (strArgument (help "The bug on which to comment.")) <*>
        (option maybeReader
                (mconcat [help "The comment. By default, I will open up your $EDITOR. Or, if you said so, I will read the comment from stdin."
                         ,short 'm'
                         ,long "message"
                         ,short 'c'
                         ,long "comment"
                         ,value Nothing]))
      deleteBug =
        fmap DeleteBug (strArgument (help "The bug to delete."))
  in alt [subparser (command "add-bug"
                             (flip info
                                   (mconcat [briefDesc,progDesc "Add a bug"])
                                   addBug))
         ,subparser (command "ab"
                             (flip info
                                   (mconcat [briefDesc
                                            ,progDesc "Alias for \"add-bug\"."])
                                   addBug))
         ,subparser (command "comment-on-bug"
                             (flip info
                                   (mconcat [briefDesc
                                            ,progDesc "Comment on a bug."])
                                   commentOnBug))
         ,subparser (command "cob"
                             (flip info
                                   (mconcat [briefDesc
                                            ,progDesc "Alias for \"comment-on-bug\"."])
                                   commentOnBug))
         ,subparser (command "delete-bug"
                             (flip info
                                   (mconcat [briefDesc
                                            ,progDesc "Delete a bug from the database."])
                                   deleteBug))
         ,subparser (command "del"
                             (flip info
                                   (mconcat [briefDesc
                                            ,progDesc "Alias for \"delete-bug\"."])
                                   deleteBug))
         ,subparser (command "rm"
                             (flip info
                                   (mconcat [briefDesc
                                            ,progDesc "Alias for \"delete-bug\"."])
                                   deleteBug))]

queryCommands :: Parser SubCommand
queryCommands =
  alt [subparser (command "get"
                          (flip info
                                (mconcat [briefDesc
                                         ,progDesc "Query information, either about the current louse project, about louse in general, about your configuration."])
                                (fmap (Query . Get)
                                      (strArgument (mconcat [help "The selector. An example would be `louse get selectors`."])))))
      ,subparser (command "set"
                          (flip info
                                (mconcat [briefDesc
                                         ,progDesc "Use this command to change variables, like whether or not a bug is open, or configuration options for louse."])
                                ((pure Query) <*>
                                 ((pure Set) <*>
                                  (strArgument
                                     (mconcat [help "The selector."
                                              ,metavar "SELECTOR"])) <*>
                                  (option strings
                                          (mconcat [help "The new value"
                                                   ,metavar "VALUE"]))))))]

strings :: ReadM [String]
strings = ReadM (ReaderT (\s -> pure (words s)))

maybeReader :: ReadM (Maybe String)
maybeReader =
  let readMaybe s
        | s == mempty = return Nothing
        | otherwise = return (Just s)
  in ReadM (ReaderT readMaybe)

alt :: Alternative f
    => [f a] -> f a
alt [] = empty
alt (x:xs) = x <|> alt xs

main :: IO ()
main = execParser argsParserInfo >>= runArgs
