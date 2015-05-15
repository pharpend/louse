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
import           Data.Louse
import           Data.Monoid
import           Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Version hiding (Version)
import           Options.Applicative
import           Paths_louse
import           System.Directory


main :: IO ()
main = execParser argsParserInfo >>= runArgs

altConcat :: Alternative f => [f a] -> f a
altConcat [] = empty
altConcat (x:xs) = x <|> altConcat xs

infoHelp :: Parser a -> InfoMod a -> ParserInfo a
infoHelp a = info (helper <*> a)

data Args = DBug BugAction
          | Init (Maybe FilePath) Bool
          | Query QueryAction
          | Schema SchemaAction
          | Status
          | Trivia TriviaAction
  deriving Show

data TriviaAction = Copyright
                  | License
                  | Readme
                  | Tutorial
                  | Version
  deriving Show

data BugAction = AddBug
               | CloseBug String
               | CommentOnBug String
               | DeleteBug String
               | EditBug String
               | ListBugs
               | ShowBug String
  deriving Show

data SchemaAction = ListSchemata
                  | Path
                  | ShowSchema String
  deriving Show

data QueryAction
  = Set String
        String
  | Get String
  deriving (Show)

runArgs :: Args -> IO ()
runArgs x =
  case x of
    DBug y ->
      case y of
        AddBug -> newBug
        CloseBug bugid -> closeBug (pack bugid)
        CommentOnBug bugid ->
          newComment (pack bugid)
        DeleteBug bugid -> deleteBug (pack bugid)
        EditBug bid -> editBug (pack bid)
        ListBugs -> listBugs Open
        ShowBug bid -> showBug (pack bid)
    Init dir force ->
      do workdir <-
           case dir of
             Nothing -> getCurrentDirectory
             Just w -> makeAbsolute w
         initInDir workdir force
    Query y ->
      case y of
        Get a ->
          do selection <-
               runExceptional (select (pack a)) :: IO Query
             decoded <-
               (=<<) runExceptional (selectGet selection) :: IO T.Text
             TIO.putStr decoded
        Set a b ->
          print (select (pack a) :: Exceptional Query)
    Schema y ->
      case y of
        ListSchemata -> listSchemata
        Path -> showSchemaDir
        ShowSchema s -> showSchema s
    Status -> status
    Trivia y ->
      case y of
        Copyright -> printOut louseCopyright
        License -> printOut louseLicense
        Readme -> printOut louseReadme
        Tutorial -> printOut louseTutorial
        Version -> printVersion
  where failNotImplemented =
          (fail "FIXME: Feature not yet implemented") :: IO a

argsParserInfo :: ParserInfo Args
argsParserInfo = infoHelp argsParser argsHelp
  where argsHelp :: InfoMod Args
        argsHelp =
          mconcat [fullDesc
                  ,header ("louse v." <> showVersion version)
                  ,progDesc "A distributed bug tracker."
                  ,footer "For information on a specific command, run `louse COMMAND --help`, where COMMAND is one of the commands listed above."]
        argsParser :: Parser Args
        argsParser =
          altConcat [Trivia <$>
                     altConcat [copyrightParser
                               ,licenseParser
                               ,readmeParser
                               ,tutorialParser
                               ,versionParser]
                    ,hsubparser (command "bug" bugInfo)
                    ,hsubparser (command "init" initInfo)
                    ,hsubparser (command "get" getInfo)
                    ,hsubparser (command "schema" schemataInfo)
                    ,hsubparser (command "set" setInfo)
                    ,hsubparser (command "status" statusInfo)]
        copyrightParser :: Parser TriviaAction
        copyrightParser =
          flag' Copyright
                (help ("Print the copyright.") <>
                 long "copyright")
        versionParser :: Parser TriviaAction
        versionParser =
          flag' Version
                (help ("Print the version (" <> showVersion version <> ").") <>
                 long "version")
        licenseParser :: Parser TriviaAction
        licenseParser =
          flag' License
                (help "Print the license (GPL version 3)." <>
                 long "license")
        tutorialParser :: Parser TriviaAction
        tutorialParser =
          flag' Tutorial
                (help "Print the tutorial." <>
                 long "tutorial")
        readmeParser :: Parser TriviaAction
        readmeParser =
          flag' Readme
                (help "Print the README." <>
                 long "readme")

initInfo :: ParserInfo Args
initInfo = infoHelp theOptions theHelp
  where theHelp =
          fullDesc <>
          progDesc "Initialize louse."
        theOptions =
          Init <$>
          option (Just <$> str)
                 (mconcat [long "workdir"
                          ,short 'w'
                          ,short 'd'
                          ,help "Working directory. Defaults to the current working directory."
                          ,value Nothing]) <*>
          switch (mconcat [long "force"
                          ,short 'f'
                          ,help "Initialize louse even if there is an existing louse project."])

statusInfo :: ParserInfo Args
statusInfo = infoHelp theOptions theHelp
  where
    theHelp = fullDesc <> progDesc "Initialize louse."
    theOptions = pure Status

schemataInfo :: ParserInfo Args
schemataInfo = infoHelp schemataOptions schemataHelp
  where
    schemataHelp = fullDesc <> progDesc "Do stuff with schemata."
    schemataOptions :: Parser Args
    schemataOptions = altConcat
                        [ subparser (command "dir" pathSchemaInfo)
                        , subparser (command "list" listSchemataInfo)
                        , subparser (command "path" pathSchemaInfo)
                        , subparser (command "show" showSchemaInfo)
                        ]
showSchemaInfo :: ParserInfo Args
showSchemaInfo = infoHelp theOptions theHelp
  where
    theHelp = fullDesc <> progDesc "Show a specific schema."
    theOptions = fmap Schema $ ShowSchema <$> strArgument (help "The schema to show")

listSchemataInfo :: ParserInfo Args
listSchemataInfo = infoHelp theOptions theHelp
  where
    theHelp = fullDesc <> progDesc "List the available schemata"
    theOptions = pure $ Schema ListSchemata

pathSchemaInfo :: ParserInfo Args
pathSchemaInfo = infoHelp theOptions theHelp
  where
    theHelp = fullDesc <> progDesc "Show the directory in which the schemata are stored"
    theOptions = pure $ Schema Path

bugInfo :: ParserInfo Args
bugInfo = infoHelp theOptions theHelp
  where theHelp =
          fullDesc <>
          progDesc "Do stuff with bugs."
        theOptions =
          altConcat [subparser (command "add" addBugInfo)
                    ,subparser (command "close" closeBugInfo)
                    ,subparser (command "comment" commentBugInfo)
                    ,subparser (command "delete" delBugInfo)
                    ,subparser (command "edit" editBugInfo)
                    ,subparser (command "list" lsBugInfo)
                    ,subparser (command "show" showBugInfo)]
        addBugInfo = infoHelp abopts abhelp
        abhelp =
          fullDesc <>
          progDesc "Add a bug"
        abopts =
          pure $
          DBug AddBug
        closeBugInfo = infoHelp cbopts cbhelp
        cbhelp =
          mappend fullDesc (progDesc "Close a bug")
        cbopts =
          fmap (DBug . CloseBug)
               (strArgument (help "The bug to close (use `louse bug list` to see a list)"))
        commentBugInfo = infoHelp cobopts cobhelp
        cobhelp =
          mappend fullDesc (progDesc "Add a comment to a bug.")
        cobopts =
          fmap (DBug . CommentOnBug)
               (strArgument (help "The bug in question (use `louse bug list` to see a list)"))
        delBugInfo = infoHelp dbopts dbhelp
        dbhelp =
          mappend fullDesc (progDesc "Delete a bug.")
        dbopts =
          fmap (DBug . DeleteBug)
               (strArgument (help "The bug to delete (use `louse bug list` to see a list)"))
        editBugInfo = infoHelp ebopts ebhelp
        ebhelp =
          mappend fullDesc (progDesc "Edit a bug manually with $EDITOR.")
        ebopts =
          fmap (DBug . EditBug)
               (strArgument (help "The bug to edit (use `louse bug list` to see a list)"))
        lsBugInfo = infoHelp lbopts lbhelp
        lbhelp =
          mappend fullDesc (progDesc "List all of the bugs")
        lbopts = pure (DBug ListBugs)
        showBugInfo = infoHelp sbopts sbhelp
        sbhelp =
          mappend fullDesc (progDesc "Pretty-print a bug")
        sbopts =
          fmap (DBug . ShowBug)
               (strArgument (help "The bug to show (use `louse bug list` to see a list)"))

getInfo :: ParserInfo Args
getInfo = infoHelp getOptions getHelp
  where getHelp =
          mconcat [fullDesc
                  ,progDesc "Query information."
                  ,footer "Use `louse get selectors` to show a list of selectors."]
        getOptions =
          fmap (Query . Get)
               (strArgument
                  (mconcat [help "The selector. An example would be \"config.whoami.name\"."
                           ,metavar "SELECTOR"]))

setInfo :: ParserInfo Args
setInfo = infoHelp getOptions getHelp
  where getHelp =
          mconcat [fullDesc
                  ,progDesc "Set some variables."
                  ,footer "Use `louse get selectors` to show a list of selectors."]
        getOptions =
          Query <$>
          (Set <$>
           (strArgument
              (mconcat [help "The selector. An example would be \"config.whoami.name\"."
                       ,metavar "SELECTOR"])) <*>
           (strArgument (mconcat [help "The new value.",metavar "VALUE"])))
