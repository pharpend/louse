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
import           Control.Monad
import           Data.Louse
import           Data.Monoid
import           Data.Version hiding (Version)
import           Options.Applicative
import           Paths_louse
import           System.Directory
import           System.IO


main :: IO ()
main = execParser argsParserInfo >>= runArgs

altConcat :: Alternative f => [f a] -> f a
altConcat [] = empty
altConcat (x:xs) = x <|> altConcat xs

infoHelp :: Parser a -> InfoMod a -> ParserInfo a
infoHelp a = info (helper <*> a)

data Args = DBug BugAction
          | Init (Maybe FilePath)
          | People PersonAction
          | Schema SchemaAction
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

data PersonAction = AddPerson
                  | DeletePerson String
                  | ListPeople
                  | ShowPerson String
  deriving Show

data SchemaAction = ListSchemata
                  | Path
                  | ShowSchema String
  deriving Show


runArgs :: Args -> IO ()
runArgs x =
  case x of
    DBug y ->
      case y of
        AddBug         -> failNotImplemented
        CloseBug _     -> failNotImplemented
        CommentOnBug _ -> failNotImplemented
        DeleteBug _    -> failNotImplemented
        EditBug _      -> failNotImplemented
        ListBugs       -> failNotImplemented
        ShowBug _      -> failNotImplemented
    Init dir -> do
      workdir <- case dir of
                   Nothing -> getCurrentDirectory
                   Just x  -> makeAbsolute x
      putStrLn workdir
      failNotImplemented
    People y ->
      case y of
        AddPerson      -> failNotImplemented
        DeletePerson _ -> failNotImplemented
        ListPeople     -> failNotImplemented
        ShowPerson _   -> failNotImplemented
    Schema y ->
      case y of
        ListSchemata -> listSchemata
        Path         -> showSchemaDir
        ShowSchema s -> showSchema s
    Trivia y ->
      case y of
        Copyright -> printOut louseCopyright
        License   -> printOut louseLicense
        Readme    -> printOut louseReadme
        Tutorial  -> printOut louseTutorial
        Version   -> printVersion
  where
    failNotImplemented = fail "FIXME: Feature not yet implemented"

argsParserInfo :: ParserInfo Args
argsParserInfo = infoHelp argsParser argsHelp
  where
    argsHelp :: InfoMod Args
    argsHelp = mconcat
                 [ fullDesc
                 , header ("louse v." <> showVersion version)
                 , progDesc "A distributed bug tracker."
                 , footer
                     "For information on a specific command, run `louse COMMAND --help`, where COMMAND is one of the commands listed above."
                 ]
    argsParser :: Parser Args
    argsParser = altConcat
                   [ hsubparser (command "bug" bugInfo)
                   , hsubparser (command "init" initInfo)
                   , hsubparser (command "ppl" pplInfo)
                   , hsubparser (command "schema" schemataInfo)
                   , hsubparser (command "schemata" schemataInfo)
                   ]
    -- copyrightParser :: Parser Args
    -- copyrightParser = flag' Copyright (help ("Print the copyright.") <>
    --                                    long "copyright")
    -- versionParser :: Parser Args
    -- versionParser = flag' Version (help ("Print the version (" <> showVersion version <> ").") <>
    --                                long "version")
    -- licenseParser :: Parser Args
    -- licenseParser = flag' License (help "Print the license (GPL version 3)." <>
    --                                long "license")
    -- tutorialParser :: Parser Args
    -- tutorialParser = flag' Tutorial (help "Print the tutorial." <>
    --                                  long "tutorial")
    -- readmeParser :: Parser Args
    -- readmeParser = flag' Readme (help "Print the README." <>
    --                              long "readme")

initInfo :: ParserInfo Args
initInfo = infoHelp theOptions theHelp
  where
    theHelp = fullDesc <> progDesc "Initialize louse."
    theOptions = Init <$> option (Just <$> str)
                            (mconcat
                               [ long "workdir"
                               , short 'w'
                               , short 'd'
                               , help "Working directory. Defaults to the current working directory."
                               , value Nothing
                               ])


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
  where
    theHelp = fullDesc <> progDesc "Do stuff with bugs."
    theOptions = altConcat
                        [ subparser (command "add" addBugInfo)
                        , subparser (command "close" addBugInfo)
                        , subparser (command "comment" addBugInfo)
                        , subparser (command "delete" addBugInfo)
                        , subparser (command "list" addBugInfo)
                        , subparser (command "show" addBugInfo)
                        ]
    addBugInfo = infoHelp abopts abhelp
    abhelp = fullDesc <> progDesc "Add a bug"
    abopts = pure $ DBug AddBug

pplInfo :: ParserInfo Args
pplInfo = infoHelp theOptions theHelp
  where
    theHelp = fullDesc <> progDesc "Do stuff with people."
    theOptions = altConcat
                        [ subparser (command "add" addPplInfo)
                        , subparser (command "delete" addPplInfo)
                        , subparser (command "list" addPplInfo)
                        , subparser (command "show" addPplInfo)
                        ]
    addPplInfo = infoHelp abopts abhelp
    abhelp = fullDesc <> progDesc "Add a person"
    abopts = pure $ People AddPerson

