-- decamp-bugtrack - distributed bugtracker
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
-- Description : Runs decamp-bugtrack
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Main where

import           Control.Applicative
import qualified Data.ByteString as Bs
import           Data.Decamp
import           Data.Monoid
import           Data.Version hiding (Version)
import           Options.Applicative
import           Paths_decamp
import           System.IO

data Args = Copyright
          | InitInteractive
          | License
          | Readme
          | Schema SchemaAction
          | Tutorial
          | Version
  deriving Show

data SchemaAction = ListSchemata
                  | Path
                  | ShowSchema String
  deriving Show

runArgs :: Args -> IO ()
runArgs Tutorial = printOut tutorial
runArgs License = printOut license
runArgs Version = printVersion
runArgs InitInteractive = interactiveInit
runArgs (Schema ListSchemata) = listSchemata
runArgs (Schema Path) = showSchemaDir
runArgs (Schema (ShowSchema s)) = showSchema s
runArgs Copyright = printOut copyright
runArgs Readme = printOut readme
runArgs x = print x

argsParserInfo :: ParserInfo Args
argsParserInfo = infoHelp argsParser argsHelp
  where
    argsHelp :: InfoMod Args
    argsHelp = mconcat
                 [ fullDesc
                 , header ("decamp v." <> showVersion version)
                 , progDesc "A distributed bug tracker."
                 , footer
                     "For information on a specific command, run `decamp COMMAND --help`, where COMMAND is one of the commands listed above."
                 ]
    argsParser :: Parser Args
    argsParser = altConcat
                   [ copyrightParser
                   , licenseParser
                   , readmeParser
                   , tutorialParser
                   , versionParser
                   , hsubparser (command "init" initOptionsInfo)
                   , hsubparser (command "schema" schemataInfo)
                   , hsubparser (command "schemata" schemataInfo)
                   ]
    copyrightParser :: Parser Args
    copyrightParser = flag' Copyright (help ("Print the copyright.") <>
                                       long "copyright")
    versionParser :: Parser Args
    versionParser = flag' Version (help ("Print the version (" <> showVersion version <> ").") <>
                                   long "version")
    licenseParser :: Parser Args
    licenseParser = flag' License (help "Print the license (GPL version 3)." <>
                                   long "license")
    tutorialParser :: Parser Args
    tutorialParser = flag' Tutorial (help "Print the tutorial." <>
                                     long "tutorial")
    readmeParser :: Parser Args
    readmeParser = flag' Readme (help "Print the README." <>
                                 long "readme")

initOptionsInfo :: ParserInfo Args
initOptionsInfo = infoHelp theOptions theHelp
  where
    theOptions = pure InitInteractive
    theHelp = fullDesc <> progDesc "Initialize decamp (interactively)"

altConcat :: Alternative f => [f a] -> f a
altConcat [] = empty
altConcat (x:xs) = x <|> altConcat xs

infoHelp :: Parser a -> InfoMod a -> ParserInfo a
infoHelp a = info (helper <*> a)

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

main :: IO ()
main = execParser argsParserInfo >>= runArgs
