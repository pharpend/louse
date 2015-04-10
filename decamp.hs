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

data Args = InitInteractive { bare :: Bool }
          | License
          | Schema SchemaAction
          | Tutorial
          | Version
  deriving Show

data SchemaAction = ListSchemata
                  | ShowSchema String
  deriving Show

runArgs :: Args -> IO ()
runArgs Tutorial = do
  fnom <- getDataFileName "TUTORIAL.md"
  hSetBinaryMode stdout False
  hSetBuffering stdout NoBuffering
  Bs.readFile fnom >>= Bs.hPut stdout

runArgs License = do
  fnom <- getDataFileName "LICENSE"
  hSetBinaryMode stdout False
  hSetBuffering stdout NoBuffering
  Bs.readFile fnom >>= Bs.hPut stdout

runArgs Version = putStrLn $ showVersion version
runArgs (InitInteractive b) = interactiveInit b 
runArgs (Schema ListSchemata) = listSchemata
runArgs (Schema (ShowSchema s)) = showSchema s
runArgs x = print x

argsParserInfo :: ParserInfo Args
argsParserInfo = infoHelp argsParser argsHelp
  where
    argsHelp :: InfoMod Args
    argsHelp =
      fullDesc <>
      header ("decamp v." <> showVersion version) <>
      progDesc "A distributed bug tracker."
    argsParser :: Parser Args
    argsParser =
      empty <|> licenseParser <|> versionParser <|>
      hsubparser (command "init" initOptionsInfo) <|>
      -- hsubparser (command "noninit" noninitOptionsInfo) <|>
      hsubparser (command "schemata" schemataInfo) <|>
      hsubparser (command "tutorial" tutorialOptionsInfo)
    versionParser :: Parser Args
    versionParser =
      flag' Version (help ("Print the version (" <> showVersion version <> ")") <>
                     long "version")
    licenseParser :: Parser Args
    licenseParser =
      flag' License (help "Print the license (GPL version 3)." <>
                     long "license")


-- noninitOptionsInfo :: ParserInfo Args
-- noninitOptionsInfo =
--   infoHelp initOptions $
--   fullDesc <>
--   progDesc "Initialize decamp (non-interactively)."
--   where initOptions :: Parser Args
--         initOptions =
--           InitNonInteractive <$>
--           switch (long "bare" <>
--                   help "Initialize decamp in this directory. (I.e. don't create a .decamp/ directory).") <*>
--           strOption (short 'n' <>
--                      long "name" <>
--                      help "The name of the project" <>
--                      metavar "NAME") <*>
--           strOption (short 'm' <>
--                      long "maintainer-name" <>
--                      help "The name of the maintainer" <>
--                      showDefault <>
--                      metavar "NAME" <>
--                      value mempty) <*>
--           strOption (short 'e' <>
--                      long "maintainer-email" <>
--                      help "The email of the maintainer" <>
--                      showDefault <>
--                      metavar "ADDRESS" <>
--                      value mempty) <*>
--           strOption (short 'u' <>
--                      long "url" <>
--                      showDefault <>
--                      help "The home page of the project" <>
--                      metavar "URL" <>
--                      value mempty) <*>
--           strOption (short 'd' <>
--                      long "description" <>
--                      help "A description of the project" <>
--                      metavar "STRING")

initOptionsInfo :: ParserInfo Args
initOptionsInfo =
  infoHelp initOptions $
    fullDesc <>
    progDesc "Initialize decamp (interactively)"
  where
    initOptions :: Parser Args
    initOptions =
      InitInteractive <$> switch
                            (long "bare" <>
                             help
                               "Initialize decamp in this directory. (I.e. don't create a .decamp/ directory).")

tutorialOptionsInfo :: ParserInfo Args
tutorialOptionsInfo =
  infoHelp tutorialOptions tutorialHelp
  where tutorialHelp =
          fullDesc <>
          progDesc "Print the tutorial."
        tutorialOptions :: Parser Args
        tutorialOptions = pure Tutorial


infoHelp :: Parser a -> InfoMod a -> ParserInfo a
infoHelp a = info (helper <*> a)

schemataInfo :: ParserInfo Args
schemataInfo = infoHelp schemataOptions schemataHelp
  where
    schemataHelp = fullDesc <> progDesc "Do stuff with schemata."
    schemataOptions :: Parser Args
    schemataOptions = subparser (command "list" listSchemataInfo) <|> subparser (command "show" showSchemaInfo)

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

main :: IO ()
main = execParser argsParserInfo >>= runArgs
