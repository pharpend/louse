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
import           Data.Monoid
import           Data.Version hiding (Version)
import           Options.Applicative
import           Options.Applicative.Arrows
import           Paths_decamp
import           System.IO

main :: IO ()
main = execParser argsParserInfo >>= runArgs

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

runArgs x = print x


argsParserInfo :: ParserInfo Args
argsParserInfo = infoHelp argsParser argsHelp
  where
    argsHelp :: InfoMod Args
    argsHelp = fullDesc <> header ("decamp v." <> showVersion version) <> progDesc
                                                                            "A distributed bug tracker."

    argsParser :: Parser Args
    argsParser =
      versionParser
      <|> licenseParser
      <|> hsubparser (command "init" initOptionsInfo)
      <|> hsubparser (command "tutorial" tutorialOptionsInfo)

    versionParser :: Parser Args
    versionParser = flag' Tutorial
                      (help ("Print the version (" <> showVersion version <> ")") <> long "version")

    licenseParser :: Parser Args
    licenseParser = flag' License (help "Print the license (GPL version 3)." <> long "license")


initOptionsInfo :: ParserInfo Args
initOptionsInfo = infoHelp initOptions fullDesc
  where
    initOptions :: Parser Args
    initOptions =
      Init <$> switch (long "bare" <>
                       help "bare") <*> switch (short 'i' <>
                                                help "interactive")

tutorialOptionsInfo :: ParserInfo Args
tutorialOptionsInfo = infoHelp tutorialOptions tutorialHelp
  where
    tutorialHelp = fullDesc <> progDesc "Print the tutorial."
    tutorialOptions :: Parser Args
    tutorialOptions = pure Tutorial


data Args = Init { bare :: Bool, interactive :: Bool }
          | License
          | Tutorial 
          | Version
  deriving Show

infoHelp :: Parser a -> InfoMod a -> ParserInfo a
infoHelp a = info (helper <*> a)
