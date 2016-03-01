-- louse - distributed bugtracker
-- Copyright (c) 2015-2016, Peter Harpending.
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
-- Description : This module runs louse
-- Copyright   : Copyright (c) 2015-2016, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Main where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.FileEmbed
import qualified Data.Text.Encoding as TE
import Options.Applicative
import System.IO
import System.Pager

altConcat :: (Foldable t, Alternative f) => t (f a) -> f a
altConcat = foldr (<|>) empty

infoHelper x = info (helper <*> x)

main :: IO ()
main = execParser mainPI >>= runArgs
  where
    mainPI =
      infoHelper actionParser $
        mconcat [ fullDesc
                , progDesc "This is a small program to fetch bug information from remote trackers, and browse the information locally."
                ]

    actionParser = altConcat (fmap subparser [ licenseParser
                                             , readmeParser
                                             ])
          
data Args = License ShowOpts
          | Readme ShowOpts
          | FetchIssues String S
  deriving (Show, Eq)     

data ShowOpts = ShowOpts { soNoPager :: Bool
                         }
  deriving (Show, Eq)     

soParser :: Parser ShowOpts
soParser =
  ShowOpts <$> switch (mconcat [ long "no-pager"
                               , help "Disable automatic piping to the system pager (usually called 'less' or 'more')."
                               ])

readmeParser :: Mod CommandFields Args
readmeParser =
  command "readme" $
    infoHelper (fmap Readme soParser) 
               (mappend fullDesc
                         (progDesc "Show the README."))
  
licenseParser :: Mod CommandFields Args
licenseParser =
  command "license" $
    infoHelper (fmap License soParser)
               (mappend fullDesc
                         (progDesc "Show the license (GPL-3)."))


runArgs :: Args -> IO ()
runArgs args = do
    hSetBuffering stdout NoBuffering
    case args of
      License so -> show' so $(embedFile "LICENSE")
      Readme so -> show' so $(embedFile "README.md")

show' :: ShowOpts -> ByteString -> IO ()
show' so bs
  | soNoPager so = B.hPut stdout bs
  | otherwise = printOrPage (TE.decodeUtf8 bs)
