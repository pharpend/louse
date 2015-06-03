{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- louse - distributed bugtracker
-- Copyright (c) 2015, Peter Harpending.
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
-- Description : The tests for Louse
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC

module Main where

import Control.Exceptional
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Louse
import Test.Hspec
import Test.QuickCheck


main :: IO ()
main =
  hspec (parallel develLouseTests)

develLouseTests :: SpecWith a
develLouseTests =
  context "Development.Louse" $
  do context "Titles" $
       do specify "mkTitle should return a Failure if given an empty string" $
            shouldThrow (runExceptional (mkTitle ""))
                        anyException
          specify "mkTitle should return a Failure if given a string > 64 chars long." $
            property (\(LongText t) ->
                        shouldThrow (runExceptional (mkTitle t))
                                    anyException)
          specify "Titles should fromString . show without loss of data" $
            property (\(title :: Title) ->
                        title ==
                        fromString (show title))
     context "Descriptions" $
       do specify "mkDescription should return a Failure if given an empty string" $
            pending
          specify "Descriptions should fromString & show without loss of data" $
            pending 

-- |Used for generating long texts >64 characters for testing
newtype LongText = LongText Text
  deriving (Eq, Show)

instance Arbitrary LongText where
  arbitrary =
    do longString <-
         suchThat (arbitrary :: Gen String)
                  (\s -> 64 < length s)
       return (LongText (T.pack longString))
       
instance Arbitrary Title where
  arbitrary =
    do string <-
         suchThat (arbitrary :: Gen String)
                  (\s ->
                     64 >= length s &&
                     not (null s))
       runExceptional (mkTitle (T.pack string))
