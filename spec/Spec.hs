{-# LANGUAGE FlexibleInstances #-}
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

import Control.Applicative
import Control.Exceptional
import Crypto.Hash.SHA1
import qualified Data.ByteString.Base16 as BH
import Data.List (sort)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Tree
import Louse
import System.IO (hClose)
import System.IO.Unsafe
import System.Process
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main =
  hspec $
  do context "Louse library" $
       parallel $
       do titleTests
          descrTests
          commentTreeTests
          sha1Tests



titleTests :: SpecWith (Arg Expectation)
titleTests =
  context "Titles" $
  do specify "mkTitle should return a Failure if given an empty string" $
       shouldThrow (runExceptional (mkTitle ""))
                   anyException
     specify "mkTitle should return a Failure if given a string > 64 chars long." $
       property (\(LongText t) ->
                   shouldThrow (runExceptional (mkTitle t))
                               anyException)
     specify "Titles should fromString . show without loss of data" $
       property (\(title :: Title) ->
                   (fromString (show title)) `shouldBe`
                   title)
descrTests :: SpecWith (Arg Expectation)
descrTests =
  context "Descriptions" $
  do specify "mkDescription should return a Failure if given an empty string" $
       shouldThrow (runExceptional (mkDescription ""))
                   anyException
     specify "Descriptions should fromString . show without loss of data" $
       property (\(description :: Description) ->
                   (fromString (show description)) `shouldBe`
                   description)

commentTreeTests :: SpecWith (Arg Expectation)
commentTreeTests =
  context "Comments" $
  do context "Comment trees" $
       do context "Encoding & decoding comment trees into rose forests" $
            do specify "fromForest . toForest . fromForest = fromForest" $
                 property (\(forest :: Forest (Author,CommentText)) ->
                             shouldBe ((fromForest (toForest (fromForest forest :: [Comment]) :: Forest (Author,CommentText))) :: [Comment])
                                      (fromForest forest))
               specify "Given a tree, all of the keys should be the SHA1 of the values" pending

sha1Tests :: SpecWith (Arg Expectation)
sha1Tests =
  context "sha1sums" $
  do specify "sha1 = TE.decodeUtf8 . BH.encode . SHA1.hash . TE.encodeUtf8" $
       do property (\(txt :: Text) ->
                      shouldBe (sha1 txt)
                               (TE.decodeUtf8 (BH.encode (hash (TE.encodeUtf8 txt)))))
     specify "sha1 function should give the same results as the sha1sum program" $
       do property (\(txt :: Text) ->
                      shouldBe (sha1 txt)
                               (unsafePerformIO (sha1sum txt)))
  where sha1sum txt =
          do (Just stdinH,Just stdoutH,Nothing,procH) <- createProcess sha1Process
             TIO.hPutStr stdinH txt
             hClose stdinH
             waitForProcess procH
             contents <- TIO.hGetContents stdoutH
             hClose stdoutH
             return (head (T.words contents))
        sha1Process =
          CreateProcess {cmdspec =
                           ShellCommand "sha1sum"
                        ,cwd = Nothing
                        ,env = Nothing
                        ,std_in = CreatePipe
                        ,std_out = CreatePipe
                        ,std_err = Inherit
                        ,close_fds = True
                        ,create_group = False
                        ,delegate_ctlc = True}

instance Arbitrary Title where
  arbitrary =
    do string <-
         suchThat (arbitrary :: Gen String)
                  (\s ->
                     64 >= length s &&
                     not (null s))
       runExceptional (mkTitle (T.pack string))

instance Arbitrary Description where
  arbitrary =
    do string <-
         suchThat (arbitrary :: Gen String)
                  (not . null)
       runExceptional (mkDescription (T.pack string))

-- |Used for generating long texts >64 characters for testing
newtype LongText = LongText Text
  deriving (Eq, Show)

instance Arbitrary LongText where
  arbitrary =
    do longString <-
         suchThat (arbitrary :: Gen String)
                  (\s -> 64 < length s)
       return (LongText (T.pack longString))

instance Arbitrary Text where
  arbitrary = fmap T.pack arbitrary

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary

instance Arbitrary t => Arbitrary (Tree t) where
  arbitrary =
    do topLevelValue <- arbitrary
       -- We can't do Node <$> arbitrary <*> arbitrary, because that
       -- creates infinite data structures.
       return (Node topLevelValue [])

instance Arbitrary Comment where
  arbitrary =
    fmap fromTree (arbitrary :: Gen (Tree (Author,CommentText)))
