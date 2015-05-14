{-# LANGUAGE TemplateHaskell #-}

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
-- Module      : Data.Louse.Query
-- Description : Query things from louse's data
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC

module Data.Louse.Query where

import Data.Text (Text)
import qualified Data.Text as T
import Safe

data Query
  = QBug {qBugQuery :: BugQuery}
  | QConfig {qConfigQuery :: ConfigQuery}
  deriving (Eq,Show)

data BugQuery
  = BQAll
  | BQClosed
  | BQOpen
  deriving (Eq, Show)
 
data ConfigQuery =
  CQWhoami {cqWhoamiQuery :: Maybe WhoamiQuery}
  deriving (Eq, Show)

data WhoamiQuery
  = WQName
  | WQEmail
  deriving (Eq,Show)

parseQuery :: Text -> Exceptable Query
parseQuery q =
  let qPieces = T.splitOn "." q
  in case headMay qPieces of
       Nothing ->
         fail "You have to submit a query."
       Just "bugs" ->
         fmap QBug
              (case atMay qPieces 1 of
                 Nothing -> pure BQAll
                 Just x ->
                   (case x of
                      "all" -> pure BQAll
                      "closed" -> pure BQClosed
                      "open" -> pure BQOpen
                      x ->
                        fail (mappend "bug: no match for value " (T.unpack x))))
       Just "config" ->
         fmap QConfig
              (case atMay qPieces 1 of
                 Nothing ->
                   fail "I need something more specific than \"config\""
                 Just "whoami" ->
                   fmap CQWhoami
                        (case atMay qPieces 2 of
                           Nothing ->
                             pure Nothing
                           Just "name" ->
                             pure (Just WQName)
                           Just "email" ->
                             pure (Just WQEmail)))
       Just x ->
         fail (mappend "toplevel: no match for value " (T.unpack x))

-- |This is basically specialized Either, or Maybe with errors
data Exceptable b
  = Failure String
  | Success b
  deriving (Eq, Show, Read)

instance Functor Exceptable where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure s) = Failure s

instance Applicative Exceptable where
  pure = Success
  Success f <*> Success x = Success (f x)
  Failure s <*> _ = Failure s
  _ <*> Failure s = Failure s

instance Monad Exceptable where
  (>>=) (Success x) f = f x
  (>>=) (Failure s) _ = Failure s
  fail = Failure
  return = pure
