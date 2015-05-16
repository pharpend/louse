{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses 
    #-}

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

import Control.Exceptional
import Control.Monad.IO.Class
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.Louse.Query.Selector
import Data.Louse.Read
import Data.Louse.Types
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Tree
import Data.Yaml
import Safe

data Query
  = QBugs {qBugsQuery :: BugsQuery}
  | QConfig {qConfigQuery :: ConfigQuery}
  | QSelectors
  deriving (Eq,Show)

data BugsQuery
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

data Pair a b =
  Pair {first :: a
       ,second :: b}
  deriving (Eq,Show)

selectorMap :: HashMap Text (Pair Selector Query)
selectorMap =
  fromSelectorList
    [Pair (Selector "about" "About louse." True False) QSelectors
    ,Pair (Selector "about.license" "Print out louse's license (GPL-3)." True False) QSelectors
    ,Pair (Selector "about.schema" "List the various schemata of louse's data files" True False)
          QSelectors
    ,Pair (Selector "about.selectors" "List all of the selectors" True False) QSelectors
    ,Pair (Selector "about.tutorial" "Print the tutorial" True False) QSelectors
    ,Pair (Selector "about.version" "Print out the version" True False) QSelectors
    ,Pair (Selector "bugs" "List the bugs" True False)
          (QBugs BQAll)
    ,Pair (Selector "bugs.all" "Same as bugs" True False)
          (QBugs BQAll)
    ,Pair (Selector "bugs.closed" "List only the closed bugs" True False)
          (QBugs BQClosed)
    ,Pair (Selector "bugs.open" "List the open bugs" True False)
          (QBugs BQOpen)
    ,Pair (Selector "config.whoami" "Information about yourself" True False)
          (QConfig (CQWhoami Nothing))
    ,Pair (Selector "config.whoami.email" "Your email address" True True)
          (QConfig (CQWhoami (Just WQEmail)))
    ,Pair (Selector "config.whoami.name" "Your full name" True True)
          (QConfig (CQWhoami (Just WQName)))]
  where fromSelectorList sels =
          H.fromList
            (do x@(Pair selector _) <- sels
                pure (name selector,x))

instance Select Query where
  select q =
    case H.lookup q selectorMap of
      Just (Pair _ query) -> pure query
      Nothing ->
        fail (unlines ["I'm sorry, I can't find a selector matching"
                      ,T.unpack q
                      ,"Try `louse get selectors` for a list."])

instance MonadIO m => SelectGet m Query Text where
  selectGet (QBugs q) = selectGet q
  selectGet (QConfig q) = selectGet q
  selectGet QSelectors =
    return (Success (unpackSelectors
                       (do (Pair selector _) <- H.elems selectorMap
                           return selector)))

instance MonadIO m => SelectSet m Query Text where
  selectSet (QBugs _) _ =
    fail (unlines ["You can't use \"set\" on bugs as a whole (although you can change attributes"
                  ,"of individual bugs). I haven't written that code yet, but you will be able to"
                  ,"in the future. Probably."])
  selectSet (QSelectors) _ =
    fail "Selectors are not settable. Settable? Whatever. You get the point. Bad user!"
  selectSet (QConfig q) x = selectSet q x

instance MonadIO m => SelectGet m BugsQuery Text where
  selectGet x =
    do louse <-
         (=<<) runExceptional (liftIO readLouse)
       let allBugs = louseBugs louse
       pure (Success (T.unlines (fmap (T.take 8)
                                      (M.keys (case x of
                                                 BQAll -> allBugs
                                                 BQClosed ->
                                                   M.filter (not . bugOpen) allBugs
                                                 BQOpen ->
                                                   M.filter bugOpen allBugs)))))

  
instance MonadIO m => SelectGet m ConfigQuery Text where
  selectGet (CQWhoami x) = selectGet x

instance MonadIO m => SelectSet m ConfigQuery Text where
  selectSet (CQWhoami (Just x)) = selectSet x
  selectSet (CQWhoami Nothing) =
    fail "You can't set your entire identity (yet). I'm working on it, though."

instance MonadIO m => SelectGet m (Maybe WhoamiQuery) Text where
  selectGet x =
    do lc <- liftIO readLouseConfig
       return (return (case whoami lc of
                         Anonymous -> "Anonymous"
                         Person n e ->
                           case x of
                             Nothing ->
                               TE.decodeUtf8 (encode (Person n e))
                             Just WQName -> n
                             Just WQEmail -> e))
  
instance MonadIO m => SelectSet m WhoamiQuery Text where
  selectSet WQName newName =
    do c@(LouseConfig oldPerson) <- liftIO readLouseConfig
       if (T.toLower newName) ==
          "anonymous"
          then liftIO (writeLouseConfig (LouseConfig Anonymous))
          else liftIO (case oldPerson of
                         Person n e ->
                           writeLouseConfig
                             (c {whoami =
                                   Person newName e})
                         Anonymous ->
                           writeLouseConfig
                             (c {whoami =
                                   Person newName mempty}))
  selectSet WQEmail newEmail =
    liftIO (do c@(LouseConfig oldPerson) <- readLouseConfig
               case oldPerson of
                 Person n e ->
                   writeLouseConfig
                     (c {whoami =
                           Person n newEmail})
                 Anonymous ->
                   writeLouseConfig
                     (c {whoami =
                           Person mempty newEmail}))


-- This is the old version of 'select'
-- -- let qPieces = T.splitOn "." q
-- -- in case headMay qPieces of
-- --      Nothing ->
-- --        fail "You have to submit a query."
-- --      Just "bugs" ->
-- --        fmap QBugs
-- --             (case atMay qPieces 1 of
-- --                Nothing -> pure BQAll
-- --                Just x ->
-- --                  (case x of
-- --                     "all" -> pure BQAll
-- --                     "closed" -> pure BQClosed
-- --                     "open" -> pure BQOpen
-- --                     x ->
-- --                       fail (mappend "bug: no match for value " (T.unpack x))))
-- --      Just "config" ->
-- --        fmap QConfig
-- --             (case atMay qPieces 1 of
-- --                Nothing ->
-- --                  fail "I need something more specific than \"config\""
-- --                Just "whoami" ->
-- --                  fmap CQWhoami
-- --                       (case atMay qPieces 2 of
-- --                          Nothing ->
-- --                            pure Nothing
-- --                          Just "name" ->
-- --                            pure (Just WQName)
-- --                          Just "email" ->
-- --                            pure (Just WQEmail)))
-- --      Just x ->
-- --        fail (mappend "toplevel: no match for value " (T.unpack x))
