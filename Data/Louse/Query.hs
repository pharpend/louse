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
import Control.Monad (mzero)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.Louse.DataFiles
import Data.Louse.Query.Selector
import Data.Louse.Read
import Data.Louse.Types
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Paths_louse
import Data.Version
import Data.Yaml
import Safe

data Query
  = QAbout {qAboutQuery :: Maybe AboutQuery}
  | QBugs {qBugsQuery :: BugsQuery}
  | QConfig {qConfigQuery :: ConfigQuery}
  deriving (Eq,Show)

data AboutQuery
  = AQLicense
  | AQSchema SchemaQuery
  | AQSelectors
  | AQTutorial
  | AQVersion
  deriving (Eq,Show)

data BugsQuery
  = BQSuchThat SuchThatQuery
  | BQBug Text
          BugQuery
  deriving (Eq,Show)

data SuchThatQuery
  = STQAll
  | STQClosed
  | STQOpen
  deriving (Eq,Show)

data BugQuery
  = BQClosed
  | BQComments
  | BQCreationDate
  | BQDescription
  | BQOpen
  | BQReporter
  | BQShow
  | BQTitle
  deriving (Eq,Show)

data ConfigQuery = CQWhoami
    { cqWhoamiQuery :: Maybe WhoamiQuery
    } deriving (Eq,Show)

data WhoamiQuery
    = WQName
    | WQEmail
    deriving (Eq,Show)

data SchemaQuery
    = SQAll
    | SQShow Text
    deriving (Eq,Show)

data SelectorPair
  =
    -- |This is for hashmap lookup
    StaticPair {pairSelector :: Selector
               ,pairQuery :: Query}
  |
    -- |This is for pairs that need special parsing, such as retrieving a
    --bug by its id. I can't list every bug in a static hashmap.
    ParsedPair {pairSelector :: Selector
               ,pairParser :: (Text -> Exceptional Query)}
  |
    -- |This is for pairs for which I am too lazy to write a parser at
    -- the moment
    NullPair {pairSelector :: Selector}
  deriving (Eq,Show)
  
instance Eq (Text -> Exceptional Query) where
  (==) _ _ = False

instance Show (Text -> Exceptional Query) where
  show _ = "Function :: Text -> Exceptional Query"

-- Take a
splitToPieces :: Text -> (Text,Text,Text)
splitToPieces q = ( T.takeWhile (/= '{') q
                  , T.takeWhile
                        (/= '}')
                        (T.drop 1 (T.dropWhile (/= '{') q))
                  , T.drop 1 (T.dropWhile (/= '}') q))

selectorMap :: HashMap Text SelectorPair
selectorMap =
  mconcat (do x <- selectorList
              case x of
                StaticPair selector _ ->
                  pure (H.singleton (name selector)
                                    x)
                _ -> pure mempty)

selectorList :: [SelectorPair]
selectorList =
  [StaticPair (Selector "about" "About louse." True False)
              (QAbout Nothing)
  ,StaticPair (Selector "about.license" "Print out louse's license (GPL-3)." True False)
              (QAbout (Just AQLicense))
  ,StaticPair (Selector "about.schema" "List the various schemata of louse's data files" True False)
              (QAbout (Just (AQSchema SQAll)))
  ,ParsedPair
     (Selector "about.schema{SCHEMA_NAME}" "Get a specific schema." True False)
     (\t ->
        let (sel,ident,rest) = splitToPieces t
        in if |  sel /= "about.schema" ->
                fail (mappend "Initial selector doesn't match \"about.schema\". Instead is "
                              (T.unpack sel))
              |  rest /= mempty ->
                fail (mappend "There isn't supposed to be anything after \"{SCHEMA_NAME\"}. Instead, I found "
                              (T.unpack rest))
              |  otherwise ->
                return (QAbout (Just (AQSchema (SQShow ident)))))
  ,StaticPair (Selector "about.selectors" "List all of the selectors" True False)
              (QAbout (Just AQSelectors))
  ,StaticPair (Selector "about.tutorial" "Print the tutorial" True False)
              (QAbout (Just AQTutorial))
  ,StaticPair (Selector "about.version" "Print out the version" True False)
              (QAbout (Just AQVersion))
  ,StaticPair (Selector "config.whoami" "Information about yourself" True False)
              (QConfig (CQWhoami Nothing))
  ,StaticPair (Selector "config.whoami.email" "Your email address" True True)
              (QConfig (CQWhoami (Just WQEmail)))
  ,StaticPair (Selector "config.whoami.name" "Your full name" True True)
              (QConfig (CQWhoami (Just WQName)))
  ,StaticPair (Selector "repo.bugs" "List the bugs" True False)
              (QBugs (BQSuchThat STQAll))
  ,StaticPair (Selector "repo.bugs.closed" "List only the closed bugs" True False)
              (QBugs (BQSuchThat STQClosed))
  ,StaticPair (Selector "repo.bugs.open" "List the open bugs" True False)
              (QBugs (BQSuchThat STQOpen))
  ,ParsedPair
     (Selector "repo.bugs.{BUGID}"
               "Show the bug whose ident is BUGID. (Short bugids are okay)."
               True
               False)
     (\t ->
        let (sel,ident,rest) = splitToPieces t
        in if |  sel /= "repo.bugs" ->
                fail (mappend "Initial selector doesn't match \"repo.bugs\". Instead is "
                              (T.unpack sel))
              |  rest == mempty ->
                return (QBugs (BQBug ident BQShow))
              |  otherwise ->
                fmap (QBugs .
                      BQBug ident)
                     (case rest of
                        "closed" ->
                          return BQClosed
                        "comments" ->
                          return BQComments
                        "creation-date" ->
                          return BQCreationDate
                        "description" ->
                          return BQDescription
                        "open" -> return BQOpen
                        "reporter" ->
                          return BQReporter
                        "title" -> return BQTitle
                        x ->
                          fail (mconcat ["No match for repo.bugs{"
                                        ,T.unpack ident
                                        ,"}."
                                        ,T.unpack x])))
  ,NullPair (Selector "repo.bugs{BUGID}.closed" "The opposite of \"open\"." True True)
  ,NullPair (Selector "repo.bugs{BUGID}.comments" "View all of the comments on a bug" True False)
  ,NullPair (Selector "repo.bugs{BUGID}.creation-date"
                      "The date at which the bug was created."
                      True
                      True)
  ,NullPair (Selector "repo.bugs{BUGID}.description" "Further description of the bug." True True)
  ,NullPair (Selector "repo.bugs{BUGID}.open" "Whether or not the bug is open." True True)
  ,NullPair (Selector "repo.bugs{BUGID}.reporter" "The person who reported the bug." True True)
  ,NullPair (Selector "repo.bugs{BUGID}.title" "The title of the bug." True True)]
  
instance Select IO Query where
  select q =
    case H.lookup q selectorMap of
      -- If we have a match in the hashmap, find it
      Just (StaticPair _ query) ->
        return (Success query)
      Just _ ->
        return (Failure "Query.hs:231 - some weird stuff is happening here")
      Nothing ->
        let parsedPairs =
              catMaybes (fmap (\x ->
                                 case x of
                                   foo@(ParsedPair _ _) ->
                                     Just foo
                                   _ -> Nothing)
                              selectorList)
            parses =
              fmap (\(ParsedPair _ parser) -> parser q) parsedPairs
            successfulParses =
              catMaybes (fmap (\x ->
                                 case x of
                                   Success foo ->
                                     Just foo
                                   _ -> Nothing)
                              parses)
            failMsgs =
              catMaybes (fmap (\x ->
                                 case x of
                                   Failure foo ->
                                     Just foo
                                   _ -> Nothing)
                              parses)
        in return (case headMay successfulParses of
                     Nothing ->
                       Failure (mappend (mconcat ["\nI wasn't able to parse the selector.\n"
                                                 ,"Most of the selectors can be looked up via a static hashmap.\n"
                                                 ,"If there was a match in the hashmap, I would have gone with that.\n"
                                                 ,"Since there wasn't, I ran the selector through my list of custom parsers.\n"
                                                 ,"You are seeing this message because every single parser failed.\n"
                                                 ,"Here are the error messages from each parser:\n"])
                                        (unlines (fmap (mappend (replicate 4 ' ')) failMsgs)))
                     Just x -> Success x)

instance SelectGet IO Query Text where
  selectGet fp (QBugs q) = selectGet fp q
  selectGet fp (QConfig q) = selectGet fp q
  selectGet _ (QAbout Nothing) =
    do fpath <- getDataFileName "README.md"
       fmap Success (TIO.readFile fpath)
  selectGet fp (QAbout (Just q)) = selectGet fp q

instance SelectSet IO Query Text where
  selectSet _ (QBugs _) _ =
    fail (unlines ["You can't use \"set\" on bugs as a whole (although you can change attributes"
                  ,"of individual bugs). I haven't written that code yet, but you will be able to"
                  ,"in the future. Probably."])
  selectSet f (QConfig q) x = selectSet f q x
  selectSet _ _ _ =
    fail "Selectors are not settable. Settable? Whatever. You get the point. Bad user!"

instance SelectGet IO BugsQuery Text where
  selectGet dirPath query =
    do louse <-
         bindr (readLouseFrom dirPath)
               (\case
                  Failure err ->
                    fail (mconcat ["I wasn't able to read a louse repo from the current directory.\n"
                                  ,"Here's the full error message:\n    "
                                  ,err])
                  Success s -> return s)
       let successList = return . Success . T.unwords
       case query of
         BQSuchThat STQAll ->
           successList (M.keys (louseBugs louse))
         BQSuchThat STQClosed ->
           successList
             (do (bugid,bug) <-
                   M.toList (louseBugs louse)
                 if not (bugOpen bug)
                    then return bugid
                    else mzero)
         BQSuchThat STQOpen ->
           successList
             (do (bugid,bug) <-
                   M.toList (louseBugs louse)
                 if bugOpen bug
                    then return bugid
                    else mzero)
         BQBug ident foo ->
           case lookupShortKey louse ident of
             Nothing ->
               return (Failure (mappend "There is no bug in the current repo with ident: "
                                        (T.unpack ident)))
             Just bug ->
               case foo of
                 BQClosed ->
                   return (Success (T.pack (show (not (bugOpen bug)))))
                 BQComments ->
                   return (Success (TE.decodeUtf8 (encode (bugComments bug))))
                 BQCreationDate ->
                   return (Success (T.pack (show (bugCreationDate bug))))
                 BQDescription ->
                   return (Success (bugDescription bug))
                 BQOpen ->
                   return (Success (T.pack (show (bugOpen bug))))
                 BQReporter ->
                   return (Success (TE.decodeUtf8 (encode (bugReporter bug))))
                 BQShow ->
                   return (Success (TE.decodeUtf8 (encode bug)))
                 BQTitle ->
                   return (Success (bugTitle bug))


instance SelectGet IO AboutQuery Text where
  selectGet _ AQLicense =
    fmap Success (readDataFileText "LICENSE")
  selectGet _ AQVersion =
    return (Success (mappend (T.pack (showVersion version)) "\n"))
  selectGet _ AQTutorial =
    fmap Success (readDataFileText "TUTORIAL.md")
  selectGet _ AQSelectors =
    return (Success (unpackSelectors
                       (do selectorPair <- selectorList
                           case selectorPair of
                             StaticPair s _ -> return s
                             ParsedPair s _ -> return s
                             NullPair s -> return s)))
  selectGet _ x =
    fail (mconcat ["You can't get ",show x," yet"])

instance SelectGet IO ConfigQuery Text where
  selectGet fp (CQWhoami x) = selectGet fp x

instance SelectSet IO ConfigQuery Text where
  selectSet fp (CQWhoami (Just x)) = selectSet fp x
  selectSet _ (CQWhoami Nothing) =
    fail "You can't set your entire identity (yet). I'm working on it, though."

instance SelectGet IO (Maybe WhoamiQuery) Text where
  selectGet _ x =
    do lc <- readLouseConfig
       return (Success (case whoami lc of
                          Anonymous -> "Anonymous"
                          Person n e ->
                            case x of
                              Nothing ->
                                TE.decodeUtf8 (encode (Person n e))
                              Just WQName -> n
                              Just WQEmail -> e))

instance SelectSet IO WhoamiQuery Text where
  selectSet _ WQName newName =
    do c@(LouseConfig oldPerson) <- readLouseConfig
       if (T.toLower newName) ==
          "anonymous"
          then writeLouseConfig (LouseConfig Anonymous)
          else case oldPerson of
                 Person _ e ->
                   writeLouseConfig
                     (c {whoami =
                           Person newName e})
                 Anonymous ->
                   writeLouseConfig
                     (c {whoami =
                           Person newName mempty})
  selectSet _ WQEmail newEmail =
    do c@(LouseConfig oldPerson) <- readLouseConfig
       case oldPerson of
         Person n _ ->
           writeLouseConfig
             (c {whoami = Person n newEmail})
         Anonymous ->
           writeLouseConfig
             (c {whoami = Person mempty newEmail}) 


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
