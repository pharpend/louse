-- decamp - distributed bugtracker
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
-- Module      : Data.Decamp.Internal.Types
-- Description : Internal types for Decamp
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- Types that are really only of use internally in 'Decamp'
-- 

module Data.Decamp.Internal.Types where

import           Crypto.Random
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types (Pair)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Base16 as Bs16
import           Data.ByteString.Lazy (toStrict)
import           Data.Decamp.Internal.MagicValues
import           Data.Decamp.Types
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time
import           Paths_decamp
import           System.Directory
import           System.IO
import           Text.Editor

-- |Create a random 20-byte-long indentifier
randomIdent :: IO ByteString
randomIdent =
  cprgCreate <$> createEntropyPool >>=
  \(rng :: SystemRNG) -> let (bs, _) = cprgGenerate _ident_length rng
                         in pure bs

-- |Convert a 'Mtnr' to a 'Person'
-- 
-- @
-- mtnrToPerson (Mtnr n e) = Person n e
-- @
mtnrToPerson :: Mtnr -> Person
mtnrToPerson (Mtnr n e) = Person n e

-- |Convert a 'Reporter' to a 'Maybe Person'
-- 
-- @
-- repToPerson Anon = Nothing
-- repToPerson (Reporter n e) = Just $ Person n e
-- @
repToPerson :: Reporter -> Maybe Person
repToPerson Anon = Nothing
repToPerson (Reporter n e) = Just $ Person n e

-- |Convert a 'NewBug' into a 'Bug'. It uses 'randomIdent' to get a
-- value for 'bugID', thus it's 'IO Bug', not just 'Bug'
parseNewBug :: NewBug -> IO Bug
parseNewBug (NewBug reporter synopsis description) = do
  currentTime <- getCurrentTime
  bugid <- decodeUtf8 <$> randomIdent
  pure $ Bug bugid (repToPerson reporter) currentTime synopsis description True []

-- |Transient data type for creating a new project
data NewProject =
       NewProject
         { prjName :: Text
         , prjMaintainers :: [Mtnr]
         , prjHomePage :: Maybe Text
         , prjDescr :: Maybe Text
         }
  deriving (Show, Eq)

-- |Transient data type for the maintainer of a project
data Mtnr = Mtnr { mtnrName :: Text, mtnrEmail :: Text }
  deriving (Show, Eq)

-- |Transient data type for the reporter of a bug
data Reporter = Reporter { repName :: Text, repEmail :: Text } | Anon
  deriving (Show, Eq)

-- |Transient data type for a new bug
data NewBug = NewBug { nbReporter :: Reporter, nbSynopsis :: Text, nbDescr :: Maybe Text }
  deriving (Show, Eq)

instance FromJSON NewProject where
  parseJSON (Object v) = NewProject <$> v .:? "name" .!= _repl_working_dir
                                    <*> v .: "maintainers"
                                    <*> v .: "homepage"
                                    <*> v .: "description"
  parseJSON _ = mzero

instance FromJSON Mtnr where
  parseJSON (Object v) = Mtnr <$> v .: "name" <*> v .: "email"
  parseJSON _ = mzero

instance FromJSON Reporter where
  parseJSON (Object v) = Reporter <$> v .: "name" <*> v .: "email"
  parseJSON Null = pure Anon
  parseJSON _ = mzero


instance FromJSON NewBug where
  parseJSON (Object v) =
    NewBug <$> v .: "reporter" <*> v .: "synopsis" <*> v .: "description"
  parseJSON _ = mzero
