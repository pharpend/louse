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
-- Module      : Louse.Bug
-- Description : The type for a Louse Bug
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- The type for a louse bug.

module Louse.Bug where

import Louse.Comment
import Louse.Person

import Control.Applicative
import Control.Exceptional
import Control.Monad (mzero)
import Data.Ord (comparing)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Yaml

-- |The type for a bug
-- 
-- Since: 0.1.0.0
data Bug =
  Bug {bugTitle :: Title
      ,bugDescription :: Description
      ,bugAuthor :: Author
      ,bugTime :: UTCTime
      ,bugComments :: [Comment]
      ,bugOpen :: Bool}
  deriving (Eq,Show)

-- |'Bug' is trivially an instance of 'FromBug'
-- 
-- Since: 0.1.0.0
instance FromBug Bug where
  fromBug = id

-- |'Bug' is trivially an instance of 'ToBug'
-- 
-- Since: 0.1.0.0
instance ToBug Bug where
  toBug = id

-- |Since: 0.1.0.0
instance FromJSON Bug where
  parseJSON (Object v) = Bug <$> v .: "bug-title"
                             <*> v .: "bug-description"
                             <*> (v .: "bug-author" <|> 
                                  v .: "bug-reporter")
                             <*> v .: "bug-time"
                             <*> v .: "bug-comments"
                             <*> v .: "bug-open"

-- | Since: 0.1.0.0
instance Ord Bug where
  compare = comparing bugTime

-- |Typeclass to convert something to a 'Bug'
-- 
-- Since: 0.1.0.0
class ToBug a  where
  toBug :: a -> Bug

-- |Convert something from a 'Bug'
-- 
-- Since: 0.1.0.0
class FromBug a where
  fromBug :: Bug -> a

-- |Yet another newtype over 'Text'. This is to make sure the
-- description is less than (or equal to) 8192 characters.
-- 
-- Use 'mkDescription' to make a description. This is an instance of
-- 'IsString', too, so, in pure code, you can just write plain strings,
-- and turn on the OverloadedStrings extension.
-- 
-- >>> :set -XOverloadedStrings 
-- >>> "hello" :: Description
-- Description {unDescription = "hello"}
-- it :: Description
-- 
-- If you give invalid input, then there will be an error:
-- 
-- >>> "" :: Description
-- *** Exception: Description mustn't be empty.
-- 
-- Since: 0.1.0.0
newtype Description =
  Description {unDescription :: Text}
  deriving (Eq)

-- |Attempt to make a description from a pure 'Text' value. This returns
-- an error if the description is empty.
-- 
-- Since: 0.1.0.0
mkDescription :: Text -> Exceptional Description
mkDescription t
  | T.null t = fail "Description mustn't be empty."
  | otherwise = return (Description t)

-- |Compares by the value of 'unDescription'.
-- 
-- Since: 0.1.0.0
instance Ord Description where
  compare = comparing unDescription

-- |Since: 0.1.0.0
instance Show Description where
  show = T.unpack . unDescription

-- |Note that this will throw an error if given invalid input.
-- 
-- Since: 0.1.0.0
instance IsString Description where
  fromString s =
    case mkDescription (T.pack s) of
      Failure foo -> error foo
      Success bar -> bar

-- |Since: 0.1.0.0
instance FromJSON Description where
  parseJSON (String s) = runExceptional $ mkDescription s
  parseJSON _ = mzero

-- |Since: 0.1.0.0
instance ToJSON Description where
  toJSON (Description s) = String s

-- |A newtype over 'Text'. Haskell doesn't have dependent types, so I
-- have to use a hack called "smart constructors" to make sure 
-- 
-- > 0 < title_length <= 64
-- 
-- Use 'mkTitle' to make a title. Alternatively, you could turn on
-- OverloadedStrings, and use 'Title''s 'IsString' instance:
-- 
-- >>> :set -XOverloadedStrings 
-- >>> "hello" :: Title
-- Title {unTitle = "hello"}
-- it :: Title
-- 
-- Note that if you give invalid input, then there will be an error:
-- 
-- >>> "" :: Title
-- *** Exception: Title mustn't be empty.
-- >>> fromString (mconcat (replicate 50 "foo")) :: Title
-- *** Exception: Title mustn't be >64 characters long.
-- 
-- Since: 0.1.0.0
newtype Title =
  Title {unTitle :: Text}
  deriving (Eq)

-- |Attempt to make a title, returning an error message if the length is
-- longer than 64 characters, or if the title is empty.
-- 
-- Since: 0.1.0.0
mkTitle :: Text -> Exceptional Title
mkTitle t
  | T.null t = fail "Title mustn't be empty."
  | 64 < T.length t = fail "Title mustn't be >64 characters long."
  | otherwise = return (Title t)

-- |Compares by the value of @unTitle@.
-- 
-- Since: 0.1.0.0
instance Ord Title where
  compare = comparing unTitle

-- |Since: 0.1.0.0
instance Show Title where
  show = T.unpack . unTitle

-- |Note that this will throw an error if you give it an invalid value.
-- 
-- Since: 0.1.0.0
instance IsString Title where
  fromString s =
    case mkTitle (T.pack s) of
      Failure err -> error err
      Success s -> s

-- |Since: 0.1.0.0
instance FromJSON Title where
  parseJSON (String s) = runExceptional $ mkTitle s
  parseJSON _ = mzero

-- |Since: 0.1.0.0
instance ToJSON Title where
  toJSON (Title s) = String s

-- |A nice little report explaining the status of a set of bugs.
-- 
-- Since: 0.1.0.0
status :: FilePath              -- ^The directory holding the status report
       -> [Bug]                 -- ^The list of bugs
       -> String                -- ^The status string
status fp bugs =
  unlines [ "Louse status for " ++ fp
          , ""
          , "Number of open bugs: " ++ show (openBugs bugs)
          , "Number of closed bugs: " ++ show (closedBugs bugs)
          , "Number of "
          ]

-- |Count the number of open bugs in a list.
-- 
-- Since: 0.1.0.0
openBugs :: [Bug] -> Int
openBugs = length . filter bugOpen

-- |Count the number of closed bugs in a list.
-- 
-- Since: 0.1.0.0
closedBugs :: [Bug] -> Int
closedBugs = length . filter (not . bugOpen)
