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
-- Module      : Data.Louse.Query.Selector
-- Description : Query information about selectors
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This is a module that is used to get information about various
-- selectors when the user runs something like @louse get selectors@
-- 
-- Note that the actual selector matching isn't implemented with these
-- types.

module Data.Louse.Query.Selector where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T

data Selector =
  Selector {name :: Text
           ,description :: Text
           ,hasGet :: Bool
           ,hasSet :: Bool}
  deriving (Eq,Show)

unpackSelectors :: [Selector] -> Text
unpackSelectors selectors =
  let longestOf field =
        last (sort (fmap T.length (fmap field selectors)))
      spacifyText str numSpaces =
        let diff =
              numSpaces + 4 -
              (T.length str)
        in (mappend str (T.replicate diff " "))
      spacify selector =
        mconcat [spacifyText (name selector)
                             (longestOf name)
                ,spacifyText (description selector)
                             (longestOf description)
                ,spacifyText (T.pack (show (hasGet selector)))
                             4
                ,spacifyText (T.pack (show (hasSet selector)))
                             4]
      headers =
        mconcat [spacifyText "SELECTOR"
                             (longestOf name)
                ,spacifyText "DESCRIPTION"
                             (longestOf description)
                ,spacifyText "GET" 4
                ,spacifyText "SET" 4]
      prettySelectors =
        (sort (do r <- selectors
                  return (spacify r)))
  in T.unlines (mappend [headers
                        ,T.replicate (last (sort (fmap T.length prettySelectors)))
                                     "-"]
                        prettySelectors)
