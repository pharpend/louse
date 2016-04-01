-- louse - command line bugtracker interface
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
-- Module      : Louse.Fetch
-- Description : Fetches things from various bugtrackers
-- Copyright   : Copyright (c) 2015, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX

module Louse.Fetch where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Yaml as Y
import GitHub
import GitHub.Data.Name
import GitHub.Endpoints.Issues
import Numeric.Natural

data A x = A x
         | An x

fetchGitHubIssuesForRepo :: String -- ^Owner
                         -> String -- ^Repo name
                         -> IO ByteString
fetchGitHubIssuesForRepo user repoName =
  issuesForRepo (n' user)
                (n' repoName)
                mempty
  >>= \case
        Left err -> fail (show err)
        Right issues -> return $ B8.pack (show issues)
  where n' = N . T.pack  
