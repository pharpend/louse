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
-- Module      : Data.Louse.IO
-- Description : I/O operations for Louse
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Louse.IO (module Data.Louse.IO, module Data.Louse.IO.DataFiles, module Data.Louse.IO.Read, randomIdent) where

import           Control.Monad
import           Crypto.Random
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Base16 as Bs16
import           Data.Louse.IO.DataFiles
import           Data.Louse.IO.Read
import qualified Data.Map as M
import           Data.Monoid
import           System.Directory
import           System.IO.Error

status :: FilePath -> IO ()
status = do
  l <- readLouseFromErr dir
  let numberOpenBugs = M.

initInDir :: FilePath -> Bool -> IO ()
initInDir dr force = do
  let dir = dr <> "/"
      rpath = mappend dir
  louseDirExists <- doesDirectoryExist $ rpath _louse_dir
  -- Try to read a louse
  tryIOError (readLouseFromErr dir) >>= \case
    -- If we can read one, something has gone terribly wrong
    Right louse -> unless force . fail $ "Louse is already initialized in " <> dir <> "."
    Left err
      -- If we can't read one, that's good
      | isDoesNotExistError err -> pure ()
      -- Otherwise, bad user!
      | otherwise -> ioError err

  -- If all is well, carry on
  createDirectoryIfMissing True $ rpath _louse_dir
  readDataFile _templ_new_project >>= Bs.writeFile (rpath _project_json <> ".sample")
  createDirectoryIfMissing True $ rpath _bugs_dir
  createDirectoryIfMissing True $ rpath _people_dir

-- |Create a random 20-byte-long indentifier
randomIdent :: IO Bs.ByteString
randomIdent =
  cprgCreate <$> createEntropyPool >>=
  \(rng :: SystemRNG) ->
    let (bs,_) = cprgGenerate _ident_length rng
    in pure $ Bs16.encode bs
  where _ident_length = 20
