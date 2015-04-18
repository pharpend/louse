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
-- Module      : Main
-- Description : Runs louse
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This is some of the most horrible code I have ever written. Please
-- don't judge me.

module Main where

import           Control.Monad
import qualified Data.ByteString as Bs
import           Data.Default
import           Data.Foldable
import           Data.IORef
import           Data.Monoid
import           Data.Louse
import           Data.Traversable
import           Safe
import           System.Directory
import           System.Posix.Env.ByteString

data ReLouse = ReLouse { _working_directory :: FilePath, _mode :: Mode }
  deriving (Eq, Show)

data Mode = Help (Maybe Bs.ByteString)
          | Init
          | Add Thing
          | Comment BugId
  deriving (Eq, Show)

data Thing = Bug | Person
  deriving (Eq, Show)

defaultReLouse :: IO ReLouse
defaultReLouse = ReLouse <$> getCurrentDirectory <*> pure (Help Nothing)

getReLouse :: [Bs.ByteString] -> IO ReLouse
getReLouse args = do
  let (fstCommand :: Maybe Bs.ByteString) = headMay args
      (sndCommand :: Maybe Bs.ByteString) = headMay =<< tailMay args
  relouse <- newIORef =<< defaultReLouse
  if | or (fmap (==fstCommand) (fmap Just ["-h", "help", "--help"])) ->
          modifyIORef' relouse (\o -> o {_mode = Help sndCommand})
     | otherwise -> pure ()
  readIORef relouse

main :: IO ()
main = runLouse =<< getReLouse =<< getArgs

runLouse :: ReLouse -> IO ()
runLouse l =
  case l of
    ReLouse _ (Help Nothing) -> fixme notImplemented
    ReLouse _ (Help (Just x)) ->
      case x of
        _ -> fixme notImplemented
    _ -> fixme notImplemented

fixme :: String -> IO ()
fixme = fail . mappend "FIXME: "

notImplemented :: String
notImplemented = "Feature not yet implemented."

_program_name :: Bs.ByteString
_program_name = "louse"
