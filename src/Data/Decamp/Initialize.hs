-- decamp-bugtrack - distributed bugtracker
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
-- Module      : Data.Decamp.Initialize
-- Description : Module to initialize Decamp in directory.
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 

module Data.Decamp.Initialize where

import qualified Data.ByteString as Bs
import           Data.Decamp.Aeson
import           Data.Decamp.Types
import           Data.List.Utils
import           Data.Monoid
import           Data.Text (pack)
import           System.Console.Readline
import           System.Directory
import           System.Exit
import           System.IO

mkProject :: String -- ^Project Name
          -> Maybe String -- ^Maintainer name
          -> Maybe String -- ^Maintainer email
          -> Maybe String -- ^Project home page
          -> Maybe String -- ^Description
          -> Project -- ^ Resulting @Project@
mkProject nom mtrn mtre hp descr =
  Project (pack nom) mtr (packmaybe hp) (packmaybe descr) []
  where
    mtr =
      case (mtrn, mtre) of
        (Nothing, Just e) ->
          Just .
          Person "Anonymous" $
            pack e
        (Just n, Just e) ->
          Just $
            Person (pack n) (pack e)
        (Just n, Nothing) -> Nothing
        (Nothing, Nothing) -> Nothing
    packmaybe x =
      case x of
        Nothing -> Nothing
        Just d  -> Just $ pack d

interactiveInit :: Bool -> IO ()
interactiveInit bar = do
  hSetBuffering stdout NoBuffering
  putStrLn
    "Alright, I'm going to ask you some questions about your project. If you make a mistake, type C-c C-c to cancel and run this command again.\n"
  dirnom <- last . split "/" <$> getCurrentDirectory
  let getPrjName = rdline ("1. What is this project's name? (Will default to " <> dirnom <> ") ") >>=
                   \case
                     Nothing  -> pure dirnom
                     Just nom -> pure nom
  nom <- getPrjName
  mtnrNom <- rdline "2. What's the name of the project maintainer? (Leave empty for anonymous) "
  mtnrEml <- rdline "3. What's the email of the project maintainer? (Leave empty for anonymous) "
  homepg <- rdline "4. If the project has a home page, what is it? (Leave empty for no home page) "
  desc <- rdline "5. Give a one-line description of the project (Leave empty for no description): "
  writeProject bar $ mkProject nom mtnrNom mtnrEml homepg desc

  where
    rdline s = readline s >>= \case
                 Nothing -> pure Nothing
                 Just x -> if | words x == [] -> pure Nothing
                              | otherwise -> pure $ Just x

writeProject :: Bool -> Project -> IO ()
writeProject bar p = do
  hSetBinaryMode stdout True
  let projectStr = encodeProject p
  cwd <- getCurrentDirectory
  let decampDir = cwd <> "/.decamp"
  let populate dir = do
        let prjpth = dir <> "/project.json"
        encodeProjectFile prjpth p
        Bs.hPut stdout $ encodeProject p
  if | bar -> populate cwd
     | otherwise -> do
        createDirectory decampDir
        populate decampDir
