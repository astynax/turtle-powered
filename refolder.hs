#!/usr/bin/env stack
{- stack script
--resolver=lts-13.24
--package=turtle
--package=system-filepath
--package=text
--package=foldl
-}
{-# LANGUAGE OverloadedStrings #-}

-- | Converts a set of .zip-files with music that looks like
--   artist1 - album_name.zip
--   artist1 - other_album.zip
--   artist2 - album_name.zip
-- to the set of folders with music and renamed covers
-- (only first image become a cover). Result will look like
--   artist1/
--     album name/
--       01 - foo.mp3
--       ...
--       cover.jpg
--     other album/
--       ...

import Control.Arrow
import Control.Foldl (list)
import Control.Monad
import Data.Maybe
import qualified Data.Text as Text
import Filesystem.Path.CurrentOS
import Turtle

main = do
    archs <- collectWithExts ["zip"] $ ls "."
    let pairs = map (id &&& prepareDirName) archs
    -- print dirs
    forM_ pairs $ \(arch, dir) -> printf (fp % " -> " % fp % "\n") arch dir
    -- make possible to check out the paths
    echo "Press any key to continue..." >> void getLine
    -- unpack
    forM_ pairs $ \(arch, dir) -> do
        mktree dir
        stdout $ Main.unzip arch dir ["*.mp3", "*.jpg", "*.png"]
    -- rename covers
    imgs <- collectWithExts ["jpg", "png"] $ lstree "."
    forM_ imgs $ \f -> do
        let newName = directory f </> "cover" <.> fromMaybe "jpg" (extension f)
        printf ("Rename " % fp % " to " % fp % "\n") f newName
        mv f newName

prepareDirName =
    rebuild . Text.breakOn " - " . Text.replace "_" " " . fromPath . basename
  where
    rebuild (artist, album) =
        fromText artist </>
        fromText (fromMaybe album $ Text.stripPrefix " - " album)

unzip arch dest masks =
    inproc
        "unzip"
        (["-j", fromPath arch] ++ masks ++ ["-d", fromPath dest])
        Turtle.empty

fromPath p = either (error $ "Bad FilePath: " ++ show p) id $ toText p

collect = flip fold list

quoted = Text.cons '"' . flip Text.snoc '"'

collectWithExts exts =
    fmap (filter $ (`elem` map Just exts) . extension) . collect
