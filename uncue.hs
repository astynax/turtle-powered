#!/usr/bin/env stack
{- stack
   script
   --resolver lts-6.25
   --package turtle
   --package text
   --package system-filepath
-}
{-# LANGUAGE OverloadedStrings #-}
{-
This script
- encodes the .CUE-file to UTF-8
- splits the .FLAC-file into separate tracks
- reads the encoded .CUE-file and gets the titles
- renames the tracks

Requirements:
- cuebreakpoints ("cuetools" package)
- shnsplit ("shntool" package)
-}

import Control.Monad
import qualified Data.Text as T
import Filesystem.Path.CurrentOS as FP hiding (empty)
import Turtle hiding (fp)
import Turtle.Format
import Turtle.Options
import Turtle.Pattern

main = do
    rawCueFile <- options "UnCUE" $ argPath "CUEFILE" "file to process"
    require rawCueFile
    -- prepare cue file
    let cueFile = rawCueFile <.> "utf8" <.> "cue"
    ensure cueFile $ convert rawCueFile cueFile
    -- split flac
    let flacFile = replaceExtension rawCueFile "flac"
    require flacFile
    ensure "split-track01.flac" $ splitFlac flacFile cueFile
    -- read indexes and titles
    echo "Reading the CUE..."
    cueContent <- strict $ input cueFile
    let pairs = match pairRegEx cueContent
    foldIO (select pairs) (FoldM (const rename_) (pure ()) return)

-- steps
convert inp outp = do
    printf ("Converting " % qfp % " to UTF-8...\n") inp
    call_ $ format ("iconv -f cp1251 -t utf-8 " % qfp % " > " % qfp) inp outp

splitFlac flacp cuep = do
    printf ("Spliting a " % qfp % "...") flacp
    view $ shnsplit $ cuebreakpoints empty
  where
    shnsplit = inshell $ format ("shnsplit -o flac " % qfp) flacp
    cuebreakpoints = inshell (format ("cuebreakpoints " % qfp) cuep)

rename_ (tn, tt) = do
    let inp = fromText $ format ("split-track" % s % ".flac") tn
        outp = fromText $ format (s % " - " % s % ".flac") tn tt
    echo $ format ("Renaming " % qfp % " to " % qfp) inp outp
    mv inp outp

-- regexes
pairRegEx =
    has $ do
        "TRACK "
        track <- selfless $ plus digit
        selfless spaces1
        selfless . plus $ notChar 'T'
        "TITLE \""
        title <- selfless $ plus (notChar '"')
        "\""
        return (track, title)

-- helpers
qfp :: Format r (Turtle.FilePath -> r)
qfp = makeFormat $ either (error . T.unpack) (\x -> "\"" <> x <> "\"") . toText

ensure p m = testfile p >>= flip unless (m >> require p)

require p =
    ensure p $ do
        printf ("File " % qfp % " not found!\n") p
        exit $ ExitFailure 1

call_ = view . flip inshell empty
