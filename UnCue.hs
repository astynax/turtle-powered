module Main (main) where

import "base" Control.Monad
import "system-filepath" Filesystem.Path.CurrentOS as FP hiding (empty)
import "turtle" Turtle hiding (fp)
import "turtle" Turtle.Format

main :: IO ()
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
  mapM_ rename_ $ match pairRegEx cueContent

-- steps
convert inp outp = do
  printf ("Converting " % qfp % " to UTF-8...\n") inp
  input inp & iconvWinToUtf8 & output outp

splitFlac flacp cuep = do
  printf ("Splitting a " % qfp % "...") flacp
  input cuep & cuebreakpoints & shnsplitFlacs flacp & view

rename_ (tn, tt) = do
  let inp = fromText $ format ("split-track" % s % ".flac") tn
      outp = fromText $ format (s % " - " % s % ".flac") tn tt
  printf ("Renaming " % qfp % " to " % qfp % "\n") inp outp
  mv inp outp

pairRegEx = has $ do
  _ <- "TRACK "
  track <- selfless $ plus digit
  _ <- selfless spaces1
  _ <- selfless . plus $ notChar 'T'
  _ <- "TITLE \""
  title <- selfless $ plus (notChar '"')
  _ <- "\""
  return (track, title)

qfp = makeFormat $ format ("\"" % fp % "\"")

ensure p make = do
  itExists <- testfile p
  unless itExists $
    make >> require p

require p = ensure p $ do
  printf ("File " % qfp % " not found!\n") p
  exit $ ExitFailure 1

toText' = either (const $ error "Oops!") id . toText

-- external programs
shnsplitFlacs flacFile = inproc "shnsplit" ["-o", "flac", toText' flacFile]

cuebreakpoints = inproc "cuebreakpoints" []

iconvWinToUtf8 = inproc "iconv" ["-f", "cp1251", "-t", "utf-8"]
