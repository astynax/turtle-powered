module Main (main) where

import "base" Control.Monad
import "base" Data.Foldable hiding (fold)
import "foldl" Control.Foldl (list)
import "managed" Control.Monad.Managed
import "process" System.Process as Process
import "system-filepath" Filesystem.Path.CurrentOS
import "turtle" Turtle
import qualified "text" Data.Text as Text

main :: IO ()
main = with (mktempfile "/tmp/" "diredit") $ \tempfile -> do
  paths <- (`fold` list) $ fmap (unsafeTextToLine . format fp) $ ls "."
  output tempfile $ select $ help ++ paths
  edit tempfile
  newPaths <- drop (length help) <$> fold (input tempfile) list
  if length newPaths /= length paths
    then echo "Number of lines doesn't match the old one!"
    else do
      let
        scenario =
          [ (l2p old, l2p new)
          | (old, new) <- zip paths newPaths
          , not (Text.isPrefixOf "#" $ lineToText new)
          , old /= new
          ]
      echo "Changes:"
      for_ scenario $ \(old, new) ->
        printf ("\"" % fp % "\" -> \"" % fp % "\"\n") old new
      echo "Continue?"
      void $ readline
      for_ scenario $ \(old, new) -> do
        let base = parent new
        exist <- testdir base
        when (not exist) $ mktree base
        mv old new
      echo "Done."

help =
  [ "# Edit any path or comment it out "
    <> "but don't reorder nor delete any lines."
  , "# You will need to confirm an actual renaming,"
    <> " so you can just exit."
  ]

edit fn = do
  () <$ Turtle.system ((Process.proc "vim" [Text.unpack $ format fp fn])
    { create_new_console = True
    }) mempty

l2p = fromText . lineToText
