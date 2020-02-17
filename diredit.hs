#!/usr/bin/env stack
{- stack script
--resolver=lts-13.24
--package=turtle
--package=system-filepath
--package=text
--package=foldl
--package=managed
--package=process
-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Foldl (list)
import Control.Monad
import Control.Monad.Managed
import Data.Foldable hiding (fold)
import qualified Data.Text as Text
import Filesystem.Path.CurrentOS
import System.Process as Process
import Turtle

-- | opens a list of files in cwd in Vim and then applies the changes

edit :: MonadIO m => Turtle.FilePath -> m ()
edit fn = do
  () <$ Turtle.system ((Process.proc "vim" [Text.unpack $ format fp fn])
    { create_new_console = True
    }) mempty

main :: IO ()
main = do
  with (mktempfile "/tmp/" "diredit") $ \tempfile -> do
    paths <- (`fold` list) $ fmap (unsafeTextToLine . format fp) $ ls "."
    let
      help =
        [ "# Edit any path or comment it out but don't reorder or delete any lines!"
        , "# You will need to confirm an actual renaming, so you can just exit editor."
        ]
    output tempfile $ select $ help ++ paths
    edit tempfile
    newPaths <- drop (length help) <$> fold (input tempfile) list
    if (length newPaths /= length paths)
      then (echo "Number of lines doesn't match the old one!")
      else do
        let
          scenario =
            [ (l2p old, l2p new)
            | (old, new) <- zip paths newPaths
            , not (Text.isPrefixOf "#" $ lineToText new)
            , old /= new
            ]
        echo "Changes:"
        for_ scenario $ \(old, new) -> do
          printf ("\"" % fp % "\" -> \"" % fp % "\"\n") old new
        echo "Continue?"
        void $ readline
        for_ scenario $ \(old, new) -> do
          let base = parent new
          exist <- testdir base
          when (not exist) $ mktree base
          mv old new
        echo "Done."
  where
    l2p = fromText . lineToText
