module Main (main) where

import "base" Control.Arrow ((&&&))
import "base" Control.Monad
import "base" Data.Maybe
import "foldl" Control.Foldl (list)
import "system-filepath" Filesystem.Path.CurrentOS
import "turtle" Turtle
import qualified "text" Data.Text as Text

main :: IO ()
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
    stdout $ unpack arch dir ["*.mp3", "*.jpg", "*.png"]
  -- rename covers
  imgs <- collectWithExts ["jpg", "png"] $ lstree "."
  forM_ imgs $ \img -> do
    let newName = directory img </> "cover" <.> fromMaybe "jpg" (extension img)
    printf ("Rename " % fp % " to " % fp % "\n") img newName
    mv img newName

prepareDirName =
  rebuild
  . Text.breakOn " - "
  . Text.replace "_" " "
  . fromPath
  . basename
  where
    rebuild (artist, album) =
      fromText artist </>
      fromText (fromMaybe album $ Text.stripPrefix " - " album)

unpack arch dest masks =
  inproc
  "unzip"
  (["-j", fromPath arch] <> masks <> ["-d", fromPath dest])
  Turtle.empty

fromPath p = either (error $ "Bad FilePath: " <> show p) id $ toText p

collect = flip fold list

collectWithExts exts =
  fmap (filter $ (`elem` map Just exts) . extension) . collect
