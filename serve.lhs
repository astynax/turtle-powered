#!/usr/bin/env stack
> {- stack script
>   --install-ghc
>   --resolver=lts-6.30
>   --package=system-filepath
>   --package=turtle
>   --package=warp
>   --package=wai
>   --package=wai-app-static
> -}

Setup

> {-# LANGUAGE OverloadedStrings #-}
>
> import Filesystem.Path.CurrentOS
> import Network.Wai.Application.Static
> import Network.Wai.Handler.Warp
> import Turtle
> import WaiAppStatic.Types

Main function

> main = do
>     (port, path) <- options "Simple static server" opts
>     printf ("Serving \""%fp%"\" on localhost:"%d%"\n") path port
>     run port $ app path

Static server w/ disabled caching

> app path =
>     staticApp
>     (defaultFileServerSettings $ encodeString path) { ssMaxAge = NoMaxAge }

CLI-options parser

> opts = (,) <$> port <*> path
>   where
>     port = optInt "port" 'p' "Port to listen to" <|> pure 8000
>     path = argPath "PATH" "Directory to serve" <|> pure "."
