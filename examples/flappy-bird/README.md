# Flappy bird example

Clone of [`Elm Plane`](https://github.com/odedw/elm-plane)

## Build

- How to build with Nix
```
cd examples/flappy-bird
nix-build
```

- How to run `GHCJSi` with Emacs?
```
# start nix shell
nix-shell -A env
# run cabal with GHCJS
$ cabal configure --ghcjs
# Start Emacs and run C+c C+l to load "Main.hs" file with `GHCJSi` in Emacs
```

- How to check changes in a browser?
```
cabal repl
# open browser localhost:6400
import Miso.Dev # needed to have a reload in a browser later w/o any issues
:r # do a reload whenever a code change happened
clearBody >> main # reloads changes in a browser at localhost:6400
```
_Note: By going that way it's not possible to run it in parallel with `ghcjsi` in Emacs as described before by running both at port 6400. You will get an error something like `Error: listen EADDRINUSE :::6400`._

## Run

```
cd examples/flappy-bird/result/bin/flappy-bird.jsexe/
python -m SimpleHTTPServer
Serving HTTP on 0.0.0.0 port 8000 ...
```

## Graphics

All graphical assets used in this game are from [Tappy Plane](http://kenney.nl/assets/tappy-plane) (License: [CC0 1.0 Universal](License: (CC0 1.0 Universal)) provided by [KENNY](http://kenney.nl)
