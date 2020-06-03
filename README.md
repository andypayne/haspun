# Haspun

Haspun is a less than bare bones web server written in Haskell.

I wrote this around 2009 and recently cleaned it up slightly to be compatible with the latest version of GHC.

## Building

Grab the [Haskell Platform](http://www.haskell.org/platform/) or your choice of [GHC](http://www.haskell.org/ghc/) setup. Or install Haskell with [ghcup](https://www.haskell.org/ghcup/):

```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
And install the [Haskell Tool Stack](https://haskellstack.org):
```shell
curl -sSL https://get.haskellstack.org/ | sh
```

Then install dependencies:

```bash
cabal install --lib network
cabal install --lib zlib
```

```bash
$ ghc haspun.hs
```

## Usage

```
haspun
  -V       --version      show version number
  -h       --help         show usage
  -c FILE  --config=FILE  set configuration file (not implemented)
  -d DIR   --docroot=DIR  set doc root
  -p PORT  --port=PORT    set port
```

Specify a port and a document root and you're off.

```bash
$ haspun --docroot=test/docroot/ --port=8081
```

Point your web server at http://localhost:8081/index.html

