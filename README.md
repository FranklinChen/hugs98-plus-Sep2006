# [Hugs](https://www.haskell.org/hugs/), a Haskell98 implementation

[![Build Status](https://travis-ci.org/FranklinChen/hugs98-plus-Sep2006.png)](https://travis-ci.org/FranklinChen/hugs98-plus-Sep2006)

```text
------------------------------------------------------------------------------
__   __ __  __  ____   ___      _________________________________________
||   || ||  || ||  || ||__      Hugs 98: Based on the Haskell 98 standard
||___|| ||__|| ||__||  __||     Copyright (c) 1994-2006
||---||         ___||           World Wide Web: http://haskell.org/hugs
||   ||                         Report bugs to: hugs-bugs@haskell.org
||   || Version:    May 2006    _________________________________________

------------------------------------------------------------------------------
```

Hugs was a popular implementation of the Haskell programming language
in the 1990s. Maintenance of it stopped in 2009, three years after the
final release in 2006.

I have updated the source code of the original 2006 distribution to
make it build again today, on Mac OS X in particular.

I am maintaining this repo of Hugs for historical purposes.

I have not tested other platforms but would welcome verification or
additional portability fixes). I did not start with the
[development version as of 2009](https://github.com/FranklinChen/Hugs)
because of uncertainty about any changes introduced since 2006.

## Easy installation

I have provided a [Homebrew](http://brew.sh/) formula so that if you
are on Mac OS X, you can install Hugs as follows:

```console
$ brew install FranklinChen/tap/hugs --HEAD
```

(If you are curious, you can look at my Homebrew tap repo [here](https://github.com/FranklinChen/homebrew-tap).)

## Alternative: manual installation

Or, to install by yourself from this repo:

```console
$ brew upgrade
$ brew install readline

# If you want to use X11 and ALUT:
$ brew install libx11 freealut
$ export LDFLAGS="-L/opt/X11/lib -L/usr/local/opt/freealut/lib"
$ export CPPFLAGS="-I/opt/X11/include -I/usr/local/opt/freealut/include"

$ export CFLAGS="-Wno-error=implicit-function-declaration -Wno-error=implicit-int"

$ ./configure
$ make
$ make install
```

## For real life work in Haskell

For real life work in Haskell, please use
[GHC](https://www.haskell.org/ghc/), which comes with

- a Hugs-inspired interpreter `ghci` that works just like Hugs, except better.
- an industrial-strength optimizing native compiler `ghc`, which generates code
running hundreds of times faster than interpreted `ghci` and
comparable to C speed.
- access to a huge ecosystem of libraries called
[Hackage](https://hackage.haskell.org/).

On Mac OS X, it's very easy to get going with
using GHC. I recommend using Homebrew to install everything you need
to get started, whether you choose to use GHC and Cabal, or [Stack](https://docs.haskellstack.org/en/stable/README/), which is what I prefer to use.

### Possible installations of your choice

```console
$ brew install ghc
$ brew install cabal-install
$ brew install  haskell-stack
```
