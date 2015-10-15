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
make it build again today, on Mac OS X in particular (I have not
tested other platforms but would welcome verification or additional
portability fixes). I did not start with the [development version as of
2009](https://github.com/FranklinChen/Hugs) because of uncertainty
about any changes introduced since 2006.

I have provided a [Homebrew](http://brew.sh/) formula so that if you
are on Mac OS X, you can install Hugs as follows:

```console
$ brew install FranklinChen/tap/hugs --HEAD
```

(If you are curious, you can look at my Homebrew tap repo [here](https://github.com/FranklinChen/homebrew-tap).)

## Included packages

The 2006 source distribution of Hugs included a variety of packages,
and I have attempted to make most of them build. Not everything has
successfully built. `X11`, `GLUT`, `OpenAL`, `ALUT` did not build
successfully.

## For real life work in Haskell

I am maintaining this repo of Hugs for historic purposes. For real
life work in Haskell, please use [GHC](https://www.haskell.org/ghc/),
which comes with

- a Hugs-inspired interpreter `ghci` that works just like Hugs, except better.
- an industrial-strength optimizing native compiler `ghc`, which generates code
running hundreds of times faster than interpreted `ghci` and
comparable to C speed.
- access to a huge ecosystem of libraries called
[Hackage](http://hackage.haskell.org/).

I use GHC every day. On Mac OS X, it's very easy to get going with
using GHC. I recommend using Homebrew to install everything you need
to get started, including GHC, Cabal, and the great [Stack] tool
(https://github.com/commercialhaskell/stack).

### Installation of GHC

```console
$ brew install ghc cabal-install haskell-stack
```
