# [Hugs](https://www.haskell.org/hugs/), a Haskell98 implementation

[![CI](https://github.com/FranklinChen/hugs98-plus-Sep2006/actions/workflows/ci.yml/badge.svg)](https://github.com/FranklinChen/hugs98-plus-Sep2006/actions/workflows/ci.yml)

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
$ export LDFLAGS="-L/opt/X11/lib -L$(brew --prefix)/opt/freealut/lib"
$ export CPPFLAGS="-I/opt/X11/include -I$(brew --prefix)/opt/freealut/include"

$ export CFLAGS="-Wno-error=implicit-function-declaration -Wno-error=implicit-int"

$ ./configure
$ make
$ make install
```

## Continuous integration and Windows development

GitHub Actions is configured for Linux x86_64 (GCC and Clang), Linux ARM64,
Debian 13, macOS Intel and Apple Silicon, and native Windows x86. Jobs exercise
both shipped and regenerated configure scripts, interpreter and library smoke
tests, cpphs fixtures, and installation into a temporary prefix. A Linux job
also builds and imports X11, OpenGL/GLUT, and OpenAL/ALUT. Consult the linked
workflow runs for results. Graphics checks do not exercise rendering or Haskell
callbacks: the legacy FFI callback generator still lacks x86_64 and ARM64 support.

After installing build dependencies, run `bash tools/ci/build.sh` to reproduce
the checks. Set `REGENERATE_CONFIGURE=yes` to regenerate the root and base
configure scripts. Set `CI_GRAPHICS=yes` to require the graphics/audio imports.
Logs are saved under `ci-logs/`; the installation stays under `ci-install/`.
The historical `make check` target still requires an external development test
suite; the new smoke checks do not replace that full language regression suite.

For Windows, use an MSYS2 **MINGW32** shell with `make`, `autoconf`, `bison`,
`mingw-w64-i686-gcc`, and `mingw-w64-i686-python`. Run the same CI script; it
also builds `src/winhugs/build/winhugs.exe` with `Makefile.mingw`. From PowerShell,
run `./tools/ci/winhugs-smoke.ps1` to test loading Prelude and evaluating a GUI
command. This restores the existing 32-bit Windows architecture; native 64-bit
WinHugs needs a separate pointer/FFI audit. Historical DOS, classic Mac,
Symbian, and .NET ports are outside this CI matrix.

The Windows job uses the legacy MINGW32 environment to preserve this ABI.
MSYS2 is phasing out 32-bit environments, so this is a restoration path, not
a long-term substitute for a 64-bit port.

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
