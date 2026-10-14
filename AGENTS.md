# Repository Guidelines

## Project Structure & Module Organization

This repository preserves the September 2006 Hugs 98 distribution with portability fixes, especially for macOS. `src/` contains the C interpreter, runtime, platform ports, and Yacc grammar (`parser.y`). `libraries/hugsbase/` holds Hugs-specific Haskell modules; `packages/` and `fptools/hslibs/` contain bundled libraries. `cpphs/` and `hsc2hs/` provide preprocessing tools. Examples live in `demos/`; documentation lives in `docs/`; `icons/` contains graphical assets. `debian/` and root packaging files support distribution builds.

## Build, Test, and Development Commands

Run commands from the repository root unless indicated otherwise.

- `brew install readline`: install the documented macOS dependency. Optional X11/ALUT dependencies and include/link flags are detailed in `README.md`.
- `CFLAGS="-Wno-error=implicit-function-declaration -Wno-error=implicit-int" ./configure`: configure the legacy C sources for modern macOS compilers. Use `--prefix=PATH` to select an installation location.
- `make`: build interpreter executables, bootstrap libraries, and documentation.
- `HUGSDIR=hugsdir src/hugs`: launch the interpreter from the build tree; evaluate `1+2`, load a changed module, and exit with `:quit`.
- `make install`: install into the configured prefix.
- `make clean` / `make distclean`: remove build products / configuration products; inspect local changes before cleanup.

## Coding Style & Naming Conventions

Match surrounding code rather than reformatting legacy files. C commonly uses four-column indentation with existing tabs, K&R declarations, and compatibility macros such as `Args`. Preserve portability conventions. Haskell modules use capitalized names matching their paths; functions generally use lower camel case. Preserve layout in `.hs` and literate `.lhs` files. Make recipes require tabs. No repository-wide formatter or linter is configured.

Edit `configure.ac`, `*.in` templates, and `src/parser.y` as appropriate instead of only changing generated outputs. The `configure` scripts are committed; regenerate them with `make regenerate` (autoconf 2.73), never by hand, and commit the regenerated outputs with the change. The build never regenerates them on its own: a later reconfigure goes through `config.status --recheck`, which keeps the original `--prefix` and other arguments.

## Testing Guidelines

After building, run `make -C libraries LibStatus` and inspect `libraries/LibStatus` for errors. For preprocessing changes, run `(cd cpphs/tests && sh runtests ../../libraries/tools/cpphs)`; fixtures use `expectN` expected-output files.

`make check` and `make verbosecheck` require the absent development `tests/` directory, so neither CI nor packagers run them. `bash tools/ci/build.sh` reproduces a CI job locally: it configures, builds, runs `tools/ci/check.py` (module loads and `tools/ci/smoke.hs`) and the cpphs fixtures, installs into `ci-install/`, and checks the installed copy. Set `CI_GRAPHICS=yes` to also require the X11, HGL, OpenGL, GLUT, OpenAL and ALUT packages. No coverage threshold is configured. Add focused reproductions for behavior changes and report unavailable checks explicitly.

## Commit & Pull Request Guidelines

History uses short, imperative subjects such as “Ignore overflow in conversion from Integer to Int.” Keep commits focused. PRs should explain the defect, affected platforms, linked issues where applicable, and exact build/test commands and results. Include compiler, architecture, and configuration flags for portability fixes; update documentation when setup changes.
