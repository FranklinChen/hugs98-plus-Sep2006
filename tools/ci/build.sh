#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
mkdir -p ci-logs
# Hugs is pre-ANSI C: keep modern compilers from making these errors. The
# flags must come from the environment, because each library package is
# configured and compiled separately and only inherits them from there.
export CFLAGS="${CFLAGS:--Wno-error=implicit-function-declaration -Wno-error=implicit-int}"
case "$(uname -s)" in
  Darwin)
    deps_prefix=$(brew --prefix)
    export CPPFLAGS="${CPPFLAGS:-} -I$deps_prefix/include -I$deps_prefix/opt/readline/include"
    export LDFLAGS="${LDFLAGS:-} -L$deps_prefix/lib -L$deps_prefix/opt/readline/lib"
    ;;
esac
if [[ ${REGENERATE_CONFIGURE:-no} == yes ]]; then
  make regenerate 2>&1 | tee ci-logs/autoconf.log
fi
configure_args=(--without-gui "--prefix=$PWD/ci-install")
# The bundled 2005 config.guess predates Linux AArch64. config.sub accepts it.
if [[ $(uname -s) == Linux && $(uname -m) == aarch64 ]]; then
  configure_args+=(--build=aarch64-unknown-linux-gnu)
fi
./configure "${configure_args[@]}" 2>&1 | tee ci-logs/configure.log
make -C src -j2 2>&1 | tee ci-logs/interpreter-build.log
case "$(uname -s)" in
  MINGW*) make -C src/winhugs -f Makefile.mingw 2>&1 | tee ci-logs/winhugs-build.log ;;
esac
make -j2 2>&1 | tee ci-logs/build.log
python3 tools/ci/check.py --build-log ci-logs/build.log ${CI_GRAPHICS:+--graphics} 2>&1 | tee ci-logs/smoke.log
# Invoke Hugs directly: native Windows generates only a .bat launcher.
case "$(uname -s)" in
  MINGW*) export CPPHS_DIFF='diff --strip-trailing-cr' ;;
esac
(cd cpphs/tests && HUGSFLAGS= HUGSDIR=../../hugsdir sh runtests \
  '../../src/runhugs -98 -P../../libraries/bootlib ../../hugsdir/programs/cpphs/Main.hs') \
  2>&1 | tee ci-logs/cpphs.log
if grep -q 'FAILED:' ci-logs/cpphs.log; then
  echo 'cpphs reported a failed fixture.' >&2
  exit 1
fi
make install 2>&1 | tee ci-logs/install.log
python3 tools/ci/check.py --installed "$PWD/ci-install" ${CI_GRAPHICS:+--graphics} 2>&1 | tee ci-logs/installed-smoke.log
