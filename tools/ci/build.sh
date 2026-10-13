#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
mkdir -p ci-logs
export CFLAGS="${CFLAGS:--std=gnu17 -Wno-error=implicit-function-declaration -Wno-error=implicit-int}"
case "$(uname -s)" in
  Darwin)
    deps_prefix=$(brew --prefix)
    export CPPFLAGS="${CPPFLAGS:-} -I$deps_prefix/include -I$deps_prefix/opt/readline/include"
    export LDFLAGS="${LDFLAGS:-} -L$deps_prefix/lib -L$deps_prefix/opt/readline/lib"
    ;;
esac
if [[ ${REGENERATE_CONFIGURE:-no} == yes ]]; then
  autoconf -f 2>&1 | tee ci-logs/autoconf.log
  (cd packages/base && autoconf -f) 2>&1 | tee ci-logs/base-autoconf.log
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
# Keep the selected shipped/regenerated configure script. Checkout timestamps
# can otherwise trigger autoreconf and a reconfigure that loses our prefix.
make -o configure -j2 2>&1 | tee ci-logs/build.log
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
make -o configure install 2>&1 | tee ci-logs/install.log
python3 tools/ci/check.py --installed "$PWD/ci-install" ${CI_GRAPHICS:+--graphics} 2>&1 | tee ci-logs/installed-smoke.log
