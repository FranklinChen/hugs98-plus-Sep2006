#!/usr/bin/env python3
"""Check real Hugs processes, including errors that do not affect exit status."""
import argparse
import os
from pathlib import Path
import re
import subprocess
import sys


def run(command, env, expected=None):
    result = subprocess.run(command, env=env, text=True, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, timeout=90)
    print(result.stdout, end="", flush=True)
    if result.returncode or "ERROR" in result.stdout or (expected and expected not in result.stdout):
        raise SystemExit(f"Failed: {command!r} (exit {result.returncode})")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--installed", type=Path)
    parser.add_argument("--graphics", action="store_true")
    parser.add_argument("--build-log", type=Path)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    binary = args.installed / "bin" if args.installed else root / "src"
    hugsdir = args.installed / "lib/hugs" if args.installed else root / "hugsdir"
    env = os.environ.copy()
    env.pop("HUGSFLAGS", None)
    env["HUGSDIR"] = str(hugsdir)
    suffix = ".exe" if sys.platform == "win32" else ""
    required = ["hugsbase", "base", "haskell98", "Cabal", "haskell-src", "QuickCheck",
                "mtl", "fgl", "HaXml", "parsec", "network", "HUnit", "time", "stm", "xhtml"]
    required.append("Win32" if sys.platform == "win32" else "unix")
    if args.graphics:
        required += ["X11", "HGL", "OpenGL", "GLUT", "OpenAL", "ALUT"]
    if args.build_log:
        skipped = set(re.findall(r"^Skipping (\S+) package$", args.build_log.read_text(), re.MULTILINE))
        failures = skipped.intersection(required)
        if failures:
            raise SystemExit(f"Required packages failed to build: {', '.join(sorted(failures))}")
    for package in required:
        directory = hugsdir / "packages" / package
        if not directory.is_dir() or not any(directory.rglob("*.hs")):
            raise SystemExit(f"Required package was not built: {package}")
    # Override the complete search path so a user's/system installation cannot
    # satisfy imports that are broken or missing in the tree under test.
    package_path = "-P{Hugs}/packages/*"
    run([str(binary / ("runhugs" + suffix)), "-98", package_path, str(root / "tools/ci/smoke.hs")],
        env, "HUGS_CI_OK")
    modules = ["Data.List", "Data.Map", "Foreign.C.Types", "System.Directory",
               "Network.Socket", "Data.Time.Clock.POSIX", "Test.HUnit", "Test.QuickCheck",
               "Text.ParserCombinators.Parsec", "Control.Monad.State", "Data.Graph.Inductive",
               "Distribution.Simple", "Language.Haskell.Parser", "Text.XML.HaXml",
               "Control.Concurrent.STM", "Text.XHtml"]
    if sys.platform != "win32":
        modules.append("System.Posix.Files")
    else:
        modules += ["System.Win32.Types", "System.Win32.Time", "System.Win32.SimpleMAPI"]
    if args.graphics:
        modules += ["Graphics.X11.Xlib", "Graphics.HGL", "Graphics.Rendering.OpenGL",
                    "Graphics.UI.GLUT", "Sound.OpenAL", "Sound.ALUT"]
    for module in modules:
        # A load failure can leave interactive hugs with a successful exit status.
        result = subprocess.run([str(binary / ("hugs" + suffix)), "-98", package_path, module],
                                input=":quit\n", env=env, text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=90)
        if result.returncode or "ERROR" in result.stdout:
            print(result.stdout)
            raise SystemExit(f"Module failed to load: {module}")
        print(f"Loaded {module}", flush=True)


if __name__ == "__main__":
    main()
