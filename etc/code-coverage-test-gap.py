#!/usr/bin/env python3
"""
"""

import argparse, tempfile, subprocess, re, sys, os, webbrowser
from os.path import exists, isdir, isfile

_ERR_PREFIX = "\033[31mcode-coverage-test-gap.py: error: "
_INFO_PREFIX = "\033[40;38;5;82m"

_PARSER = argparse.ArgumentParser(
    prog="code-coverage-test-gap.py", usage="%(prog)s [options]"
)
_PARSER.add_argument(
    "tstfiles",
    nargs="+",
    type=str,
    help="the test files you want to check code coverage for"
    + "(must be at least one)",
)
_PARSER.add_argument(
    "--gap-root",
    nargs="?",
    type=str,
    help="the gap root directory (default: ~/gap)",
    default="~/gap/",
)
_PARSER.add_argument(
    "--open",
    nargs="?",
    type=str,
    help=("open the html page for this file " + "(default: None)"),
    default=None,
)

_ARGS = _PARSER.parse_args()
if not _ARGS.gap_root[-1] == "/":
    _ARGS.gap_root += "/"

if exists("gap") and isdir("gap"):
    _PROFILE_DIR = "/gap/"
elif exists("lib") and isdir("lib"):
    _PROFILE_DIR = "/lib/"
else:
    sys.exit(_ERR_PREFIX + "no directory gap or lib to profile!\033[0m")

_ARGS.gap_root = os.path.expanduser(_ARGS.gap_root)
if not (exists(_ARGS.gap_root) and isdir(_ARGS.gap_root)):
    sys.exit(_ERR_PREFIX + "can't find GAP root directory!\033[0m")

for f in _ARGS.tstfiles:
    if not (exists(f) and isfile(f)):
        sys.exit(_ERR_PREFIX + f + " does not exist!\033[0m")

_DIR = tempfile.mkdtemp()
print(f"{_INFO_PREFIX}Using temporary directory: {_DIR}\033[0m")

_COMMANDS = 'echo "'
for f in _ARGS.tstfiles:
    _COMMANDS += f'Test(\\"{f}\\");;\n'
_COMMANDS += (
    '''UncoverageLineByLine();;
LoadPackage(\\"profiling\\", false);;
filesdir := \\"'''
    + os.getcwd()
    + _PROFILE_DIR
    + """\\";;\n"""
)
_COMMANDS += 'outdir := \\"' + _DIR + '\\";;\n'
_COMMANDS += 'x := ReadLineByLineProfile(\\"' + _DIR + '/profile.gz\\");;\n'
_COMMANDS += 'OutputAnnotatedCodeCoverageFiles(x, filesdir, outdir);"'

pro1 = subprocess.Popen(_COMMANDS, stdout=subprocess.PIPE, shell=True)
_RUN_GAP = _ARGS.gap_root + "gap -A -m 1g -T --cover " + _DIR + "/profile.gz"

try:
    pro2 = subprocess.Popen(_RUN_GAP, stdin=pro1.stdout, shell=True)
    pro2.wait()
except KeyboardInterrupt:
    pro1.terminate()
    pro1.wait()
    pro2.terminate()
    pro2.wait()
    sys.exit("\033[31mKilled!\033[0m")
except (subprocess.CalledProcessError, IOError, OSError):
    sys.exit(_ERR_PREFIX + "Something went wrong calling GAP!\033[0m")

suffix = ""
if _ARGS.open:
    filename = (
        _DIR
        + "/"
        + os.getcwd().replace("/", "_")
        + "_"
        + _ARGS.open.replace("/", "_")
        + ".html"
    )
    p = re.compile(r"<tr class='missed'><td><a name=\"line(\d+)\">")
    with open(filename, "r") as f:
        m = p.search(f.read())
        if m:
            suffix += f"#line{m.group(1)}"
else:
    filename = _DIR + "/index.html"

if exists(filename) and isfile(filename):
    if _ARGS.open:
        filename += suffix
    try:
        webbrowser.get("safari").open(f"file://{filename}", new=2)
    except Exception:
        webbrowser.open(f"file://{filename}", new=2)
else:
    sys.exit(f"\n{_ERR_PREFIX}Failed to open file://{filename}\033[0m")

print(f"\n{_INFO_PREFIX}SUCCESS!\033[0m")
sys.exit(0)
