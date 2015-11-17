#!/usr/bin/env python
"""
"""

import argparse, tempfile, subprocess, sys

PARSER = argparse.ArgumentParser(prog='code-coverage-test.py', usage='%(prog)s [options]')
PARSER.add_argument('file', nargs=1, type=str, help='the test file')
ARGS = PARSER.parse_args()

DIR = tempfile.gettempdir()
print '\033[35musing temporary directory: ' + DIR + '\033[0m'

COMMANDS = '''echo "CoverageLineByLine(\\"''' + DIR + '''/profile.gz\\");;
LoadPackage(\\"semigroups\\", false);;
SEMIGROUPS.Test(\\"''' + ARGS.file[0] + '''\\", rec(silent := true));;
UnprofileLineByLine();;
LoadPackage(\\"profiling\\", false);;
filesdir := Concatenation(SEMIGROUPS.PackageDir, \\"/gap/\\");;
outdir := \\"''' + DIR + '''\\";;
x := ReadLineByLineProfile(\\"''' + DIR + '''/profile.gz\\");;
OutputAnnotatedCodeCoverageFiles(x, filesdir, outdir);"'''

PS = subprocess.Popen(COMMANDS, stdout=subprocess.PIPE, shell=True)

try:
    subprocess.check_call('~/gap/bin/gap.sh -A -r -m 1g -T',
                          stdin=PS.stdout, shell=True)
except subprocess.CalledProcessError:
    sys.exit('code-coverage-test: something went wrong calling GAP!')

subprocess.call(('open', DIR + '/index.html'))
