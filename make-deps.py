#!/usr/bin/env python
"""
This module compiles the dependencies of the Semigroups package.
"""

import argparse, os, sys, subprocess

PACKAGES = ["io", "orb", "grape", "digraphs"]

PARSER = argparse.ArgumentParser(prog='make-deps.py', usage='%(prog)s [options]')
PARSER.add_argument('--gap-root', nargs='?', type=str,
                    help='the gap root directory (default: ~/gap)',
                    default='~/gap')
PARSER.add_argument('--pkg-dir', nargs='?', type=str,
                    help='the pkg directory (default: gap-root/pkg/)',
                    default='~/gap/pkg/')
ARGS = PARSER.parse_args()

if not ARGS.gap_root[-1] == '/':
    ARGS.gap_root += '/'

if not ARGS.pkg_dir[-1] == '/':
    ARGS.pkg_dir += '/'

ARGS.gap_root = os.path.expanduser(ARGS.gap_root)
ARGS.pkg_dir = os.path.expanduser(ARGS.pkg_dir)

if not (os.path.exists(ARGS.gap_root) or os.path.isdir(ARGS.gap_root)):
    sys.exit('ERROR: can\'t GAP root directory!')
if not (os.path.exists(ARGS.pkg_dir) or os.path.isdir(ARGS.pkg_dir)):
    sys.exit('ERROR: can\'t pkg directory!')

DIR = os.getcwd()

for name in PACKAGES:
    os.chdir(ARGS.pkg_dir)
    dirs = []
    for pkg in os.listdir(ARGS.pkg_dir):
        if os.path.isdir(pkg) and pkg.startswith(name):
            dirs.append(pkg)
    dirs.sort()
    if len(dirs) == 0:
        print 'ERROR: can\'t find ' + name
        continue

    print 'making ' + os.getcwd() + ' . . . '
    os.chdir(dirs[-1])
    print 'making ' + os.getcwd() + ' . . . '
    bash = ['./configure', 'make clean', './configure', 'make']
    for x in bash:
        subprocess.call(x, shell=True)
