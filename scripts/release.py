#!/usr/bin/env python
"""
This is a script for checking that the Semigroups package is releasable, i.e.
that it passes all the tests in all configurations.
"""

import textwrap, os, argparse, tempfile, subprocess, sys, os, webbrowser

_WRAPPER = textwrap.TextWrapper(break_on_hyphens=False, width=72)

def _red_string(string):
    """ args:         string       = a string

        returns:      a text wrapped red string
    """
    return '\n        '.join(_WRAPPER.wrap('\033[1;31m' + string + '\033[0m'))

def _green_string(string):
    """ args:         string       = a string

        returns:      a text wrapped green string
    """
    return '\n        '.join(_WRAPPER.wrap('\033[1;32m' + string + '\033[0m'))

def _blue_string(string):
    """ args:         string       = a string

        returns:      a text wrapped blue string
    """
    return '\n        '.join(_WRAPPER.wrap('\033[1;34m' + string + '\033[0m'))

def _magenta_string(string):
    """ args:         string       = a string

        returns:      a text wrapped magenta string
    """
    return '\n        '.join(_WRAPPER.wrap('\033[1;35m' + string + '\033[0m'))


_PARSER = argparse.ArgumentParser(prog='code-coverage-test.py',
                                  usage='%(prog)s [options]')
_PARSER.add_argument('--gap-root', nargs='?', type=str,
                     help='the gap root directory (default: ~/gap)',
                     default='~/gap/')

_ARGS = _PARSER.parse_args()

if not _ARGS.gap_root[-1] == '/':
    _ARGS.gap_root += '/'

_ARGS.gap_root = os.path.expanduser(_ARGS.gap_root)

if not (os.path.exists(_ARGS.gap_root) and os.path.isdir(_ARGS.gap_root)):
    sys.exit(_red_string('code-coverage-test.py: error: can\'t find GAP root' +
                         ' directory!'))


