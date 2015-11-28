#!/usr/bin/env python
'''
This is a script for checking that the Semigroups package is releasable, i.e.
that it passes all the tests in all configurations.
'''

#TODO verbose mode

import textwrap, os, argparse, tempfile, subprocess, sys, os, signal

################################################################################
# Strings for printing
################################################################################

_WRAPPER = textwrap.TextWrapper(break_on_hyphens=False, width=80)

def _red_string(string, wrap=True):
    'red string'
    if wrap:
        return '\n        '.join(_WRAPPER.wrap('\033[1;31m' + string + '\033[0m'))
    else:
        return '\033[1;31m' + string + '\033[0m'

def _green_string(string):
    'green string'
    return '\n        '.join(_WRAPPER.wrap('\033[1;32m' + string + '\033[0m'))

def _yellow_string(string):
    'yellow string'
    return '\n        '.join(_WRAPPER.wrap('\033[1;33m' + string + '\033[0m'))

def _blue_string(string):
    'blue string'
    return '\n        '.join(_WRAPPER.wrap('\033[1;34m' + string + '\033[0m'))

def _magenta_string(string):
    'magenta string'
    return '\n        '.join(_WRAPPER.wrap('\033[1;35m' + string + '\033[0m'))

################################################################################
# Parse the arguments
################################################################################

_PARSER = argparse.ArgumentParser(prog='code-coverage-test.py',
                                  usage='%(prog)s [options]')
_PARSER.add_argument('--gap-root', nargs='?', type=str,
                     help='the gap root directory (default: ~/gap)',
                     default='~/gap/')
_PARSER.add_argument('--pkg-dir', nargs='?', type=str,
                     help='the pkg directory (default: gap-root/pkg/)',
                     default='~/gap/pkg/')
_PARSER.add_argument('--verbose', dest='verbose', action='store_true',
                     help='verbose mode (default: False)')
_PARSER.set_defaults(verbose=False)

_ARGS = _PARSER.parse_args()

if not _ARGS.gap_root[-1] == '/':
    _ARGS.gap_root += '/'
if not _ARGS.pkg_dir[-1] == '/':
    _ARGS.pkg_dir += '/'

_ARGS.gap_root = os.path.expanduser(_ARGS.gap_root)
_ARGS.pkg_dir = os.path.expanduser(_ARGS.pkg_dir)

if not (os.path.exists(_ARGS.gap_root) and os.path.isdir(_ARGS.gap_root)):
    sys.exit(_red_string('release.py: error: can\'t find GAP root' +
                         ' directory!'))
if not (os.path.exists(_ARGS.pkg_dir) or os.path.isdir(_ARGS.pkg_dir)):
    sys.exit(_red_string('release.py: error: can\'t find package directory!'))

################################################################################
# Get the cwd and a temporary dir
################################################################################

_CWD = os.getcwd()
_DIR_TMP = tempfile.gettempdir()
print '\033[35musing temporary directory: ' + _DIR_TMP + '\033[0m'
_FILE = _DIR_TMP + '/testlog'

################################################################################
################################################################################


def _run_gap():
    'returns the bash script to run GAP and detect errors'
    out = _ARGS.gap_root + 'bin/gap.sh -r -A -T -m 1g'
    if not _ARGS.verbose:
        out += ' -q'
    return out

_LOG_NR = 0

def _log_file():
    'returns the string "LogTo(a unique file);"'
    global _LOG_NR
    _LOG_NR += 1
    log_file = _FILE + str(_LOG_NR) + '.log'
    return log_file

################################################################################
# Functions
################################################################################

def _run_test(message, stop_for_diffs, *arg):
    '''echo the GAP commands in the string <commands> into _GAPTest, after
       printing the string <message>.'''

    print _magenta_string(message + ' . . . '),
    sys.stdout.flush()

    log_file = _log_file()
    commands = 'echo "LogTo(\\"' + log_file + '\\");\n' + '\n'.join(arg) + '"'

    pro1 = subprocess.Popen(commands,
                            stdout=subprocess.PIPE,
                            shell=True)
    try:
        devnull = open(os.devnull, 'w')
        pro2 = subprocess.Popen(_run_gap(),
                                stdin=pro1.stdout,
                                stdout=devnull,
                                stderr=devnull,
                                shell=True)
        pro2.wait()
    except KeyboardInterrupt:
        pro1.terminate()
        pro1.wait()
        pro2.terminate()
        pro2.wait()
        sys.exit(_red_string('Killed!'))
    except subprocess.CalledProcessError:
        print _red_string('FAILED!')
        if stop_for_diffs:
            sys.exit(0)

    try:
        log = open(log_file, 'r').read()
    except IOError:
        print _red_string('release.py: error: ' + log_file + ' not found!')
        sys.exit(0)

    if len(log) == 0:
        print _red_string('release.py: warning: ' + log_file + ' is empty!')

    if (log.find('########> Diff') != -1
            or log.find('# WARNING') != -1
            or log.find('#E ') != -1
            or log.find('Error') != -1
            or log.find('brk>') != -1
            or log.find('LoadPackage("semigroups", false);\nfail') != -1):
        print _magenta_string('FAILED!')
        for x in open(log_file, 'r').readlines():
            print _red_string(x.rstrip(), False)
        if stop_for_diffs:
            sys.exit(0)

    print _magenta_string('PASSED!')

################################################################################

def _get_ready_to_make(package_name):
    os.chdir(_ARGS.pkg_dir)
    for pkg in os.listdir(_ARGS.pkg_dir):
        if os.path.isdir(pkg) and pkg.startswith(package_name):
            package_dir = pkg

    if not package_dir:
        sys.exit(_red_string('release.py: error: can\'t find the ' + package_name
                             + ' directory'))

    os.chdir(package_dir)

################################################################################

def _exec(command):
    try: #FIXME use popen here
        pro = subprocess.call(command + ' &> /dev/null',
                              shell=True)
    except KeyboardInterrupt:
        os.kill(pro.pid, signal.SIGKILL)
        sys.exit(_red_string('Killed!'))
    except subprocess.CalledProcessError:
        sys.exit(_red_string('release.py: error: ' + command + ' failed!!'))

################################################################################

def _make_clean(name):
    print _yellow_string(_pad('make clean ' + name) + ' . . . '),
    sys.stdout.flush()
    _get_ready_to_make(name)
    _exec('make clean')
    os.chdir(_CWD)
    print _yellow_string('DONE!')

################################################################################

def _configure_make(name):
    print _yellow_string(_pad('Compiling ' + name) + ' . . . '),
    sys.stdout.flush()
    _get_ready_to_make(name)
    _exec('./configure')
    _exec('make')
    os.chdir(_CWD)
    print _yellow_string('DONE!')

################################################################################

def _man_ex_str(name):
    return ('ex := ExtractExamples(\\"'  + _ARGS.gap_root + 'doc/ref\\", \\"'
            + name + '\\", [\\"' + name + '\\"], \\"Section\\");' +
            ' RunExamples(ex);')

def _pad(string):
    for i in xrange(27 - len(string)):
        string += ' '
    return string

################################################################################
# the GAP commands to run the tests
################################################################################

_LOAD = 'LoadPackage(\\"semigroups\\", false);'
_LOAD_SMALLSEMI = 'LoadPackage(\\"smallsemi\\", false);'
_LOAD_ONLY_NEEDED = 'LoadPackage(\\"semigroups\\", false : OnlyNeeded);'
_TEST_STANDARD = 'SemigroupsTestStandard();'
_TEST_INSTALL = 'SemigroupsTestInstall();'
_TEST_ALL = 'SemigroupsTestAll();'
_TEST_MAN_EX = 'SemigroupsTestManualExamples();'
_MAKE_DOC = 'SemigroupsMakeDoc();'
_VALIDATE_PACKAGE_INFO = ('ValidatePackageInfo(\\"' + _ARGS.gap_root +
                          'pkg/semigroups/PackageInfo.g\\");')

_TEST_GAP_QUICK = 'Test(\\"' + _ARGS.gap_root + 'tst/testinstall/trans.tst\\");'
_TEST_GAP_QUICK += 'Test(\\"' + _ARGS.gap_root + 'tst/testinstall/pperm.tst\\");'
_TEST_GAP_QUICK += 'Test(\\"' + _ARGS.gap_root + 'tst/testinstall/semigrp.tst\\");'
_TEST_GAP_QUICK += 'Test(\\"' + _ARGS.gap_root + 'tst/teststandard/reesmat.tst\\");'
_TEST_GAP_QUICK += _man_ex_str('trans.xml')
_TEST_GAP_QUICK += _man_ex_str('pperm.xml')
_TEST_GAP_QUICK += _man_ex_str('invsgp.xml')
_TEST_GAP_QUICK += _man_ex_str('reesmat.xml')
_TEST_GAP_QUICK += _man_ex_str('mgmadj.xml')
_TEST_GAP_QUICK += 'Test(\\"' + _ARGS.gap_root + 'tst/teststandard/bugfix.tst\\");'
_TEST_GAP_QUICK += 'Read(\\"' + _ARGS.gap_root + 'tst/testinstall.g\\");'

################################################################################
# Run the tests
################################################################################

def main():
    _run_test('Validating PackageInfo.g   ', True, _VALIDATE_PACKAGE_INFO)
    _run_test('Loading package            ', True, _LOAD)
    _run_test('Loading only needed        ', True, _LOAD_ONLY_NEEDED)
    _run_test('Loading Smallsemi first    ', True, _LOAD_SMALLSEMI, _LOAD)

    _make_clean('grape')
    _run_test('Loading Grape not compiled ', True, _LOAD)

    _configure_make('grape')
    _run_test('Loading Grape compiled     ', True, _LOAD)

    _make_clean('orb')
    _run_test('Loading Orb not compiled   ', True, _LOAD)

    _configure_make('orb')
    _run_test('Loading Orb compiled       ', True, _LOAD)

    _run_test('Compiling the doc          ', True, _LOAD, _MAKE_DOC)

    print _blue_string(_pad('Testing with Orb compiled') + ' . . .')
    _run_test('testinstall.tst            ', True, _LOAD, _TEST_INSTALL)
    _run_test('manual examples            ', True, _LOAD, _TEST_MAN_EX)
    _run_test('tst/*                      ', True, _LOAD, _TEST_ALL)
    _run_test('GAP quick tests            ', False, _LOAD, _TEST_GAP_QUICK)

    print _blue_string(_pad('Testing with Orb uncompiled') + ' . . .')
    _make_clean('orb')
    _run_test('testinstall.tst            ', True, _LOAD, _TEST_INSTALL)
    _run_test('manual examples            ', True, _LOAD, _TEST_MAN_EX)
    _run_test('tst/*                      ', True, _LOAD, _TEST_ALL)
    _run_test('GAP quick tests            ', False, _LOAD, _TEST_GAP_QUICK)
    _configure_make('orb')

    print _blue_string(_pad('Testing only needed') + ' . . .')
    _run_test('testinstall.tst            ', True, _LOAD_ONLY_NEEDED, _TEST_INSTALL)
    _run_test('manual examples            ', True, _LOAD_ONLY_NEEDED, _TEST_MAN_EX)
    _run_test('tst/*                      ', True, _LOAD_ONLY_NEEDED, _TEST_ALL)
    _run_test('GAP quick tests            ', False, _LOAD, _TEST_GAP_QUICK)

    #print _blue_string('make teststandard . . .')
    #os.chdir(_ARGS.gap_root)
    #_exec('make teststandard')
    #os.chdir(_CWD)

    print '\n\033[32mSUCCESS!\033[0m'

if __name__ == '__main__':
    main() #TODO add try, except Killed here
