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
    return '\n        '.join(_WRAPPER.wrap('\033[32m' + string + '\033[0m'))

def _cyan_string(string):
    'cyan string'
    return '\n        '.join(_WRAPPER.wrap('\033[36m' + string + '\033[0m'))

def _blue_string(string):
    'blue string'
    return '\n        '.join(_WRAPPER.wrap('\033[34m' + string + '\033[0m'))

def _magenta_string(string):
    'magenta string'
    return '\n        '.join(_WRAPPER.wrap('\033[35m' + string + '\033[0m'))

################################################################################
# Parse the arguments
################################################################################

_LOG_NR = 0

def _run_gap(gap_root, verbose=False):
    'returns the bash script to run GAP and detect errors'
    out = gap_root + 'bin/gap.sh -r -A -T -m 1g'
    if not verbose:
        out += ' -q'
    return out

def _log_file():
    'returns the string "LogTo(a unique file);"'
    global _LOG_NR
    _LOG_NR += 1
    log_file = 'test-' + str(_LOG_NR) + '.log'
    return log_file

################################################################################
# Functions
################################################################################

def _run_test(gap_root, message, stop_for_diffs, *arg):
    '''echo the GAP commands in the string <commands> into _GAPTest, after
       printing the string <message>.'''

    print _pad(_magenta_string(message + ' . . . ')),
    sys.stdout.flush()

    log_file = _log_file()
    commands = 'echo "LogTo(\\"' + log_file + '\\");\n' + '\n'.join(arg) + '"'

    pro1 = subprocess.Popen(commands,
                            stdout=subprocess.PIPE,
                            shell=True)
    try:
        devnull = open(os.devnull, 'w')
        pro2 = subprocess.Popen(_run_gap(gap_root),
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
        for line in open(log_file, 'r').readlines():
            print _red_string(line.rstrip(), False)
        if stop_for_diffs:
            sys.exit(1)

    print _green_string('PASSED!')

################################################################################

def _get_ready_to_make(pkg_dir, package_name):
    os.chdir(pkg_dir)
    for pkg in os.listdir(pkg_dir):
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

def _make_clean(gap_root, name):
    print _cyan_string(_pad('Deleting ' + name + ' binary') + ' . . . '),
    cwd = os.getcwd()
    sys.stdout.flush()
    _get_ready_to_make(gap_root, name)
    _exec('make clean')
    os.chdir(cwd)
    print _cyan_string('DONE!')

################################################################################

def _configure_make(directory, name):
    print _cyan_string(_pad('Compiling ' + name) + ' . . . '),
    cwd = os.getcwd()
    sys.stdout.flush()
    _get_ready_to_make(directory, name)
    _exec('./configure')
    _exec('make')
    os.chdir(cwd)
    print _cyan_string('DONE!')

################################################################################

def _man_ex_str(gap_root, name):
    return ('ex := ExtractExamples(\\"'  + gap_root + 'doc/ref\\", \\"'
            + name + '\\", [\\"' + name + '\\"], \\"Section\\");' +
            ' RunExamples(ex);')

def _pad(string, extra=0):
    for i in xrange(extra + 27 - len(string)):
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
_TEST_SMALLSEMI = 'SmallsemiTestAll();\n SmallsemiTestManualExamples();'
_TEST_MAN_EX = 'SemigroupsTestManualExamples();'
_MAKE_DOC = 'SemigroupsMakeDoc();'

def _validate_package_info(gap_root, pkg_name):
    return ('ValidatePackageInfo(\\"' + gap_root +
            'pkg/' + pkg_name + '/PackageInfo.g\\");')

def _test_gap_quick(gap_root):

    string = 'Test(\\"' + gap_root + 'tst/testinstall/trans.tst\\");'
    string += 'Test(\\"' + gap_root + 'tst/testinstall/pperm.tst\\");'
    string += 'Test(\\"' + gap_root + 'tst/testinstall/semigrp.tst\\");'
    string += 'Test(\\"' + gap_root + 'tst/teststandard/reesmat.tst\\");'
    string += _man_ex_str(gap_root, 'trans.xml')
    string += _man_ex_str(gap_root, 'pperm.xml')
    string += _man_ex_str(gap_root, 'invsgp.xml')
    string += _man_ex_str(gap_root, 'reesmat.xml')
    string += _man_ex_str(gap_root, 'mgmadj.xml')
    string += 'Test(\\"' + gap_root + 'tst/teststandard/bugfix.tst\\");'
    string += 'Read(\\"' + gap_root + 'tst/testinstall.g\\");'

    return string

############################################################################
# Run the tests
############################################################################

def run_semigroups_tests(gap_root, pkg_dir, pkg_name):
    cwd = os.getcwd()
    tmpdir = tempfile.mkdtemp()
    filename = tmpdir + '/testlog'

    #print '\033[35musing temporary directory: ' + tmpdir + '\033[0m'
    print _blue_string(_pad('Running tests in ' + gap_root))
    _run_test(gap_root,
              'Validating PackageInfo.g   ', True,
              _validate_package_info(gap_root, pkg_name))
    _run_test(gap_root, 'Loading package            ', True, _LOAD)
    _run_test(gap_root, 'Loading only needed        ', True, _LOAD_ONLY_NEEDED)
    _run_test(gap_root, 'Loading Smallsemi first    ', True, _LOAD_SMALLSEMI, _LOAD)
    _run_test(gap_root, 'Loading Smallsemi second   ', True, _LOAD, _LOAD_SMALLSEMI)

    _make_clean(pkg_dir, 'grape')
    _run_test(gap_root, 'Loading Grape not compiled ', True, _LOAD)

    _configure_make(pkg_dir, 'grape')
    _run_test(gap_root, 'Loading Grape compiled     ', True, _LOAD)

    _make_clean(pkg_dir, 'orb')
    _run_test(gap_root, 'Loading Orb not compiled   ', True, _LOAD)

    _configure_make(pkg_dir, 'orb')
    _run_test(gap_root, 'Loading Orb compiled       ', True, _LOAD)

    _run_test(gap_root, 'Compiling the doc          ', True, _LOAD, _MAKE_DOC)
    _run_test(gap_root, 'Testing Smallsemi          ',
              True,
              _LOAD,
              _LOAD_SMALLSEMI,
              _TEST_SMALLSEMI)

    print _blue_string(_pad('Testing with Orb compiled') + ' . . .')
    _run_test(gap_root, 'testinstall.tst            ', True, _LOAD, _TEST_INSTALL)
    _run_test(gap_root, 'manual examples            ', True, _LOAD, _TEST_MAN_EX)
    _run_test(gap_root, 'tst/*                      ', True, _LOAD, _TEST_ALL)
    _run_test(gap_root, 'GAP quick tests            ', False, _LOAD,
              _test_gap_quick(gap_root))

    print _blue_string(_pad('Testing with Orb uncompiled') + ' . . .')
    _make_clean(pkg_dir, 'orb')
    _run_test(gap_root, 'testinstall.tst            ', True, _LOAD, _TEST_INSTALL)
    _run_test(gap_root, 'manual examples            ', True, _LOAD, _TEST_MAN_EX)
    _run_test(gap_root, 'tst/*                      ', True, _LOAD, _TEST_ALL)
    _run_test(gap_root, 'GAP quick tests            ', False, _LOAD,
              _test_gap_quick(gap_root))
    _configure_make(pkg_dir, 'orb')

    print _blue_string(_pad('Testing only needed') + ' . . .')
    _run_test(gap_root, 'testinstall.tst            ', True, _LOAD_ONLY_NEEDED,
              _TEST_INSTALL)
    _run_test(gap_root, 'manual examples            ', True, _LOAD_ONLY_NEEDED,
              _TEST_MAN_EX)
    _run_test(gap_root, 'tst/*                      ', True, _LOAD_ONLY_NEEDED,
              _TEST_ALL)
    _run_test(gap_root, 'GAP quick tests            ', False, _LOAD,
              _test_gap_quick(gap_root))

    print '\n\033[32mSUCCESS!\033[0m'

################################################################################
# Run the script
################################################################################

def main():
    parser = argparse.ArgumentParser(prog='release.py',
                                     usage='%(prog)s [options]')
    parser.add_argument('--gap-root', nargs='*', type=str,
                        help='the gap root directory (default: ~/gap)',
                        default=['~/gap/'])
    parser.add_argument('--pkg-dir', nargs='?', type=str,
                        help='the pkg directory (default: gap-root/pkg/)',
                        default='~/gap/pkg/')
    parser.add_argument('--pkg-name', nargs='?', type=str,
                        help='the pkg name (default: semigroups)',
                        default='semigroups')
    parser.add_argument('--verbose', dest='verbose', action='store_true',
                        help='verbose mode (default: False)')
    parser.set_defaults(verbose=False)

    args = parser.parse_args()

    for i in xrange(len(args.gap_root)):
        if args.gap_root[i][-1] != '/':
            args.gap_root[i] += '/'
    if not args.pkg_dir[-1] == '/':
        args.pkg_dir += '/'
    args.gap_root = [os.path.expanduser(x) for x in args.gap_root]
    args.pkg_dir = os.path.expanduser(args.pkg_dir)

    for gap_root_dir in args.gap_root:
        if not (os.path.exists(gap_root_dir) and os.path.isdir(gap_root_dir)):
            sys.exit(_red_string('release.py: error: can\'t find GAP root' +
                                 ' directory' + gap_root_dir + '!'))
    if not (os.path.exists(args.pkg_dir) or os.path.isdir(args.pkg_dir)):
        sys.exit(_red_string('release.py: error: can\'t find package' +
                             ' directory!'))

    print args.gap_root

    run_semigroups_tests(args.gap_root, args.pkg_dir, args.pkg_name)

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(_red_string('Killed!'))
