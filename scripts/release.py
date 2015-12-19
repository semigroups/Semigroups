#!/usr/bin/env python
'''
Create the archive of the Semigroups package for release, and copy the relevant
things to the webpage.
'''

import textwrap, os, argparse, tempfile, subprocess, sys, os, re, shutil, gzip
import test, time, webbrowser

_WEBPAGE_DIR = os.path.expanduser('~/Sites/public_html/semigroups')
_MAMP_DIR = '/Applications/MAMP/'
_SEMIGROUPS_REPO_DIR = os.path.expanduser('~/gap/pkg/semigroups')

################################################################################
# Strings for printing
################################################################################

_WRAPPER = textwrap.TextWrapper(break_on_hyphens=False, width=80)

def _red_string(string, wrap=True):
    'red string'
    if wrap:
        return '\n        '.join(_WRAPPER.wrap('\033[31m' + string + '\033[0m'))
    else:
        return '\033[31m' + string + '\033[0m'

def _green_string(string):
    'green string'
    return '\n        '.join(_WRAPPER.wrap('\033[1;32m' + string + '\033[0m'))

def _yellow_string(string):
    'yellow string'
    return '\n        '.join(_WRAPPER.wrap('\033[1;33m' + string + '\033[0m'))

def _cyan_string(string):
    'blue string'
    return '\n        '.join(_WRAPPER.wrap('\033[36m' + string + '\033[0m'))

def _other_magenta_string(string):
    'magenta string'
    return '\n        '.join(_WRAPPER.wrap('\033[1;35m' + string + '\033[0m'))

def _magenta_string(string):
    'magenta string'
    return '\n        '.join(_WRAPPER.wrap('\033[35m' + string + '\033[0m'))

################################################################################
# Check the version number in the branch against that in the PackageInfo.g
################################################################################

def _version_number_package_info():
    '''returns the version number from the PackageInfo.g file, exits if this
    file doesn't exist or the version number can't be found.'''

    if not (os.path.exists('PackageInfo.g') and
            os.path.isfile('PackageInfo.g')):
        sys.exit(_red_string('release.py: error: cannot find PackageInfo.g file'))
    else:
        try:
            contents = open('PackageInfo.g', 'r').read()
        except IOError:
            sys.exit(_red_string('release.py: error: cannot read PackageInfo.g'))
        match = re.compile(r'Version\s*:=\s*"((\d.*)+)"').search(contents)
        if match:
            return match.group(1)
        else:
            sys.exit(_red_string('release.py: error: could not determine the',
                                 'version number in PackageInfo.g'))

def _check_date_package_info(verbose):
    '''checks if the date in the PackageInfo.g file is today's date'''

    if not (os.path.exists('PackageInfo.g') and
            os.path.isfile('PackageInfo.g')):
        sys.exit(_red_string('release.py: error: cannot find PackageInfo.g file'))
    else:
        try:
            contents = open('PackageInfo.g', 'r').read()
        except IOError:
            sys.exit(_red_string('release.py: error: cannot read PackageInfo.g'))
        match = re.compile(r'Date\s*:=\s*"(\d\d/\d\d/\d\d\d\d)"').search(contents)
        if match:
            today = time.strftime("%d/%m/%Y")
            if match.group(1) != today and verbose:
                print _cyan_string('Date in PackageInfo.g is ' + match.group(1)
                                    + ' but today is ' + today)
            return match.group(1) == today
        else:
            sys.exit(_red_string('release.py: error: could not determine the',
                                 'version number in PackageInfo.g'))

def _exec(string, verbose):
    if verbose:
        print _cyan_string(string + ' . . .')
    try:
        devnull = open(os.devnull, 'w')
        proc = subprocess.Popen(string,
                                stdout=devnull,
                                stderr=devnull,
                                shell=True)
        proc.wait()
    except OSError:
        sys.exit(_red_string('release.py: error: ' + string + ' failed'))
    except subprocess.CalledProcessError:
        sys.exit(_red_string('release.py: error: ' +  string + ' failed'))

def _version_hg():
    '''returns the version number from the branch name in mercurial, exits if
    something goes wrong'''
    try:
        hg_version = subprocess.check_output(['hg', 'branch']).strip()
    except OSError:
        sys.exit(_red_string('release.py: error: could not determine the ',
                             'version number'))
    except subprocess.CalledProcessError:
        sys.exit(_red_string('release.py: error: could not determine the ',
                             'version number'))
    return hg_version

def _hg_identify():
    'returns the current changeset'
    try:
        hg_identify = subprocess.check_output(['hg', 'identify']).strip()
    except OSError:
        sys.exit(_red_string('release.py: error: could not determine the ',
                             'version number'))
    except subprocess.CalledProcessError:
        sys.exit(_red_string('release.py: error: could not determine the ',
                             'version number'))
    return hg_identify.split('+')[0]

def _copy_doc(dst, verbose):
    for filename in os.listdir('doc'):
        if os.path.splitext(filename)[-1] in ['.html', '.txt', '.pdf', '.css',
                '.js', '.six']:
            if verbose:
                print _cyan_string('Copying ' + filename +
                                     ' to archive . . .')
            shutil.copy('doc/' + filename, dst)

def _delete_xml_files(docdir, verbose):
    for filename in os.listdir(docdir):
        if os.path.splitext(filename)[-1] == '.xml':
            if verbose:
                print _cyan_string('Deleting ' + filename +
                                      ' from archive . . .')
            os.remove(os.path.join(docdir, filename))

def _start_mamp():
    _exec('open ' + _MAMP_DIR + 'MAMP.app', False)
    cwd = os.getcwd()
    os.chdir(_MAMP_DIR + 'bin')
    _exec('./start.sh', False)
    os.chdir(cwd)

def _stop_mamp():
    cwd = os.getcwd()
    os.chdir(_MAMP_DIR + 'bin')
    _exec('./stop.sh', False)
    os.chdir(cwd)

################################################################################
# The main event
################################################################################

def main():
    # parse the args
    parser = argparse.ArgumentParser(prog='release.py',
                                     usage='%(prog)s [options]')

    parser.add_argument('--verbose', dest='verbose', action='store_true',
                        help='verbose mode (default: False)')
    parser.set_defaults(verbose=False)
    parser.add_argument('--gap-root', nargs='*', type=str,
                        help='the gap root directory (default: [~/gap])',
                        default=['~/gap/'])
    parser.add_argument('--pkg-dir', nargs='?', type=str,
                        help='the pkg directory (default: gap-root/pkg/)',
                        default='~/gap/pkg/')

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

    # get the version number
    vers = _version_number_package_info()
    tmpdir_base = tempfile.mkdtemp()
    tmpdir = tmpdir_base + '/semigroups-' + vers

    if vers != _version_hg():
        sys.exit(_red_string('release.py: error: the version number in the ' +
                             'PackageInfo.g file is ' + vers + ' but the branch ' +
                             'name is ' + _version_hg()))

    if not _check_date_package_info(args.verbose):
        sys.exit(_red_string('release.py: error: date in PackageInfo.g ' +
                             'is not today!'))

    print _magenta_string('The version number is: ' + vers)
    if args.verbose:
        print _cyan_string('Using temporary directory: ' + tmpdir)

    # archive . . .
    print _magenta_string('Archiving using hg . . .')
    _exec('hg archive ' + tmpdir, args.verbose)

    # handle the doc . . .
    _copy_doc(tmpdir + '/doc/', args.verbose)

    # delete extra files and dirs
    for filename in ['.hgignore', '.hgtags', '.gaplint_ignore',
                     'configure.ac', 'Makefile.am']:
        if (os.path.exists(os.path.join(tmpdir, filename))
                and os.path.isfile(os.path.join(tmpdir, filename))):
            print _magenta_string('Deleting file ' + filename + ' . . .')
            try:
                os.remove(os.path.join(tmpdir, filename))
            except OSError:
                sys.exit(_red_string('release.py: error: could not delete' +
                                     filename))

    print _magenta_string('Deleting directory scripts . . .')
    try:
        shutil.rmtree(os.path.join(tmpdir, 'scripts'))
    except OSError:
        sys.exit(_red_string('release.py: error: could not delete scripts/*'))

    print _magenta_string('Running the tests on the archive . . .')
    os.chdir(tmpdir_base)
    for directory in args.gap_root:
        semigroups_dir = os.path.join(directory, 'pkg/semigroups-' + vers)
        try:
            shutil.copytree('semigroups-' + vers, semigroups_dir)
            if os.path.exists(os.path.join(directory, 'pkg/semigroups')):
                shutil.move(os.path.join(directory, 'pkg/semigroups'), tmpdir_base)
        except Exception as e:
            sys.exit(_red_string(str(e)))

        try:
            test.run_semigroups_tests(directory,
                                      directory + '/pkg',
                                      'semigroups-' + vers)
        except (OSError, IOError) as e:
            sys.exit(_red_string(str(e)))
        finally:
            if os.path.exists(os.path.join(tmpdir_base, 'semigroups')):
                shutil.move(os.path.join(tmpdir_base, 'semigroups'),
                            os.path.join(directory, 'pkg/semigroups'))
            shutil.rmtree(semigroups_dir)

    print _magenta_string('Creating the tarball . . .')

    _exec('tar -cpf semigroups-' + vers + '.tar semigroups-' + vers +
          '; gzip -9 semigroups-' + vers + '.tar', args.verbose)

    print _magenta_string('Copying to webpage . . .')
    try:
        shutil.copy('semigroups-' + vers + '.tar.gz', _WEBPAGE_DIR)
        shutil.copy('semigroups-' + vers + '/README.md', _WEBPAGE_DIR)
        shutil.copy('semigroups-' + vers + '/PackageInfo.g', _WEBPAGE_DIR)
        shutil.copy('semigroups-' + vers + '/CHANGELOG.md', _WEBPAGE_DIR)
        shutil.copytree('semigroups-' + vers + '/doc', _WEBPAGE_DIR + 'doc')
    except:
        sys.exit(_red_string('release.py: error: could not copy to the webpage!'))

    os.chdir(_WEBPAGE_DIR)
    print _magenta_string('Adding archive to webpage repo . . .')
    _exec('hg add semigroups-' + vers + '.tar.gz', args.verbose)
    print _magenta_string('Committing webpage repo . . .')
    _exec('hg commit -m "Releasing Semigroups "' + vers + '"', args.verbose)

    _start_mamp()
    webbrowser.open('http://localhost:8888/public_html/semigroups.php')
    _stop_mamp()

    publish = input('Publish the webpage? (y/n)')
    if publish == 'y':
        print _magenta_string('Pushing webpage to server . . .')
        _exec('hg push', args.verbose)

    os.chdir(_SEMIGROUPS_REPO_DIR)

    print _magenta_string('Merging ' + vers + ' into default . . .')
    _exec('hg up -r default', args.verbose)
    _exec('hg merge -r ' + vers, args.verbose)

    print _magenta_string('Closing branch ' + vers + ' . . .')
    _exec('hg up -r ' + vers, args.verbose)
    _exec('hg commit --close-branch -m "closing branch"', args.verbose)

    print _magenta_string('Updating to default branch . . .')
    _exec('hg up -r default', args.verbose)

    print _green_string('SUCCESS!')
    sys.exit(1)

################################################################################
# So that we can load this as a module if we want
################################################################################

if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(_red_string('Killed!'))

#cd /tmp
#ditto --norsrc semigroups semigroups_copy
#rm -r semigroups
#mv semigroups_copy semigroups-2.7.1
#
#cp semigroups-2.7.1.tar.gz ~/Sites/public_html/semigroups
#
#cd
#cd ~/Sites/public_html/semigroups
#hg add semigroups-2.7.1.tar.gz
#cp ~/semigroups/README.md .
#cp ~/semigroups/PackageInfo.g .
#cp ~/semigroups/CHANGELOG.md .
#
#cp ~/semigroups/doc/* doc/
