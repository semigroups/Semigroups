#!/usr/bin/env python
'''
Create the archive of the Semigroups package for release, and copy the relevant
things to the webpage.
'''

import textwrap, os, argparse, tempfile, subprocess, sys, os, re, shutil, gzip
import release

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
    return '\n        '.join(_WRAPPER.wrap('\033[34m' + string + '\033[0m'))

def _other_blue_string(string):
    'blue string'
    return '\n        '.join(_WRAPPER.wrap('\033[1;34m' + string + '\033[0m'))

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
        sys.exit(_red_string('archive.py: error: cannot find PackageInfo.g file'))
    else:
        try:
            contents = open('PackageInfo.g', 'r').read()
        except IOError:
            sys.exit(_red_string('archive.py: error: cannot read PackageInfo.g'))
        match = re.compile(r'Version\s*:=\s*"((\d.*)+)"').search(contents)
        if match:
            return match.group(1)
        else:
            sys.exit(_red_string('archive.py: error: could not determine the',
                                 'version number in PackageInfo.g'))

def _exec(string, verbose):
    if verbose:
        print _blue_string(string + ' . . .')
    try:
        devnull = open(os.devnull, 'w')
        proc = subprocess.Popen(string,
                                stdout=devnull,
                                stderr=devnull,
                                shell=True)
        proc.wait()
    except OSError:
        sys.exit(_red_string('archive.py: error: ' + string + ' failed'))
    except subprocess.CalledProcessError:
        sys.exit(_red_string('archive.py: error: ' +  string + ' failed'))

def _version_hg():
    '''returns the version number from the branch name in mercurial, exits if
    something goes wrong'''
    try:
        hg_version = subprocess.check_output(['hg', 'branch']).strip()
    except OSError:
        sys.exit(_red_string('archive.py: error: could not determine the ',
                             'version number'))
    except subprocess.CalledProcessError:
        sys.exit(_red_string('archive.py: error: could not determine the ',
                             'version number'))
    return hg_version

def _copy_doc(dst, verbose):
    for filename in os.listdir('doc'):
        if os.path.splitext(filename)[-1] in ['.html', '.txt', '.pdf', '.css',
                '.js', '.six']:
            if verbose:
                print _blue_string('Copying ' + filename +
                                     ' to archive . . .')
            shutil.copy('doc/' + filename, dst)

def _delete_xml_files(docdir, verbose):
    for filename in os.listdir(docdir):
        if os.path.splitext(filename)[-1] == '.xml':
            if verbose:
                print _blue_string('Deleting ' + filename +
                                      ' from archive . . .')
            os.remove(os.path.join(docdir, filename))

################################################################################
# The main event
################################################################################

def main():
    # parse the args
    parser = argparse.ArgumentParser(prog='archive.py',
                                     usage='%(prog)s [options]')

    parser.add_argument('--verbose', dest='verbose', action='store_true',
                        help='verbose mode (default: False)')
    parser.set_defaults(verbose=False)
    parser.add_argument('--gap-root', nargs='?', type=str,
                        help='the gap root directory (default: ~/gap)',
                        default='~/gap/')
    parser.add_argument('--pkg-dir', nargs='?', type=str,
                        help='the pkg directory (default: gap-root/pkg/)',
                        default='~/gap/pkg/')

    args = parser.parse_args()

    if not args.gap_root[-1] == '/':
        args.gap_root += '/'
    if not args.pkg_dir[-1] == '/':
        args.pkg_dir += '/'

    args.gap_root = os.path.expanduser(args.gap_root)
    args.pkg_dir = os.path.expanduser(args.pkg_dir)

    if not (os.path.exists(args.gap_root) and os.path.isdir(args.gap_root)):
        sys.exit(_red_string('release.py: error: can\'t find GAP root' +
                             ' directory!'))
    if not (os.path.exists(args.pkg_dir) or os.path.isdir(args.pkg_dir)):
        sys.exit(_red_string('release.py: error: can\'t find package' +
                             ' directory!'))

    # get the version number
    vers = _version_number_package_info()
    tmpdir_base = tempfile.mkdtemp()
    tmpdir = tmpdir_base + '/semigroups-' + vers

    if vers != _version_hg():
        sys.exit(_red_string('archive.py: error: the version number in the ' +
                             'PackageInfo.g file is ' + vers + ' but the branch ' +
                             'name is ' + _version_hg()))

    print _magenta_string('The version number is: ' + vers)
    if args.verbose:
        print _other_blue_string('Using temporary directory: ' + tmpdir)

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
                sys.exit(_red_string('archive.py: error: could not delete' +
                                     filename))

    print _magenta_string('Deleting directory scripts . . .')
    try:
        shutil.rmtree(os.path.join(tmpdir, 'scripts'))
    except OSError:
        sys.exit(_red_string('archive.py: error: could not delete scripts/*'))

    print _magenta_string('Running the tests on the archive . . .')
    os.chdir(tmpdir_base)
    try:
        shutil.copytree('semigroups-' + vers, args.pkg_dir + 'semigroups-' + vers)
        shutil.move(os.path.join(args.pkg_dir, 'semigroups'), tmpdir_base)
    except IOError:
        sys.exit(_red_string('archive.py: error: could not copy to the pkg dir'))

    try:
        release.run_semigroups_tests(args.gap_root,
                                     args.pkg_dir,
                                     'semigroups-' + vers)
    except:
        shutil.move(os.path.join(tmpdir_base, 'semigroups'), args.pkg_dir)
        os.remove(args.pkg_dir, 'semigroups-' + vers)
        sys.exit(_red_string('archive.py: error: tests failed, aborting!'))

    shutil.move(os.path.join(tmpdir_base, 'semigroups'), args.pkg_dir)
    os.remove(args.pkg_dir, 'semigroups-' + vers)

    print _magenta_string('Creating the tarball . . .')

    _delete_xml_files(tmpdir + '/doc', args.verbose)
    _exec('tar -cpf semigroups-' + vers + '.tar semigroups-' + vers +
          '; gzip -9 semigroups-' + vers + '.tar', args.verbose)

    print _magenta_string('Copying the archive to the desktop . . .')
    try:
        shutil.copy('semigroups-' + vers + '.tar.gz',
                    os.path.expanduser('~/Desktop'))
    except:
        sys.exit(_red_string('archive.py: error: could not copy to the desktop'))

    print _green_string('SUCCESS!')
    sys.exit(0)

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
