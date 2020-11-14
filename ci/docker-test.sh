#!/usr/bin/env bash

# If a command fails, exit this script with an error code
set -e
set -o pipefail

# This script is intended to be run inside the docker container
# jamesdbmitchell/gap-docker:version-?.?.?

echo -e "\nInstalling dependencies . . . "
sudo apt-get --yes update
sudo apt-get --yes upgrade
sudo apt-get install libtool curl --yes

# TODO check that environment variables exist

if [ "$SUITE" == "lint" ]; then
  # Install cpplint and gaplint
  sudo pip install cpplint
  sudo pip install gaplint
fi

echo -e "\nFixing permissions . . . "
sudo chown -R gap: $GAP_HOME/pkg/semigroups

################################################################################
# Install libsemigroups
cd $GAP_HOME/pkg/semigroups
if [ "$SUITE" != "external-libsemigroups" ]; then
  ./prerequisites.sh
else
  # Autoreconf requires that this directory exists even if we don't use the
  # included libsemigroups . . .
  mkdir libsemigroups 
fi

################################################################################
# Compile Semigroups package

if [ "$SUITE" != "lint" ]; then
  echo -e "\nCompiling the Semigroups package..."
  cd $GAP_HOME/pkg/semigroups
  ./autogen.sh
  ./configure $PKG_FLAGS $EXTRA_PKG_FLAGS
  make -j4
fi

if [ "$SUITE" != "test" ] && [ "$SUITE" != "coverage" ] && [ "$SUITE" != "lint" ]; then
  echo -e "\nError, unrecognised Travis suite: $SUITE"
  exit 1
fi

# Common curl settings
CURL="curl --connect-timeout 5 --max-time 10 --retry 5 --retry-delay 0 --retry-max-time 40 -L"

################################################################################
# Install digraphs, genss, io, orb, images, and profiling
PKGS=( "digraphs" "genss" "io" "orb" "images")
if [ "$SUITE"  == "coverage" ]; then
  PKGS+=( "profiling" )
fi
if [ "$PACKAGES" == "latest" ]; then 
  PKGS+=( "datastructures" )
fi

for PKG in "${PKGS[@]}"; do
  cd $GAP_HOME/pkg

  # Get the relevant version number
  if [ "$PACKAGES" == "latest" ] || [ "$PKG" == "profiling" ]; then
    VERSION=`$CURL -s "https://github.com/gap-packages/$PKG/releases/latest" | grep \<title\>Release | awk -F' ' '{print $2}'`
  else
    VERSION=`grep "\"$PKG\"" $GAP_HOME/pkg/semigroups/PackageInfo.g | awk -F'"' '{print $4}' | cut -c3-`
  fi

  if [ -z $VERSION ]; then
    echo -e "\nCould not determine the version number of the package $PKG!! Aborting..."
    exit 1
  fi

  URL="https://github.com/gap-packages/$PKG/releases/download/v$VERSION/$PKG-$VERSION.tar.gz"
  echo -e "\nDownloading $PKG-$VERSION ($PACKAGES version), from URL:\n$URL"
  $CURL "$URL" -o $PKG-$VERSION.tar.gz
  tar xf $PKG-$VERSION.tar.gz && rm $PKG-$VERSION.tar.gz

  if [ -f $PKG-$VERSION/configure ]; then
    if [ "$PKG" == "orb" ] || [ "$PKG" == "datastructures" ]; then
      cd $PKG-$VERSION && ./configure && make # orb doesn't accept package flags
    else
      cd $PKG-$VERSION && ./configure $PKG_FLAGS && make
    fi
  fi
done

################################################################################
# Start running the tests
touch $GAP_HOME/testlog.txt
TESTLOG="$GAP_HOME/testlog.txt"
GAPSH="$GAP_HOME/bin/gap.sh"
SEMI_DIR="$GAP_HOME/pkg/semigroups"

if [ "$SUITE" == "lint" ]; then

  echo -e "\nLinting with gaplint and cpplint..."
  cd $SEMI_DIR
  gaplint `grep "^\s\+gaplint" Makefile.am | cut -d " " -f2-`
  cpplint --extensions=c,cc,h `grep "^\s\+cpplint" Makefile.am | cut -d " " -f2-`

elif [ "$SUITE" == "coverage" ]; then

  echo -e "\nPerforming code coverage tests..."
  for TEST in $SEMI_DIR/tst/standard/*.tst; do
    FILENAME=${TEST##*/}
    if [ ! `grep -E "$FILENAME" $SEMI_DIR/.covignore` ]; then
      $SEMI_DIR/scripts/travis-coverage.py $TEST $THRESHOLD | tee -a $TESTLOG
    else
      echo -e "\033[35mignoring $FILENAME, which is listed in .covignore\033[0m"
    fi
  done

else

  # Temporary workaround because of GAP being weird with memory
  # if [ "$ABI" == "32" ]; then
  #   MEM=1g
  # elif [ "$ABI" == "64" ]; then
  #   MEM=2g
  # fi

  cd $SEMI_DIR/tst/workspaces
  echo -e "\nRunning SaveWorkspace tests..."
  echo "LoadPackage(\"semigroups\"); SemigroupsTestInstall(); Test(\"save-workspace.tst\"); quit; quit; quit;" |
    $GAPSH -A -m 768m -o $MEM -T 2>&1 | tee -a $TESTLOG

  echo -e "\nRunning LoadWorkspace tests..."
  echo "Test(\"load-workspace.tst\"); SemigroupsTestInstall(); quit; quit; quit;" |
    $GAPSH -L test-output.w -A -x 80 -m 768m -o $MEM -T 2>&1 | tee -a $TESTLOG

  echo -e "\nRunning Semigroups package standard tests and manual examples..."
  echo "LoadPackage(\"semigroups\"); SemigroupsTestStandard(); SEMIGROUPS.TestManualExamples();" |
    $GAPSH -A -x 80 -m 768m -o $MEM -T 2>&1 | tee -a $TESTLOG

  # Run GAP tests, but only in 64-bit, since they're far too slow in 32-bit
  if [ "$ABI" == "64" ]; then
    echo -e "\nRunning GAP's testinstall tests with Semigroups loaded..."
    echo "LoadPackage(\"semigroups\"); Read(\"$GAP_HOME/tst/testinstall.g\");" |
      $GAPSH -A -x 80 -m 100m -o 1g -K 2g -T 2>&1 | tee -a $TESTLOG

    # Run GAP testbugfix with Semigroups loaded; this only works with GAP master
    if [ "$GAP" == "master" ]; then
      echo -e "\nRunning GAP's testbugfix tests with Semigroups loaded..."
      # Delete some problematic or very long-running tests
      rm $GAP_HOME/tst/testbugfix/2016-03-03-t00332.tst
      rm $GAP_HOME/tst/testbugfix/2018-05-24-IntermediateSubgroups.tst
      rm $GAP_HOME/tst/testbugfix/2018-09-13-MTC.tst
      rm $GAP_HOME/tst/testbugfix/2018-12-06-GroupWithGenerators.tst
      echo "LoadPackage(\"semigroups\"); Read(\"$GAP_HOME/tst/testbugfix.g\");" |
        $GAPSH -A -x 80 -m 100m -o 1g -K 2g -T 2>&1 | tee -a $TESTLOG
    fi
  fi
fi

( ! grep -E "Diff|brk>|#E|Error|Errors detected|# WARNING|Syntax warning|Couldn't open saved workspace|insufficient|WARNING in|FAILED|Total errors found:" $TESTLOG )
