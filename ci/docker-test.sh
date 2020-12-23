#!/usr/bin/env bash

# If a command fails, exit this script with an error code
set -e
set -o pipefail

# This script is intended to be run inside the docker container
# jamesdbmitchell/gap-docker:version-?.?.?

if [ "$SUITE" != "test" ] && [ "$SUITE" != "coverage" ]; then
  echo -e "\nError, unrecognised Travis suite: $SUITE"
  exit 1
fi

################################################################################
# Start running the tests
touch $GAP_HOME/testlog.txt
TESTLOG="$GAP_HOME/testlog.txt"
GAPSH="$GAP_HOME/bin/gap.sh"
SEMI_DIR="$GAP_HOME/pkg/semigroups"

if [ "$SUITE" == "coverage" ]; then

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
  if [ "$ABI" == "32" ]; then
    MEM=1g
  elif [ "$ABI" == "64" ]; then
    MEM=2g
  fi

  cd $SEMI_DIR/tst/workspaces
  echo -e "\nRunning SaveWorkspace tests..."
  echo "LoadPackage(\"semigroups\"); SemigroupsTestInstall(); Test(\"save-workspace.tst\"); quit; quit; quit;" |
    $GAPSH -A -m 768m -o $MEM -T 2>&1 | tee -a $TESTLOG

  echo -e "\nRunning LoadWorkspace tests..."
  echo "Test(\"load-workspace.tst\"); SemigroupsTestInstall(); quit; quit; quit;" | $GAPSH -L test-output.w -A -x 80 -m 768m -o $MEM -T 2>&1 | tee -a $TESTLOG

  echo -e "\nRunning Semigroups package standard tests and manual examples..."
  echo "LoadPackage(\"semigroups\"); SemigroupsTestStandard(); SEMIGROUPS.TestManualExamples();" |
    $GAPSH -A -x 80 -m 768m -o $MEM -T 2>&1 | tee -a $TESTLOG

  # Run GAP tests, but only in 64-bit, since they're far too slow in 32-bit
  if [ "$ABI" == "64" ]; then
    echo -e "\nRunning GAP's testinstall tests with Semigroups loaded..."
    # Delete some problematic tests
    rm $GAP_HOME/tst/testinstall/strings.tst
    rm -f $GAP_HOME/tst/testinstall/stringobj.tst
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
