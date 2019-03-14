# If a command fails, exit this script with an error code
set -e
set -o pipefail

touch $GAPROOT/testlog.txt
TESTLOG="$GAPROOT/testlog.txt"
GAPSH="$GAPROOT/bin/gap.sh"
SEMI_DIR="$GAPROOT/pkg/semigroups"

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

  cd $SEMI_DIR/tst/workspaces
  echo -e "\nRunning SaveWorkspace tests..."
  echo "LoadPackage(\"semigroups\"); SemigroupsTestInstall(); Test(\"save-workspace.tst\"); quit; quit; quit;" |
    $GAPSH -A -r -m 1g -T 2>&1 | tee -a $TESTLOG

  echo -e "\nRunning LoadWorkspace tests..."
  echo "Test(\"load-workspace.tst\"); SemigroupsTestInstall(); quit; quit; quit;" |
    $GAPSH -L test-output.w -A -x 80 -r -m 1g -T 2>&1 | tee -a $TESTLOG

  echo -e "\nRunning Semigroups package standard tests and manual examples..."
  echo "LoadPackage(\"semigroups\"); SemigroupsTestStandard(); SEMIGROUPS.TestManualExamples();" |
    $GAPSH -A -x 80 -r -m 1g -T 2>&1 | tee -a $TESTLOG

  # Run GAP tests, but only in 64-bit, since they're far too slow in 32-bit
  if [ "$ABI" == "64" ]; then
    echo -e "\nRunning GAP's testinstall tests with Semigroups loaded..."
    echo "LoadPackage(\"semigroups\"); Read(\"$GAPROOT/tst/testinstall.g\");" |
      $GAPSH -A -x 80 -r -m 100m -o 1g -K 2g -T 2>&1 | tee -a $TESTLOG

    # Run GAP testbugfix with Semigroups loaded; this only works with GAP master
    if [ "$GAP" == "master" ]; then
      echo -e "\nRunning GAP's testbugfix tests with Semigroups loaded..."
      # Delete some very long-running tests
      rm $GAPROOT/tst/testbugfix/2016-03-03-t00332.tst
      rm $GAPROOT/tst/testbugfix/2018-05-24-IntermediateSubgroups.tst
      rm $GAPROOT/tst/testbugfix/2018-09-13-MTC.tst
      echo "LoadPackage(\"semigroups\"); Read(\"$GAPROOT/tst/testbugfix.g\");" |
        $GAPSH -A -x 80 -r -m 100m -o 1g -K 2g -T 2>&1 | tee -a $TESTLOG
    fi
  fi
fi

( ! grep -E "Diff|brk>|#E|Error|Errors detected|# WARNING|Syntax warning|Couldn't open saved workspace|insufficient|WARNING in|FAILED|Total errors found:" $TESTLOG )
