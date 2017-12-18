# If a command fails, exit this script with an error code
set -e
set -o pipefail

# Create the testlog and remember its location
cd ../..
touch testlog.txt
TESTLOG="`pwd`/testlog.txt"

if [ "$SUITE" == "lint" ]; then

  cd ../lint/gaplint
  GAPLINT="`pwd`/gaplint.py"
  cd ../cpplint
  CPPLINT="`pwd`/cpplint.py"

  cd ../../gap/pkg/semigroups

  $GAPLINT `grep "^\s\+gaplint" Makefile.am | cut -d " " -f2-`
  $CPPLINT --extensions=c,cc,h `grep "^\s\+cpplint" Makefile.am | cut -d " " -f2-`

else

  GAP_DIR=`pwd`
  cd $GAP_DIR/pkg/semigroups

  if [ "$SUITE" == "coverage" ]; then

    echo -e "\nPerforming code coverage tests..."
    for TESTFILE in tst/standard/*.tst; do
      FILENAME=${TESTFILE##*/}
      if [ ! `grep -E "$FILENAME" .covignore` ]; then
        $GAP_DIR/pkg/semigroups/scripts/travis-coverage.py $TESTFILE $THRESHOLD | tee -a $TESTLOG
      else
        echo -e "\033[35mignoring $FILENAME, since it is listed in .covignore\033[0m"
      fi
    done

  else
    # Run the SaveWorkspace tests
    cd tst/workspaces
    echo -e "\nRunning SaveWorkspace tests..."
    echo "LoadPackage(\"semigroups\", false); SemigroupsTestInstall(); Test(\"save-workspace.tst\"); quit; quit; quit;" | $GAP_DIR/bin/gap.sh -A -r -m 1g -T 2>&1 | tee -a $TESTLOG
    echo "Test(\"load-workspace.tst\"); SemigroupsTestInstall(); quit; quit; quit;" | $GAP_DIR/bin/gap.sh -L test-output.w -A -r -m 1g -T 2>&1 | tee -a $TESTLOG
    rm test-output.w
    cd ../..
    # Run all tests and manual examples
    echo -e "\nRunning tests and manual examples..."
    echo "LoadPackage(\"semigroups\"); SemigroupsTestAll();" | $GAP_DIR/bin/gap.sh -A -r -m 1g -T 2>&1 | tee -a $TESTLOG
    if [ ! "$ABI" == "32" ]; then
      echo "LoadPackage(\"semigroups\"); Read(\"$GAP_DIR/tst/testinstall.g\");" | $GAP_DIR/bin/gap.sh -A -x 80 -r -m 100m -o 1g -K 2g -T 2>&1 | tee -a $TESTLOG
    fi
  fi
fi

( ! grep -E "Diff|brk>|#E|Error|Errors detected|# WARNING|fail|Syntax warning|Couldn't open saved workspace|insufficient|WARNING in|FAILED|Total errors found:" $TESTLOG )
