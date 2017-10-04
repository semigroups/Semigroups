# If a command fails, exit this script with an error code
set -e
set -o pipefail

# Create the testlog and remember its location
cd ../..
touch testlog.txt
TESTLOG="`pwd`/testlog.txt"

if [ ! -z "$LINT" ]; then

  cd ../gaplint
  export PATH=$PATH:`pwd`
  cd ../gap/pkg/semigroups
  # Can't use make lint since it requires compilation
  `grep "^\s\+gaplint" Makefile.am`
  `grep "^\s\+cpplint" Makefile.am`

elif [ ! -z "$GAP_BRANCH" ]; then

  GAP_DIR=`pwd`

  # Work out the Digraphs versions/branches in which to run the tests
  if [ ! -z "$DIGRAPHS_BR" ]; then
    DIGRAPHS_BRANCH=$DIGRAPHS_BR
  else
    cd $GAP_DIR/pkg/semigroups
    DIGRAPHS_BRANCH=v`grep "\"digraphs\"" PackageInfo.g| awk -F'"' '{print $4}' | cut -c3-`
  fi

  echo -e "\nRunning tests with Digraphs package in $DIGRAPHS_BRANCH..."
  cd $GAP_DIR/pkg/digraphs
  git checkout $DIGRAPHS_BRANCH
  echo "Compiling Digraphs..."
  ./autogen.sh
  ./configure $PKG_FLAGS
  make
  cd $GAP_DIR/pkg/semigroups

  if [ ! -z "$COVERAGE" ]; then

    echo -e "\nPerforming code coverage tests..."
    for TESTFILE in tst/standard/*.tst; do
      FILENAME=${TESTFILE##*/}
      if [ ! `grep -E "$FILENAME" .covignore` ]; then
        ../digraphs/scripts/travis-coverage.py $TESTFILE $THRESHOLD | tee -a $TESTLOG
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
    echo "LoadPackage(\"semigroups\"); SemigroupsTestStandard(); SEMIGROUPS.TestManualExamples(); Read(\"$GAP_DIR/tst/testinstall.g\");" | $GAP_DIR/bin/gap.sh -A -r -m 1g -T 2>&1 | tee $TESTLOG
  fi
fi

( ! grep -E "Diff|brk>|#E|Error|Errors detected|# WARNING|fail|Syntax warning|Couldn't open saved workspace|insufficient|WARNING in|FAILED|Total errors found:" $TESTLOG )
