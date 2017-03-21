# If a command fails, exit this script with an error code
set -e
set -o pipefail

# Run the standard tests and manual examples
cd ../..
echo -e "\nRunning standard tests and manual examples..."
echo "LoadPackage(\"semigroups\"); SemigroupsTestStandard(); SEMIGROUPS.TestManualExamples(); quit; quit; quit;" | bin/gap.sh -A -r -m 1g -T 2>&1 | tee testlog.txt

# Run the SaveWorkspace tests
echo -e "\nRunning SaveWorkspace tests..."
echo "LoadPackage(\"semigroups\", false); SemigroupsTestInstall(); Test(\"pkg/semigroups/tst/workspaces/save-workspace.tst\"); quit; quit; quit;" | bin/gap.sh -A -r -m 1g -T 2>&1 | tee -a testlog.txt
echo "LoadPackage(\"semigroups\", false); Test(\"pkg/semigroups/tst/workspaces/load-workspace.tst\"); SemigroupsTestInstall(); quit; quit; quit;" | bin/gap.sh -L pkg/semigroups/tst/workspaces/test-output.w -A -r -m 1g -T 2>&1 | tee -a testlog.txt
rm pkg/semigroups/tst/workspaces/test-output.w

# Run gaplint
cd pkg/semigroups
echo -e "\nRunning gaplint..."
../../../gaplint/gaplint.py gap/*/*.gi | tee -a ../../testlog.txt

# Run coverage checks
if [ ! -z "$COVERAGE" ]; then
  echo -e "\nPerforming code coverage tests..."
  for testfile in tst/standard/*.tst; do
    filename=${testfile##*/}
    if [ ! `grep -E "$filename" .covignore` ]; then
      ../digraphs/scripts/travis-coverage.py $testfile 95 | tee -a ../../testlog.txt
    else
      echo -e "\033[35mignoring $filename, since it is listed in .covignore\033[0m"
    fi
  done
else
  echo -e "\nNot performing code coverage tests..."
fi

# Check the logs for invalid phrases
( ! grep -E "Diff|brk>|#E|Error|error|# WARNING|fail|Syntax warning|Couldn't open saved workspace|insufficient|WARNING|FAILED" ../../testlog.txt )
