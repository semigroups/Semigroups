# If a command fails, exit this script with an error code
set -e

# Run the standard tests and manual examples
echo -en 'travis_fold:start:RunTests\r'
cd ../..
echo "LoadPackage(\"semigroups\"); SemigroupsTestStandard(); SEMIGROUPS.TestManualExamples(); quit; quit; quit;" | bin/gap.sh -A -r -m 1g -T 2>&1 | tee testlog.txt
echo -en 'travis_fold:end:RunTests\r'

# Run the SaveWorkspace tests
echo -en 'travis_fold:start:SaveWorkspaceTests\r'
echo "LoadPackage(\"semigroups\", false); SemigroupsTestInstall(); Test(\"pkg/semigroups/tst/workspaces/save-workspace.tst\"); quit; quit; quit;" | bin/gap.sh -A -r -m 1g -T 2>&1 | tee -a testlog.txt
echo "LoadPackage(\"semigroups\", false); Test(\"pkg/semigroups/tst/workspaces/load-workspace.tst\"); SemigroupsTestInstall(); quit; quit; quit;" | bin/gap.sh -L pkg/semigroups/tst/workspaces/test-output.w -A -r -m 1g -T 2>&1 | tee -a testlog.txt
rm pkg/semigroups/tst/workspaces/test-output.w
echo -en 'travis_fold:end:SaveWorkspaceTests\r'

# Check the logs for invalid phrases
( ! grep -E "########> Diff|brk>|#E|Error|# WARNING|fail|Syntax warning|Couldn't open saved workspace" testlog.txt )
