# This script should be run from the semigroups directory, with the command
#     scripts/test-workspaces.sh
# It will run the SaveWorkspace tests in tst/workspaces, and check for issues

# If a command fails, exit this script with an error code
set -e

# Run the SaveWorkspace tests
echo "LoadPackage(\"semigroups\", false); SemigroupsTestInstall(); Test(\"tst/workspaces/save-workspace.tst\"); quit; quit; quit;" | ../../bin/gap.sh -A -r -m 1g -T 2>&1 | tee testlog.txt
echo "LoadPackage(\"semigroups\", false); Test(\"tst/workspaces/load-workspace.tst\"); SemigroupsTestInstall(); quit; quit; quit;" | ../../bin/gap.sh -L tst/workspaces/test-output.w -A -r -m 1g -T 2>&1 | tee -a testlog.txt
rm tst/workspaces/test-output.w

# Check the logs for invalid phrases
( ! grep -E "Diff|brk>|#E|Error|# WARNING|fail|Syntax warning|Couldn't open saved workspace" testlog.txt )

# Delete the logs
rm testlog.txt
