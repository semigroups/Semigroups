echo "LoadPackage(\"semigroups\"); SemigroupsTestStandard(); SEMIGROUPS.TestManualExamples(); quit; quit; quit;" | bin/gap.sh -A -r -m 1g -T 2>&1 | tee testlog.txt
( ! grep -E "########> Diff|brk>|#E|Error|# WARNING|fail|Syntax warning" testlog.txt )
