echo "LoadPackage(\"semigroups\"); SemigroupsTestStandard(); quit;" | sh bin/gap.sh | tee testlog.txt | grep --colour=always -A 1 -E "########> Diff|$" ; ( ! grep "########> Diff" testlog.txt )
