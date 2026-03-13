LoadPackage("semigroups", false);;
if SemigroupsTestStandard(rec(earlyStop := false)) then
  QUIT_GAP(0);
else
  QUIT_GAP(1);
fi;
FORCE_QUIT_GAP(1);
