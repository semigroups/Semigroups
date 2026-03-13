LoadPackage("semigroups", false);;
if SemigroupsTestInstall() then
  QUIT_GAP(0);
else
  QUIT_GAP(1);
fi;
FORCE_QUIT_GAP(1);
