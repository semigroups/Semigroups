LoadPackage("semigroups", false);;
if SEMIGROUPS.TestDir(
    DirectoriesPackageLibrary(
      "semigroups",
      "tst/standard/libsemigroups")[1]![1],
    [rec(earlyStop := false)]) then
  QUIT_GAP(0);
else
  QUIT_GAP(1);
fi;
FORCE_QUIT_GAP(1);
