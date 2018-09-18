#############################################################################
##
#W  teststandard.g
#Y  Copyright (C) 2018                                      Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
LoadPackage("semigroups", false);;
if SemigroupsTestStandard(rec(suppressStatusMessage := true)) then
  Print("#I  No errors detected while testing\n\n");
  QUIT_GAP(0);
else
  Print("#I  Errors detected while testing\n\n");
  QUIT_GAP(1);
fi;
FORCE_QUIT_GAP(1); # if we ever get here, there was an error
