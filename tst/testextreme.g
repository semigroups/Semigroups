#############################################################################
##
#W  testextreme.g
#Y  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
LoadPackage("semigroups", false);;
# These "{No} errors detected" lines currently have to be printed in this way
# to satisfy the automated GAP testing system that runs on Jenkins.
if SemigroupsTestExtreme() then
  Print("#I  No errors detected while testing\n\n");
  QUIT_GAP(0);
else
  Print("#I  Errors detected while testing\n\n");
  QUIT_GAP(1);
fi;
FORCE_QUIT_GAP(1); # if we ever get here, there was an error
