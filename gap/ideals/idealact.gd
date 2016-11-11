#############################################################################
##
#W  idealac.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# the current idea is use the new setup for regular and inverse semigroups, and
# to use the

DeclareCategory("IsSemigroupIdealData", IsSemigroupData);
DeclareCategory("IsRegularIdealData", IsSemigroupIdealData);
DeclareAttribute("SemigroupIdealData", IsSemigroupIdeal);
DeclareOperation("Enumerate", [IsSemigroupIdealData, IsCyclotomic, IsRecord]);
