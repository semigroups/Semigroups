###########################################################################
##
#W  semigroupe.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("ExhaustiveData", IsFinite and IsSemigroup, "mutable");
DeclareCategory("IsExhaustiveData", IsList);
DeclareFilter("IsClosedExhaustiveData", IsExhaustiveData);

DeclareOperation("Enumerate", [IsExhaustiveData]);
DeclareOperation("Enumerate", [IsExhaustiveData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsExhaustiveData, IsCyclotomic, IsFunction]);

