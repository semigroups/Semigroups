###########################################################################
##
#W  exhaust.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsExhaustiveSemigroup", IsSemigroup and IsFinite);
DeclareAttribute("ExhaustiveData", IsFinite and IsSemigroup, "mutable");
# a non-exhaustive semigroup can have exhaustive data but not the other way
# around. 

DeclareCategory("IsExhaustiveData", IsSemigroupData);

DeclareOperation("Enumerate", [IsExhaustiveData]);
DeclareOperation("Enumerate", [IsExhaustiveData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsExhaustiveData, IsCyclotomic, IsFunction]);

