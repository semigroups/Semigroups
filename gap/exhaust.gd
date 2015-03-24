###########################################################################
##
#W  exhaust.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# there is a good reason this is not a category!
DeclareProperty("IsExhaustiveSemigroup", IsSemigroup and IsFinite);
InstallImmediateMethod(IsExhaustiveSemigroup, IsSemigroup and IsFinite, 
0, S-> not IsActingSemigroup(S));

DeclareAttribute("ExhaustiveData", IsFinite and IsSemigroup, "mutable");
# a acting semigroup can have generic data but not the other way
# around. 

DeclareProperty("IsExhaustiveData", IsSemigroupData);

DeclareOperation("Enumerate", [IsExhaustiveData]);
DeclareOperation("Enumerate", [IsExhaustiveData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsExhaustiveData, IsCyclotomic, IsFunction]);

