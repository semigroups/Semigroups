###########################################################################
##
#W  exhaust.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# an acting semigroup can have generic data but not the other way
# around. 

DeclareCategory("IsGenericSemigroupData", IsList);

# there is a good reason this is not a category!

DeclareAttribute("GenericSemigroupData", IsFinite and IsSemigroup, "mutable");

DeclareOperation("Enumerate", [IsGenericSemigroupData]);
DeclareOperation("Enumerate", [IsGenericSemigroupData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsGenericSemigroupData, IsCyclotomic, IsFunction]);
