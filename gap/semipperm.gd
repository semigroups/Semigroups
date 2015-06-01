#############################################################################
##
#W  semipperm.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareProperty("IsPartialPermSemigroupGreensClass", IsGreensClass);

DeclareAttribute("CyclesOfPartialPerm", IsPartialPerm);
DeclareAttribute("CyclesOfPartialPermSemigroup",
                 IsPartialPermSemigroup);
DeclareAttribute("ComponentRepsOfPartialPermSemigroup",
                 IsPartialPermSemigroup);
DeclareAttribute("ComponentsOfPartialPermSemigroup",
                 IsPartialPermSemigroup);

DeclareOperation("AsPartialPermSemigroup", [IsSemigroup]);

DeclareOperation("NumberPartialPerm", [IsPartialPerm, IsPosInt]);
DeclareOperation("PartialPermNumber", [IsPosInt, IsPosInt]);
DeclareOperation("SubsetNumber", [IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation("NumberSubset", [IsList, IsPosInt]);
DeclareOperation("NumberSubsetOfEqualSize", [IsList, IsPosInt]);

DeclareGlobalFunction("SEMIGROUPS_SubsetNumber");
