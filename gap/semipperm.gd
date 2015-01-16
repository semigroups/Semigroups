#############################################################################
##
#W  semipperm.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("CyclesOfPartialPerm", IsPartialPerm);

DeclareAttribute("ComponentRepsOfPartialPermSemigroup",
IsPartialPermSemigroup);
DeclareAttribute("ComponentsOfPartialPermSemigroup",
IsPartialPermSemigroup);
DeclareAttribute("CyclesOfPartialPermSemigroup",
IsPartialPermSemigroup);

DeclareOperation("AsPartialPermSemigroup", [IsSemigroup]);

DeclareOperation("NumberPartialPerm", [IsPartialPerm]);
DeclareOperation("NumberPartialPerm", [IsPartialPerm, IsPosInt]);
DeclareOperation("PartialPermNumber", [IsPosInt, IsPosInt]);
DeclareOperation("SubsetNumber", [IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation("SubsetNumber", [IsPosInt, IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation("NumberSubset", [IsList, IsPosInt]);

DeclareGlobalFunction("SEMIGROUPS_SubsetNumber");
