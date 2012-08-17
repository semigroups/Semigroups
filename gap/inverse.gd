#############################################################################
##
#W  inverse.gd
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## functions and methods for inverse semigroups of partial permutations

DeclareAttribute("NaturalPartialOrder", IsPartialPermSemigroup and
IsInverseSemigroup);

DeclareGlobalFunction("IsJoinIrreducible");

#old

DeclareGlobalFunction("CreateOrbSCCMultipliers");
DeclareGlobalFunction("CreateOrbSCCMultipliersNC");
DeclareGlobalFunction("CreateOrbSCCSchutzGp");
DeclareGlobalFunction("CreateOrbSCCSchutzGpNC");
DeclareGlobalFunction("EnumerateInverseSemiData");
DeclareAttribute("LongOrb", IsPartialPermSemigroup and IsInverseSemigroup,
 "mutable");
DeclareAttribute("OrbMultipliers", IsGreensClass);
DeclareAttribute("OrbSCCStabChain", IsGreensClass);
DeclareGlobalFunction("ShortOrb");


