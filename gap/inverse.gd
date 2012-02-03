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

DeclareGlobalFunction("CreateSCCMultipliers");
DeclareGlobalFunction("CreateSchutzGp");
DeclareGlobalFunction("EnumerateRangesOrb");
DeclareProperty("IsGreensClassOfPartPermSemigroup", IsGreensClass);
DeclareProperty("IsGreensClassOfInverseSemigroup", IsGreensClass);
DeclareAttribute("StabChainOfSchutzGp", IsGreensClass);
#JDM more filters above...
DeclareAttribute("RangesOrb", IsPartialPermSemigroup and IsInverseSemigroup, "mutable");
DeclareGlobalFunction("ShortOrb");
