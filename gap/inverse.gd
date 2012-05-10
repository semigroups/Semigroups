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

DeclareGlobalFunction("CreateOrbSCCMultipliers");
DeclareGlobalFunction("CreateOrbSCCMultipliersNC");
DeclareGlobalFunction("CreateOrbSCCSchutzGp");
DeclareGlobalFunction("CreateOrbSCCSchutzGpNC");
DeclareGlobalFunction("EnumerateInverseSemiData");
DeclareProperty("IsGreensClassOfPartPermSemigroup", IsGreensClass);
DeclareProperty("IsGreensClassOfInverseSemigroup", IsGreensClass);
DeclareAttribute("LongOrb", IsPartialPermSemigroup and IsInverseSemigroup,
 "mutable");
DeclareAttribute("NaturalPartialOrder", IsPartialPermSemigroup and
IsInverseSemigroup);
DeclareAttribute("OrbMultipliers", IsGreensClass);
DeclareAttribute("OrbSCCStabChain", IsGreensClass);
DeclareAttribute("Points", IsPartialPermSemigroup);
DeclareGlobalFunction("ShortOrb");

DeclareSynonymAttr("DomainOfPartialPermSemigroup", Points);
