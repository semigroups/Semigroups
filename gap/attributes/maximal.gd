#############################################################################
##
##  attributes/maximal.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##                                                          Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("IsMaximalSubsemigroup", [IsSemigroup, IsSemigroup]);
DeclareAttribute("MaximalSubsemigroups", IsSemigroup);
DeclareAttribute("NrMaximalSubsemigroups", IsSemigroup);
DeclareOperation("MaximalSubsemigroups", [IsSemigroup, IsRecord]);
DeclareOperation("MaximalSubsemigroupsNC", [IsSemigroup, IsRecord]);
