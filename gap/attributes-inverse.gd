#############################################################################
##
#W  attributes-inverse.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##                                                       Wilf Wilson,
##                                                       Rhiannon Dougall,
##                                                       Robert Hancock
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("SameMinorantsSubgroup", IsGroupHClass);
DeclareAttribute("SmallerDegreePartialPermRepresentation", IsInverseSemigroup);
DeclareAttribute("VagnerPrestonRepresentation", IsInverseSemigroup);
DeclareAttribute("JoinIrreducibleDClasses", IsInverseSemigroup);

DeclareOperation("Minorants", [IsInverseSemigroup and IsPartialPermSemigroup,
IsPartialPerm]);
DeclareGlobalFunction("SupremumIdempotentsNC");

DeclareOperation("IsMajorantlyClosed", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("IsMajorantlyClosedNC", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("MajorantClosure", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("MajorantClosureNC", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("IsJoinIrreducible", [IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm]);
DeclareOperation("RightCosetsOfInverseSemigroup", [IsInverseSemigroup and IsPartialPermSemigroup, IsInverseSemigroup and IsPartialPermSemigroup]);