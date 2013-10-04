#############################################################################
##
#W  attributes-inverse.gd
#Y  Copyright (C) 2013                                   James D. Mitchell,
##                                                       Wilf Wilson,
##                                                       Rhiannon Dougall,
##                                                       Robert Hancock
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("IsJoinIrreducible", [IsInverseSemigroup and
IsPartialPermSemigroup, IsPartialPerm]);
DeclareOperation("IsMajorantlyClosed", [IsInverseSemigroup and
IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("IsMajorantlyClosedNC", [IsInverseSemigroup and
IsPartialPermSemigroup, IsPartialPermCollection]);

DeclareAttribute("JoinIrreducibleDClasses", IsInverseSemigroup and
IsPartialPermSemigroup);

DeclareOperation("MajorantClosure", [IsInverseSemigroup and
IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("MajorantClosureNC", [IsInverseSemigroup and
IsPartialPermSemigroup, IsPartialPermCollection]);
DeclareOperation("Minorants", [IsInverseSemigroup and IsPartialPermSemigroup,
IsPartialPerm]);

DeclareOperation("RightCosetsOfInverseSemigroup", [IsInverseSemigroup and
IsPartialPermSemigroup, IsInverseSemigroup and IsPartialPermSemigroup]);

DeclareAttribute("SameMinorantsSubgroup", IsGroupHClass and IsPartialPermCollection);
DeclareAttribute("SmallerDegreePartialPermRepresentation", IsInverseSemigroup and
IsPartialPermSemigroup);
DeclareGlobalFunction("SupremumIdempotentsNC");

DeclareAttribute("VagnerPrestonRepresentation", IsInverseSemigroup and IsPartialPermSemigroup);
