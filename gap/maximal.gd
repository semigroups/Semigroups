#############################################################################
##
#W  maximal.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("MaximalSubsemigroups", IsSemigroup);
DeclareOperation("MaximalSubsemigroups", [IsSemigroup, IsGroup]);
DeclareOperation("MaximalSubsemigroupsNC", [IsReesMatrixSubsemigroup, IsGroup, IsList, IsAssociativeElement]);
DeclareOperation("MaximalSubsemigroupsNC", [IsReesZeroMatrixSubsemigroup, IsGroup, IsRecord, IsList, IsList, IsList]);
DeclareOperation("IsMaximalSubsemigroup", [IsSemigroup, IsSemigroup]);
