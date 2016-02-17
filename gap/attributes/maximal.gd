#############################################################################
##
#W  maximal.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("MaximalSubsemigroups", IsSemigroup);
DeclareOperation("MaximalSubsemigroups", [IsSemigroup, IsGroup]);
# FIXME What does this do?
DeclareOperation("MaximalSubsemigroupsNC", [IsReesMatrixSubsemigroup, IsGroup,
                 IsList, IsAssociativeElement]);
# FIXME What does this do?
DeclareOperation("MaximalSubsemigroupsNC", [IsReesZeroMatrixSubsemigroup,
                 IsGroup, IsDigraph, IsList, IsList, IsList]);
DeclareOperation("IsMaximalSubsemigroup", [IsSemigroup, IsSemigroup]);
