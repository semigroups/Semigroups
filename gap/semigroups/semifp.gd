#############################################################################
##
##  semifp.gd
##  Copyright (C) 2020-2021                                   Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("ElementOfFpSemigroup", [IsFpSemigroup, IsAssocWord]);
DeclareOperation("ElementOfFpMonoid", [IsFpMonoid, IsAssocWord]);
DeclareOperation("ParseRelations", [IsDenseList, IsString]);
DeclareAttribute("UnderlyingCongruence", IsFpSemigroup);
DeclareAttribute("UnderlyingCongruence", IsFpMonoid);
