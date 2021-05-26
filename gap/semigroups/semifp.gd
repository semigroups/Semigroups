#############################################################################
##
##  semifp.gd
##  Copyright (C) 2020                                  Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
DeclareOperation("ParseRelations", [IsDenseList, IsString]);

DeclareOperation("ElementOfFpSemigroup", [IsFpSemigroup, IsAssocWord]);
DeclareOperation("ElementOfFpMonoid", [IsFpMonoid, IsAssocWord]);
