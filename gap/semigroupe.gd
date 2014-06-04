###########################################################################
##
#W  semigroupe.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("SEEData", IsFinite and IsSemigroup, "mutable");
DeclareCategory("IsSEEData", IsList);
DeclareFilter("IsClosedSEEData", IsSEEData);

DeclareOperation("Enumerate", [IsSEEData]);
DeclareOperation("Enumerate", [IsSEEData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsSEEData, IsCyclotomic, IsFunction]);

