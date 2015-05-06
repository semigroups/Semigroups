#############################################################################
##
#W  display.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("DotDClasses", IsSemigroup, "mutable");
DeclareAttribute("DotSemilatticeOfIdempotents", IsInverseSemigroup, "mutable");
DeclareOperation("DotDClasses", [IsSemigroup, IsRecord]);

DeclareGlobalFunction("TikzBipartition");
DeclareGlobalFunction("TikzBlocks");
DeclareGlobalFunction("TikzStringForBipartition");
DeclareGlobalFunction("TikzStringForBlocks");
