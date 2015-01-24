#############################################################################
##
#W  display.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("DotDClasses", IsActingSemigroup, "mutable");
DeclareAttribute("DotDClasses", IsReesZeroMatrixSemigroup, "mutable");
DeclareAttribute("DotSemilatticeOfIdempotents", IsInverseSemigroup, "mutable");
DeclareOperation("DotDClasses", [IsActingSemigroup, IsRecord]);
DeclareOperation("DotDClasses", [IsReesZeroMatrixSemigroup, IsRecord]);

DeclareGlobalFunction("TikzBipartition");
DeclareGlobalFunction("TikzBlocks");
DeclareGlobalFunction("TikzStringForBlocks");
DeclareGlobalFunction("TikzStringForBipartition");
