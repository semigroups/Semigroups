#############################################################################
##
#W  display.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("DotDClasses", IsActingSemigroup, "mutable");
DeclareAttribute("DotDClasses", IsReesZeroMatrixSemigroup, "mutable");
DeclareAttribute("DotSemilatticeOfIdempotents", IsInverseSemigroup, "mutable");
DeclareOperation("DotDClasses", [IsActingSemigroup, IsRecord]);

DeclareGlobalFunction("TikzBipartition");
DeclareGlobalFunction("TikzBlocks");

DeclareGlobalFunction("TikzStringForBlocks");
DeclareGlobalFunction("TikzStringForBipartition");
DeclareGlobalFunction("TikzBipartitionRight");
DeclareGlobalFunction("TikzBipartitionLeft");
DeclareGlobalFunction("TikzBipartitionLeftRight");
DeclareGlobalFunction("TikzRightBlocks");
DeclareGlobalFunction("TikzLeftBlocks");
