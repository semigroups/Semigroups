#############################################################################
##
#W  pictures.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareGlobalFunction("TikzEnd");
DeclareGlobalFunction("TikzStringForBlocks");
DeclareGlobalFunction("TikzBipartition");
DeclareGlobalFunction("TikzBipartitionRight");
DeclareGlobalFunction("TikzBipartitionLeft");
DeclareGlobalFunction("TikzBipartitionLeftRight");
DeclareGlobalFunction("TikzStringForBipartition");
DeclareGlobalFunction("TikzRightBlocks");
DeclareGlobalFunction("TikzLeftBlocks");
DeclareGlobalFunction("TikzBlocks");

DeclareOperation("DotDClasses", [IsActingSemigroup]);
# this should be an attribute JDM!
DeclareOperation("DotDClasses", [IsActingSemigroup, IsRecord]);


