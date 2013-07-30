############################################################################
##
#W  blocks.gd
#Y  Copyright (C) 2011-13                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsBlocks", IsList);
DeclareCategoryCollections("IsBlocks");

DeclareGlobalFunction("ORB_HashFunctionForBlocks");

DeclareAttribute("NrBlocks", IsBipartition);
DeclareAttribute("NrBlocks", IsBlocks);
DeclareAttribute("NrLeftBlocks", IsBipartition);
DeclareAttribute("NrRightBlocks", IsBipartition);

DeclareAttribute("RankOfBlocks", IsBlocks);
DeclareAttribute("DegreeOfBlocks", IsBlocks);

DeclareAttribute("LeftBlocks", IsBipartition);
DeclareAttribute("RightBlocks", IsBipartition);

DeclareAttribute("ExtRepOfBlocks", IsBlocks);
DeclareGlobalFunction("BlocksByExtRep");

DeclareGlobalFunction("OnRightBlocks");
DeclareGlobalFunction("OnLeftBlocks");

DeclareOperation("PermRightBlocks", [IsList, IsBipartition]);

DeclareGlobalFunction("InverseRightBlocks");
DeclareGlobalFunction("InverseLeftBlocks");
DeclareGlobalFunction("BlocksIdempotentTester");
DeclareGlobalFunction("BlocksIdempotentCreator");

DeclareGlobalFunction("FuseRightBlocks");
DeclareGlobalFunction("FuseLeftBlocks");

