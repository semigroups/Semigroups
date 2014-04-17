############################################################################
##
#W  blocks.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsBlocks", IsList);
DeclareCategoryCollections("IsBlocks");

DeclareGlobalFunction("ORB_HashFunctionForBlocks");
DeclareGlobalFunction("BlocksNC");
DeclareGlobalFunction("BlocksByIntRepNC");
DeclareOperation("ProjectionFromBlocks", [IsBlocks]);

DeclareAttribute("NrBlocks", IsBipartition);
DeclareAttribute("NrBlocks", IsBlocks);
DeclareAttribute("NrLeftBlocks", IsBipartition);
DeclareAttribute("NrRightBlocks", IsBipartition);

DeclareAttribute("RankOfBlocks", IsBlocks);
DeclareAttribute("NrTransverseBlocks", IsBlocks);

DeclareAttribute("DegreeOfBlocks", IsBlocks);

DeclareAttribute("LeftBlocks", IsBipartition);
DeclareAttribute("RightBlocks", IsBipartition);

DeclareAttribute("ExtRepOfBlocks", IsBlocks);
DeclareGlobalFunction("BlocksByExtRep");

DeclareOperation("JoinOfBlocks", [IsBlocks, IsBlocks]);

DeclareGlobalFunction("OnRightBlocks");
DeclareGlobalFunction("OnLeftBlocks");

DeclareOperation("PermRightBlocks", [IsList, IsBipartition]);
DeclareOperation("PermLeftBlocks", [IsList, IsBipartition]);

DeclareGlobalFunction("InverseRightBlocks");
DeclareGlobalFunction("InverseLeftBlocks");
DeclareGlobalFunction("BlocksIdempotentTester");
DeclareGlobalFunction("BlocksIdempotentCreator");

DeclareGlobalFunction("FuseRightBlocks");
DeclareGlobalFunction("FuseLeftBlocks");

