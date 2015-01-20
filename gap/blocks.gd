############################################################################
##
#W  blocks.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsBlocks", IsList);
DeclareCategoryCollections("IsBlocks");

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

DeclareGlobalFunction("OnRightBlocks");
DeclareGlobalFunction("OnLeftBlocks");

DeclareOperation("PermRightBlocks", [IsList, IsBipartition]);
DeclareOperation("PermLeftBlocks", [IsList, IsBipartition]);

DeclareGlobalFunction("InverseRightBlocks");
DeclareGlobalFunction("InverseLeftBlocks");
DeclareGlobalFunction("BlocksIdempotentTester");
DeclareGlobalFunction("BlocksIdempotentCreator");
