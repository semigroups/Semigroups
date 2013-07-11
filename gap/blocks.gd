
DeclareAttribute("NrBlocks", IsBipartition);
DeclareAttribute("NrLeftBlocks", IsBipartition);
DeclareAttribute("NrRightBlocks", IsBipartition);

DeclareGlobalFunction("RankOfBlocks");
DeclareGlobalFunction("DegreeOfBlocks");

DeclareAttribute("LeftBlocks", IsBipartition);
DeclareAttribute("RightBlocks", IsBipartition);

DeclareGlobalFunction("ExtRepOfBlocks");
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

