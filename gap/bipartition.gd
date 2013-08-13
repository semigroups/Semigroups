############################################################################
##
#W  bipartition.gd
#Y  Copyright (C) 2011-13                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsBipartition", IsMultiplicativeElementWithOne and
 IsAssociativeElementWithAction);
DeclareCategoryCollections("IsBipartition");

DeclareGlobalFunction("BipartitionNC");
DeclareGlobalFunction("Bipartition");

DeclareAttribute("DegreeOfBipartition", IsBipartition);
DeclareAttribute("RankOfBipartition", IsBipartition);
DeclareAttribute("NrTransverseBlocks", IsBipartition);

DeclareAttribute("ExtRepOfBipartition", IsBipartition);

DeclareSynonymAttr("LeftProjection", LeftOne);
DeclareSynonymAttr("RightProjection", RightOne);
DeclareOperation("InverseOp", [IsBipartition]);
DeclareOperation("RandomBipartition", [IsPosInt]);

DeclareOperation("IdentityBipartition", [IsPosInt]);
DeclareOperation("BipartitionByIntRepNC", [IsList]);
DeclareOperation("BipartitionByIntRep", [IsList]);

DeclareOperation("AsBipartition", [IsPerm, IsPosInt]);
DeclareOperation("AsBipartition", [IsPerm]);
DeclareOperation("AsBipartition", [IsTransformation, IsPosInt]);
DeclareOperation("AsBipartition", [IsTransformation]);
DeclareOperation("AsBipartition", [IsPartialPerm, IsPosInt]);
DeclareOperation("AsBipartition", [IsPartialPerm]);
DeclareOperation("AsBipartition", [IsBipartition, IsPosInt]);
DeclareOperation("AsBipartition", [IsBipartition]);

DeclareAttribute("AsTransformationNC", IsBipartition);
DeclareAttribute("AsPermutationNC", IsBipartition);

DeclareProperty("IsTransBipartition", IsBipartition);
DeclareProperty("IsPermBipartition", IsBipartition);
DeclareProperty("IsPartialPermBipartition", IsBipartition);
DeclareGlobalFunction("PermLeftQuoBipartitionNC");
DeclareOperation("PermLeftQuoBipartition", [IsBipartition, IsBipartition]);
DeclareGlobalFunction("OnRightBlocksBipartitionByPerm");

#collections
DeclareAttribute("DegreeOfBipartitionCollection", IsBipartitionCollection);

#internal...
DeclareGlobalFunction("TransverseBlocksLookup");
DeclareGlobalFunction("BipartRightBlocksConj");

