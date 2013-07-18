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

DeclareSynonym("IsBipartitionSemigroup", IsSemigroup and
IsBipartitionCollection);

DeclareAttribute("DegreeOfBipartition", IsBipartition);
DeclareAttribute("RankOfBipartition", IsBipartition);
DeclareAttribute("ExtRepBipartition", IsBipartition);

DeclareAttribute("LeftProjection", IsBipartition);
DeclareAttribute("RightProjection", IsBipartition);
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

DeclareProperty("IsTransBipartition", IsBipartition);
DeclareProperty("IsPermBipartition", IsBipartition);
DeclareProperty("IsPartialPermBipartition", IsBipartition);
DeclareGlobalFunction("PermLeftQuoBipartitionNC");
DeclareOperation("PermLeftQuoBipartition", [IsBipartition, IsBipartition]);

#internal...
DeclareGlobalFunction("OnRightBlocksPerm");
DeclareGlobalFunction("TransverseBlocksLookup");
DeclareGlobalFunction("BipartRightBlocksConj");

DeclareAttribute("DegreeOfBipartitionCollection", IsBipartitionCollection);
