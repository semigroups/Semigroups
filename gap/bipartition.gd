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

DeclareSynonym("IsBipartitionSemigroup", IsSemigroup and
IsBipartitionCollection);

DeclareAttribute("NrBlocks", IsBipartition);
DeclareAttribute("NrKernelClasses", IsBipartition);
DeclareAttribute("DegreeOfBipartition", IsBipartition);
DeclareAttribute("RankOfBipartition", IsBipartition);
DeclareAttribute("ExtRepBipartition", IsBipartition);

#old

#DeclareOperation("AsBipartition", [IsPerm, IsPosInt]);
#DeclareAttribute("DegreeOfBipartitionSemigroup", IsBipartitionSemigroup);
#DeclareAttribute("DegreeOfBipartitionCollection", IsBipartitionCollection);
#DeclareAttribute("RightSignedPartition", IsBipartition);
#DeclareAttribute("LeftSignedPartition", IsBipartition);
#DeclareProperty("IsBipartitionSemigroupGreensClass", IsGreensClass);
#DeclareOperation("OnRightSignedPartition", [IsList, IsBipartition]);
#DeclareOperation("OnLeftSignedPartition", [IsList, IsBipartition]);
#DeclareOperation("RandomBipartition", [IsPosInt]);
#DeclareOperation("RankOfBipartition", [IsBipartition]) ;
#
#DeclareGlobalFunction("INV_SIGNED_PART_BIPART");
