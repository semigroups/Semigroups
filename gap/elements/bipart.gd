############################################################################
##
#W  bipart.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategoryKernel("IsBipartition",
                      IsMultiplicativeElementWithInverse
                      and IsAssociativeElementWithStar,
                      IS_BIPART);

DeclareCategoryCollections("IsBipartition");
DeclareCategoryCollections("IsBipartitionCollection");

DeclareGlobalFunction("Bipartition");

DeclareAttribute("DegreeOfBipartition", IsBipartition);
DeclareAttribute("RankOfBipartition", IsBipartition);
DeclareAttribute("NrTransverseBlocks", IsBipartition);
DeclareAttribute("NrLeftBlocks", IsBipartition);
DeclareAttribute("NrRightBlocks", IsBipartition);
DeclareAttribute("NrBlocks", IsBipartition);

DeclareAttribute("LeftBlocks", IsBipartition);
DeclareAttribute("RightBlocks", IsBipartition);

DeclareAttribute("DomainOfBipartition", IsBipartition);
DeclareAttribute("CodomainOfBipartition", IsBipartition);

DeclareAttribute("IntRepOfBipartition", IsBipartition);
DeclareSynonymAttr("LeftProjection", LeftOne);
DeclareSynonymAttr("RightProjection", RightOne);
DeclareOperation("RandomBipartition", [IsPosInt]);
DeclareOperation("RandomBlockBijection", [IsPosInt]);
DeclareOperation("RandomBipartition", [IsRandomSource, IsPosInt]);
DeclareOperation("RandomBlockBijection", [IsRandomSource, IsPosInt]);

DeclareOperation("NaturalLeqBlockBijection", [IsBipartition, IsBipartition]);
DeclareOperation("NaturalLeqPartialPermBipartition",
                 [IsBipartition, IsBipartition]);
DeclareOperation("PartialPermLeqBipartition", [IsBipartition, IsBipartition]);

DeclareOperation("IdentityBipartition", [IsPosInt]);
DeclareOperation("IdentityBipartition", [IsZeroCyc]);
DeclareOperation("BipartitionByIntRep", [IsList]);

DeclareOperation("AsBipartition", [IsPerm, IsInt]);
DeclareOperation("AsBipartition", [IsPerm]);
DeclareOperation("AsBipartition", [IsTransformation, IsPosInt]);
DeclareOperation("AsBipartition", [IsTransformation, IsZeroCyc]);
DeclareOperation("AsBipartition", [IsTransformation]);
DeclareOperation("AsBipartition", [IsPartialPerm, IsPosInt]);
DeclareOperation("AsBipartition", [IsPartialPerm, IsZeroCyc]);
DeclareOperation("AsBipartition", [IsPartialPerm]);
DeclareOperation("AsBipartition", [IsBipartition, IsPosInt]);
DeclareOperation("AsBipartition", [IsBipartition, IsZeroCyc]);
DeclareOperation("AsBipartition", [IsBipartition]);
DeclareOperation("AsBipartition", [IsPBR, IsPosInt]);
DeclareOperation("AsBipartition", [IsPBR, IsZeroCyc]);
DeclareOperation("AsBipartition", [IsPBR]);

DeclareOperation("AsBlockBijection", [IsPartialPerm, IsPosInt]);
DeclareOperation("AsBlockBijection", [IsPartialPerm]);
DeclareOperation("AsBlockBijection", [IsBipartition, IsPosInt]);
DeclareOperation("AsBlockBijection", [IsBipartition]);

DeclareProperty("IsBlockBijection", IsBipartition);
DeclareProperty("IsUniformBlockBijection", IsBipartition);
DeclareProperty("IsTransBipartition", IsBipartition);
DeclareProperty("IsDualTransBipartition", IsBipartition);
DeclareProperty("IsPermBipartition", IsBipartition);
DeclareProperty("IsPartialPermBipartition", IsBipartition);

DeclareOperation("PermLeftQuoBipartition", [IsBipartition, IsBipartition]);

#collections
DeclareAttribute("DegreeOfBipartitionCollection", IsBipartitionCollection);
DeclareOperation("OneMutable", [IsBipartitionCollection]);
