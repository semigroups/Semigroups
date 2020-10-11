############################################################################
##
##  elements/bipart.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
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

DeclareOperation("AsBipartition", [IsAssociativeElement, IsPosInt]);
DeclareOperation("AsBipartition", [IsAssociativeElement, IsZeroCyc]);
DeclareOperation("AsBipartition", [IsAssociativeElement]);

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

# Collections
DeclareAttribute("DegreeOfBipartitionCollection", IsBipartitionCollection);
DeclareOperation("OneMutable", [IsBipartitionCollection]);
