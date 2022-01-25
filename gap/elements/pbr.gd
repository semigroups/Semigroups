############################################################################
##
##  elements/pbr.gd
##  Copyright (C) 2015-2022                              Attila Egri-Nagy
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# This file contains a declarations for partitioned binary relations (PBRs) as
# defined in:
#
# MARTIN, Paul; MAZORCHUK, Volodymyr.
# Partitioned Binary Relations. MATHEMATICA SCANDINAVICA, v113, n1, p. 30-52,
# https://arxiv.org/abs/1102.0862

DeclareCategory("IsPBR",
                IsMultiplicativeElementWithInverse and
                IsAssociativeElementWithStar);

DeclareCategoryCollections("IsPBR");
DeclareCategoryCollections("IsPBRCollection");

InstallTrueMethod(IsGeneratorsOfSemigroup, IsPBRCollection);

DeclareOperation("PBR", [IsDenseList, IsDenseList]);
DeclareGlobalFunction("PBRNC");
DeclareOperation("RandomPBR", [IsPosInt]);
DeclareOperation("RandomPBR", [IsPosInt, IsFloat]);
DeclareOperation("EmptyPBR", [IsPosInt]);
DeclareOperation("IdentityPBR", [IsPosInt]);
DeclareOperation("UniversalPBR", [IsPosInt]);

DeclareAttribute("DegreeOfPBR", IsPBR);
DeclareAttribute("DegreeOfPBRCollection", IsPBRCollection);

DeclareOperation("NumberPBR", [IsPBR]);
DeclareOperation("PBRNumber", [IsPosInt, IsPosInt]);

DeclareOperation("AsPBR", [IsMultiplicativeElement, IsPosInt]);
DeclareOperation("AsPBR", [IsMultiplicativeElement]);

DeclareProperty("IsEmptyPBR", IsPBR);
DeclareProperty("IsIdentityPBR", IsPBR);
DeclareProperty("IsUniversalPBR", IsPBR);
DeclareProperty("IsBipartitionPBR", IsPBR);
DeclareProperty("IsTransformationPBR", IsPBR);
DeclareProperty("IsPartialPermPBR", IsPBR);
DeclareProperty("IsPermPBR", IsPBR);
DeclareProperty("IsBlockBijectionPBR", IsPBR);
DeclareProperty("IsDualTransformationPBR", IsPBR);
