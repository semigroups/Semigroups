############################################################################
##
#W  partbinrel.gd
#Y  Copyright (C) 2015                                   Attila Egri-Nagy
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# This file contains a declarations for partitioned binary relations (PBRs) as
# defined in:
#
# MARTIN, Paul; MAZORCHUK, Volodymyr.
# Partitioned Binary Relations. MATHEMATICA SCANDINAVICA, v113, n1, p. 30-52,
# http://arxiv.org/abs/1102.0862

DeclareCategory("IsPBR",
                IsMultiplicativeElementWithInverse and
                IsAssociativeElementWithStar);

DeclareCategoryCollections("IsPBR");
DeclareCategoryCollections("IsPBRCollection");

BindGlobal("PBRFamily",
           NewFamily("PBRFamily",
                     IsPBR,
                     CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("PBRType",
           NewType(PBRFamily,
                   IsPBR and IsPositionalObjectRep));

DeclareAttribute("DegreeOfPBR",
                 IsPBR);
DeclareGlobalFunction("PBR");
DeclareGlobalFunction("ExtRepOfPBR");
DeclareOperation("RandomPBR", [IsPosInt]);
DeclareOperation("AsPBR", [IsAssociativeElement, IsPosInt]);
DeclareOperation("AsPBR", [IsAssociativeElement]);
DeclareOperation("NumberPBR", [IsPBR]);
DeclareOperation("PBRNumber", [IsPosInt, IsPosInt]);

DeclareProperty("IsEmptyPBR", IsPBR);
DeclareProperty("IsUniversalPBR", IsPBR);
DeclareProperty("IsBipartitionPBR", IsPBR);
DeclareProperty("IsTransformationPBR", IsPBR);
DeclareProperty("IsPartialPermPBR", IsPBR);
DeclareProperty("IsBlockBijectionPBR", IsPBR);
DeclareProperty("IsDualTransformationPBR", IsPBR);
