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

DeclareCategory("IsPartitionedBinaryRelation", 
                IsMultiplicativeElementWithInverse and
                IsAssociativeElementWithStar);
                
DeclareCategoryCollections("IsPartitionedBinaryRelation");
DeclareCategoryCollections("IsPartitionedBinaryRelationCollection");

BindGlobal("PartitionedBinaryRelationFamily",
           NewFamily("PartitionedBinaryRelationFamily",
                     IsPartitionedBinaryRelation,
                     CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("PartitionedBinaryRelationType",
           NewType(PartitionedBinaryRelationFamily,
                   IsPartitionedBinaryRelation and IsPositionalObjectRep));

DeclareAttribute("DegreeOfPartitionedBinaryRelation",
                 IsPartitionedBinaryRelation);
DeclareSynonymAttr("DegreeOfPBR", DegreeOfPartitionedBinaryRelation);
DeclareGlobalFunction("PartitionedBinaryRelation");
DeclareGlobalFunction("ExtRepOfPBR");
DeclareOperation("RandomPartitionedBinaryRelation", [IsPosInt]);
DeclareOperation("AsPartitionedBinaryRelation", [IsAssociativeElement, IsPosInt]);
DeclareOperation("AsPartitionedBinaryRelation", [IsAssociativeElement]);
DeclareOperation("NumberPBR", [IsPartitionedBinaryRelation]);
DeclareOperation("PBRNumber", [IsPosInt, IsPosInt]);

DeclareProperty("IsEmptyPBR", IsPartitionedBinaryRelation);
DeclareProperty("IsUniversalPBR", IsPartitionedBinaryRelation);
DeclareProperty("IsBipartitionPBR", IsPartitionedBinaryRelation);
DeclareProperty("IsTransformationPBR", IsPartitionedBinaryRelation);
DeclareProperty("IsPartialPermPBR", IsPartitionedBinaryRelation);
DeclareProperty("IsDualTransformationPBR", IsPartitionedBinaryRelation);

DeclareSynonym("PBR", PartitionedBinaryRelation);
DeclareSynonym("AsPBR", AsPartitionedBinaryRelation);
DeclareSynonym("RandomPBR", RandomPartitionedBinaryRelation);
