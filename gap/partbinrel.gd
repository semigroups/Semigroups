############################################################################
##
#W  partbinrel.gd
#Y  Copyright (C) 2015                                   Attila Egri-Nagy
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## Partitioned Binary Relations
## Binary relations on 2n points with a different multiplication
## as described in: MARTIN, Paul; MAZORCHUK, Volodymyr.
## Partitioned Binary Relations. MATHEMATICA SCANDINAVICA,v113, n1, p. 30-52, 
## http://arxiv.org/abs/1102.0862
## 
## This is the most general diagram semigroup containing all other types,
## therefore it can be used for testing the specialized implementations,
## thus the efficiency is not the first priority here.

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
DeclareGlobalFunction("PartitionedBinaryRelation");
DeclareGlobalFunction("ExtRepOfPBR");
DeclareOperation("RandomPartitionedBinaryRelation", [IsPosInt]);
DeclareOperation("AsPartitionedBinaryRelation", [IsAssociativeElement]);

DeclareSynonym("PBR", PartitionedBinaryRelation);
DeclareSynonym("AsPBR", AsPartitionedBinaryRelation);
DeclareSynonym("RandomPBR", RandomPartitionedBinaryRelation);
