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
## thus the efficiency is not the first prioroty here.

DeclareCategory("IsPartitionedBinaryRelation", IsMultiplicativeElementWithOne
        and IsAssociativeElement and IsAttributeStoringRep
        and IsMultiplicativeElementWithInverse);

DeclareCategoryCollections("IsPartitionedBinaryRelation");

DeclareRepresentation( "IsPartitionedBinaryRelationRep",
                       IsComponentObjectRep,
        [ "a11", # Dom to Dom
          "a12", # Dom to Codom
          "a21", # Codom to Dom
          "a22"]); # Codom to Codom (see Sect. 5.4 in arxiv.org/abs/1102.0862)

BindGlobal("PartitionedBinaryRelationFamily",
        NewFamily("PartitionedBinaryRelationFamily",
                IsPartitionedBinaryRelation and IsPartitionedBinaryRelationRep,
                CanEasilySortElements,
                CanEasilySortElements));

BindGlobal("PartitionedBinaryRelationType",
        NewType(PartitionedBinaryRelationFamily,IsPartitionedBinaryRelation));

DeclareOperation("PartitionedBinaryRelation",[IsBinaryRelation]);
DeclareGlobalFunction("DegreeOfPartitionedBinaryRelation");
DeclareGlobalFunction("CombinePartitionedBinaryRelations");
