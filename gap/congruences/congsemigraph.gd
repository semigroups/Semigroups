############################################################################
##
##  congsemigraph.gd
##  Copyright (C) 2022                     Marina Anagnostopoulou-Merkouri
##                                                          James Mitchell
##
##  Licensing information can be found in the README file of this package.
##
############################################################################

DeclareCategory("IsCongruenceByWangPair",
                IsSemigroupCongruence
                and CanComputeEquivalenceRelationPartition
                and IsMagmaCongruence
                and IsAttributeStoringRep
                and IsFinite);

DeclareOperation("CongruenceByWangPair",
                 [IsGraphInverseSemigroup,
                  IsHomogeneousList,
                  IsHomogeneousList]);

DeclareOperation("AsCongruenceByWangPair", [IsSemigroupCongruence]);

DeclareOperation("MinimalHereditarySubsetsVertex",
                 [IsDigraph, IsPosInt]);

DeclareAttribute("GeneratingCongruencesOfLattice", IsGraphInverseSemigroup);

DeclareCategory("IsTraceOfCongruenceByWangPair",
    IsSemigroupCongruence
    and CanComputeEquivalenceRelationPartition
    and IsMagmaCongruence
    and IsAttributeStoringRep
    and IsFinite);

DeclareAttribute("TraceOfCongruenceByWangPair", IsCongruenceByWangPair);

DeclareAttribute("EdgesWithRange", IsGraphInverseSemigroupElement);
DeclareAttribute("EdgesWithSource", IsGraphInverseSemigroupElement);
DeclareOperation("PathsWithRange", [IsGraphInverseSemigroupElement]);
DeclareOperation("PathsWithSource", [IsGraphInverseSemigroupElement]);
