############################################################################
##
##  congsemigraph.gd
##  Copyright (C) 2021                       Marina Anagnostopoulou-Merkouri
##
##                                                            James Mitchell
##
##  Licensing information can be found in the README file of this package.
##
############################################################################

DeclareCategory("IsCongruenceByWangPair",
                IsSemigroupCongruence and IsAttributeStoringRep and IsFinite);

DeclareOperation("CongruenceByWangPair",
[IsGraphInverseSemigroup, IsHomogeneousList, IsHomogeneousList]);

DeclareOperation("AsCongruenceByWangPair", [IsSemigroupCongruence]);

DeclareOperation("MinimalHereditarySubsetsVertex",
                 [IsGraphInverseSemigroup, IsPosInt]);

DeclareOperation("GeneratingCongruencesOfLattice",
                 [IsGraphInverseSemigroup]);
