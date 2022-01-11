############################################################################
##
##  congsemigraph.gd
##  Copyright (C) 2021                       Marina Anagnostopoulou-Merkouri
<<<<<<< HEAD
##                                                            James Mitchell
=======
##                                                            James Mitchell 
>>>>>>> eea201edb1e5d61f29b143e3bd8991f9c2db996d
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
