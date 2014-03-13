############################################################################
##
#W  normalizer.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


DeclareOperation("Normalizer", [IsPermGroup, IsSemigroup, IsRecord]);
DeclareOperation("Normalizer", [IsSemigroup, IsRecord]);
DeclareOperation("Normalizer", [IsSemigroup]);

DeclareGlobalFunction("DeterministicSemigroupNormalizer");
DeclareGlobalFunction("NonDeterministicSemigroupNormalizer");

