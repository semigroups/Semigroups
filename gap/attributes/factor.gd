############################################################################
##
##  attributes/factor.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("MinimalFactorization",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("NonTrivialFactorization",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("Factorization", [IsLambdaOrb, IsPosInt, IsPerm]);
DeclareOperation("Factorization", [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("TraceSchreierTreeForward", [IsSemigroupData, IsPosInt]);
