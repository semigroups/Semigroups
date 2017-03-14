############################################################################
##
#W  factor.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("MinimalFactorization",
                 [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("Factorization", [IsLambdaOrb, IsPosInt, IsPerm]);
DeclareOperation("Factorization", [IsSemigroup, IsMultiplicativeElement]);
DeclareOperation("TraceSchreierTreeForward", [IsSemigroupData, IsPosInt]);
