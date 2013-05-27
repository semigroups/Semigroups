#############################################################################
###
##W  slp.gd
##Y  Copyright (C) 2013                                   James D. Mitchell
###
###  Licensing information can be found in the README file of this package.
###
##############################################################################
###

DeclareGlobalFunction("LambdaOrbWords");
DeclareGlobalFunction("LambdaOrbSchutzSchreier");
DeclareOperation("Factorization", [IsLambdaOrb, IsPosInt, IsPerm]);
DeclareOperation("Factorization", [IsActingSemigroup, IsAssociativeElement]);

DeclareGlobalFunction("LambdaOrbSLP");
DeclareOperation("SemigroupElementSLP", [IsActingSemigroup,
IsAssociativeElement]);

DeclareOperation("ShorterSLPStabChain", [IsPermGroup]);
DeclareOperation("SiftShorterSLP", [IsPermGroup, IsPerm]);

