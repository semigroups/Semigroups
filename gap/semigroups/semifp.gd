#############################################################################
##
##  semigroups/semifp.gd
##  Copyright (C) 2020-2022                                   Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareOperation("ElementOfFpSemigroup", [IsFpSemigroup, IsAssocWord]);
DeclareOperation("ElementOfFpMonoid", [IsFpMonoid, IsAssocWord]);
DeclareOperation("ParseRelations", [IsDenseList, IsString]);
DeclareAttribute("UnderlyingCongruence", IsFpSemigroup);
DeclareAttribute("UnderlyingCongruence", IsFpMonoid);
DeclareAttribute("Length", IsFpSemigroup);
DeclareAttribute("Length", IsFpMonoid);

DeclareSynonym("IsSubsemigroupOfFpMonoid",
               IsSemigroup and IsElementOfFpMonoidCollection);

DeclareAttribute("EmbeddingFpMonoid", IsFpSemigroup);

DeclareOperation("ReversedOp", [IsElementOfFpSemigroup]);
DeclareOperation("ReversedOp", [IsElementOfFpMonoid]);

DeclareGlobalFunction("FreeMonoidAndAssignGeneratorVars");
DeclareGlobalFunction("FreeSemigroupAndAssignGeneratorVars");
