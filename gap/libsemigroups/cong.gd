###########################################################################
##
##  cong.gd
##  Copyright (C) 2021                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsAnyCongruenceCategory", IsEquivalenceRelation);
DeclareCategory("IsCongruenceCategory",
                IsAnyCongruenceCategory and IsSemigroupCongruence and
                IsMagmaCongruence);
DeclareCategory("IsLeftCongruenceCategory",
                IsAnyCongruenceCategory and IsLeftSemigroupCongruence and
                IsLeftMagmaCongruence);
DeclareCategory("IsRightCongruenceCategory",
                IsAnyCongruenceCategory and IsRightSemigroupCongruence and
                IsRightMagmaCongruence);

DeclareCategory("CanComputeCppCongruence", IsAnyCongruenceCategory);

DeclareAttribute("Kind", CanComputeCppCongruence);
DeclareAttribute("GeneratingPairs", IsAnyCongruenceCategory);

DeclareGlobalFunction("CppCongruence");

DeclareOperation("CongruenceWordToClassIndex",
                 [CanComputeCppCongruence, IsHomogeneousList]);
DeclareOperation("CongruenceWordToClassIndex",
                 [CanComputeCppCongruence, IsMultiplicativeElement]);

DeclareCategory("IsCongruenceClassOfCanComputeCppCongruence",
                IsEquivalenceClass and IsAttributeStoringRep);

DeclareAttribute("EquivalenceRelationPartitionIncludingSingletons",
                 IsAnyCongruenceCategory);

DeclareOperation("CongruenceLessNC",
                 [CanComputeCppCongruence,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);
