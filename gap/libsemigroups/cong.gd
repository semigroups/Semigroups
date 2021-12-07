###########################################################################
##
##  cong.gd
##  Copyright (C) 2021                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#TODO(now) move everything from here

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
DeclareAttribute("Kind", IsAnyCongruenceCategory);
DeclareAttribute("GeneratingPairs", IsAnyCongruenceCategory);
DeclareCategory("IsAnyCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep);
DeclareAttribute("EquivalenceRelationPartitionWithSingletons",
                 IsAnyCongruenceCategory);
# to here into cong.gd

DeclareCategory("CanComputeCppCongruence", IsAnyCongruenceCategory);

DeclareGlobalFunction("CppCongruence");

DeclareOperation("CongruenceWordToClassIndex",
                 [CanComputeCppCongruence, IsHomogeneousList]);
DeclareOperation("CongruenceWordToClassIndex",
                 [CanComputeCppCongruence, IsMultiplicativeElement]);

DeclareOperation("CongruenceLessNC",
                 [CanComputeCppCongruence,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);
