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
DeclareAttribute("Kind", CanComputeCppCongruence);
DeclareAttribute("GeneratingPairs", IsAnyCongruenceCategory);
# TODO rename IsAnyCongruenceClass
DeclareCategory("IsCongruenceClassOfCanComputeCppCongruence",
                IsEquivalenceClass and IsAttributeStoringRep);
DeclareAttribute("EquivalenceRelationPartitionIncludingSingletons",
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
