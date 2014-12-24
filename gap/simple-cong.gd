############################################################################
##
#W  simple-cong.gd
#Y  Copyright (C) 2014                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("SEMIGROUPS_CONG_SIMPLE",
        IsSemigroupCongruence and IsAttributeStoringRep);

DeclareGlobalFunction("SEMIGROUPS_SIMPLECONG_FROM_RMSCONG");
DeclareGlobalFunction("SEMIGROUPS_SIMPLECONG_FROM_PAIRS");

DeclareCategory("SEMIGROUPS_CONGCLASS_SIMPLE",
        IsCongruenceClass and IsAttributeStoringRep and IsAssociativeElement);

DeclareGlobalFunction("SEMIGROUPS_SIMPLECLASS_FROM_RMSCLASS");

DeclareGlobalFunction("SemigroupCongruence");
DeclareAttribute("CongruencesOfSemigroup", IsSemigroup);