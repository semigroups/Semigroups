############################################################################
##
#W  simple-cong.gd
#Y  Copyright (C) 2014                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("SEMIGROUPS_CongSimple",
        IsSemigroupCongruence and IsAttributeStoringRep);

DeclareGlobalFunction("SEMIGROUPS_SimpleCongFromRMSCong");
DeclareGlobalFunction("SEMIGROUPS_SimpleCongFromPairs");

DeclareCategory("SEMIGROUPS_CongClassSimple",
        IsCongruenceClass and IsAttributeStoringRep and IsAssociativeElement);

DeclareGlobalFunction("SEMIGROUPS_SimpleClassFromRMSclass");

DeclareGlobalFunction("SemigroupCongruence");
DeclareAttribute("CongruencesOfSemigroup", IsSemigroup);
