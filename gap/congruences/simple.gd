############################################################################
##
#W  cong-simple.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on finite (0-)simple semigroups,
## using isomorphisms to Rees (0-)matrix semigroups and methods in
## cong-reesmat.gd/gi.  These functions are not intended for direct use by an
## end-user.
##

DeclareCategory("SEMIGROUPS_CongSimple",
                IsSemigroupCongruence and IsAttributeStoringRep and IsFinite);

DeclareGlobalFunction("SEMIGROUPS_SimpleCongFromRMSCong");
DeclareGlobalFunction("SEMIGROUPS_SimpleCongFromPairs");

DeclareCategory("SEMIGROUPS_CongClassSimple",
                IsCongruenceClass and IsAttributeStoringRep and
                IsAssociativeElement);

DeclareGlobalFunction("SEMIGROUPS_SimpleClassFromRMSclass");
