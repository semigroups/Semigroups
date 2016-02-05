#############################################################################
##
#W  extreme/semigroups.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

gap> START_TEST("Semigroups package: extreme/semigroups.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# previously the second arg here (an ideal) would have been added using
#   AsList to the set of generators of S. This is slow with the acting stuff
#   turned off. 
gap> S := Semigroup(AsPartialPermSemigroup(AlternatingGroup(8)),
>                   SemigroupIdeal(SymmetricInverseMonoid(8),
>                                  PartialPerm([1 .. 7])));
<partial perm semigroup of rank 8 with 19 generators>
gap> Size(S);
1421569

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#E#
gap> STOP_TEST("Semigroups package: extreme/semigroups.tst");
