#############################################################################
##
#W  extreme/greens-acting-regular.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


#@local S
gap> START_TEST("Semigroups package: extreme/greens-acting-regular.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test NrIdempotents for a large regular *-semigroup
gap> S := JonesMonoid(15);
<regular bipartition *-monoid of degree 15 with 14 generators>
gap> NrIdempotents(S);
3923351

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/greens-actnig-regular.tst");
