#############################################################################
##
#W  extreme/gracreg.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

gap> START_TEST("Semigroups package: extreme/gracreg.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test NrIdempotents for a large regular *-semigroup
gap> S := JonesMonoid(15);
<regular bipartition *-monoid of degree 15 with 14 generators>
gap> NrIdempotents(S);
3923351

#E# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/gracreg.tst");
