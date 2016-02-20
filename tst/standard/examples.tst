#############################################################################
##
#W  standard/examples.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/examples.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# Catalan monoid
gap> S := CatalanMonoid(1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := CatalanMonoid(2);
<commutative transformation monoid of degree 2 with 1 generator>
gap> Size(S);
2
gap> S := CatalanMonoid(3);
<transformation monoid of degree 3 with 2 generators>
gap> Size(S);
5
gap> S := CatalanMonoid(4);
<transformation monoid of degree 4 with 3 generators>
gap> Size(S);
14

#E# 
gap> STOP_TEST("Semigroups package: standard/examples.tst");
