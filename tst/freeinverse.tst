#############################################################################
##
##  freeinverse,tst 
#Y  Copyright (C) 2011-13
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
gap> START_TEST("Semigroups package: freeinverse.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#
gap> S := FreeInverseSemigroup(3);
<free inverse semigroup on the generators [ x1, x2, x3 ]>
gap> Size(S);
infinity
gap> x := S.1;
x1
gap> y := S.2;
x2
gap> z := S.3;
x3
gap> u := x^5 * y^3 * z;
x1*x1*x1*x1*x1*x2*x2*x2*x3
gap> u^-1;
x3^-1*x2^-1*x2^-1*x2^-1*x1^-1*x1^-1*x1^-1*x1^-1*x1^-1
gap> x^2 * y = x^2 * y;
true
gap> x * x^-1 = y * y^-1;
false

#
gap> S := FreeInverseSemigroup("a", "b", "c");
<free inverse semigroup on the generators [ a, b, c ]>
gap> Size(S);
infinity
gap>  x := S.1;
a
gap> y := S.2;
b
gap> z := S.3;
c
gap> u := x^5 * y^3 * z;
a*a*a*a*a*b*b*b*c
gap> u^-1;
c^-1*b^-1*b^-1*b^-1*a^-1*a^-1*a^-1*a^-1*a^-1
gap>  x^2 * y = x^2 * y;
true
gap> x * x^-1 = y * y^-1;
false

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST("Semigroups package: freeinverse.tst", 0);
