#############################################################################
##
#W  standard/gree.tst
#Y  Copyright (C) 2016                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/gree.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test IsXTrivial (for Green's classes)
gap> S := Semigroup(
> [Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5) ^ 2, Z(5) ^ 0]]),
>  Matrix(GF(5), [[Z(5) ^ 0, Z(5)], [Z(5), Z(5) ^ 3]]),
>  Matrix(GF(5), [[Z(5) ^ 0, Z(5) ^ 3], [0 * Z(5), 0 * Z(5)]]),
>  Matrix(GF(5), [[Z(5), Z(5) ^ 0], [0 * Z(5), Z(5) ^ 3]]),
>  Matrix(GF(5), [[Z(5), Z(5) ^ 0], [Z(5) ^ 0, Z(5)]]),
>  Matrix(GF(5), [[Z(5) ^ 2, 0 * Z(5)], [Z(5), 0 * Z(5)]]),
>  Matrix(GF(5), [[Z(5) ^ 2, Z(5)], [0 * Z(5), 0 * Z(5)]])]);;
gap> D := GreensDClassOfElement(S,
> Matrix(GF(5), [[Z(5) ^ 3, Z(5) ^ 2], [Z(5) ^ 3, Z(5)]]));
<Green's D-class: Matrix(GF(5), [[Z(5)^3, Z(5)^2], [Z(5)^3, Z(5)]])>
gap> IsHTrivial(D);
false
gap> IsLTrivial(D);
false
gap> IsRTrivial(D);
false

#E# 
gap> STOP_TEST("Semigroups package: standard/gree.tst");
