#############################################################################
##
#W  standard/semiffmat.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semiffmat.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Issue 210
gap> x := Matrix(GF(2^2),
> [[Z(2^2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2)],
>  [Z(2^2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2)],
>  [0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2)],
>  [0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2)],
>  [0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2)],
>  [0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2)]]);;
gap> S := Monoid(x, rec(generic := true));
<monoid of 6x6 matrices over GF(2^2) with 1 generator>
gap> HasIsFinite(S);
true
gap> Size(S);
7

# Issue 211
gap> S := FullMatrixMonoid(3, 3);;
gap> One(S) in S;
true
gap> H := GroupHClass(DClass(S, One(S)));
<Green's H-class: Matrix(GF(3), [[Z(3)^0, 0*Z(3), 0*Z(3)], 
   [0*Z(3), Z(3)^0, 0*Z(3)], [0*Z(3), 0*Z(3), Z(3)^0]])>
gap> IsomorphismPermGroup(H);;

#E# 
gap> STOP_TEST("Semigroups package: standard/semiffmat.tst");
