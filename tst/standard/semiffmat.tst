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
gap> S := FullMatrixSemigroup(3, 3);;
gap> One(S) in S;
true
gap> H := GroupHClass(DClass(S, One(S)));
<Green's H-class: Matrix(GF(3), [[Z(3)^0, 0*Z(3), 0*Z(3)], 
   [0*Z(3), Z(3)^0, 0*Z(3)], [0*Z(3), 0*Z(3), Z(3)^0]])>
gap> IsomorphismPermGroup(H); 
MappingByFunction( <Green's H-class: Matrix(GF(3), [[Z(3)^0, 0*Z(3), 0*Z(3)], 
   [0*Z(3), Z(3)^0, 0*Z(3)], [0*Z(3), 0*Z(3), Z(3)^0]])>, Group([ (), (10,19)
(11,20)(12,21)(13,22)(14,23)(15,24)(16,25)(17,26)(18,27), (2,7,10,20,18,5,25,
21,15,14,17,8,16)(3,4,19,12,23,9,13,11,26,27,24,6,
22) ]), function( x ) ... end, function( x ) ... end )

#E# 
gap> STOP_TEST("Semigroups package: standard/semiffmat.tst");
