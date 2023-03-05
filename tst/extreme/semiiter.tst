#############################################################################
##
#W  extreme/semiiter.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This test file contains some tests for the performance of
## IteratorIteratorSorted, Enumerator, EnumeratorSorted, and AsSSortedList.


#@local A, S, a, acting, enum, iter, x
gap> START_TEST("Semigroups package: extreme/semiiter.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#
gap> S := PartialTransformationMonoid(7);
<regular transformation monoid of degree 8 with 4 generators>
gap> Size(S);
2097152
gap> S := Semigroup(S, rec(acting := false));
<transformation monoid of degree 8 with 4 generators>
gap> Size(S);
2097152
gap> iter := Iterator(S);
<iterator>
gap> for x in iter do od;  # ~400ms
gap> iter := IteratorSorted(S);
<iterator>
gap> for x in iter do od;  # ~1.8s (has to sort the elements first)
gap> iter := IteratorSorted(S);
<iterator>
gap> for x in iter do od;  # ~600ms
gap> enum := Enumerator(S);
<enumerator of <transformation monoid of size 2097152, degree 8 with 4 
 generators>>
gap> ForAll([1 .. Length(enum)], i -> Position(enum, enum[i]) = i);  # ~5s
true
gap> enum := EnumeratorSorted(S);
<enumerator of <transformation monoid of size 2097152, degree 8 with 4 
 generators>>
gap> enum[1];
Transformation( [ 1, 1, 1, 1, 1, 1, 1 ] )
gap> enum[10000];
Transformation( [ 1, 1, 3, 4, 5, 2, 8, 8 ] )
gap> enum[Size(S)];
Transformation( [ 8, 8, 8, 8, 8, 8, 8, 8 ] )
gap> A := AsSet(S);;    # ~320ms
gap> for a in A do od;  # ~33ms

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/semiiter.tst");
