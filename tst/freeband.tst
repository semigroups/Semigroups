#############################################################################
##
##  freeband.tst
#Y  Copyright (C) 2013-14                                   Julius Jonusas
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

gap> START_TEST("Semigroups package: freeband.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#T# FreeBand (with default generators) and basic methods
gap> S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> Size(S);
159
gap> Size(Elements(S));
159
gap> S :=FreeBand(4);
<free band on the generators [ x1, x2, x3, x4 ]>
gap> Size(S);
332380
gap> Size(Elements(S));
332380

#T# FreeBand: D-class iterator
gap> S := FreeBand(5);
<free band on the generators [ x1, x2, x3, x4, x5 ]>
gap> x := S.3*S.2*S.1;
x3x2x1
gap> D := GreensDClassOfElement(S, x);
{x3x2x1}
gap> iter := Iterator(D);
<iterator>
gap> NextIterator(iter);
x3x2x3x1x3x2x3
gap> NextIterator(iter);
x2x3x1x3x2x3
gap> NextIterator(iter);
x3x2x1x3x2x3

#T# FreeBand: iterator
gap> S :=FreeBand(10);
<free band on the generators [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 ]>
gap> iter := Iterator(S);
<iterator>
gap> NextIterator(iter);
x1
gap> NextIterator(iter);
x2
gap> NextIterator(iter);
x2x1x2
gap> NextIterator(iter);
x1x2
gap> NextIterator(iter);
x2x1
gap> NextIterator(iter);
x1x2x1

#T# FreeBand: size
gap> Size(FreeBand(1));
1
gap> Size(FreeBand(2));
6
gap> Size(FreeBand(3));
159
gap> Size(FreeBand(4));
332380
gap> Size(FreeBand(7));
3641839910835401567626683593436003894250931310990279691

#T# FreeBand: /<
gap> S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> x*y < y;
false
gap> x := Generators(S)[1];
x1
gap> y := Generators(S)[2];
x2
gap> z := Generators(S)[3];
x3
gap> x < y;
true
gap> y < z;
true
gap> x*y < z;
true
gap> x*y < y;
false
gap> z < x*y*x*y*x;
false

#T# FreeBand: full iterator
S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> list := [];;
gap> iter := Iterator(S);;
gap> next := NextIterator(iter);;
gap> while next <> fail do
> Add(list, next);
> next := NextIterator(iter);
> od;
gap> Size(list);
159
gap> IsDuplicateFree(list);
true

#T# FreeBand: equality
gap> S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> x := Generators(S)[1];
x1
gap> y := Generators(S)[2];
x2
gap> z := Generators(S)[3];
x3
gap> x * y = z;
false
gap> x * y * y = z;
false
gap> x * x = x;
true
gap> x*y*x*y*z*x*y*z = x*y*z;
true

# FreeBand: IsFreeBand
gap> IsFreeBand(FreeBand(4));
true
gap> IsFreeBand(FreeBand(4, "b"));
true
gap> IsFreeBand(SymmetricGroup(6));
false
gap> IsFreeBand(FullTransformationMonoid(7));
false

#T# FreeBand: \*
gap> iter := Iterator(FreeBand(4, "b"));;
gap> x := NextIterator(iter);;
gap> for i in [1..10000] do NextIterator(iter); od;
gap> y := NextIterator(iter);
b3b4b1b3b4b3b2b1b4b2b4b3b2b3b4
gap> T := Semigroup(x, y);;
gap> IsFreeBandSubsemigroup(T);
true
gap> Size(T);
5

# FreeBand: Subsemigroup
gap> iter := Iterator(FreeBand(4, "b"));
<iterator>
gap> x := NextIterator(iter);
b1
gap> for i in [1..10000] do NextIterator(iter); od;
gap> y := NextIterator(iter);
b3b4b1b3b4b3b2b1b4b2b4b3b2b3b4
gap> T := Semigroup(x, y);
<semigroup with 2 generators>
gap> IsFreeBandSubsemigroup(T);
true
gap> Size(T);

#E#
gap> STOP_TEST("Semigroups package: freeband.tst");
