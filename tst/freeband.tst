#############################################################################
##
##  freeband.tst
#Y  Copyright (C) 2013-14                                      Julius Jonusas
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
gap> START_TEST("Semigroups package: freeband.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#
gap> s := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> Size(s);
159
gap> Size(Elements(s));
159
gap> s := FreeBand(4);
<free band on the generators [ x1, x2, x3, x4 ]>
gap> Size(s);
332380
gap> Size(Elements(s));
332380

#
gap> s := FreeBand(5);
<free band on the generators [ x1, x2, x3, x4, x5 ]>
gap> x := s.3*s.2*s.1;
x3x2x1
gap> D := GreensDClassOfElement(s, x);
{x3x2x1}
gap> iter := Iterator(D);
<iterator>
gap> NextIterator(iter);
x3x2x3x1x3x2x3
gap> NextIterator(iter);
x2x3x1x3x2x3
gap> NextIterator(iter);
x3x2x1x3x2x3

#
gap> s := FreeBand(10);
<free band on the generators [ x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 ]>
gap> iter := Iterator(s);
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

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST("Semigroups package: freeband.tst", 0);
