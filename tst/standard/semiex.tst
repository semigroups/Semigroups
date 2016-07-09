#############################################################################
##
#W  standard/semiex.tst
#Y  Copyright (C) 2016                                  James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semiex.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test EndomorphismsPartition 1
gap> EndomorphismsPartition([-1]);
Error, Semigroups: EndomorphismsPartition: usage,
the argument <partition> must be a list of positive integers,
gap> EndomorphismsPartition([1,1,1]);
<full transformation monoid of degree 3>
gap> EndomorphismsPartition([5]);
<full transformation monoid of degree 5>
gap> part := [2, 1, 3];
[ 2, 1, 3 ]
gap> EndomorphismsPartition(part);
<transformation semigroup of degree 6 with 7 generators>
gap> part;
[ 2, 1, 3 ]

# Test EndomorphismsPartition 2
gap> List(Partitions(11), EndomorphismsPartition);
[ <full transformation monoid of degree 11>, 
  <transformation semigroup of degree 11 with 6 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 5 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 12 generators>, 
  <transformation semigroup of degree 11 with 12 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 9 generators>, 
  <transformation semigroup of degree 11 with 12 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 9 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 13 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 15 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 9 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 13 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 12 generators>, 
  <transformation semigroup of degree 11 with 13 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 6 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 12 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 5 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 11 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 10 generators>, 
  <transformation semigroup of degree 11 with 6 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 8 generators>, 
  <transformation semigroup of degree 11 with 6 generators>, 
  <transformation semigroup of degree 11 with 7 generators>, 
  <transformation semigroup of degree 11 with 6 generators>, 
  <transformation semigroup of degree 11 with 5 generators>, 
  <full transformation monoid of degree 11> ]

#E# 
gap> STOP_TEST("Semigroups package: standard/semiex.tst");
