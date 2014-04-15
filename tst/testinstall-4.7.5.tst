#############################################################################
##
#W  testinstall.tst
#Y  Copyright (C) 2011-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

gap> START_TEST("Semigroups package: testinstall-4.7.5.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

# Issue #63 (problem with Monoid and InverseMonoid when one of the arguments is
# a monoid)
gap> S:=Semigroup(PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ) );
<trivial partial perm group on 5 pts with 0 generators>
gap> T:=Monoid(S,  PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ));;
gap> Length(GeneratorsOfMonoid(T))=2 
> or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> One(S) in T or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> One(S)=One(T);
false
gap> GeneratorsOfSemigroup(T)=
> [ PartialPerm( [ 1, 2, 3, 4, 5, 6 ] ), 
> PartialPerm([ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ), 
> PartialPerm([ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] )]
> or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true
gap> GeneratorsOfMonoid(T)=
> [ PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ), 
>   PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ) ]
> or not CompareVersionNumbers(GAPInfo.Version,"4.7.5");
true

#
gap> S:=InverseSemigroup(PartialPerm( [ 1, 2, 4, 5, 6 ], [ 1, 2, 4, 5, 6 ] ) );;
gap> T:=InverseMonoid(S,  PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 5, 4, 1, 3 ] ));
<inverse partial perm monoid on 6 pts with 2 generators>
gap> GeneratorsOfMonoid(T);
[ <identity partial perm on [ 1, 2, 4, 5, 6 ]>, [6,3,4,1,2,5], [5,2,1,4,3,6] ]
gap> GeneratorsOfSemigroup(T);
[ <identity partial perm on [ 1, 2, 3, 4, 5, 6 ]>, 
  <identity partial perm on [ 1, 2, 4, 5, 6 ]>, [6,3,4,1,2,5], [5,2,1,4,3,6] ]
gap> GeneratorsOfInverseMonoid(T);
[ <identity partial perm on [ 1, 2, 4, 5, 6 ]>, [6,3,4,1,2,5] ]
gap> GeneratorsOfInverseSemigroup(T);
[ <identity partial perm on [ 1, 2, 4, 5, 6 ]>, [6,3,4,1,2,5], 
  <identity partial perm on [ 1, 2, 3, 4, 5, 6 ]> ]
gap> One(S) in T;
true

#
gap> SemigroupsStopTest();
gap> STOP_TEST( "Semigroups package: testinstall-4.7.5.tst", 10000);
