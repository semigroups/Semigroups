#############################################################################
##
#W  inverse-cong.tst
#Y  Copyright (C) 2014-15                                   Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: inverse-cong.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#T# Create an inverse semigroup
gap> s := InverseSemigroup( [ PartialPerm( [ 1, 2, 3, 5 ], [ 2, 7, 3, 4 ] ),
>  PartialPerm( [ 1, 3, 4, 5 ], [ 7, 2, 4, 6 ] ), 
>  PartialPerm( [ 1, 2, 3, 4, 6 ], [ 2, 3, 4, 6, 1 ] ),
>  PartialPerm( [ 1, 2, 4, 6 ], [ 2, 4, 3, 7 ] ),
>  PartialPerm( [ 1, 2, 4, 6 ], [ 3, 1, 7, 2 ] ), 
>  PartialPerm( [ 1, 2, 5, 6 ], [ 5, 1, 6, 3 ] ),
>  PartialPerm( [ 1, 2, 3, 6 ], [ 7, 3, 4, 2 ] ) ] );;

#T# Construct one congruence by generating pairs
gap> cong := SemigroupCongruence(s,
>  [ PartialPerm( [ 4 ], [ 7 ] ), PartialPerm( [ 2 ], [ 1 ] ) ] );
<semigroup congruence over <inverse partial perm semigroup of size 4165, 
on 7 pts with 7 generators> with congruence pair (106,57)>

#T# Try some methods
gap> x := PartialPerm( [ 4 ], [ 5 ] );;
gap> y := PartialPerm( [ 1, 2, 5 ], [ 5, 1, 6 ] );;
gap> z := PartialPerm( [ 6 ], [ 1 ] );;
gap> [x,y] in cong;
false
gap> [x,z] in cong;
true
gap> [y,z] in cong;
false

#T# Congruence classes
gap> classx := CongruenceClassOfElement(cong, x);
{PartialPerm( [ 4 ], [ 5 ] )}
gap> classy := CongruenceClassOfElement(cong, y);;
gap> classz := CongruenceClassOfElement(cong, z);;
gap> classx = classy;
false
gap> classz = classx;
true
gap> x in classx;
true
gap> y in classx;
false
gap> x in classz;
true
gap> z * y in classz * classy;
true
gap> y * x in classy * classx;
true
gap> Size(classx);
50

#T# Quotients
gap> q := s / cong;;

#T# Convert to and from semigroup congruence by generating pairs
gap> pairs := GeneratingPairsOfSemigroupCongruence(cong);;
gap> ccong := SemigroupCongruence(s, pairs);;
gap> ccong = cong;
true
gap> ccong := AsSemigroupCongruenceByGeneratingPairs(cong);
<semigroup congruence over <inverse partial perm semigroup of size 4165, 
on 7 pts with 7 generators> with 1 generating pairs>
gap> [x,y] in ccong;
false
gap> [x,z] in ccong;
true
gap> [y,z] in ccong;
false

#E# 
gap> STOP_TEST( "Semigroups package: inverse-cong.tst");
