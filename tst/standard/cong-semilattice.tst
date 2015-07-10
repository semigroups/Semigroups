#############################################################################
##
#W  cong-semilattice.tst
#Y  Copyright (C) 2015                                         Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: cong-semilattice.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#T# CongSemilatticeTest1: 
gap> e := InverseSemigroup( [ PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] ),
>                             PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] ), 
>                             PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] ),
>                             PartialPerm( [ 2, 4 ], [ 2, 4 ] ),
>                             PartialPerm( [ 2, 3 ], [ 2, 3 ] ), 
>                             PartialPerm( [ 3, 4 ], [ 3, 4 ] ) ] );;
gap> IsSemilatticeAsSemigroup(e);
true
gap> pairs := [
>    [PartialPermNC([],[]), PartialPermNC([2,3],[2,3])],
>    [PartialPermNC([],[]), PartialPermNC([2],[2])],
>    [PartialPermNC([4],[4]), PartialPermNC([1,3],[1,3])],
>    [PartialPermNC([1,2,4],[1,2,4]), PartialPermNC([1,2,3],[1,2,3])] ];;
gap> cong := SemigroupCongruence(e, pairs);;
gap> IsSemilatticeCongruence(cong);
true

# Reflexivity
gap> [ PartialPerm( [ 1, 2 ], [ 1, 2 ] ), PartialPerm( [ 1, 2 ], [ 1, 2 ] ) ] in cong;
true

# Elements related by a pair
gap> [ PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] ), PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] ) ] in cong;
true
gap> [ PartialPerm( [ 1, 2 ], [ 1, 2 ] ), PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] ) ] in cong;
true

# Elements in different blocks
gap> [ PartialPerm( [ 1 ], [ 1 ] ), PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] ) ] in cong;
false

# Elements in no block
gap> [ PartialPerm( [ 1, 4 ], [ 1, 4 ] ), PartialPerm( [ 1, 2 ], [ 1, 2 ] ) ] in cong;
false

# Elements related by transitivity of two different pairs
gap> [ PartialPerm( [ 2 ], [ 2 ] ), PartialPerm( [ 1, 3 ], [ 1, 3 ] ) ] in cong;
true
gap> BlockCoincidenceTable(cong);
[ 1, 1, 1, 2 ]

#T# CongSemilatticeTest2: Bigger example
gap> s := InverseSemigroup( [
>   PartialPerm( [1,2,3,4,5,6,7,8], [10,7,2,5,6,9,3,8] ),
>   PartialPerm( [1,2,3,4,6,7,9], [1,6,2,8,5,9,7] ),
>   PartialPerm( [1,2,3,4,9], [3,5,10,4,6] ),
>   PartialPerm( [1,2,3,5,6,7,8,9], [7,4,1,6,9,5,2,3] ),
>   PartialPerm( [1,2,4,5,8,9], [9,5,4,8,6,1] ),
>   PartialPerm( [1,3,4,5,8,9], [10,6,7,9,4,1] ),
>   PartialPerm( [1,2,3,8,10], [1,2,7,9,4] ),
>   PartialPerm( [1,2,3,5,6,10], [4,3,6,2,1,10] ),
>   PartialPerm( [1,2,3,4,5,7,8,10], [5,2,8,4,1,10,3,7] ),
>   PartialPerm( [1,2,3,4,6,7,8,9,10], [8,10,3,7,1,5,9,2,6] ) ] );;
gap> e := IdempotentGeneratedSubsemigroup(s);;
gap> IsSemilatticeAsSemigroup(e);
true
gap> pairs := [
>   [PartialPerm([2,4,5,6,7],[2,4,5,6,7]), PartialPerm([2,3,9],[2,3,9])],
>   [PartialPerm([6,9],[6,9]), PartialPerm([5,7],[5,7])],
>   [PartialPerm([1,5,6,7,8],[1,5,6,7,8]), PartialPerm([1,2,8],[1,2,8])],
>   [PartialPerm([3,6,8,9],[3,6,8,9]), PartialPerm([2,6,7,8],[2,6,7,8])],
>   [PartialPerm([1,2,3,5,8,9,10],[1,2,3,5,8,9,10]), PartialPerm([1,8,9],[1,8,9])],
>   [PartialPerm([2,3,4,8,9,10],[2,3,4,8,9,10]),PartialPerm([2,3,5,7],[2,3,5,7])] ];;
gap> cong := SemigroupCongruence(e, pairs);;
gap> [ PartialPerm( [ 2, 5, 6, 7 ], [ 2, 5, 6, 7 ] ), PartialPerm( [ 2, 4, 5, 7 ], [ 2, 4, 5, 7 ] ) ] in cong;
true
gap> [ PartialPerm( [ 2, 4, 5, 6 ], [ 2, 4, 5, 6 ] ), PartialPerm( [ 1, 2, 6, 8 ], [ 1, 2, 6, 8 ] ) ] in cong;
false
gap> [ PartialPerm( [ 2, 5, 6, 7 ], [ 2, 5, 6, 7 ] ), PartialPerm( [ 2, 9 ], [ 2, 9 ] ) ] in cong;   
true
gap> [ PartialPerm( [ 6, 9 ], [ 6, 9 ] ), PartialPerm( [ 3, 4, 6, 8, 10 ], [ 3, 4, 6, 8, 10 ] ) ] in cong;
false
gap> [ PartialPerm( [ 2, 3, 8, 9, 10 ], [ 2, 3, 8, 9, 10 ] ), PartialPerm( [ 2, 4, 5, 7 ], [ 2, 4, 5, 7 ] ) ] in cong;
true
gap> BlockCoincidenceTable(cong);
[ 1, 2, 3, 4, 5, 1 ]

#T# CongSemilatticeTest3: Congruence classes
gap> e := InverseSemigroup( [ PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] ),
>                             PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] ), 
>                             PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] ),
>                             PartialPerm( [ 2, 4 ], [ 2, 4 ] ),
>                             PartialPerm( [ 2, 3 ], [ 2, 3 ] ), 
>                             PartialPerm( [ 3, 4 ], [ 3, 4 ] ) ] );;
gap> IsSemilatticeAsSemigroup(e);
true
gap> pairs := [
>   [ PartialPerm( [ 2, 3 ], [ 2, 3 ] ), PartialPerm( [ 2, 4 ], [ 2, 4 ] ) ], 
>   [ PartialPerm( [  ], [  ] ), PartialPerm( [ 2 ], [ 2 ] ) ], 
>   [ PartialPerm( [ 3, 4 ], [ 3, 4 ] ), PartialPerm( [ 1, 4 ], [ 1, 4 ] ) ] ];;
gap> cong := SemigroupCongruence(e, pairs);;
gap> IsSemilatticeCongruence(cong);
true
gap> BlockCoincidenceTable(cong);
[ 1, 1, 2 ]
gap> x := PartialPerm([4],[4]);;
gap> c1 := CongruenceClassOfElement(cong, x);;
gap> x in c1;
true
gap> PartialPerm([3,4],[3,4]) in c1;
true
gap> PartialPerm([1,3],[1,3]) in c1;
false
gap> c1 := CongruenceClassOfElement(cong, PartialPerm([],[]));;
gap> c2 := CongruenceClassOfElement(cong, PartialPerm([2,3],[2,3]));;
gap> c1 = c2;
true
gap> c2 := CongruenceClassOfElement(cong, PartialPerm([1],[1]));;
gap> c1 = c2;
false
gap> c1 := CongruenceClassOfElement(cong, PartialPerm([1],[1]));;
gap> c1 = c2;
true
gap> c2 := CongruenceClassOfElement(cong, PartialPerm([1,2,3],[1,2,3]));;
gap> c1 = c2;
false
gap> c1 * c2 = c1;
true
gap> c2 * c2 = c2;
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(s);
gap> Unbind(e);
gap> Unbind(x);
gap> Unbind(c1);
gap> Unbind(c2);
gap> Unbind(pairs);
gap> Unbind(cong);

#E#
gap> STOP_TEST( "Semigroups package: cong-semilattice.tst");
