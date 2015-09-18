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
gap> SEMIGROUPS_StartTest();

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

# Bad usage
gap> [ PartialPerm( [ 1, 2 ], [ 1, 2 ] ) ] in cong;
Error, Semigroups: \in: usage,
the first arg <pair> must be a list of length 2,
gap> [ PartialPerm( [ 1, 2 ], [ 1, 2 ] ), PartialPerm( [ 1, 5 ], [ 1, 5 ] ) ] in cong;
Error, Semigroups: \in: usage,
elements of the first arg <pair> must be in range of the second
arg <cong>,

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
gap> CongruenceClassOfElement(cong, PartialPerm( [ 1, 5 ], [ 1, 5 ] ));
Error, Semigroups: EquivalenceClassOfElement: usage,
the second arg <elm> must be in the semigroup of first arg <cong>,
gap> BlockCoincidenceTable(cong);
[ 1, 1, 2 ]
gap> NrCongruenceClasses(cong);
9
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
gap> cong2 := SemigroupCongruence(e, []);;
gap> cong = cong2;
false
gap> c3 := CongruenceClassOfElement(cong2, PartialPerm([1,2,3],[1,2,3]));;
gap> c2 = c3;
false
gap> c2 * c3;
Error, Semigroups: \*: usage,
the args <c1> and <c2> must be classes of the same congruence,
gap> c1 * c2 = c1;
true
gap> c2 * c2 = c2;
true
gap> NonTrivialCongruenceClasses(cong);
[ {PartialPerm( [ 2, 3 ], [ 2, 3 ] )}, {PartialPerm( [ 3, 4 ], [ 3, 4 ] )} ]
gap> NonTrivialCongruenceClasses(cong2);
[  ]
gap> ImagesElm(cong, PartialPerm([8,9],[8,9]));
Error, Semigroups: ImagesElm: usage,
the second arg <elm> must be in the semigroup of the first arg <cong>,
gap> c1 := CongruenceClassOfElement(cong, PartialPerm([1,3,4],[1,3,4]));;
gap> Enumerator(c1);
[ <identity partial perm on [ 1, 3, 4 ]> ]
gap> AsList(c1);
[ <identity partial perm on [ 1, 3, 4 ]> ]
gap> c2 := CongruenceClassOfElement(cong, PartialPerm([],[]));;
gap> AsList(c2);
[ <empty partial perm>, <identity partial perm on [ 2 ]>, 
  <identity partial perm on [ 2, 3 ]>, <identity partial perm on [ 2, 4 ]> ]
gap> c1 := CongruenceClassOfElement(cong, PartialPerm([],[]));;
gap> enum := Enumerator(c1);;
gap> enum[1];
<empty partial perm>
gap> enum[2];
<identity partial perm on [ 2 ]>
gap> x := enum[2];
<identity partial perm on [ 2 ]>
gap> enum[3];
<identity partial perm on [ 2, 3 ]>
gap> Position(enum, x);
2
gap> Position(enum, PartialPerm([2,4],[2,4]));
4
gap> Position(enum, 42);
fail
gap> Size(enum);
4
gap> Size(c1);
4
gap> c1 := CongruenceClassOfElement(cong, PartialPerm([],[]));;
gap> enum := Enumerator(c1);;
gap> enum[2];
<identity partial perm on [ 2 ]>
gap> Size(c1);
4
gap> c1 := CongruenceClassOfElement(cong, PartialPerm([],[]));;
gap> enum := Enumerator(c1);;
gap> PartialPerm([2,3],[2,3]) in enum;
true
gap> AsList(c1);
[ <empty partial perm>, <identity partial perm on [ 2, 3 ]>, 
  <identity partial perm on [ 2 ]>, <identity partial perm on [ 2, 4 ]> ]
gap> CongruenceClasses(cong);
[ {PartialPerm( [ 2, 3 ], [ 2, 3 ] )}, {PartialPerm( [ 3, 4 ], [ 3, 4 ] )}, 
  {PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] )}, 
  {PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] )}, 
  {PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] )}, 
  {PartialPerm( [ 1, 3 ], [ 1, 3 ] )}, {PartialPerm( [ 3 ], [ 3 ] )}, 
  {PartialPerm( [ 1, 2 ], [ 1, 2 ] )}, {PartialPerm( [ 1 ], [ 1 ] )} ]

#T# CongSemilatticeTest4: ElementsBetween
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
gap> hi := PartialPerm( [ 3, 5, 8, 9, 10 ], [ 3, 5, 8, 9, 10 ] );;
gap> lo := PartialPerm( [ 5, 9 ], [ 5, 9 ] );;
gap> x := PartialPerm( [ 1, 2, 4, 5, 8 ], [ 1, 2, 4, 5, 8 ] );;
gap> SemilatticeElementsBetween(e, x, lo);
Error, Semigroups: SemilatticeElementsBetween: usage,
<bottom> * <top> must be equal to <bottom>,
gap> SemilatticeElementsBetween(e, lo, 42);
Error, Semigroups: SemilatticeElementsBetween: usage,
<bottom> and <top> must be elements of <s>,
gap> SemilatticeElementsBetween(e, lo, hi);
[ <identity partial perm on [ 5, 9 ]>, <identity partial perm on [ 5, 8, 9 ]>,
  <identity partial perm on [ 3, 5, 9 ]>, 
  <identity partial perm on [ 3, 5, 8, 9 ]>, 
  <identity partial perm on [ 5, 9, 10 ]>, 
  <identity partial perm on [ 5, 8, 9, 10 ]>, 
  <identity partial perm on [ 3, 5, 9, 10 ]>, 
  <identity partial perm on [ 3, 5, 8, 9, 10 ]> ]
gap> hi := PartialPerm( [ 3, 5, 8, 9, 10 ], [ 3, 5, 8, 9, 10 ] );;
gap> lo := PartialPerm( [  ], [  ] );;
gap> SemilatticeElementsBetween(e, lo, hi);
[ <identity partial perm on [ 5, 8 ]>, <identity partial perm on [ 3, 5, 8 ]>,
  <identity partial perm on [ 8, 9 ]>, <identity partial perm on [ 5, 9 ]>, 
  <identity partial perm on [ 5, 8, 9 ]>, 
  <identity partial perm on [ 3, 8, 9 ]>, 
  <identity partial perm on [ 3, 5, 9 ]>, 
  <identity partial perm on [ 3, 5, 8, 9 ]>, 
  <identity partial perm on [ 9, 10 ]>, <identity partial perm on [ 8, 10 ]>, 
  <identity partial perm on [ 8, 9, 10 ]>, <identity partial perm on [ 5, 10 ]
    >, <identity partial perm on [ 5, 9, 10 ]>, 
  <identity partial perm on [ 5, 8, 10 ]>, 
  <identity partial perm on [ 5, 8, 9, 10 ]>, 
  <identity partial perm on [ 3, 10 ]>, <identity partial perm on [ 3, 9, 10 ]
    >, <identity partial perm on [ 3, 8, 10 ]>, 
  <identity partial perm on [ 3, 8, 9, 10 ]>, 
  <identity partial perm on [ 3, 5, 10 ]>, 
  <identity partial perm on [ 3, 5, 9, 10 ]>, 
  <identity partial perm on [ 3, 5, 8, 10 ]>, 
  <identity partial perm on [ 3, 5, 8, 9, 10 ]>, 
  <identity partial perm on [ 8 ]>, <identity partial perm on [ 5 ]>, 
  <empty partial perm>, <identity partial perm on [ 3, 8 ]>, 
  <identity partial perm on [ 3, 5 ]>, <identity partial perm on [ 3 ]>, 
  <identity partial perm on [ 9 ]>, <identity partial perm on [ 3, 9 ]>, 
  <identity partial perm on [ 10 ]> ]

#T# MeetSemigroupCongruences
gap> e := InverseSemigroup( [ PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] ),
>                             PartialPerm( [ 1, 2, 3 ], [ 1, 2, 3 ] ),
>                             PartialPerm( [ 1, 2, 4 ], [ 1, 2, 4 ] ),
>                             PartialPerm( [ 2, 4 ], [ 2, 4 ] ),
>                             PartialPerm( [ 2, 3 ], [ 2, 3 ] ),
>                             PartialPerm( [ 3, 4 ], [ 3, 4 ] ) ] );;
gap> IsSemilatticeAsSemigroup(e);;
gap> p1 := [[PartialPermNC([1,2,4],[1,2,4]), PartialPermNC([1,2,3],[1,2,3])]];;
gap> p2 := [[PartialPermNC([4],[4]), PartialPermNC([1,2,3],[1,2,3])]];;
gap> c1 := SemigroupCongruence(e, p1);;
gap> c2 := SemigroupCongruence(e, p2);;
gap> cc := MeetSemigroupCongruences(c1,c2);
<semigroup congruence over <inverse partial perm semigroup of rank 4 with
 6 generators> with 1 generating pairs>
gap> NonTrivialCongruenceClasses(cc);
[ {PartialPerm( [ 1, 2 ], [ 1, 2 ] )} ]
gap> Elements(last[1]);
[ <identity partial perm on [ 1, 2 ]>, <identity partial perm on [ 1, 2, 3 ]> 
 ]
gap> f := InverseSemigroup( [ PartialPerm( [ 1, 3, 4 ], [ 1, 3, 4 ] ),
>                             PartialPerm( [ 2, 4 ], [ 2, 4 ] ),
>                             PartialPerm( [ 3, 4 ], [ 3, 4 ] ) ] );;
gap> IsSemilatticeAsSemigroup(f);;
gap> p2 := [[PartialPermNC([1,3,4],[1,3,4]), PartialPermNC([3,4],[3,4])]];;
gap> c2 := SemigroupCongruence(f, p2);;
gap> MeetSemigroupCongruences(c1, c2);
Error, Semigroups: MeetOfSemigroupCongruences: usage,
args <cong1> and <cong2> must be over the same semigroup,

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(s);
gap> Unbind(e);
gap> Unbind(x);
gap> Unbind(p1);
gap> Unbind(p2);
gap> Unbind(c1);
gap> Unbind(c2);
gap> Unbind(cc);
gap> Unbind(pairs);
gap> Unbind(cong);
gap> Unbind(lo);
gap> Unbind(hi);

#E#
gap> STOP_TEST( "Semigroups package: cong-semilattice.tst");
