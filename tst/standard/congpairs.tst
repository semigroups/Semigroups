#############################################################################
##
#W  standard/congpairs.tst
#Y  Copyright (C) 2014-16                                   Michael Torpey
##                                                          Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/congpairs.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# PairsCongTest2: Checking robustness against infinite semigroups
gap> S := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> x := GeneratorsOfSemigroup(S)[1];
s1
gap> gens := [x ^ 2, x ^ 4];
[ s1^2, s1^4 ]
gap> cong := SemigroupCongruence(S, gens);
<semigroup congruence over <free semigroup on the generators [ s1 ]> with 
1 generating pairs>

# The next test is now valid (but would run forever)
#gap> NonTrivialCongruenceClasses(cong);
#Error, no method found! For debugging hints type ?Recovery from NoMethodFound
#Error, no 2nd choice method found for `NonTrivialEquivalenceClasses' on 1 argu\
#ments
gap> gens in cong;
true
gap> EquivalenceRelationLookup(cong);
Error, Semigroups: EquivalenceRelationLookup: usage,
<cong> must be over a finite semigroup,
gap> NrCongruenceClasses(cong);
3
gap> class := CongruenceClassOfElement(cong, x);;
gap> cong2 := SemigroupCongruence(S, [x ^ 2, x ^ 2]);;
gap> class := CongruenceClassOfElement(cong2, x);;
gap> enum := Enumerator(class);
[ s1 ]
gap> Size(class);
1
gap> x ^ 2 in class;
false
gap> ImagesElm(cong2, x ^ 5);
[ s1^5 ]

# PairsCongTest3: \= for two semigroup congruences
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5])];;
gap> S := Semigroup(Transformation([1]));;
gap> T := Monoid(gens);;
gap> u := UniversalSemigroupCongruence(S);
<universal semigroup congruence over <trivial transformation group of 
 degree 0 with 1 generator>>
gap> v := SemigroupCongruence(T, [gens[1], gens[1]]);
<semigroup congruence over <commutative non-regular transformation monoid of 
 degree 10 with 1 generator> with 0 generating pairs>
gap> NrCongruenceClasses(v);
6
gap> Size(T);
6
gap> u = v;
false
gap> u := UniversalSemigroupCongruence(T);
<universal semigroup congruence over <commutative non-regular transformation 
 monoid of size 6, degree 10 with 1 generator>>
gap> u = v;
false
gap> gens := List(T, x -> [gens[1], x]);;
gap> v := SemigroupCongruence(T, gens);
<semigroup congruence over <commutative non-regular transformation monoid 
 of size 6, degree 10 with 1 generator> with 5 generating pairs>
gap> u = v;
true
gap> NrCongruenceClasses(u);
1

# PairsCongTest4: \* for two semigroup congruence classes
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5])];;
gap> S := Semigroup(gens);;
gap> gens := List(S, x -> [gens[1], x]);;
gap> u := SemigroupCongruence(S, gens);  # universal congruence
<semigroup congruence over <commutative non-regular transformation semigroup 
 of degree 10 with 1 generator> with 4 generating pairs>
gap> u = UniversalSemigroupCongruence(S);
true
gap> v := SemigroupCongruence(S, [gens[1], gens[1]]);  # trivial congruence
<semigroup congruence over <commutative non-regular transformation semigroup 
 of degree 10 with 1 generator> with 0 generating pairs>
gap> classes := Set(CongruenceClasses(v));
[ <congruence class of Transformation( [ 1, 2, 2, 1, 2, 6, 6, 9, 9, 1 ] )>, 
  <congruence class of Transformation( [ 2, 6, 6, 2, 6, 9, 9, 1, 1, 2 ] )>, 
  <congruence class of Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] )>, 
  <congruence class of Transformation( [ 6, 9, 9, 6, 9, 1, 1, 2, 2, 6 ] )>, 
  <congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )> ]
gap> CongruenceClasses(u)[1] in classes;
false
gap> classes[1] * CongruenceClasses(u)[1];
Error, Semigroups: \*: usage,
the args must be classes of the same congruence,
gap> CongruenceClasses(u)[1] * classes[1];
Error, Semigroups: \*: usage,
the args must be classes of the same congruence,
gap> classes[3] * classes[4];
<congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )>
gap> classes[4] * classes[3];
<congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )>
gap> CongruenceClassOfElement(v, Representative(classes[5] * classes[2])) =
> CongruenceClassOfElement(v, 
>                          Representative(classes[5]) *
>                          Representative(classes[2]));
true
gap> classes[3] = classes[3];
true
gap> classes[2] = classes[3];
false
gap> AsList(classes[4]);
[ Transformation( [ 6, 9, 9, 6, 9, 1, 1, 2, 2, 6 ] ) ]
gap> Size(classes[4]);
1

# A semigroup congruence example
gap> S := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4]),
>                    Transformation([1, 3, 4, 1, 3])]);;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> pair2 := [Transformation([4, 3, 4, 3, 4]),
>              Transformation([3, 4, 3, 4, 3])];;
gap> cong := SemigroupCongruence(S, pair1, pair2);
<semigroup congruence over <transformation semigroup of degree 5 with 4 
 generators> with 2 generating pairs>
gap> Print(cong); Print("\n");
SemigroupCongruence( Semigroup( [ Transformation( [ 2, 1, 1, 2, 1 ] ), 
  Transformation( [ 3, 4, 3, 4, 3 ] ), Transformation( [ 4, 3, 3, 4, 4 ] ), 
  Transformation( [ 1, 3, 4, 1, 3 ] ) ] ), 
[ [ Transformation( [ 3, 4, 3, 4, 3 ] ), Transformation( [ 1, 2, 1, 2, 1 ] ) ]
    , 
  [ Transformation( [ 4, 3, 4, 3, 4 ] ), Transformation( [ 3, 4, 3, 4, 3 ] ) 
     ] ] )
gap> [Transformation([4, 4, 3, 4, 4]), Transformation([3, 3, 1, 3, 3])] in cong;
true
gap> [Transformation([4, 4, 3, 4, 4]), Transformation([3, 3, 1, 3, 3])] in cong;
true
gap> [Transformation([3, 4, 3, 3, 4]), Transformation([1, 3, 4, 1, 3])] in cong;
false
gap> [Transformation([3, 4, 3, 3, 4]), Transformation([1, 3, 4, 1, 3])] in cong;
false
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> NonTrivialCongruenceClasses(cong);
[ <congruence class of Transformation( [ 2, 1, 1, 2, 1 ] )> ]
gap> classes := CongruenceClasses(cong);
[ <congruence class of Transformation( [ 2, 1, 1, 2, 1 ] )>, 
  <congruence class of Transformation( [ 1, 3, 4, 1, 3 ] )> ]
gap> ImagesElm(cong, Transformation([1, 3, 4, 1, 3]));
[ Transformation( [ 1, 3, 4, 1, 3 ] ) ]
gap> cong = JoinSemigroupCongruences(cong, cong);
true
gap> T := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4]),
>                    Transformation([2, 3, 2, 2, 3, 1]),
>                    Transformation([1, 3, 4, 1, 3])]);;
gap> cong2 := SemigroupCongruence(T, pair1, pair2);;
gap> EquivalenceClassOfElement(cong, Transformation([2, 3, 2, 2, 3, 1]));
Error, Semigroups: EquivalenceClassOfElement: usage,
the second arg <elm> must be in the semigroup of the first arg <cong>,
gap> JoinSemigroupCongruences(cong, cong2);
Error, Semigroups: SEMIGROUPS.JoinCongruences: usage,
the congruences must be defined over the same semigroup,
gap> IsSubrelation(cong, cong2);
Error, Semigroups: IsSubrelation: usage,
congruences must be defined over the same semigroup,
gap> cong := LeftSemigroupCongruence(S, pair1, pair2);;
gap> IsSubrelation(cong2, cong);
Error, Semigroups: IsSubrelation: usage,
congruences must be defined over the same semigroup,
gap> cong := RightSemigroupCongruence(S, pair1, pair2);;
gap> IsSubrelation(cong2, cong);
Error, Semigroups: IsSubrelation: usage,
congruences must be defined over the same semigroup,

# A left semigroup congruence example that is also right
gap> S := Semigroup(Transformation([2, 1, 1, 2, 1]),
>                   Transformation([3, 4, 3, 4, 4]),
>                   Transformation([3, 4, 3, 4, 3]),
>                   Transformation([4, 3, 3, 4, 4]));;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> pair2 := [Transformation([4, 3, 4, 3, 4]),
>              Transformation([3, 4, 3, 4, 3])];;
gap> cong := LeftSemigroupCongruence(S, pair1, pair2);
<left semigroup congruence over <transformation semigroup of degree 5 with 4 
 generators> with 2 generating pairs>
gap> JoinLeftSemigroupCongruences(cong, cong) = cong;
true
gap> [Transformation([3, 4, 4, 3, 3]), Transformation([1, 2, 2, 1, 1])] in cong;
true
gap> [Transformation([3, 4, 4, 3, 3]), Transformation([1, 2, 2, 1, 1])] in cong;
true
gap> [Transformation([1, 2, 1, 2, 2]), Transformation([1, 2, 2, 1, 2])] in cong;
false
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 4, 1, 1, 1, 2, 3, 4, 2, 2, 3, 3, 4, 4 ]
gap> NonTrivialEquivalenceClasses(cong);
[ <left congruence class of Transformation( [ 2, 1, 1, 2, 1 ] )>, 
  <left congruence class of Transformation( [ 3, 4, 3, 4, 4 ] )>, 
  <left congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )>, 
  <left congruence class of Transformation( [ 4, 3, 3, 4, 4 ] )> ]
gap> LeftCongruenceClasses(cong);
[ <left congruence class of Transformation( [ 2, 1, 1, 2, 1 ] )>, 
  <left congruence class of Transformation( [ 3, 4, 3, 4, 4 ] )>, 
  <left congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )>, 
  <left congruence class of Transformation( [ 4, 3, 3, 4, 4 ] )> ]
gap> IsRightSemigroupCongruence(cong);
true

# A left semigroup congruence example that is not right
gap> S := Semigroup([Transformation([1, 3, 4, 1, 3, 7, 5]),
>                    Transformation([5, 7, 1, 6, 1, 7, 6])]);;
gap> pair := [Transformation([1, 1, 1, 1, 1, 4, 1]),
>             Transformation([1, 6, 5, 7, 5, 6, 7])];;
gap> cong := LeftSemigroupCongruence(S, pair);;
gap> Print(cong); Print("\n");
LeftSemigroupCongruence( Semigroup( 
[ Transformation( [ 1, 3, 4, 1, 3, 7, 5 ] ), 
  Transformation( [ 5, 7, 1, 6, 1, 7, 6 ] ) ] ), 
[ 
  [ Transformation( [ 1, 1, 1, 1, 1, 4, 1 ] ), 
      Transformation( [ 1, 6, 5, 7, 5, 6, 7 ] ) ] ] )
gap> [Transformation([7, 1, 7, 7, 1, 5, 5]),
>     Transformation([1, 7, 1, 1, 7, 1, 1])] in cong;
true
gap> IsSemigroupCongruence(cong);
false
gap> IsRightSemigroupCongruence(cong);
false

# A right semigroup congruence example that is left
gap> S := Semigroup([Transformation([2, 3, 5, 1, 4, 6, 1]),
>                    Transformation([2, 6, 4, 2, 6, 1, 2])]);;
gap> pairs := [[Transformation([6, 1, 2, 6, 1, 2, 6]),
>               Transformation([5, 4, 1, 3, 2, 6, 3])],
>             [Transformation([2, 3, 5, 1, 4, 6, 1]),
>              Transformation([5, 6, 2, 5, 6, 3, 5])]];;
gap> cong := RightSemigroupCongruence(S, pairs);;
gap> JoinRightSemigroupCongruences(cong, cong) = cong;
true
gap> [Transformation([2, 6, 4, 2, 6, 1, 2]),
>     Transformation([6, 2, 6, 5, 5, 3, 5])] in cong;
true
gap> IsSemigroupCongruence(cong);
true
gap> IsLeftSemigroupCongruence(cong);
true

# A right semigroup congruence example that is not left
gap> S := Semigroup(Transformation([2, 1, 1, 2, 1]),
>                   Transformation([3, 4, 3, 4, 4]),
>                   Transformation([3, 4, 3, 4, 3]),
>                   Transformation([4, 3, 3, 4, 4]));;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> pair2 := [Transformation([4, 3, 4, 3, 4]),
>              Transformation([3, 4, 3, 4, 3])];;
gap> cong := RightSemigroupCongruence(S, pair1, pair2);
<right semigroup congruence over <transformation semigroup of degree 5 with 4 
 generators> with 2 generating pairs>
gap> Print(cong); Print("\n");
RightSemigroupCongruence( Semigroup( [ Transformation( [ 2, 1, 1, 2, 1 ] ), 
  Transformation( [ 3, 4, 3, 4, 4 ] ), Transformation( [ 3, 4, 3, 4, 3 ] ), 
  Transformation( [ 4, 3, 3, 4, 4 ] ) ] ), 
[ [ Transformation( [ 3, 4, 3, 4, 3 ] ), Transformation( [ 1, 2, 1, 2, 1 ] ) ]
    , 
  [ Transformation( [ 4, 3, 4, 3, 4 ] ), Transformation( [ 3, 4, 3, 4, 3 ] ) 
     ] ] )
gap> [Transformation([3, 4, 3, 4, 3]), Transformation([1, 2, 1, 2, 1])] in cong;
true
gap> [Transformation([3, 4, 3, 4, 3]), Transformation([1, 2, 1, 2, 1])] in cong;
true
gap> [Transformation([3, 4, 4, 3, 3]), Transformation([1, 2, 2, 1, 1])] in cong;
false
gap> [Transformation([3, 4, 4, 3, 3]), Transformation([1, 2, 2, 1, 1])] in cong;
false
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 4, 5, 6, 7, 8, 3, 9, 10, 11, 3, 3, 12, 13 ]
gap> NonTrivialCongruenceClasses(cong);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `NonTrivialCongruenceClasses' on 1 argum\
ents
gap> NonTrivialEquivalenceClasses(cong);
[ <right congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )> ]
gap> IsLeftSemigroupCongruence(cong);
false
gap> lcong := LeftSemigroupCongruence(S, pair1, pair2);;
gap> IsSubrelation(lcong, cong);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `IsSubrelation' on 2 arguments

# \in: Bad input
gap> S := Semigroup(Transformation([2, 1, 1, 2, 1]),
>                   Transformation([2, 4, 2, 3, 5]),
>                   Transformation([3, 4, 3, 4, 3]),
>                   Transformation([4, 3, 3, 4, 4]));;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> cong := SemigroupCongruence(S, pair1);;
gap> [Transformation([2, 1, 1, 2, 1])] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,

# Classes
gap> S := Semigroup([
> Transformation([1, 5, 4, 2, 1]), Transformation([2, 1, 1, 1, 3])]);;
gap> pair := [Transformation([2, 2, 2, 3, 2]), Transformation([2, 2, 1, 1, 2])];
[ Transformation( [ 2, 2, 2, 3, 2 ] ), Transformation( [ 2, 2, 1, 1, 2 ] ) ]
gap> cong := SemigroupCongruence(S, pair);;
gap> class := CongruenceClassOfElement(cong, Transformation([1, 2, 2, 2, 1]));;
gap> enum := Enumerator(class);;
gap> Transformation([1, 2, 2, 2, 1]) in enum;
true
gap> Transformation([2, 2, 3, 2, 2]) in enum;
true
gap> Transformation([1, 5, 4, 2, 1]) in enum;
false
gap> enum[3] in class;
true
gap> Transformation([1, 5, 4, 2, 1]) in class;
false
gap> Transformation([1, 2, 2, 2, 1]) in class;
true
gap> Transformation([2, 2, 3, 2, 2]) in class;
true
gap> enum[2000];
Error, List Element: <list>[2000] must have an assigned value
gap> cong := SemigroupCongruence(S, pair);;
gap> class := CongruenceClassOfElement(cong, Transformation([1, 2, 2, 2, 1]));;
gap> enum := Enumerator(class);;
gap> x := enum[1];;
gap> EquivalenceRelationCanonicalLookup(cong);;
gap> Position(enum, Transformation([2, 2, 2, 2, 3]));
25
gap> Position(enum, x);
1
gap> cong := SemigroupCongruence(S, pair);;
gap> class := CongruenceClassOfElement(cong, Transformation([1, 2, 2, 2, 1]));;
gap> Transformation([1, 1, 5, 1, 1]) in class;
true
gap> Transformation([6, 2, 3, 4, 1, 1]) in class;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,
gap> Size(class);
89

# Test multithreading in TC
gap> S := DirectProduct(
> FullTransformationMonoid(6), Semigroup(Transformation([2, 1])));
<transformation monoid of degree 8 with 4 generators>
gap> pairs := [[Transformation([1, 1, 1, 1, 1, 1, 8, 7]), Transformation([1, 1,
> 1, 1, 1, 1])],
> [Transformation([1, 1, 1, 1, 1, 1]), Transformation([2, 2, 2, 2, 2, 2])],
> [Transformation([1, 1, 1, 1, 1, 1]), Transformation([3, 3, 3, 3, 3, 3])],
> [Transformation([1, 1, 1, 1, 1, 1]), Transformation([4, 4, 4, 4, 4, 4])],
> [Transformation([1, 1, 1, 1, 1, 1]), Transformation([5, 5, 5, 5, 5, 5])],
> [Transformation([1, 1, 1, 1, 1, 1]), Transformation([6, 6, 6, 6, 6, 6])],
> [Transformation([1, 1, 1, 1, 1, 1, 8, 7]), Transformation([1, 2, 3, 4, 5, 6,
> 8, 7])]];;
gap> cong2 := RightSemigroupCongruence(S, pairs);;
gap> NrEquivalenceClasses(cong2);
1

# Test multithreading in TC
gap> S := DirectProduct(FullTransformationMonoid(6),
>                       Semigroup(Transformation([2, 1])));;
gap> pair1 := [Transformation([1, 2, 3, 4, 5, 6, 8, 7]),
>              IdentityTransformation];;
gap> cong1 := RightSemigroupCongruence(S, pair1);;
gap> NrEquivalenceClasses(cong1);
46656

# Test duplicate generators of semigroup
gap> S := Semigroup(Matrix(IsBooleanMat, [[0, 1], [1, 0]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [1, 1]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [0, 0]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [0, 0]]));;
gap> cong := LeftSemigroupCongruence(S, [S.1, S.2]);;
gap> NrEquivalenceClasses(cong);
4
gap> cong := RightSemigroupCongruence(S, [S.1, S.2]);;
gap> NrEquivalenceClasses(cong);
4
gap> cong := SemigroupCongruence(S, [S.1, S.2]);;
gap> NrEquivalenceClasses(cong);
1
gap> S := Semigroup(Matrix(IsBooleanMat, [[0, 1], [1, 0]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [1, 1]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [0, 0]]));;
gap> cong := LeftSemigroupCongruence(S, [S.1, S.2]);;
gap> NrEquivalenceClasses(cong);
4
gap> cong := RightSemigroupCongruence(S, [S.1, S.2]);;
gap> NrEquivalenceClasses(cong);
4
gap> cong := SemigroupCongruence(S, [S.1, S.2]);;
gap> NrEquivalenceClasses(cong);
1

# Test duplicate generators of semigroup as generating pairs
gap> S := Semigroup(Matrix(IsBooleanMat, [[0, 1], [1, 0]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [1, 1]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [0, 0]]), 
>                   Matrix(IsBooleanMat, [[1, 0], [0, 0]]));;
gap> cong := LeftSemigroupCongruence(S, [S.3, S.4]);;
gap> NrEquivalenceClasses(cong);
16
gap> cong := RightSemigroupCongruence(S, [S.3, S.4]);;
gap> NrEquivalenceClasses(cong);
16
gap> cong := SemigroupCongruence(S, [S.3, S.4]);;
gap> NrEquivalenceClasses(cong);
16

# JoinSemigroupCongruences
gap> S := PartitionMonoid(3);;
gap> pairs1 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, -1, -2, -3], [3]])]];;
gap> pairs2 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, 3, -1, -2], [-3]])],
>               [Bipartition([[1, 2, -1, -2], [3, -3]]),
>                Bipartition([[1, 2, -3], [3, -1, -2]])]];;
gap> cong1 := SemigroupCongruence(S, pairs1);;
gap> cong2 := SemigroupCongruence(S, pairs2);;
gap> cong3 := JoinSemigroupCongruences(cong1, cong2);
<semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 3 generating pairs>
gap> pairs1[1] in cong3;
true
gap> pairs2[1] in cong3;
true
gap> pairs2[2] in cong3;
true
gap> IsSubrelation(cong3, cong1) and IsSubrelation(cong3, cong2);
true
gap> IsSubrelation(cong2, cong1);
false

# JoinLeftSemigroupCongruences
gap> S := PartitionMonoid(3);;
gap> pairs1 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, -1, -2, -3], [3]])]];;
gap> pairs2 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, 3, -1, -2], [-3]])],
>               [Bipartition([[1, 2, -1, -2], [3, -3]]),
>                Bipartition([[1, 2, -3], [3, -1, -2]])]];;
gap> cong1 := LeftSemigroupCongruence(S, pairs1);;
gap> cong2 := LeftSemigroupCongruence(S, pairs2);;
gap> JoinLeftSemigroupCongruences(cong1, cong2);
<left semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 3 generating pairs>
gap> pairs1[1] in cong3;
true
gap> pairs2[1] in cong3;
true
gap> pairs2[2] in cong3;
true
gap> IsSubrelation(cong3, cong1) and IsSubrelation(cong3, cong2);
true
gap> IsSubrelation(cong2, cong1);
false

# JoinRightSemigroupCongruences
gap> S := PartitionMonoid(3);;
gap> pairs1 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, -1, -2, -3], [3]])]];;
gap> pairs2 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, 3, -1, -2], [-3]])],
>               [Bipartition([[1, 2, -1, -2], [3, -3]]),
>                Bipartition([[1, 2, -3], [3, -1, -2]])]];;
gap> cong1 := RightSemigroupCongruence(S, pairs1);;
gap> cong2 := RightSemigroupCongruence(S, pairs2);;
gap> JoinRightSemigroupCongruences(cong1, cong2);
<right semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 3 generating pairs>
gap> pairs1[1] in cong3;
true
gap> pairs2[1] in cong3;
true
gap> pairs2[2] in cong3;
true
gap> IsSubrelation(cong3, cong1) and IsSubrelation(cong3, cong2);
true
gap> IsSubrelation(cong2, cong1);
false

# Test \in for pair of the form [x, x]
gap> S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> cong := SemigroupCongruence(S, [S.1, S.1 * S.2]);
<semigroup congruence over <free band on the generators [ x1, x2, x3 ]> with 
1 generating pairs>
gap> [S.1, S.1] in cong;
true

# EquivalenceRelationPartition
gap> S := PartialBrauerMonoid(2);;
gap> pair := [[Bipartition([[1, 2], [-1], [-2]]),
>              Bipartition([[1, -1], [2], [-2]])]];;
gap> cong := SemigroupCongruence(S, pair);;
gap> EquivalenceRelationPartition(cong);
[ [ <bipartition: [ 1, 2 ], [ -1, -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ -1, -2 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, -1 ], [ -2 ]>, 
      <bipartition: [ 1, 2 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, -2 ], [ 2 ], [ -1 ]>, 
      <bipartition: [ 1 ], [ 2, -2 ], [ -1 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ -1 ], [ -2 ]> ] ]
gap> cong := SemigroupCongruence(S, []);;
gap> EquivalenceRelationPartition(cong) = [];
true
gap> cong := UniversalSemigroupCongruence(S);;
gap> Length(EquivalenceRelationPartition(cong)) = 1;
true

# SemigroupCongruenceByGeneratingPairs bad input
gap> S := FullTransformationMonoid(3);;
gap> SemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2, S.3]]);
Error, Semigroups: SemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of length 2,
gap> SemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2], [S.1, 42]]);
Error, Semigroups: SemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of elements of <S>,
gap> LeftSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2, S.3]]);
Error, Semigroups: LeftSemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of length 2,
gap> LeftSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2], [S.1, 42]]);
Error, Semigroups: LeftSemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of elements of <S>,
gap> RightSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2, S.3]]);
Error, Semigroups: RightSemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of length 2,
gap> RightSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2], [S.1, 42]]);
Error, Semigroups: RightSemigroupCongruenceByGeneratingPairs: usage,
<pairs> must all be lists of elements of <S>,

# SemigroupCongruenceByGeneratingPairs for infinite semigroups
gap> S := Semigroup(
>    [Matrix(IsMaxPlusMatrix, [[-2, 2, 0], [-1, 0, 0], [1, -3, 1]]),
>     Matrix(IsMaxPlusMatrix, [[-infinity, 0, 0], [0, 1, 0], [1, -1, 0]])]);;
gap> pairs := [[S.1, S.2]];;
gap> SemigroupCongruenceByGeneratingPairs(S, pairs);
<semigroup congruence over <semigroup of 3x3 max-plus matrices with 2 
 generators> with 1 generating pairs>
gap> LeftSemigroupCongruenceByGeneratingPairs(S, pairs);
<left semigroup congruence over <semigroup of 3x3 max-plus matrices with 2 
 generators> with 1 generating pairs>
gap> RightSemigroupCongruenceByGeneratingPairs(S, pairs);
<right semigroup congruence over <semigroup of 3x3 max-plus matrices with 2 
 generators> with 1 generating pairs>

# a left congruence over an fp semigroup
gap> F := FreeSemigroup(2);;
gap> S := F / [[F.2 ^ 2, F.2],
>              [F.1 ^ 3, F.1 ^ 2],
>              [F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.1 * F.2) ^ 2 * F.1, F.1],
>              [(F.2 * F.1) ^ 2 * F.2, F.2]];;
gap> pair := [S.1 * S.2 * S.1, S.1];;
gap> cong := LeftSemigroupCongruence(S, pair);;
gap> class := LeftCongruenceClassOfElement(cong, S.1);;
gap> Size(class);
2
gap> Elements(class) = [S.1, S.1 * S.2 * S.1];
true
gap> classes := EquivalenceClasses(cong);;
gap> Length(classes);
8
gap> ForAll(S, x -> Number(classes, c -> x in c) = 1);
true
gap> part := CongruenceByGeneratingPairsPartition(cong);;
gap> SortedList(List(part, Size)) = [1, 1, 1, 1, 1, 2, 2, 2];
true
gap> Set(Flat(part)) = [1 .. Size(S)];
true

# a right congruence over an fp semigroup
gap> F := FreeSemigroup(2);;
gap> S := F / [[F.2 ^ 2, F.2],
>              [F.1 ^ 3, F.1 ^ 2],
>              [F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.1 * F.2) ^ 2 * F.1, F.1],
>              [(F.2 * F.1) ^ 2 * F.2, F.2]];;
gap> pair := [S.1 * S.2 * S.1, S.1];;
gap> cong := RightSemigroupCongruence(S, pair);;
gap> class := RightCongruenceClassOfElement(cong, S.1);;
gap> Elements(class) = [S.1, S.1 * S.2 * S.1];
true
gap> cong2 := RightSemigroupCongruence(S, [[S.1, S.2]]);;
gap> class2 := EquivalenceClassOfElement(cong2, S.1);;
gap> class < class2;
false

# \in: Bad input for an fp semigroup
gap> F := FreeSemigroup(2);;
gap> S := F / [[F.2 ^ 2, F.2],
>              [F.1 ^ 3, F.1 ^ 2],
>              [F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.1 * F.2) ^ 2 * F.1, F.1],
>              [(F.2 * F.1) ^ 2 * F.2, F.2]];;
gap> pair := [S.1 * S.2 * S.1, S.1];;
gap> cong := RightSemigroupCongruence(S, pair);;
gap> [Transformation([2, 1, 1, 2, 1])] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,

# comparing congruence classes over fp semigroups
gap> F := FreeSemigroup(3);;
gap> gens := GeneratorsOfSemigroup(F);;
gap> rels := [];;
gap> x := 0;;
gap> for x in [1 .. Length(gens) - 1] do
> Append(rels, List(gens, y -> [gens[x] * y, y * gens[x]]));
> Add(rels, [gens[x] ^ (x + 1), gens[x]]);
> Add(rels, [gens[x] * gens[Length(gens)], gens[x]]);
> Add(rels, [gens[Length(gens)] * gens[x], gens[x]]);
> od;
gap> S := F / rels;;
gap> sgens := GeneratorsOfSemigroup(S);;
gap> cong := SemigroupCongruenceByGeneratingPairs(S, [[sgens[1], sgens[2]]]);;
gap> class1 := EquivalenceClassOfElement(cong, sgens[3]);;
gap> class2 := EquivalenceClassOfElement(cong, sgens[2]);;
gap> class1 < class2;
false
gap> class2 < class1;
true
gap> class1 > class2;
true
gap> class2 > class1;
false
gap> class1 = class2;
false
gap> class1 <> class2;
true

# creating a congruence over an fp monoid
gap> F := FreeMonoid(2);;
gap> S := F / [[F.2 ^ 2, F.2], [F.1 ^ 3, F.1 ^ 2]];;
gap> SemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2]]);
<semigroup congruence over <fp monoid on the generators [ m1, m2 ]> with 
1 generating pairs>
gap> LeftSemigroupCongruenceByGeneratingPairs(F, [[F.1, F.2]]);
<left semigroup congruence over <free monoid on the generators 
[ m1, m2 ]> with 1 generating pairs>
gap> RightSemigroupCongruenceByGeneratingPairs(F, [[F.1, F.2]]);
<right semigroup congruence over <free monoid on the generators 
[ m1, m2 ]> with 1 generating pairs>
gap> LeftSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2]]);
<left semigroup congruence over <fp monoid on the generators [ m1, m2 ]> with 
1 generating pairs>
gap> RightSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2]]);
<right semigroup congruence over <fp monoid on the generators 
[ m1, m2 ]> with 1 generating pairs>

# SEMIGROUPS_UnbindVariables
gap> Unbind(F);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(class);
gap> Unbind(class1);
gap> Unbind(class2);
gap> Unbind(classes);
gap> Unbind(cong);
gap> Unbind(cong1);
gap> Unbind(cong2);
gap> Unbind(cong3);
gap> Unbind(enum);
gap> Unbind(gens);
gap> Unbind(lcong);
gap> Unbind(pair);
gap> Unbind(pair1);
gap> Unbind(pair2);
gap> Unbind(pairs);
gap> Unbind(pairs1);
gap> Unbind(pairs2);
gap> Unbind(rels);
gap> Unbind(sgens);
gap> Unbind(u);
gap> Unbind(v);
gap> Unbind(x);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congpairs.tst");
