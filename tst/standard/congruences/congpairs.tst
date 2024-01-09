#############################################################################
##
#W  standard/congruences/congpairs.tst
#Y  Copyright (C) 2014-2022                                 Wilf A. Wilson
##                                                          Michael Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local C, F, M, S, T, ccong, class, class1, class2, classes, cong, cong1
#@local cong2, cong3, enum, gens, lcong, pair, pair1, pair2, pairs, pairs1
#@local pairs2, part, part1, rels, sgens, u, v, x
gap> START_TEST("Semigroups package: standard/congruences/congpairs.tst");
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
<2-sided semigroup congruence over <free semigroup on the generators 
[ s1 ]> with 1 generating pairs>

# The next test is now valid (but would run forever)
#gap> NonTrivialEquivalenceClasses(cong);
#Error, no method found! For debugging hints type ?Recovery from NoMethodFound
#Error, no 2nd choice method found for `NonTrivialEquivalenceClasses' on 1 argu\
#ments
gap> gens in cong;
true
gap> EquivalenceRelationLookup(cong);
Error, the argument (a 2-sided congruence) must have finite range
gap> NrEquivalenceClasses(cong);
3
gap> class := EquivalenceClassOfElement(cong, x);;
gap> cong2 := SemigroupCongruence(S, [x ^ 2, x ^ 2]);;
gap> class := EquivalenceClassOfElement(cong2, x);;
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
gap> v := SemigroupCongruence(T, [gens[1], gens[1]]);;
gap> Size(GeneratingPairsOfSemigroupCongruence(v));
0
gap> NrEquivalenceClasses(v);
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
<2-sided semigroup congruence over <commutative non-regular transformation 
 monoid of size 6, degree 10 with 1 generator> with 5 generating pairs>
gap> u = v;
true
gap> NrEquivalenceClasses(u);
1

# PairsCongTest4: \* for two semigroup congruence classes
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5])];;
gap> S := Semigroup(gens);;
gap> gens := List(S, x -> [gens[1], x]);;
gap> u := SemigroupCongruence(S, gens);;  # universal congruence
gap> HasGeneratingPairsOfMagmaCongruence(u);
true
gap> Size(GeneratingPairsOfSemigroupCongruence(u));
4
gap> u = UniversalSemigroupCongruence(S);
true
gap> v := SemigroupCongruence(S, [gens[1], gens[1]]);;  # trivial congruence
gap> HasGeneratingPairsOfMagmaCongruence(v);
true
gap> Size(GeneratingPairsOfSemigroupCongruence(v));
0
gap> classes := Set(EquivalenceClasses(v));
[ <2-sided congruence class of Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1,
      5 ] )>, <2-sided congruence class of Transformation( [ 6, 9, 9, 6, 9, 1,
     1, 2, 2, 6 ] )>, <2-sided congruence class of Transformation( [ 9, 1, 1,
      9, 1, 2, 2, 6, 6, 9 ] )>, 
  <2-sided congruence class of Transformation( [ 1, 2, 2, 1, 2, 6, 6, 9, 9,
      1 ] )>, <2-sided congruence class of Transformation( [ 2, 6, 6, 2, 6, 9,
     9, 1, 1, 2 ] )> ]
gap> EquivalenceClasses(u)[1] in classes;
false
gap> classes[1] * EquivalenceClasses(u)[1];
Error, the arguments (cong. classes) are not classes of the same congruence
gap> EquivalenceClasses(u)[1] * classes[1];
Error, the arguments (cong. classes) are not classes of the same congruence
gap> classes[3] * classes[4];
<2-sided congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6,
  9 ] )>
gap> classes[4] * classes[3];
<2-sided congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6,
  9 ] )>
gap> EquivalenceClassOfElement(v, Representative(classes[5] * classes[2])) =
> EquivalenceClassOfElement(v,
>                          Representative(classes[5]) *
>                          Representative(classes[2]));
true
gap> classes[3] = classes[3];
true
gap> classes[2] = classes[3];
false
gap> AsList(classes[4]);
[ Transformation( [ 1, 2, 2, 1, 2, 6, 6, 9, 9, 1 ] ) ]
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
gap> cong := SemigroupCongruence(S, pair1, pair2);;
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
gap> NonTrivialEquivalenceClasses(cong);
[ <2-sided congruence class of Transformation( [ 2, 1, 1, 2, 1 ] )> ]
gap> classes := EquivalenceClasses(cong);
[ <2-sided congruence class of Transformation( [ 2, 1, 1, 2, 1 ] )>, 
  <2-sided congruence class of Transformation( [ 1, 3, 4, 1, 3 ] )> ]
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
Error, the 2nd argument (a mult. elt.) does not belong to the range of the 1st\
 argument (a 2-sided congruence)
gap> JoinSemigroupCongruences(cong, cong2);
Error, cannot form the join of congruences over different semigroups
gap> IsSubrelation(cong, cong2);
false
gap> cong := LeftSemigroupCongruence(S, pair1, pair2);;
gap> IsSubrelation(cong2, cong);
false
gap> cong := RightSemigroupCongruence(S, pair1, pair2);;
gap> IsSubrelation(cong2, cong);
false

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
gap> EquivalenceClasses(cong);
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
Error, the 1st argument (a list) does not have length 2
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, the items in the 1st argument (a list) do not all belong to the range o\
f the 2nd argument (a 2-sided semigroup congruence)

# Classes
gap> S := Semigroup([
> Transformation([1, 5, 4, 2, 1]), Transformation([2, 1, 1, 1, 3])]);;
gap> pair := [Transformation([2, 2, 2, 3, 2]),
>             Transformation([2, 2, 1, 1, 2])];;
gap> cong := SemigroupCongruence(S, pair);;
gap> class := EquivalenceClassOfElement(cong, Transformation([1, 2, 2, 2, 1]));;
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
gap> class := EquivalenceClassOfElement(cong, Transformation([1, 2, 2, 2, 1]));;
gap> enum := Enumerator(class);;
gap> x := enum[1];;
gap> EquivalenceRelationCanonicalLookup(cong);;
gap> Position(enum, Transformation([2, 2, 2, 2, 3]));
25
gap> Position(enum, x);
1
gap> cong := SemigroupCongruence(S, pair);;
gap> class := EquivalenceClassOfElement(cong, Transformation([1, 2, 2, 2, 1]));;
gap> Transformation([1, 1, 5, 1, 1]) in class;
true
gap> Transformation([6, 2, 3, 4, 1, 1]) in class;
false
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
<2-sided semigroup congruence over <regular bipartition *-monoid of size 203, 
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
<2-sided semigroup congruence over <free band on the generators 
[ x1, x2, x3 ]> with 1 generating pairs>
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
gap> IsEmpty(EquivalenceRelationPartition(cong));
true
gap> cong := UniversalSemigroupCongruence(S);;
gap> Length(EquivalenceRelationPartition(cong)) = 1;
true

# SemigroupCongruenceByGeneratingPairs bad input
gap> S := FullTransformationMonoid(3);;
gap> x := Transformation([1, 1, 1, 1]);;
gap> SemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2, S.3]]);
Error, the 2nd argument <pairs> must consist of lists of length 2
gap> SemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2], [S.1, x]]);
Error, the 2nd argument <pairs> must consist of lists of elements of the 1st a\
rgument <S> (a semigroup)
gap> LeftSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2, S.3]]);
Error, the 2nd argument <pairs> must consist of lists of length 2
gap> LeftSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2], [S.1, x]]);
Error, the 2nd argument <pairs> must consist of lists of elements of the 1st a\
rgument <S> (a semigroup)
gap> RightSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2, S.3]]);
Error, the 2nd argument <pairs> must consist of lists of length 2
gap> RightSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2], [S.1, x]]);
Error, the 2nd argument <pairs> must consist of lists of elements of the 1st a\
rgument <S> (a semigroup)

# SemigroupCongruenceByGeneratingPairs for infinite semigroups
gap> S := Semigroup(
>    [Matrix(IsMaxPlusMatrix, [[-2, 2, 0], [-1, 0, 0], [1, -3, 1]]),
>     Matrix(IsMaxPlusMatrix, [[-infinity, 0, 0], [0, 1, 0], [1, -1, 0]])]);;
gap> pairs := [[S.1, S.2]];;
gap> SemigroupCongruenceByGeneratingPairs(S, pairs);
<2-sided semigroup congruence over <semigroup of 3x3 max-plus matrices with 2 
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
gap> class := EquivalenceClassOfElement(cong, S.1);;
gap> Size(class);
2
gap> Elements(class) = [S.1, S.1 * S.2 * S.1];
true
gap> classes := EquivalenceClasses(cong);;
gap> Length(classes);
8
gap> ForAll(S, x -> Number(classes, c -> x in c) = 1);
true
gap> part := EquivalenceRelationPartitionWithSingletons(cong);;
gap> SortedList(List(part, Size)) = [1, 1, 1, 1, 1, 2, 2, 2];
true
gap> Set(Flat(part)) = Set(S);
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
gap> class := EquivalenceClassOfElement(cong, S.1);;
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
Error, the 1st argument (a list) does not have length 2
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, the items in the 1st argument (a list) do not all belong to the range o\
f the 2nd argument (a right semigroup congruence)

# comparing congruence classes over fp semigroups
gap> F := FreeSemigroup(3);;
gap> gens := GeneratorsOfSemigroup(F);;
gap> rels := [];;
gap> x := 0;;
gap> for x in [1 .. Length(gens) - 1] do
> Append(rels, List(gens, y -> [gens[x] * y, y * gens[x]]));
> Add(rels, [gens[x] ^ (x + 1), gens[x]]);
> Add(rels, [gens[x] * Last(gens), gens[x]]);
> Add(rels, [Last(gens) * gens[x], gens[x]]);
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
<2-sided semigroup congruence over <fp monoid with 2 generators and 
  2 relations of length 10> with 1 generating pairs>
gap> LeftSemigroupCongruenceByGeneratingPairs(F, [[F.1, F.2]]);
<left semigroup congruence over <free monoid on the generators 
[ m1, m2 ]> with 1 generating pairs>
gap> RightSemigroupCongruenceByGeneratingPairs(F, [[F.1, F.2]]);
<right semigroup congruence over <free monoid on the generators 
[ m1, m2 ]> with 1 generating pairs>
gap> LeftSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2]]);
<left semigroup congruence over <fp monoid with 2 generators and 
  2 relations of length 10> with 1 generating pairs>
gap> RightSemigroupCongruenceByGeneratingPairs(S, [[S.1, S.2]]);
<right semigroup congruence over <fp monoid with 2 generators and 
  2 relations of length 10> with 1 generating pairs>

# tests from congfpmon.tst
gap> F := FreeMonoid(2);;
gap> M := F / [[F.2 ^ 2, F.2], [F.1 ^ 3, F.1 ^ 2]];;
gap> cong := SemigroupCongruenceByGeneratingPairs(M, [[M.2]]);
Error, the 2nd argument <pairs> must consist of lists of length 2
gap> cong := SemigroupCongruenceByGeneratingPairs(M, [[M.1, F.1]]);
Error, the 2nd argument <pairs> must consist of lists of elements of the 1st a\
rgument <S> (a semigroup)
gap> cong := LeftSemigroupCongruenceByGeneratingPairs(M, [[M.2]]);
Error, the 2nd argument <pairs> must consist of lists of length 2
gap> cong := LeftSemigroupCongruenceByGeneratingPairs(M, [[M.1, F.1]]);
Error, the 2nd argument <pairs> must consist of lists of elements of the 1st a\
rgument <S> (a semigroup)
gap> cong := RightSemigroupCongruenceByGeneratingPairs(M, [[M.2]]);
Error, the 2nd argument <pairs> must consist of lists of length 2
gap> cong := RightSemigroupCongruenceByGeneratingPairs(M, [[M.1, F.1]]);
Error, the 2nd argument <pairs> must consist of lists of elements of the 1st a\
rgument <S> (a semigroup)
gap> cong := SemigroupCongruenceByGeneratingPairs(M, [[M.1, M.2]]);;
gap> [M.1, M.2, M.2 ^ 2] in cong;
Error, the 1st argument (a list) does not have length 2
gap> [F.1, F.2] in cong;
Error, the items in the 1st argument (a list) do not all belong to the range o\
f the 2nd argument (a 2-sided semigroup congruence)
gap> EquivalenceClassOfElement(cong, Transformation([1, 2, 1]));
Error, the 2nd argument (a mult. elt.) does not belong to the range of the 1st\
 argument (a 2-sided congruence)

# A 2-sided example
gap> F := FreeMonoid(2);;
gap> M := F / [[F.2 ^ 2, F.2],
>              [F.1 ^ 3, F.1 ^ 2],
>              [F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.2 * F.1) ^ 2 * F.2, F.2]];;
gap> Size(M);
13
gap> M.1 ^ 2 = M.2 * M.1;
false
gap> (M.2 * M.1) ^ 2 * M.2 * M.1 ^ 2 = M.1 ^ 3;
true
gap> pair := [M.1 ^ 2 * M.2 * M.1, M.1 * M.2 * M.1];;
gap> cong := SemigroupCongruence(M, pair);
<2-sided semigroup congruence over <fp monoid with 2 generators and 
  5 relations of length 30> with 1 generating pairs>
gap> NrEquivalenceClasses(cong);
3
gap> [M.2, M.2 * M.1] in cong;
true
gap> part := EquivalenceRelationPartition(cong);;
gap> Length(part) = 1;
true
gap> Length(part[1]) = 11;
true
gap> Size(EquivalenceRelationCanonicalPartition(cong));
1
gap> Size(EquivalenceRelationCanonicalPartition(cong)[1]);
11
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 ]
gap> ImagesElm(cong, GeneratorsOfMonoid(M)[1]);
[ m1 ]
gap> ImagesElm(cong, One(M));
[ <identity ...> ]
gap> classes := EquivalenceClasses(cong);;
gap> SortedList(List(classes, Size));
[ 1, 1, 11 ]
gap> gens := GeneratorsOfMonoid(M);;
gap> class1 := EquivalenceClassOfElement(cong, gens[2] * gens[1]);;
gap> class2 := EquivalenceClassOfElement(cong, gens[1]);;
gap> gens[1] ^ 2 in class1;
true
gap> gens[1] in class1;
false
gap> class1 = class2;
false
gap> enum := Enumerator(class1);;
gap> AsSSortedList(enum);
[ m2, m1^2, m1*m2, m2*m1, m1^2*m2, m1*m2*m1, m2*m1*m2, m1^2*m2*m1, (m1*m2)^2, 
  (m2*m1)^2, (m1*m2)^2*m1 ]
gap> Size(enum);
11
gap> class1 * class2 = EquivalenceClassOfElement(cong, gens[2] ^ 20 * gens[1] ^ 42);
true
gap> class1 * class2 = EquivalenceClassOfElement(cong, One(M));
false

# A left congruence example
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 * F.2 ^ 2, F.2 ^ 2],
>              [F.2 ^ 3, F.2 ^ 2],
>              [F.1 ^ 4, F.1],
>              [F.2 * F.1 ^ 2 * F.2, F.2 ^ 2],
>              [F.2 * F.1 ^ 3 * F.2, F.2],
>              [(F.2 * F.1) ^ 2 * F.2, F.2],
>              [F.2 ^ 2 * F.1 ^ 3, F.2 ^ 2],
>              [F.2 * (F.2 * F.1) ^ 2, F.2 ^ 2 * F.1 ^ 2]];;
gap> Size(M);
40
gap> cong := LeftSemigroupCongruence(M, [M.1, M.2 ^ 3]);
<left semigroup congruence over <fp monoid with 2 generators and 
  8 relations of length 51> with 1 generating pairs>
gap> IsLeftSemigroupCongruence(cong);
true
gap> HasIsSemigroupCongruence(cong);
false
gap> NrEquivalenceClasses(cong);
11
gap> [M.1 ^ 9, M.2 * M.1 ^ 3 * M.2 * M.1] in cong;
true
gap> part := EquivalenceRelationPartition(cong);;
gap> Length(part) = 1;
true
gap> Length(part[1]) = 30;
true
gap> part := EquivalenceRelationCanonicalPartition(cong);;
gap> Size(part);
1
gap> Size(part[1]);
30
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 2, 4, 2, 2, 2, 5, 2, 2, 6, 2, 7, 2, 2, 8, 2, 2, 2, 9, 2, 2, 10, 2, 
  2, 2, 2, 11, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ]
gap> Set(ImagesElm(cong, M.1)) = part[1];
true
gap> ImagesElm(cong, One(M));
[ <identity ...> ]
gap> classes := EquivalenceClasses(cong);;
gap> SortedList(List(classes, Size));
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 30 ]
gap> class1 := EquivalenceClassOfElement(cong, M.2 * M.1 ^ 2);;
gap> class2 := EquivalenceClassOfElement(cong, M.2);;
gap> M.1 in class1;
true
gap> M.2 in class1;
false
gap> class1 = class2;
false
gap> enum := Enumerator(class1);;
gap> M.2 * M.1 in enum;
true
gap> M.2 * M.1 ^ 3 in enum;
true
gap> M.2 * M.1 * M.2 * M.1 in enum;
true
gap> Size(enum);
30

# A right congruence example
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 * F.2 ^ 2, F.2 ^ 2],
>              [F.2 ^ 3, F.2 ^ 2],
>              [F.1 ^ 4, F.1],
>              [F.2 * F.1 ^ 2 * F.2, F.2 ^ 2],
>              [F.2 * F.1 ^ 3 * F.2, F.2],
>              [(F.2 * F.1) ^ 2 * F.2, F.2],
>              [F.2 ^ 2 * F.1 ^ 3, F.2 ^ 2],
>              [F.2 * (F.2 * F.1) ^ 2, F.2 ^ 2 * F.1 ^ 2]];;
gap> Size(M);
40
gap> cong := RightSemigroupCongruence(M, [M.1, M.2 ^ 3]);
<right semigroup congruence over <fp monoid with 2 generators and 
  8 relations of length 51> with 1 generating pairs>
gap> IsRightSemigroupCongruence(cong);
true
gap> HasIsSemigroupCongruence(cong);
false
gap> NrEquivalenceClasses(cong);
13
gap> [M.1 ^ 9, M.2 * M.1 ^ 3 * M.2 * M.1] in cong;
false
gap> [M.2 * M.1 * M.2 ^ 2, M.1 ^ 4] in cong;
true
gap> part := EquivalenceRelationCanonicalPartition(cong);;
gap> Length(part) = 4;
true
gap> Set(part, Length) = [4, 8, 11];
true
gap> part := EquivalenceRelationCanonicalPartition(cong);;
gap> Length(part);
4
gap> SortedList(List(part, Length));
[ 4, 8, 8, 11 ]
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 4, 2, 5, 2, 6, 7, 4, 8, 9, 4, 2, 6, 6, 7, 10, 11, 6, 7, 4, 2, 2, 
  2, 6, 12, 6, 7, 4, 4, 2, 13, 2, 6, 6, 4, 2, 2, 4 ]
gap> part1 := First(part, l -> M.1 in l);;
gap> Set(ImagesElm(cong, M.1)) = part1;
true
gap> NrEquivalenceClasses(cong);
13
gap> ImagesElm(cong, One(M));
[ <identity ...> ]
gap> classes := EquivalenceClasses(cong);;
gap> SortedList(List(classes, Size));
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 8, 8, 11 ]
gap> class1 := EquivalenceClassOfElement(cong, M.1 * (M.2 * M.1) ^ 2 * M.2);;
gap> class2 := EquivalenceClassOfElement(cong, M.2 ^ 2 * M.1);;
gap> M.1 in class1;
true
gap> M.2 in class1;
false
gap> class1 = class2;
false
gap> enum := Enumerator(class1);;
gap> M.2 ^ 2 in enum;
true
gap> M.1 * (M.1 * M.2) ^ 2 * M.1 ^ 3 in enum;
true
gap> enum[Position(enum, M.1 * (M.1 * M.2) ^ 2 * M.1 ^ 3)]
> = M.1 * (M.1 * M.2) ^ 2 * M.1 ^ 3;
true
gap> Size(enum);
11

# Joining two congs together
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 ^ 4, F.1 ^ 3],
>              [F.1 ^ 3 * F.2, F.1 ^ 3],
>              [F.1 * F.2 ^ 2 * F.1, F.1 ^ 2],
>              [F.1 * F.2 ^ 3, F.1],
>              [F.2 * F.1 ^ 3, F.1 ^ 3],
>              [F.2 ^ 3 * F.1, F.1],
>              [F.2 ^ 4, F.2],
>              [F.1 ^ 2 * F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.1 * F.2) ^ 2 * F.1, F.1],
>              [(F.2 * F.1) ^ 2 * F.1, F.1 * F.2 * F.1 ^ 2]];;
gap> Size(M);
39
gap> cong1 := SemigroupCongruence(M, [M.1, M.2]);;
gap> cong2 := SemigroupCongruence(M, [M.1, M.1 ^ 2]);;
gap> cong1 = cong2;
false
gap> IsSubrelation(cong1, cong2);
true
gap> JoinSemigroupCongruences(cong1, cong2) = cong1;
true
gap> M := F / [[F.1, F.2]];;
gap> cong3 := SemigroupCongruence(M, [M.1, M.2 ^ 10]);;
gap> JoinSemigroupCongruences(cong1, cong3);
Error, cannot form the join of congruences over different semigroups

# Joining two left congs together
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 ^ 4, F.1 ^ 3],
>              [F.1 ^ 3 * F.2, F.1 ^ 3],
>              [F.1 * F.2 ^ 2 * F.1, F.1 ^ 2],
>              [F.1 * F.2 ^ 3, F.1],
>              [F.2 * F.1 ^ 3, F.1 ^ 3],
>              [F.2 ^ 3 * F.1, F.1],
>              [F.2 ^ 4, F.2],
>              [F.1 ^ 2 * F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.1 * F.2) ^ 2 * F.1, F.1],
>              [(F.2 * F.1) ^ 2 * F.1, F.1 * F.2 * F.1 ^ 2]];;
gap> Size(M);
39
gap> cong1 := LeftSemigroupCongruence(M, [M.1, M.2]);;
gap> cong2 := LeftSemigroupCongruence(M, [M.1, M.1 ^ 2]);;
gap> cong1 = cong2;
false
gap> IsSubrelation(cong1, cong2);
true
gap> JoinLeftSemigroupCongruences(cong1, cong2) = cong1;
true
gap> M := F / [[F.1, F.2]];;
gap> cong3 := SemigroupCongruence(M, [M.1, M.2 ^ 10]);;
gap> JoinLeftSemigroupCongruences(cong1, cong3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `JoinLeftSemigroupCongruences' on 2 argu\
ments

# Joining two right congs together
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 ^ 4, F.1 ^ 3],
>              [F.1 ^ 3 * F.2, F.1 ^ 3],
>              [F.1 * F.2 ^ 2 * F.1, F.1 ^ 2],
>              [F.1 * F.2 ^ 3, F.1],
>              [F.2 * F.1 ^ 3, F.1 ^ 3],
>              [F.2 ^ 3 * F.1, F.1],
>              [F.2 ^ 4, F.2],
>              [F.1 ^ 2 * F.2 * F.1 ^ 2, F.1 ^ 2],
>              [F.1 * (F.1 * F.2) ^ 2, F.1 ^ 2 * F.2 * F.1],
>              [(F.1 * F.2) ^ 2 * F.1, F.1],
>              [(F.2 * F.1) ^ 2 * F.1, F.1 * F.2 * F.1 ^ 2]];;
gap> Size(M);
39
gap> cong1 := RightSemigroupCongruence(M, [M.1, M.2]);;
gap> cong2 := RightSemigroupCongruence(M, [M.1, M.1 ^ 2]);;
gap> cong1 = cong2;
false
gap> IsSubrelation(cong1, cong2);
true
gap> JoinRightSemigroupCongruences(cong1, cong2) = cong1;
true
gap> M := F / [[F.1, F.2]];;
gap> cong3 := SemigroupCongruence(M, [M.1, M.2 ^ 10]);;
gap> JoinRightSemigroupCongruences(cong1, cong3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `JoinRightSemigroupCongruences' on 2 arg\
uments

# AsSemigroupCongruenceByGeneratingPairs
gap> S := ReesMatrixSemigroup(SymmetricGroup(3), [[(1, 2), ()], [(), (1, 3)]]);
<Rees matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
gap> cong := RMSCongruenceByLinkedTriple(S, Group((1, 2, 3)),
>                                        [[1], [2]], [[1, 2]]);;
gap> ccong := AsSemigroupCongruenceByGeneratingPairs(cong);;
gap> C := UniversalSemigroupCongruence(S);;
gap> AsSemigroupCongruenceByGeneratingPairs(C);
<semigroup congruence over <Rees matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
  with linked triple (S3,1,1)>
gap> S := InverseSemigroup([PartialPerm([1, 2], [1, 2]),
>                           PartialPerm([1, 2], [2, 3])]);;
gap> pairs := [PartialPerm([], []), PartialPerm([1], [1])];;
gap> C := SemigroupCongruence(S, pairs);
<2-sided semigroup congruence over <inverse partial perm semigroup 
 of size 14, rank 3 with 2 generators> with 1 generating pairs>
gap> AsSemigroupCongruenceByGeneratingPairs(C);
<2-sided semigroup congruence over <inverse partial perm semigroup 
 of size 14, rank 3 with 2 generators> with 1 generating pairs>
gap> C := RightSemigroupCongruence(S, pairs);
<right semigroup congruence over <inverse partial perm semigroup of size 14, 
 rank 3 with 2 generators> with 1 generating pairs>
gap> AsRightSemigroupCongruenceByGeneratingPairs(C);
<right semigroup congruence over <inverse partial perm semigroup of size 14, 
 rank 3 with 2 generators> with 1 generating pairs>
gap> C := LeftSemigroupCongruence(S, pairs);
<left semigroup congruence over <inverse partial perm semigroup of size 14, 
 rank 3 with 2 generators> with 1 generating pairs>
gap> AsLeftSemigroupCongruenceByGeneratingPairs(C);
<left semigroup congruence over <inverse partial perm semigroup of size 14, 
 rank 3 with 2 generators> with 1 generating pairs>

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congruences/congpairs.tst");
