#############################################################################
##
#W  standard/froidure-pin.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/froidure-pin.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test idempotents work with ClosureSemigroup
gap> NrIdempotents(FullTransformationMonoid(6));
1057
gap> S := FullTransformationSemigroup(6);
<full transformation monoid of degree 6>
gap> T := Semigroup(S.1, rec(acting := false));
<commutative transformation semigroup of degree 6 with 1 generator>
gap> Idempotents(T);
[ IdentityTransformation ]
gap> T := ClosureSemigroup(T, [S.2], T!.opts);
<transformation semigroup of degree 6 with 2 generators>
gap> Idempotents(T);
[ IdentityTransformation ]
gap> T := ClosureSemigroup(T, [S.3], T!.opts);
<transformation semigroup of degree 6 with 3 generators>
gap> Idempotents(T);;
gap> Length(last);
1057
gap> D := DClass(T, S.3);;
gap> ID := Idempotents(D);
[ Transformation( [ 1, 2, 3, 4, 5, 2 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 4 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 5 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 1 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 3 ] ), Transformation( [ 3, 2, 3 ] ), 
  Transformation( [ 2, 2 ] ), Transformation( [ 4, 2, 3, 4 ] ), 
  Transformation( [ 6, 2, 3, 4, 5, 6 ] ), Transformation( [ 5, 2, 3, 4, 5 ] ),
  Transformation( [ 1, 4, 3, 4 ] ), Transformation( [ 1, 3, 3 ] ), 
  Transformation( [ 1, 5, 3, 4, 5 ] ), Transformation( [ 1, 1 ] ), 
  Transformation( [ 1, 6, 3, 4, 5, 6 ] ), Transformation( [ 1, 2, 5, 4, 5 ] ),
  Transformation( [ 1, 2, 4, 4 ] ), Transformation( [ 1, 2, 6, 4, 5, 6 ] ), 
  Transformation( [ 1, 2, 2 ] ), Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 1, 2, 3, 6, 5, 6 ] ), Transformation( [ 1, 2, 3, 5, 5 ] ),
  Transformation( [ 1, 2, 3, 1 ] ), Transformation( [ 1, 2, 3, 3 ] ), 
  Transformation( [ 1, 2, 3, 2 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
  Transformation( [ 1, 2, 3, 4, 6, 6 ] ), Transformation( [ 1, 2, 3, 4, 2 ] ),
  Transformation( [ 1, 2, 3, 4, 4 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ) ]
gap> ForAll(ID, x -> IsIdempotent(x) and x in D);
true

# CanComputeFroidurePin for RZMS
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[(), (), ()], [0, (), 0],
>  [(), 0, ()]]);;
gap> CanComputeFroidurePin(S);
true

# CanComputeFroidurePin for a quotient semigroup
gap> S := FullTransformationMonoid(4);;
gap> cong := SemigroupCongruence(S, [S.2, S.3]);;
gap> CanComputeFroidurePin(S / cong);
true
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> cong := SemigroupCongruence(S, [[S.1, S.1]]);;
gap> CanComputeFroidurePin(S / cong);
false

# CanComputeFroidurePin for a free band
gap> CanComputeFroidurePin(FreeBand(4));
true
gap> CanComputeFroidurePin(FreeBand(5));
true

# Test GapFroidurePin
gap> S := FreeBand(5);;
gap> GapFroidurePin(S);
rec( elts := [ x1, x2, x3, x4, x5 ], final := [ 1, 2, 3, 4, 5 ], 
  first := [ 1, 2, 3, 4, 5 ], found := false, gens := [ x1, x2, x3, x4, x5 ], 
  genslookup := [ 1, 2, 3, 4, 5 ], genstoapply := [ 1 .. 5 ], 
  ht := <tree hash table len=12517 used=5 colls=0 accs=10>, 
  left := [ [  ], [  ], [  ], [  ], [  ] ], len := 1, lenindex := [ 1 ], 
  nr := 5, nrrules := 0, one := false, parent := <free band on the generators 
    [ x1, x2, x3, x4, x5 ]>, pos := 1, prefix := [ 0, 0, 0, 0, 0 ], 
  reduced := [ [ false, false, false, false, false ], 
      [ false, false, false, false, false ], 
      [ false, false, false, false, false ], 
      [ false, false, false, false, false ], 
      [ false, false, false, false, false ] ], 
  right := [ [  ], [  ], [  ], [  ], [  ] ], rules := [  ], stopper := false, 
  suffix := [ 0, 0, 0, 0, 0 ], words := [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ] ] 
 )
gap> S := RegularBooleanMatMonoid(3);;
gap> GapFroidurePin(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `GapFroidurePin' on 1 arguments
gap> Size(S);
506
gap> GapFroidurePin(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `GapFroidurePin' on 1 arguments
gap> S := Semigroup(Generators(S), Generators(S));;
gap> GapFroidurePin(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `GapFroidurePin' on 1 arguments

# EnumeratorSorted
gap> S := HallMonoid(3);;
gap> en := EnumeratorSorted(S);
<enumerator of <monoid of size 247, 3x3 boolean matrices with 4 generators>>
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i);
true
gap> ForAll(S, x -> x in en);
true
gap> IsBound(en[100]);
true
gap> IsBound(en[256]);
false
gap> it := Iterator(en);
<iterator>
gap> for x in it do od;
gap> AsList(en) = AsSet(S);
true
gap> S := HallMonoid(3);;
gap> AsSSortedList(S);;
gap> en := EnumeratorSorted(S);;
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i);
true
gap> ForAll(S, x -> x in en);
true
gap> IsBound(en[100]);
true
gap> IsBound(en[256]);
false
gap> AsList(en) = AsSet(S);
true
gap> EnumeratorSorted(FreeBand(2));
[ x1, x2x1x2, x2x1, x2, x1x2, x1x2x1 ]
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> EnumeratorSorted(S);
Error, the 1st argument (a semigroup) is not finite

# IteratorSorted
gap> S := HallMonoid(3);;
gap> it := IteratorSorted(S);
<iterator>
gap> IsDoneIterator(it);
false
gap> valid := true;;
gap> for x in it do 
>   if not x in S then 
>     valid := false;
>   fi;
> od;
gap> valid;
true
gap> IsDoneIterator(it);
true

# gap> NextIterator(it);
# Error, ./bin/include/libsemigroups/froidure-pin-impl.hpp:367:sorted_at: expect\
# ed value in range [0, 247), got 247
gap> it := ShallowCopy(it);;
gap> IsDoneIterator(it);
true
gap> valid := true;;
> for x in it do 
>   if not x in S then 
>     valid := false;
>   fi;
> od;
gap> valid;
true
gap> IsDoneIterator(it);
true

# gap> NextIterator(it);
#  Error, ./bin/include/libsemigroups/froidure-pin-impl.hpp:367:sorted_at: expect\
#  ed value in range [0, 247), got 248
gap> ListIterator(it) = AsSet(S);  # it's done
false
gap> it := ShallowCopy(it);;
gap> ListIterator(it) = AsSet(S);
false
gap> S := HallMonoid(3);;
gap> AsSSortedList(S);;
gap> it := IteratorSorted(S);;
gap> IsDoneIterator(it);
false
gap> valid := true;;
> for x in it do 
>   if not x in S then 
>     valid := false;
>   fi;
> od;
gap> valid;
true
gap> IsDoneIterator(it);
true
gap> NextIterator(it);
Error, List Element: <list>[248] must have an assigned value

# AsListCanonical for CanComputeFroidurePin (without known generators)
gap> S := TriangularBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 6 generators>
gap> I := SemigroupIdeal(S, S.6);;
gap> ForAll(AsListCanonical(I), x -> x in I);
true
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> AsListCanonical(S);
Error, the 1st argument (a semigroup) is not finite

# Enumerator for CanComputeFroidurePin 
gap> S := TriangularBooleanMatMonoid(3);;
gap> Enumerator(S);
<enumerator of <monoid of 3x3 boolean matrices with 6 generators>>

# EnumeratorCanonical for CanComputeFroidurePin 
gap> S := TriangularBooleanMatMonoid(3);;
gap> AsListCanonical(S);;
gap> EnumeratorCanonical(S);;
gap> S := TriangularBooleanMatMonoid(3);;
gap> en := EnumeratorCanonical(S);
<enumerator of <monoid of 3x3 boolean matrices with 6 generators>>
gap> it := Iterator(en);
<iterator>
gap> for x in it do od;
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i);
true
gap> ForAll(S, x -> x in en);
true
gap> IsBound(en[10]);
true
gap> IsBound(en[256]);
false
gap> AsList(en) = AsListCanonical(S);
true
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> en := EnumeratorCanonical(S);
<enumerator of <commutative semigroup of 2x2 max-plus matrices with 1 
 generator>>
gap> en[1];
Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]])
gap> en[10];
Matrix(IsMaxPlusMatrix, [[10, 9], [7, 10]])
gap> en[100];
Matrix(IsMaxPlusMatrix, [[100, 99], [97, 100]])
gap> en[1000];
Matrix(IsMaxPlusMatrix, [[1000, 999], [997, 1000]])
gap> en[20000];
Matrix(IsMaxPlusMatrix, [[20000, 19999], [19997, 20000]])
gap> Length(en);
infinity
gap> IsFinite(S);
false

# EnumeratorCanonical for CanComputeFroidurePin without generators
gap> G := Range(IsomorphismPermGroup(SmallGroup(6, 1)));;
gap> mat := [[G.1, G.2], [G.1 * G.2, G.1], [G.2, G.2]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> CanComputeFroidurePin(S);
true
gap> HasGeneratorsOfSemigroup(S);
false
gap> en := EnumeratorCanonical(S);;
gap> en[1];
(1,(),1)
gap> en[2];
(1,(1,6)(2,5)(3,4),1)
gap> en[15];
(2,(1,2)(3,6)(4,5),2)
gap> en[36];
(2,(1,2)(3,6)(4,5),3)
gap> Length(en);
36
gap> IsBound(en[37]);
false
gap> for x in it do od;
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i);
true
gap> ForAll(S, x -> x in en);
true

# IteratorCanonical
gap> S := UnitriangularBooleanMatMonoid(3);;
gap> it := Iterator(S);
<iterator>
gap> IsDoneIterator(it);
false
gap> valid := true;;
gap> for x in it do 
>   if not x in S then 
>     valid := false;
>   fi;
> od;
gap> valid;
true
gap> IsDoneIterator(it);
true
gap> NextIterator(it);
fail
gap> it := Iterator(S);
<iterator>
gap> IsDoneIterator(it);
false
gap> valid := true;;
gap> for x in it do 
>   if not x in S then 
>     valid := false;
>   fi;
> od;
gap> valid;
true
gap> IsDoneIterator(it);
true
gap> NextIterator(it);
fail
gap> ListIterator(it) = AsListCanonical(S);  # it's done
false
gap> it := Iterator(S); 
<iterator>
gap> ListIterator(it) = AsListCanonical(S);
true
gap> S := HallMonoid(3);;
gap> AsListCanonical(S);;
gap> it := Iterator(S);;
gap> IsDoneIterator(it);
false
gap> valid := true;;
> for x in it do 
>   if not x in S then 
>     valid := false;
>   fi;
> od;
gap> valid;
true
gap> IsDoneIterator(it);
true
gap> NextIterator(it);
Error, List Element: <list>[248] must have an assigned value

# Idempotents
gap> S := UnitriangularBooleanMatMonoid(3);;
gap> Idempotents(S);
[ Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [0, 1, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 1, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 1], [0, 0, 1]]) ]
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> Idempotents(S);
Error, the 1st argument (a semigroup) is not finite

# PositionCanonical, for wrong family, wrong degree
gap> S := UnitriangularBooleanMatMonoid(3);;
gap> PositionCanonical(S, IdentityTransformation);
Error, expected boolean matrix but got transformation (small)!
gap> S := Semigroup(FullTransformationMonoid(3), rec(acting := false));;
gap> PositionCanonical(S, Transformation([1, 1, 1, 1]));
fail

# Position 
gap> S := UnitriangularBooleanMatMonoid(3);;
gap> Position(S, Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]));
1
gap> Enumerate(S);;
gap> Position(S, Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]));
1
gap> Position(S, IdentityTransformation);
Error, expected boolean matrix but got transformation (small)!
gap> S := Semigroup(FullTransformationMonoid(3), rec(acting := false));;
gap> Position(S, Transformation([1, 1, 1, 1]));
fail

# PositionSorted
gap> S := UnitriangularBooleanMatMonoid(3);;
gap> PositionSorted(S, Matrix(IsBooleanMat, 
>                             [[1, 0, 0], [0, 1, 0], [0, 0, 1]]));
1
gap> Enumerate(S);;
gap> PositionSorted(S, Matrix(IsBooleanMat, 
>                             [[1, 0, 0], [0, 1, 0], [0, 0, 1]]));
1
gap> PositionSorted(S, IdentityTransformation);
Error, expected boolean matrix but got transformation (small)!
gap> S := Semigroup(FullTransformationMonoid(3), rec(acting := false));;
gap> PositionSorted(S, Transformation([1, 1, 1, 1]));
fail
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> PositionSorted(S, S.1);
Error, the 1st argument (a semigroup) is not finite

# Left/RightCayleyDigraph, for an infinite CanComputeFroidurePin semigroup
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> RightCayleyDigraph(S);
Error, the 1st argument (a semigroup) is not finite
gap> LeftCayleyDigraph(S);
Error, the 1st argument (a semigroup) is not finite

# Left/RightCayleyGraphSemigroup
gap> S := Semigroup(FullTransformationMonoid(3));;
gap> LeftCayleyGraphSemigroup(S);
[ [ 1, 2, 3, 4 ], [ 2, 5, 8, 9 ], [ 3, 6, 1, 10 ], [ 4, 7, 7, 4 ], 
  [ 5, 1, 6, 14 ], [ 6, 8, 5, 15 ], [ 7, 11, 4, 10 ], [ 8, 3, 2, 16 ], 
  [ 9, 12, 12, 9 ], [ 10, 13, 13, 10 ], [ 11, 4, 11, 22 ], [ 12, 17, 9, 16 ], 
  [ 13, 18, 10, 4 ], [ 14, 19, 19, 14 ], [ 15, 20, 20, 15 ], 
  [ 16, 21, 21, 16 ], [ 17, 9, 17, 26 ], [ 18, 10, 18, 26 ], 
  [ 19, 23, 14, 15 ], [ 20, 24, 15, 14 ], [ 21, 25, 16, 9 ], 
  [ 22, 22, 22, 22 ], [ 23, 14, 23, 27 ], [ 24, 15, 24, 22 ], 
  [ 25, 16, 25, 27 ], [ 26, 26, 26, 26 ], [ 27, 27, 27, 27 ] ]
gap> RightCayleyGraphSemigroup(S);
[ [ 1, 2, 3, 4 ], [ 2, 5, 6, 7 ], [ 3, 8, 1, 7 ], [ 4, 9, 10, 4 ], 
  [ 5, 1, 8, 11 ], [ 6, 3, 2, 11 ], [ 7, 12, 13, 7 ], [ 8, 6, 5, 4 ], 
  [ 9, 14, 15, 10 ], [ 10, 16, 4, 10 ], [ 11, 17, 18, 11 ], 
  [ 12, 19, 20, 13 ], [ 13, 21, 7, 13 ], [ 14, 4, 16, 22 ], [ 15, 10, 9, 22 ],
  [ 16, 15, 14, 4 ], [ 17, 23, 24, 18 ], [ 18, 25, 11, 18 ], 
  [ 19, 7, 21, 22 ], [ 20, 13, 12, 22 ], [ 21, 20, 19, 7 ], 
  [ 22, 26, 26, 22 ], [ 23, 11, 25, 22 ], [ 24, 18, 17, 22 ], 
  [ 25, 24, 23, 11 ], [ 26, 27, 22, 26 ], [ 27, 22, 27, 22 ] ]

# AsSet, for an infinite CanComputeFroidurePin semigroup
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> AsSet(S);
Error, the 1st argument (a semigroup) is not finite

# Size, for an infinite CanComputeFroidurePin semigroup
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> Size(S);
infinity

# MultiplicationTable
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> MultiplicationTable(S);
Error, the first argument (a semigroup) must be finite,
gap> S := RegularBooleanMatMonoid(1);
<commutative monoid of 1x1 boolean matrices with 1 generator>
gap> MultiplicationTable(S);
[ [ 1, 1 ], [ 1, 2 ] ]

# NrIdempotents
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> NrIdempotents(S);
Error, the 1st argument (a semigroup) is not finite
gap> S := RegularBooleanMatMonoid(1);;
gap> NrIdempotents(S);
2
gap> S := RegularBooleanMatMonoid(1);;
gap> Idempotents(S);;
gap> NrIdempotents(S);
2

# MinimalFactorization
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> MinimalFactorization(S, Matrix(IsMaxPlusMatrix, [[100, 99], [97, 100]]));
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> S := FullTransformationMonoid(3);;
gap> MinimalFactorization(S, Transformation([1, 1, 1]));
[ 4, 2, 2, 4 ]
gap> MinimalFactorization(S, Transformation([1, 1, 1, 1]));
Error, the 2nd argument (a mult. element) is not an element of the first argum\
ent (a semigroup)

# IsFinite
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> IsFinite(S);
false
gap> S := Semigroup(Matrix(IsIntegerMatrix, [[1, 0], [0, -1]]));;
gap> IsFinite(S);
true

# MultiplicationTable
gap> S := ReesMatrixSemigroup(Group([(1, 2)]), [[(), (1, 2)], [(), ()]]);;
gap> MultiplicationTable(S);
[ [ 1, 2, 3, 4, 3, 4, 1, 2 ], [ 1, 2, 3, 4, 1, 2, 3, 4 ], 
  [ 3, 4, 1, 2, 1, 2, 3, 4 ], [ 3, 4, 1, 2, 3, 4, 1, 2 ], 
  [ 5, 6, 7, 8, 7, 8, 5, 6 ], [ 5, 6, 7, 8, 5, 6, 7, 8 ], 
  [ 7, 8, 5, 6, 5, 6, 7, 8 ], [ 7, 8, 5, 6, 7, 8, 5, 6 ] ]

# PositionCanonical (for a group)
gap> G := Group((1, 2, 3), (1, 2));;
gap> CanComputeCppFroidurePin(G);
false

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/froidure-pin.tst");
