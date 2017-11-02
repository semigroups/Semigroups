#############################################################################
##
#W  standard/fropin.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/fropin.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test idempotents work with ClosureSemigroupDestructive
gap> NrIdempotents(FullTransformationMonoid(6));
1057
gap> S := FullTransformationSemigroup(6);
<full transformation monoid of degree 6>
gap> T := Semigroup(S.1, rec(acting := false));
<commutative transformation semigroup of degree 6 with 1 generator>
gap> EN_SEMI_IDEMPOTENTS(T);
[ IdentityTransformation ]
gap> T := SEMIGROUPS.ClosureSemigroupDestructive(T, [S.2], T!.opts);
<transformation semigroup of degree 6 with 2 generators>
gap> EN_SEMI_IDEMPOTENTS(T);
[ IdentityTransformation ]
gap> T := SEMIGROUPS.ClosureSemigroupDestructive(T, [S.3], T!.opts);
<transformation semigroup of degree 6 with 3 generators>
gap> EN_SEMI_IDEMPOTENTS(T);;
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

# IsGeneratorsOfEnumerableSemigroup for RZMS
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[(), (), ()], [0, (), 0],
>  [(), 0, ()]]);;
gap> HasIsGeneratorsOfEnumerableSemigroup(S);
true
gap> IsGeneratorsOfEnumerableSemigroup(S);
true

# IsGeneratorsOfEnumerableSemigroup for RZMS element coll
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[(), (), ()], [0, (), 0],
>  [(), 0, ()]]);;
gap> coll := AsSet(S){[1 .. 10]};;
gap> IsGeneratorsOfEnumerableSemigroup(coll);
true

# IsGeneratorsOfEnumerableSemigroup for a quotient semigroup
gap> S := FullTransformationMonoid(4);;
gap> cong := SemigroupCongruence(S, [S.2, S.3]);;
gap> IsGeneratorsOfEnumerableSemigroup(S / cong);
true
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> cong := SemigroupCongruence(S, [[S.1, S.1]]);;
gap> IsGeneratorsOfEnumerableSemigroup(S / cong);
false

# IsGeneratorsOfEnumerableSemigroup for a free band
gap> IsGeneratorsOfEnumerableSemigroup(FreeBand(4));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `[]' on 2 arguments
gap> IsGeneratorsOfEnumerableSemigroup(FreeBand(5));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `[]' on 2 arguments

# Test FROPIN
gap> S := FreeBand(5);;
gap> FROPIN(S);
Error, Semigroups: FROPIN: usage,
the argument must be a semigroup with at least 1 generator,
gap> S := RegularBooleanMatMonoid(3);;
gap> FROPIN(S);
rec( batch_size := 8192, 
  elts := [ Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]), 
      Matrix(IsBooleanMat, [[0, 1, 0], [1, 0, 0], [0, 0, 1]]), 
      Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 1], [1, 0, 0]]), 
      Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [1, 0, 1]]), 
      Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 0]]) ], 
  final := [ 1, 2, 3, 4, 5 ], first := [ 1, 2, 3, 4, 5 ], found := false, 
  gens := [ Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]), 
      Matrix(IsBooleanMat, [[0, 1, 0], [1, 0, 0], [0, 0, 1]]), 
      Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 1], [1, 0, 0]]), 
      Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [1, 0, 1]]), 
      Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 0]]) ], 
  genslookup := [ 1, 2, 3, 4, 5 ], genstoapply := [ 1 .. 5 ], 
  ht := <tree hash table len=12517 used=5 colls=0 accs=10>, 
  left := [ [  ], [  ], [  ], [  ], [  ] ], len := 1, lenindex := [ 1 ], 
  nr := 5, nrrules := 0, one := 1, 
  parent := <monoid of 3x3 boolean matrices with 4 generators>, pos := 1, 
  prefix := [ 0, 0, 0, 0, 0 ], 
  reduced := [ [ false, false, false, false, false ], 
      [ false, false, false, false, false ], 
      [ false, false, false, false, false ], 
      [ false, false, false, false, false ], 
      [ false, false, false, false, false ] ], report := false, 
  right := [ [  ], [  ], [  ], [  ], [  ] ], rules := [  ], stopper := false, 
  suffix := [ 0, 0, 0, 0, 0 ], words := [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ] ] 
 )
gap> Size(S);
506
gap> FROPIN(S);
rec( batch_size := 8192, 
  elts := [ Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]), 
      Matrix(IsBooleanMat, [[0, 1, 0], [1, 0, 0], [0, 0, 1]]), 
      Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 1], [1, 0, 0]]), 
      Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [1, 0, 1]]), 
      Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 0]]) ], 
  final := [ 1, 2, 3, 4, 5 ], first := [ 1, 2, 3, 4, 5 ], found := false, 
  gens := [ Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]), 
      Matrix(IsBooleanMat, [[0, 1, 0], [1, 0, 0], [0, 0, 1]]), 
      Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 1], [1, 0, 0]]), 
      Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [1, 0, 1]]), 
      Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 0]]) ], 
  genslookup := [ 1, 2, 3, 4, 5 ], genstoapply := [ 1 .. 5 ], 
  ht := <tree hash table len=12517 used=5 colls=0 accs=10>, 
  left := [ [  ], [  ], [  ], [  ], [  ] ], len := 1, lenindex := [ 1 ], 
  nr := 5, nrrules := 0, one := 1, 
  parent := <monoid of size 506, 3x3 boolean matrices with 4 generators>, 
  pos := 1, prefix := [ 0, 0, 0, 0, 0 ], 
  reduced := [ [ false, false, false, false, false ], 
      [ false, false, false, false, false ], 
      [ false, false, false, false, false ], 
      [ false, false, false, false, false ], 
      [ false, false, false, false, false ] ], report := false, 
  right := [ [  ], [  ], [  ], [  ], [  ] ], rules := [  ], stopper := false, 
  suffix := [ 0, 0, 0, 0, 0 ], words := [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ] ] 
 )
gap> S := Semigroup(Generators(S), Generators(S));;
gap> FROPIN(S);;

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
Error, Semigroups: EnumeratorSorted: usage,
the first argument (a semigroup) must be finite,

# IteratorSorted
gap> S := HallMonoid(3);;
gap> it := IteratorSorted(S);
<iterator>
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
fail
gap> it := ShallowCopy(it);;
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
fail
gap> ListIterator(it) = AsSet(S); # it's done
false
gap> it := ShallowCopy(it);;
gap> ListIterator(it) = AsSet(S);
true
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

# AsListCanonical for IsEnumerableSemigroupRep (without known generators)
gap> S := TriangularBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 6 generators>
gap> I := SemigroupIdeal(S, S.6);;
gap> ForAll(AsListCanonical(I), x -> x in I);
true
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> AsListCanonical(S);
Error, Semigroups: AsListCanonical: usage,
the first argument (a semigroup) must be finite,

# Enumerator for IsEnumerableSemigroupRep 
gap> S := TriangularBooleanMatMonoid(3);;
gap> Enumerator(S);
<enumerator of <monoid of 3x3 boolean matrices with 6 generators>>

# EnumeratorCanonical for IsEnumerableSemigroupRep 
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

# IteratorCanonical
gap> S := UnitriangularBooleanMatMonoid(3);;
gap> it := Iterator(S);
<iterator>
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
fail
gap> it := ShallowCopy(it);;
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
fail
gap> ListIterator(it) = AsListCanonical(S); # it's done
false
gap> it := ShallowCopy(it);;
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
Error, Semigroups: AsListCanonical: usage,
the first argument (a semigroup) must be finite,

# PositionCanonical, for wrong family, wrong degree
gap> S := UnitriangularBooleanMatMonoid(3);;
gap> PositionCanonical(S, IdentityTransformation);
fail
gap> S := Semigroup(FullTransformationMonoid(3), rec(acting := false));;
gap> PositionCanonical(S, Transformation([1, 1, 1, 1]));
fail

# Position 
gap> S := UnitriangularBooleanMatMonoid(3);;
gap> Position(S, Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]));
fail
gap> Enumerate(S);;
gap> Position(S, Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]));
1
gap> Position(S, IdentityTransformation);
fail
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
fail
gap> S := Semigroup(FullTransformationMonoid(3), rec(acting := false));;
gap> PositionSorted(S, Transformation([1, 1, 1, 1]));
fail
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> PositionSorted(S, S.1);
Error, Semigroups: PositionSortedOp: usage,
the first argument (a semigroup) must be finite,

# Left/RightCayleyGraphSemigroup, for an infinite enumerable semigroup
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> RightCayleyGraphSemigroup(S);
Error, Semigroups: RightCayleyGraphSemigroup: usage,
the first argument (a semigroup) must be finite,
gap> LeftCayleyGraphSemigroup(S);
Error, Semigroups: LeftCayleyGraphSemigroup: usage,
the first argument (a semigroup) must be finite,

# AsSet, for an infinite enumerable semigroup
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> AsSet(S);
Error, Semigroups: AsSet: usage,
the first argument (a semigroup) must be finite,

# Size, for an infinite enumerable semigroup
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> Size(S);
infinity

# MultiplicationTable
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> MultiplicationTable(S);
Error, Semigroups: MultiplicationTable: usage,
the first argument (a semigroup) must be finite,
gap> S := RegularBooleanMatMonoid(1);
<commutative monoid of 1x1 boolean matrices with 1 generator>
gap> MultiplicationTable(S);
[ [ 1, 1 ], [ 1, 2 ] ]

# NrIdempotents
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]));;
gap> NrIdempotents(S);
Error, Semigroups: AsListCanonical: usage,
the first argument (a semigroup) must be finite,
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
Error, Semigroups: MinimalFactorization:
the second argument <x> is not an element of the first argument <S>,

#E#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/fropin.tst");
