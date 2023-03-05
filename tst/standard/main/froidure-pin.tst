#############################################################################
##
#W  standard/main/froidure-pin.tst
#Y  Copyright (C) 2016-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains tests for main/froidure-pin.g* which use the
# implementation of the Froidure-Pin algorithm in the Semigroups package kernel
# module and not the version in libsemigroups.

#@local F, G, ListIterator, LoopIterator, N, R, S, T, TestEnumerator
#@local TestIterator, acting, an, cong, copy, elts, final, first, found, gens
#@local genslookup, genstoapply, ht, i, left, len, lenindex, list, mat, nr
#@local nrrules, one, out, parent, pos, prefix, reduced, right, rules, stopper
#@local suffix, valid, words, x
gap> START_TEST("Semigroups package: standard/main/froidure-pin.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> ListIterator := function(it)
>  local out, i, x;
>  out := [];
>  i := 0;
>  for x in it do
>    i := i + 1;
>    out[i] := x;
>  od;
>  return out;
> end;;
gap> TestEnumerator := function(en)
> return ForAll(en, x -> en[Position(en, x)] = x) 
> and ForAll([1 .. Length(en)], i -> Position(en, en[i]) = i)
> and ForAll(en, x -> x in en)
> and ForAll([1 .. Length(en)], i -> IsBound(en[i]))
> and ForAll([Length(en) + 1 .. Length(en) + 10], i -> not IsBound(en[i]));
> end;;
gap> TestIterator := function(S, it)
> local LoopIterator;
> LoopIterator := function(it)
>   local valid, len, x;
>   valid := true;;
>   len := 0;
>   for x in it do 
>     len := len + 1;
>     if not x in S then 
>       valid := false;
>       break;
>     fi;
>   od;
>   return valid and IsDoneIterator(it) and len = Size(S);
> end;
> return LoopIterator(it) and LoopIterator(ShallowCopy(it));
> end;;

# CanUseGapFroidurePin for RZMS
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[(), (), ()], [0, (), 0],
>  [(), 0, ()]]);;
gap> CanUseGapFroidurePin(S);
true

# CanUseGapFroidurePin for a quotient semigroup
gap> S := FullTransformationMonoid(4);;
gap> cong := SemigroupCongruence(S, [S.2, S.3]);;
gap> CanUseLibsemigroupsFroidurePin(S / cong);
true
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> cong := SemigroupCongruence(S, [[S.1, S.1]]);;
gap> CanUseLibsemigroupsFroidurePin(S / cong);
false

# CanUseGapFroidurePin for a free band
gap> CanUseGapFroidurePin(FreeBand(4));
true
gap> CanUseGapFroidurePin(FreeBand(5));
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
gap> EnumeratorSorted(FreeBand(2));
[ x1, x2x1x2, x2x1, x2, x1x2, x1x2x1 ]
gap> TestEnumerator(EnumeratorSorted(FreeBand(2)));
true

# EnumeratorCanonical for CanUseGapFroidurePin without generators
gap> G := Range(IsomorphismPermGroup(SmallGroup(6, 1)));;
gap> mat := [[G.1, G.2], [G.1 * G.2, G.1], [G.2, G.2]];;
gap> S := ReesMatrixSemigroup(G, mat);;
gap> CanUseGapFroidurePin(S);
true
gap> HasGeneratorsOfSemigroup(S);
false
gap> TestEnumerator(EnumeratorCanonical(S));;

# MultiplicationTable
gap> S := ReesMatrixSemigroup(Group([(1, 2)]), [[(), (1, 2)], [(), ()]]);;
gap> GeneratorsOfSemigroup(S);
[ (1,(1,2),1), (2,(),2) ]
gap> S := Semigroup(S.1, S.2 ^ 2);
<subsemigroup of 2x2 Rees matrix semigroup with 2 generators>
gap> CanUseGapFroidurePin(S);
true
gap> MultiplicationTable(S);
[ [ 1, 2, 3, 4, 3, 4, 1, 2 ], [ 1, 2, 3, 4, 1, 2, 3, 4 ], 
  [ 3, 4, 1, 2, 1, 2, 3, 4 ], [ 3, 4, 1, 2, 3, 4, 1, 2 ], 
  [ 5, 6, 7, 8, 7, 8, 5, 6 ], [ 5, 6, 7, 8, 5, 6, 7, 8 ], 
  [ 7, 8, 5, 6, 5, 6, 7, 8 ], [ 7, 8, 5, 6, 7, 8, 5, 6 ] ]
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(), (1, 2)], [(), 0]]);;
gap> GeneratorsOfSemigroup(S);
[ (1,(1,2),1), (1,(),2), (2,(),1) ]
gap> S := Semigroup(S.1, S.2 ^ 2);
<subsemigroup of 2x2 Rees 0-matrix semigroup with 2 generators>
gap> CanUseGapFroidurePin(S);
true
gap> MultiplicationTable(S);
[ [ 1, 2, 3, 4 ], [ 1, 2, 3, 4 ], [ 3, 4, 1, 2 ], [ 3, 4, 1, 2 ] ]

# PositionCanonical (for a group)
gap> G := Group((1, 2, 3), (1, 2));;
gap> CanUseGapFroidurePin(G);
true

# CanUseGapFroidurePin for a dual semigroup
gap> S := Semigroup([Transformation([1, 3, 2]), Transformation([1, 4, 4, 2])]);
<transformation semigroup of degree 4 with 2 generators>
gap> T := DualSemigroup(S);
<dual semigroup of <transformation semigroup of degree 4 with 2 generators>>
gap> CanUseGapFroidurePin(T);
true

# GapFroidurePin, for a semigroup with identity in generators, and duplicate
# generators
gap> S := Group((1, 2, 3), (), ());
Group([ (1,2,3), (), () ])
gap> GapFroidurePin(S);
rec( elts := [ (1,2,3), () ], final := [ 1, 2 ], first := [ 1, 2 ], 
  found := false, gens := [ (1,2,3), (), () ], genslookup := [ 1, 2, 2 ], 
  genstoapply := [ 1 .. 3 ], ht := <tree hash table len=12517 used=2 colls=
    0 accs=5>, left := [ [  ], [  ] ], len := 1, lenindex := [ 1 ], nr := 2, 
  nrrules := 1, one := 2, parent := Group([ (1,2,3), (), () ]), pos := 1, 
  prefix := [ 0, 0 ], 
  reduced := [ [ false, false, false ], [ false, false, false ] ], 
  right := [ [  ], [  ] ], rules := [ [ [ 3 ], [ 2 ] ] ], stopper := false, 
  suffix := [ 0, 0 ], words := [ [ 1 ], [ 2 ] ] )

# AsListCanonical
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> TestEnumerator(EnumeratorCanonical(S));
true
gap> EnumeratorCanonical(S)[10];
fail
gap> AsListCanonical(S);
[ x1, x2, x1x2, x2x1, x1x2x1, x2x1x2 ]
gap> S := ReesMatrixSemigroup(Group([(1, 2)]), [[(), (1, 2)], [(), ()]]);;
gap> AsListCanonical(S);
[ (1,(1,2),1), (2,(),2), (1,(),1), (1,(),2), (2,(1,2),1), (1,(1,2),2), 
  (2,(),1), (2,(1,2),2) ]
gap> EnumeratorCanonical(S);
[ (1,(1,2),1), (2,(),2), (1,(),1), (1,(),2), (2,(1,2),1), (1,(1,2),2), 
  (2,(),1), (2,(1,2),2) ]
gap> Enumerator(S);
<enumerator of Rees matrix semigroup>

# AsListCanonical/AsSet for an infinite semigroup
gap> S := GraphInverseSemigroup(CycleDigraph(2));
<infinite graph inverse semigroup with 2 vertices, 2 edges>
gap> AsSet(S);
Error, the argument (a semigroup) is not finite
gap> AsListCanonical(S);
Error, the argument (a semigroup) is not finite
gap> EnumeratorSorted(S);
Error, the argument (a semigroup) is not finite
gap> N := EnumeratorCanonical(S);
<enumerator of <infinite graph inverse semigroup with 2 vertices, 2 edges>>
gap> N[1];
e_1

# Iterators
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> TestIterator(S, IteratorCanonical(S));
true
gap> TestIterator(S, Iterator(S));
true
gap> TestIterator(S, IteratorSorted(S));
true
gap> F := FreeSemigroup(2);;
gap> R := [[F.1 * F.2, F.2 * F.1]];;
gap> S := F / R;
<fp semigroup with 2 generators and 1 relation of length 6>
gap> NextIterator(Iterator(S));
s1
gap> IteratorSorted(S);
Error, the argument (a semigroup) is not finite
gap> NextIterator(IteratorCanonical(S));
s1

# Size
gap> S := Semigroup(FreeBand(2));
<semigroup with 2 generators>
gap> Size(S);
6
gap> F := FreeSemigroup(2);;
gap> R := [[F.1 * F.2, F.2 * F.1]];;
gap> S := F / R;
<fp semigroup with 2 generators and 1 relation of length 6>
gap> Size(S);
infinity

# \in 
gap> S := Semigroup(FreeBand(2));
<semigroup with 2 generators>
gap> ForAll(AsList(S), x -> x in S);
true

# Nr/Idempotents
gap> S := Semigroup(FreeBand(2));
<semigroup with 2 generators>
gap> Idempotents(S);
[ x1, x2, x1x2, x2x1, x1x2x1, x2x1x2 ]
gap> NrIdempotents(S);
6
gap> S := GraphInverseSemigroup(CycleDigraph(2));
<infinite graph inverse semigroup with 2 vertices, 2 edges>
gap> Idempotents(S);
Error, the argument (a semigroup) is not finite
gap> NrIdempotents(S);
Error, the argument (a semigroup) is not finite

# PositionCanonical
gap> S := Semigroup(FreeBand(2));
<semigroup with 2 generators>
gap> PositionCanonical(S, 1);
fail
gap> S := Semigroup(FreeBand(5));
<semigroup with 5 generators>
gap> PositionCanonical(S, S.1 * S.2 * S.3 * S.2 * S.1);
422
gap> x := Matrix(GF(2 ^ 2),
> [[Z(2 ^ 2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [Z(2 ^ 2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)]]);;
gap> S := Monoid(x, rec(acting := false));
<commutative monoid of 6x6 matrices over GF(2^2) with 1 generator>
gap> CanUseGapFroidurePin(S);
true
gap> PositionCanonical(S, Matrix(GF(2), [[Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2),
> 0 * Z(2), 0 * Z(2)], [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
> [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2)], [0 * Z(2), 0 *
> Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
> [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2)], [0 * Z(2), 0 *
> Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0]]));
fail

# Position
gap> S := Semigroup(FreeBand(2));
<semigroup with 2 generators>
gap> Position(S, S.1);
1
gap> Position(S, S.2);
2
gap> Position(S, S.1 * S.2);
fail
gap> Position(S, 1);
fail

# PositionSorted
gap> S := Semigroup(FreeBand(2));;
gap> list := AsListCanonical(S);;
gap> copy := ShallowCopy(list);;
gap> Sort(copy, {x, y} -> PositionSorted(S, x) < PositionSorted(S, y));
gap> SortedList(list) = copy;
true
gap> PositionSorted(S, 1);
fail
gap> S := GraphInverseSemigroup(CycleDigraph(2));
<infinite graph inverse semigroup with 2 vertices, 2 edges>
gap> PositionSorted(S, S.1);
Error, the 1st argument (a semigroup) is not finite

# IsEnumerated
gap> S := Semigroup(FreeBand(2));;
gap> IsEnumerated(S);
false
gap> Size(S);
6
gap> IsEnumerated(S);
true

# Enumerate
gap> S := Semigroup(FreeBand(2));;
gap> Enumerate(S, 10);
<semigroup with 2 generators>
gap> Enumerate(S);
<semigroup with 2 generators>

# Left/RightCayleyGraphSemigroup
gap> S := Semigroup(FreeBand(2));;
gap> LeftCayleyGraphSemigroup(S);
[ [ 1, 4 ], [ 3, 2 ], [ 3, 6 ], [ 5, 4 ], [ 5, 4 ], [ 3, 6 ] ]
gap> RightCayleyGraphSemigroup(S);
[ [ 1, 3 ], [ 4, 2 ], [ 5, 3 ], [ 4, 6 ], [ 5, 3 ], [ 4, 6 ] ]
gap> LeftCayleyDigraph(S);
<immutable digraph with 6 vertices, 12 edges>
gap> RightCayleyDigraph(S);
<immutable digraph with 6 vertices, 12 edges>
gap> S := GraphInverseSemigroup(CycleDigraph(2));
<infinite graph inverse semigroup with 2 vertices, 2 edges>
gap> LeftCayleyGraphSemigroup(S);
Error, the argument (a semigroup) is not finite
gap> RightCayleyGraphSemigroup(S);
Error, the argument (a semigroup) is not finite
gap> LeftCayleyDigraph(S);
Error, the argument (a semigroup) is not finite
gap> RightCayleyDigraph(S);
Error, the argument (a semigroup) is not finite

# Minimal/Factorization
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> T := Semigroup(S.1);;
gap> Factorization(T, 10);
Error, the 2nd argument (a positive integer) is greater than the size of the 1\
st argument (a semigroup)
gap> Factorization(T, 1);
[ 1 ]
gap> Factorization(T, T.1 ^ 10);
[ 1 ]
gap> MinimalFactorization(T, 1);
[ 1 ]
gap> MinimalFactorization(T, T.1 ^ 10);
[ 1 ]
gap> MinimalFactorization(T, S.2);
Error, the 2nd argument (a mult. elt.) is not an element of the 1st argument (\
a semigroup)
gap> Factorization(T, S.2);
Error, the 2nd argument (a mult. elt.) is not an element of the 1st argument (\
a semigroup)

# RulesOfSemigroup
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> RulesOfSemigroup(S);
[ [ [ 1, 1 ], [ 1 ] ], [ [ 2, 2 ], [ 2 ] ], [ [ 1, 2, 1, 2 ], [ 1, 2 ] ], 
  [ [ 2, 1, 2, 1 ], [ 2, 1 ] ] ]
gap> S := GraphInverseSemigroup(CycleDigraph(2));
<infinite graph inverse semigroup with 2 vertices, 2 edges>
gap> RulesOfSemigroup(S);
Error, the argument (a semigroup) is not finite

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/main/froidure-pin.tst");
