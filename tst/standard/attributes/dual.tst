############################################################################
##
#W  standard/attributes/dual.tst
#Y  Copyright (C) 2018-2022                                     Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local BruteForceAntiIsoCheck, BruteForceInverseCheck, D, DS, S, T, U, V
#@local antiiso, antiso, d, i, inv, invantiso, iso, x, y
gap> START_TEST("Semigroups package: standard/attributes/dual.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

# helper functions
gap> BruteForceAntiIsoCheck := function(antiiso)
>   local x, y;
>   if not IsInjective(antiiso) or not IsSurjective(antiiso) then
>     return false;
>   fi;
>   for x in Generators(Source(antiiso)) do
>     for y in Generators(Source(antiiso)) do
>       if x ^ antiiso * y ^ antiiso <> (y * x) ^ antiiso then
>         return false;
>       fi;
>     od;
>   od;
>   return true;
> end;;
gap> BruteForceInverseCheck := function(map)
> local inv;
>   inv := InverseGeneralMapping(map);
>   return ForAll(Source(map), x -> x = (x ^ map) ^ inv)
>     and ForAll(Range(map), x -> x = (x ^ inv) ^ map);
> end;;

#  Creation of dual semigroups and elements - 1
gap> S := Semigroup([Transformation([1, 3, 2]), Transformation([1, 4, 4, 2])]);
<transformation semigroup of degree 4 with 2 generators>
gap> T := DualSemigroup(S);
<dual semigroup of <transformation semigroup of degree 4 with 2 generators>>
gap> S := Semigroup([Transformation([1, 3, 2]), Transformation([1, 4, 4, 2])]);;
gap> T := DualSemigroup(S);
<dual semigroup of <transformation semigroup of degree 4 with 2 generators>>
gap> AsSSortedList(T);
[ <Transformation( [ 1, 2, 2 ] ) in the dual semigroup>, 
  <IdentityTransformation in the dual semigroup>, 
  <Transformation( [ 1, 3, 2 ] ) in the dual semigroup>, 
  <Transformation( [ 1, 3, 3 ] ) in the dual semigroup>, 
  <Transformation( [ 1, 4, 4, 2 ] ) in the dual semigroup>, 
  <Transformation( [ 1, 4, 4, 3 ] ) in the dual semigroup> ]
gap> Size(T);
6

#  Creation of dual semigroups and elements - 2
gap> S := Semigroup([Transformation([2, 6, 3, 2, 4, 2]),
> Transformation([5, 5, 6, 1, 4, 5]),
> Transformation([5, 3, 1, 6, 4, 5])]);;
gap> T := DualSemigroup(S);
<dual semigroup of <transformation semigroup of degree 6 with 3 generators>>
gap> Size(T);
385
gap> Size(T) = Size(S);
true
gap> AsSortedList(T) = AsSortedList(List(S,
> s -> SEMIGROUPS.DualSemigroupElementNC(T, s)));
true
gap> iso := AntiIsomorphismDualSemigroup(S);;
gap> AsSortedList(T) = AsSortedList(List(S,
> s -> s ^ iso));
true

#  Creation of dual semigroups and elements - 3
gap> S := FullTransformationMonoid(20);;
gap> T := DualSemigroup(S);;
gap> HasGeneratorsOfSemigroup(T);
true
gap> HasGeneratorsOfMonoid(T);
true

#  DClasses of dual semigroups - 1
gap> S := Semigroup([Transformation([2, 6, 3, 2, 4, 2]),
> Transformation([5, 5, 6, 1, 4, 5]),
> Transformation([5, 3, 1, 6, 4, 5])]);;
gap> T := DualSemigroup(S);;
gap> DClasses(T);
[ <Green's D-class: <object>>, <Green's D-class: <object>>, 
  <Green's D-class: <object>>, <Green's D-class: <object>>, 
  <Green's D-class: <object>>, <Green's D-class: <object>>, 
  <Green's D-class: <object>>, <Green's D-class: <object>>, 
  <Green's D-class: <object>>, <Green's D-class: <object>>, 
  <Green's D-class: <object>>, <Green's D-class: <object>>, 
  <Green's D-class: <object>>, <Green's D-class: <object>>, 
  <Green's D-class: <object>> ]
gap> DS := AsSortedList(DClasses(T));;
gap> for i in [1 .. Size(DS) - 1] do
> if not DS[i] < DS[i + 1] then
> Print("comparison failure");
> fi;
> od;

#  Green's classes of dual semigroups
gap> S := Semigroup([Transformation([2, 6, 3, 2, 4, 2]),
> Transformation([5, 5, 6, 1, 4, 5])]);;
gap> T := DualSemigroup(S);;
gap> iso := AntiIsomorphismDualSemigroup(T);;
gap> ForAll(DClasses(T),
> x -> AsSortedList(List(x, y -> y ^ iso)) =
> AsSortedList(GreensDClassOfElement(S,
> Representative(x) ^ iso)));
true
gap> ForAll(LClasses(T),
> x -> AsSortedList(List(x, y -> y ^ iso)) =
> AsSortedList(GreensRClassOfElement(S,
> Representative(x) ^ iso)));
true
gap> ForAll(RClasses(T),
> x -> AsSortedList(List(x, y -> y ^ iso)) =
> AsSortedList(GreensLClassOfElement(S,
> Representative(x) ^ iso)));
true
gap> ForAll(HClasses(T),
> x -> AsSortedList(List(x, y -> y ^ iso)) =
> AsSortedList(GreensHClassOfElement(S,
> Representative(x) ^ iso)));
true

#  Representatives
gap> S := FullTransformationMonoid(20);;
gap> T := DualSemigroup(S);;
gap> Representative(T);
<IdentityTransformation in the dual semigroup>

#  Size
gap> S := FullTransformationMonoid(20);;
gap> T := DualSemigroup(S);;
gap> Size(T);
104857600000000000000000000

#  AsList
gap> S := FullTransformationMonoid(6);;
gap> T := DualSemigroup(S);;
gap> AsList(T);;

#  One and MultiplicativeNeutralElement - 1
gap> S := FullBooleanMatMonoid(5);;
gap> T := DualSemigroup(S);;
gap> One(Representative(T));
<Matrix(IsBooleanMat, [[1, 0, 0, 0, 0], [0, 1, 0, 0, 0], [0, 0, 1, 0, 0], 
  [0, 0, 0, 1, 0], [0, 0, 0, 0, 1]]) in the dual semigroup>
gap> One(Representative(T)) = MultiplicativeNeutralElement(T);
true

#  One and MultiplicativeNeutralElement - 2
gap> S := Semigroup([Transformation([2, 6, 3, 2, 4, 2]),
> Transformation([5, 5, 6, 1, 4, 5])]);;
gap> IsMonoidAsSemigroup(S);
false
gap> T := DualSemigroup(S);;
gap> One(Representative(T));
<IdentityTransformation in the dual semigroup>
gap> MultiplicativeNeutralElement(T);
fail

#  AntiIsomorphisms
gap> S := FullTransformationMonoid(5);;
gap> T := DualSemigroup(S);;
gap> HasAntiIsomorphismTransformationSemigroup(T);
true
gap> Range(AntiIsomorphismTransformationSemigroup(T)) = S;
true
gap> antiso := AntiIsomorphismDualSemigroup(S);
MappingByFunction( <full transformation monoid of degree 5>, <dual semigroup o\
f <full transformation monoid of degree 5>>, function( x ) ... end, function( \
x ) ... end )
gap> inv := AntiIsomorphismTransformationSemigroup(T);
MappingByFunction( <dual semigroup of <full transformation monoid of degree 5>\
>, <full transformation monoid of degree 5>, function( x ) ... end, function( \
x ) ... end )
gap> ForAll(S, x -> (x ^ antiso) ^ inv = x);
true
gap> invantiso := InverseGeneralMapping(antiso);
MappingByFunction( <dual semigroup of <full transformation monoid of degree 5>\
>, <full transformation monoid of degree 5>, function( x ) ... end, function( \
x ) ... end )
gap> ForAll(S, x -> (x ^ antiso) ^ invantiso = x);
true

#  AntiIsomorphism brute force checking - 1
gap> S := FullTropicalMaxPlusMonoid(2, 4);;
gap> antiiso := AntiIsomorphismDualSemigroup(S);;
gap> BruteForceAntiIsoCheck(antiiso);
true
gap> BruteForceInverseCheck(antiiso);
true

#  AntiIsomorphism brute force checking - 2
gap> S := PlanarPartitionMonoid(3);;
gap> antiiso := AntiIsomorphismDualSemigroup(S);;
gap> BruteForceAntiIsoCheck(antiiso);
true
gap> BruteForceInverseCheck(antiiso);
true

#  AntiIsomorphism brute force checking - 3
gap> S := OrderEndomorphisms(6);;
gap> antiiso := AntiIsomorphismDualSemigroup(S);;
gap> BruteForceAntiIsoCheck(antiiso);
true
gap> BruteForceInverseCheck(antiiso);
true

#  Subsemigroups of a dual semigroup
gap> S := FullBooleanMatMonoid(4);
<monoid of 4x4 boolean matrices with 7 generators>
gap> T := DualSemigroup(S);
<dual semigroup of <monoid of 4x4 boolean matrices with 7 generators>>
gap> U := Semigroup(GeneratorsOfSemigroup(T){[3 .. 5]});
<dual semigroup of <semigroup of 4x4 boolean matrices with 3 generators>>
gap> AsList(U);
[ <Matrix(IsBooleanMat, [[1, 1, 0, 0], [1, 0, 1, 0], [0, 1, 1, 0], 
      [0, 0, 0, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 0, 0], [1, 0, 1, 0], [0, 1, 0, 1], 
      [0, 0, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], 
      [1, 0, 0, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 1, 0], [1, 1, 1, 0], 
      [0, 0, 0, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 0, 1], [1, 1, 1, 1], 
      [0, 0, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 0, 0], [1, 0, 1, 0], [0, 1, 1, 0], 
      [1, 0, 0, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 1, 0], [1, 0, 1, 1], 
      [0, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 0, 1], [1, 0, 1, 1], 
      [0, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 0, 0], [1, 0, 1, 0], [1, 1, 0, 1], 
      [1, 0, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 0, 0], [1, 0, 1, 0], [0, 1, 1, 0], 
      [1, 1, 0, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 0, 0], [1, 0, 1, 0], [0, 1, 0, 1], 
      [1, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 1], [1, 1, 1, 1], [1, 1, 1, 1], 
      [0, 0, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 1, 0], [1, 1, 1, 0], 
      [1, 0, 0, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 1, 1], [1, 1, 1, 1], 
      [0, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 1], [1, 1, 1, 1], [1, 1, 1, 1], 
      [0, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 0, 1], [1, 1, 1, 1], 
      [1, 0, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 1, 0], [1, 1, 1, 0], 
      [1, 1, 0, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 0, 1], [1, 1, 1, 1], 
      [1, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 1, 0], [1, 1, 1, 1], 
      [1, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 1], [1, 1, 1, 1], [1, 1, 1, 1], 
      [1, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 1, 0], [1, 0, 1, 1], 
      [1, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 1, 1], [1, 1, 1, 1], 
      [1, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 0, 1], [1, 0, 1, 1], 
      [1, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 1, 0], [1, 1, 1, 0], 
      [1, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 0, 0], [1, 0, 1, 0], [1, 1, 0, 1], 
      [1, 1, 1, 1]]) in the dual semigroup>, 
  <Matrix(IsBooleanMat, [[1, 1, 1, 1], [1, 1, 1, 1], [1, 1, 1, 1], 
      [1, 0, 1, 1]]) in the dual semigroup> ]
gap> Size(last);
26
gap> Size(DualSemigroup(U));
26
gap> V := DualSemigroup(U);
<semigroup of size 26, 4x4 boolean matrices with 3 generators>
gap> V = Semigroup(GeneratorsOfSemigroup(S){[3 .. 5]});
true

#  UnderlyingElementOfDualSemigroupElement
gap> S := SingularPartitionMonoid(4);;
gap> D := DualSemigroup(S);;
gap> d := Representative(D);
<<block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ], [ 4, -4 ]>
  in the dual semigroup>
gap> UnderlyingElementOfDualSemigroupElement(d);
<block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ], [ 4, -4 ]>
gap> Representative(S);
<block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ], [ 4, -4 ]>
gap> UnderlyingElementOfDualSemigroupElement(S);
Error, the argument is not an element represented as a dual semigroup element
gap> T := Semigroup(GeneratorsOfSemigroup(D){[1000 .. 2000]});
<dual semigroup of <bipartition semigroup of degree 4 with 1001 generators>>
gap> UnderlyingElementOfDualSemigroupElement(Representative(T)) in
> [Bipartition([[1, 2, -1], [3, -2], [4], [-3, -4]]),
>  Bipartition([[1], [2, -3, -4], [3, 4, -1], [-2]]),
>  Bipartition([[1, -2], [2, -3, -4], [3], [4, -1]])];
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/dual.tst");
