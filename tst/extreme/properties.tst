#############################################################################
##
#W  extreme/properties.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local I, S, acting, d, f, g, gens, i, inv, iso, iter, j, rms, s, semis, small
#@local t
gap> START_TEST("Semigroups package: extreme/properties.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# PropertiesTest3
gap> semis :=
> [Semigroup([Transformation([2, 2, 4, 4]),
>      Transformation([5, 3, 4, 4, 6, 6])]),
>  Semigroup([Transformation([5, 4, 4, 2, 1]),
>      Transformation([2, 5, 5, 4, 1])]),
>  Semigroup([Transformation([1, 2, 1, 3, 3]),
>      Transformation([2, 2, 3, 5, 5])]),
>  Semigroup([Transformation([1, 2, 1, 3, 3]),
>      Transformation([2, 2, 3, 5, 5])]),
>  Semigroup([Transformation([8, 7, 5, 3, 1, 3, 8, 8]),
>      Transformation([5, 1, 4, 1, 4, 4])]),
>  Semigroup([Transformation([3, 1, 2, 3, 2, 3, 2, 3]),
>      Transformation([2, 5, 8, 5, 2, 5, 7, 8])]),
>  Semigroup([Transformation([3, 3, 2, 6, 2, 4, 4, 6]),
>      Transformation([5, 1, 7, 8, 7, 5, 8, 1])]),
>  Semigroup([Transformation([3, 3, 2, 6, 2, 4, 4, 6, 3, 4, 6]),
>      Transformation([4, 4, 6, 1, 3, 3, 3, 3, 11, 11, 11])]),
>  Semigroup([Transformation([3, 3, 2, 6, 2, 4, 4, 6, 3, 4, 6]),
>      Transformation([4, 4, 6, 1, 3, 3, 3, 3, 11, 11, 11]),
>      Transformation([2, 2, 3, 4, 4, 6, 6, 6, 6, 6])]),
>  Semigroup([Transformation([3, 3, 2, 6, 2, 4, 4, 6, 3, 4, 6]),
>      Transformation([4, 4, 6, 1, 3, 3, 3, 3, 11, 11, 11]),
>      Transformation([2, 2, 3, 4, 4, 6, 6, 6, 6, 6]),
>      Transformation([2, 2, 3, 4, 4, 6, 6, 6, 6, 11, 11])]),
>  Semigroup([Transformation([12, 3, 6, 4, 6, 11, 9, 6, 6, 7, 6, 12]),
>      Transformation([10, 7, 2, 11, 7, 3, 12, 4, 3, 8, 7, 5])]),
>  Semigroup([Transformation([3, 2, 12, 2, 7, 9, 4, 2, 1, 12, 11, 12]),
>      Transformation([3, 6, 12, 7, 2, 2, 3, 6, 1, 7, 11, 1])]),
>  Semigroup(
>    [Transformation([2, 2]), Transformation([2, 3, 4, 5, 6, 1])]),
>  Semigroup([Transformation([2, 1, 4, 5, 6, 7, 3, 2, 1]),
>      Transformation([2, 1, 4, 2, 1, 4, 2, 1, 4])]),
>  Semigroup([Transformation([5, 2, 5, 5, 8, 10, 8, 5, 2, 10]),
>      Transformation([2, 2, 5, 5, 5, 8, 8, 8, 8, 8])]),
>  Semigroup([Transformation([4, 6, 3, 8, 5, 6, 10, 4, 3, 7]),
>      Transformation([5, 6, 6, 3, 8, 6, 3, 7, 8, 4]),
>      Transformation([8, 6, 3, 2, 8, 10, 9, 2, 6, 2])]),
>  Semigroup([Transformation([1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10]),
>      Transformation([2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7])]),
>  Semigroup([Transformation([10, 8, 7, 4, 1, 4, 10, 10, 7, 2]),
>      Transformation([5, 2, 5, 5, 9, 10, 8, 3, 8, 10])]),
>  Semigroup([Transformation([2, 1, 4, 5, 3, 7, 8, 9, 10, 6]),
>      Transformation([1, 2, 4, 3]),
>      Transformation([1, 2, 3, 4, 5, 6, 10, 9, 8, 7]),
>      Transformation([9, 1, 4, 3, 6, 9, 3, 4, 3, 9])]),
>  Semigroup([Transformation([13, 10, 9, 5, 1, 5, 13, 13, 8, 2, 7, 2, 6]),
>             Transformation([6, 11, 12, 10, 4, 10, 13, 5, 8, 5, 11, 6, 9])]),
>  Semigroup([Transformation([12, 10, 8, 5, 1, 5, 12, 12, 8, 2, 6, 2]),
>      Transformation([5, 6, 10, 11, 10, 4, 10, 12, 5, 7, 4, 10]),
>      Transformation([6, 8, 12, 5, 4, 8, 10, 7, 4, 1, 10, 11])]),
>  Semigroup([Transformation([2, 3, 4, 5, 1, 8, 7, 6, 2, 7]),
>      Transformation([5, 4, 1, 2, 3, 7, 6, 5, 4, 1]),
>      Transformation([2, 1, 4, 3, 2, 1, 4, 4, 3, 3])])];;
gap> List([1 .. 15], i -> IsCompletelyRegularSemigroup(semis[i]));
[ false, true, false, false, false, true, true, true, true, true, false, 
  false, false, false, true ]
gap> List([15 .. 22], i -> IsCompletelyRegularSemigroup(semis[i]));
[ true, false, true, false, false, false, false, false ]

# PropertiesTest4
gap> s := Semigroup(Transformation([3, 3, 2, 6, 2, 4, 4, 6]),
> Transformation([3, 3, 2, 6, 2, 4, 4, 6]));;
gap> IsSimpleSemigroup(s);
true

# PropertiesTest5
gap> s := Semigroup(Transformation([2, 3, 4, 5, 1, 8, 7, 6, 2, 7]),
> Transformation([2, 3, 4, 5, 6, 8, 7, 1, 2, 2]));;
gap> IsSimpleSemigroup(s);
true

# PropertiesTest6
gap> s := Semigroup(
> Transformation([2, 1, 1, 2, 1]), Transformation([3, 4, 3, 4, 4]),
> Transformation([3, 4, 3, 4, 3]), Transformation([4, 3, 3, 4, 4]));;
gap> IsCompletelySimpleSemigroup(s);
true

# PropertiesTest7
gap> s := Semigroup(Transformation([4, 4, 4, 1, 1, 6, 7, 8, 9, 10, 11, 1]),
> Transformation([6, 6, 6, 7, 7, 1, 4, 8, 9, 10, 11, 7]),
> Transformation([8, 8, 8, 9, 9, 10, 11, 1, 4, 6, 7, 9]),
> Transformation([2, 2, 2, 4, 4, 6, 7, 8, 9, 10, 11, 4]),
> Transformation([1, 1, 1, 5, 5, 6, 7, 8, 9, 10, 11, 5]),
> Transformation([1, 1, 4, 4, 4, 6, 7, 8, 9, 10, 11, 1]),
> Transformation([1, 1, 7, 4, 4, 6, 7, 8, 9, 10, 11, 6]));;
gap> IsCompletelySimpleSemigroup(s);
true

# PropertiesTest8
gap> s := Semigroup(Transformation([1, 2, 2, 1, 2]),
> Transformation([3, 4, 3, 4, 4]),
> Transformation([3, 4, 3, 4, 3]),
> Transformation([4, 3, 3, 4, 4]));;
gap> IsCompletelySimpleSemigroup(s);
true

# PropertiesTest9
gap> s := semis[12];;
gap> d := GreensDClassOfElement(s,
> Transformation([12, 2, 1, 3, 6, 6, 12, 2, 3, 3, 11, 3]));;
gap> g := GroupHClassOfGreensDClass(d);;
gap> s := Semigroup(AsList(g));;
gap> IsGroupAsSemigroup(s);
true
gap> IsGroupAsSemigroup(Range(IsomorphismTransformationSemigroup(
>  Group([(2, 4)(3, 5), (1, 2, 3, 5, 4)]))));
true
gap> IsGroupAsSemigroup(semis[11]);
false

# PropertiesTest10
gap> List(semis, IsCliffordSemigroup);
[ false, true, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false, false, false, false, false, false, false 
 ]
gap> ForAll(GreensDClasses(semis[2]), x -> Length(GreensHClasses(x)) = 1 and
> IsRegularDClass(x));
true
gap> IsCliffordSemigroup(semis[2]);
true
gap> ForAll(GreensDClasses(semis[2]), x -> Length(GreensHClasses(x)) = 1 and
> IsRegularDClass(x));
true

# PropertiesTest11
gap> s := Semigroup(
> Transformation([1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
>                 4, 4, 4, 4, 4, 4, 4]),
> Transformation([1, 2, 3, 4, 5, 6, 7, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
>                 4]),
> Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 4, 4, 4, 4, 4, 4, 4, 4,
>                 4]),
> Transformation([1, 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 12, 13, 14, 15, 16, 4, 4, 4,
>                 4, 4]),
> Transformation([1 .. 21] * 1));;
gap> IsLTrivial(s);
true

# PropertiesTest12
gap> gens := [
> Transformation([1, 2, 1, 3, 3]),
> Transformation([2, 2, 3, 5, 5])];;
gap> s := Monoid(gens);;
gap> IsLTrivial(s);
true
gap> d := DClass(s, Transformation([2, 2, 1, 1, 1]));;
gap> IsLTrivial(d);
true

# PropertiesTest13
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
> Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
> Transformation([4, 1, 8, 3, 5, 7, 3, 5]),
> Transformation([4, 3, 4, 5, 6, 4, 1, 2]),
> Transformation([5, 4, 8, 8, 5, 6, 1, 5]),
> Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
> Transformation([7, 1, 2, 2, 2, 7, 4, 5]),
> Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);;
gap> iter := IteratorOfDClasses(s);;
gap> repeat
>   d := NextIterator(iter);
> until IsDoneIterator(iter) or IsLTrivial(d);
gap> d = DClass(s, Transformation([2, 8, 3, 7, 1, 5, 2, 6])) 
> or d = DClass(s, Transformation([5, 5, 5, 5, 5, 5, 5, 5]));
true
gap> IsLTrivial(d);
true
gap> Size(d) in [1, 8];
true
gap> repeat
>   d := NextIterator(iter);
> until IsDoneIterator(iter) or not IsLTrivial(d) and IsRTrivial(d);
gap> d;;
gap> IsLTrivial(d);
false
gap> IsRTrivial(d);
true
gap> NrLClasses(d);
1
gap> NrRClasses(d);
4560
gap> IsRTrivial(s);
false

# PropertiesTest14
gap> gens := [Transformation([3, 4, 1, 2, 1]),
>   Transformation([4, 2, 1, 5, 5]),
>   Transformation([4, 2, 2, 2, 4])];;
gap> s := Semigroup(gens);;
gap> IsRTrivial(s);
false

# PropertiesTest15
gap> gens := [Transformation([1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10]),
> Transformation([2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7])];;
gap> s := Monoid(gens);;
gap> IsRTrivial(s);
false
gap> IsHTrivial(s);
false

# PropertiesTest16
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>   Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>   Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>   Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);;
gap> IsAperiodicSemigroup(s);
false

# PropertiesTest17
gap> gens := [Transformation([2, 6, 7, 2, 6, 1, 1, 5]),
>   Transformation([3, 8, 1, 4, 5, 6, 7, 1]),
>   Transformation([4, 3, 2, 7, 7, 6, 6, 5]),
>   Transformation([7, 1, 7, 4, 2, 5, 6, 3])];;
gap> s := Monoid(gens);;
gap> IsCombinatorialSemigroup(s);
false

# PropertiesTest18
gap> gens := [Transformation([3, 4, 1, 2, 1]),
>   Transformation([4, 2, 1, 5, 5]),
>   Transformation([4, 2, 2, 2, 4])];;
gap> s := Semigroup(gens);;
gap> IsAperiodicSemigroup(s);
false

# PropertiesTest19
gap> gens := [Transformation([13, 10, 9, 5, 1, 5, 13, 13, 8, 2, 7, 2, 6]),
> Transformation([6, 11, 12, 10, 4, 10, 13, 5, 8, 5, 11, 6, 9])];;
gap> s := Semigroup(gens);;
gap> IsAperiodicSemigroup(s);
false

# PropertiesTest20
gap> gens := [Transformation([12, 10, 8, 5, 1, 5, 12, 12, 8, 2, 6, 2]),
> Transformation([5, 6, 10, 11, 10, 4, 10, 12, 5, 7, 4, 10]),
> Transformation([6, 8, 12, 5, 4, 8, 10, 7, 4, 1, 10, 11])];;
gap> s := Monoid(gens);;
gap> IsAperiodicSemigroup(s);
false

# PropertiesTest21
gap> gens := [Transformation([2, 3, 4, 5, 1, 8, 7, 6, 2, 7]),
> Transformation([5, 4, 1, 2, 3, 7, 6, 5, 4, 1]),
> Transformation([2, 1, 4, 3, 2, 1, 4, 4, 3, 3])];;
gap> s := Monoid(gens);;
gap> IsAperiodicSemigroup(s);
false

# PropertiesTest22
gap> gens := [Transformation([1, 2, 1, 3, 3]),
> Transformation([2, 2, 3, 5, 5])];;
gap> s := Monoid(gens);;
gap> IsAperiodicSemigroup(s);
true

# PropertiesTest23
gap> gens := [Transformation([1, 3, 2, 6, 5, 4, 8, 7, 9, 10]),
> Transformation([1, 2, 6, 4, 8, 3, 9, 5, 7, 10]),
> Transformation([1, 10, 10, 10, 10, 10, 7, 8, 10, 10]),
> Transformation([1, 10, 3, 10, 10, 6, 10, 10, 10, 10])];;
gap> s := Semigroup(gens);;
gap> IsInverseSemigroup(s);
true

# PropertiesTest24
gap> gens := [
> Transformation([1, 4, 5, 16, 2, 11, 13, 7, 12, 8, 15, 6, 14, 10, 9, 3,
>                 17]),
> Transformation([1, 17, 17, 17, 17, 6, 7, 8, 9, 10, 11, 17, 17, 17, 17,
>                 16, 17]),
> Transformation([1, 2, 3, 17, 17, 6, 17, 17, 17, 17, 11, 17, 17, 14, 15,
>                 16, 17]),
> Transformation([1, 2, 17, 4, 17, 17, 7, 17, 17, 10, 17, 17, 13, 17, 15,
>                 16, 17])];;
gap> s := Semigroup(gens);;
gap> IsInverseSemigroup(s);
true

# PropertiesTest25
gap> gens := [Transformation([1, 2, 10, 4, 5, 13, 7, 8, 15, 3, 11, 16, 6, 14,
>   9, 12, 17]),
> Transformation([1, 8, 10, 4, 5, 6, 14, 2, 15, 3, 11, 12, 13, 7, 9, 16,
>   17]),
> Transformation([1, 8, 17, 4, 5, 17, 14, 2, 17, 17, 11, 17, 17, 7, 17, 17,
>   17]),
> Transformation([1, 2, 17, 4, 8, 17, 7, 5, 17, 17, 14, 17, 17, 11, 17, 17,
>   17]),
> Transformation([1, 17, 4, 10, 9, 17, 17, 17, 15, 3, 11, 17, 17, 17, 5, 17,
>   17]),
> Transformation([1, 17, 4, 3, 15, 17, 17, 17, 9, 10, 11, 17, 17, 17, 5, 17,
>   17]),
> Transformation([1, 17, 17, 17, 5, 6, 7, 17, 9, 17, 17, 17, 13, 14, 15, 17,
>   17]),
> Transformation([1, 2, 17, 17, 5, 17, 17, 8, 9, 17, 17, 12, 17, 17, 15, 16,
>   17]),
> Transformation([1, 17, 3, 17, 5, 17, 7, 17, 17, 10, 17, 12, 17, 14, 17, 16,
>   17]),
> Transformation([1, 17, 17, 4, 5, 6, 17, 17, 17, 17, 11, 12, 13, 17, 17, 16,
>   17]),
> Transformation([1, 2, 3, 17, 5, 6, 17, 8, 17, 10, 17, 17, 13, 17, 17, 17,
>   17])];;
gap> s := Semigroup(gens);;
gap> IsInverseSemigroup(s);
true

# PropertiesTest26
gap> gens := [Transformation([1, 2, 2]), Transformation([1, 2, 1]),
>   Transformation([2, 2, 3]), Transformation([3, 2, 3]),
>   Transformation([1, 3, 3]), Transformation([1, 1, 3])];;
gap> s := Semigroup(gens);;
gap> IsIdempotentGenerated(s);
true

# PropertiesTest27
gap> gens := [Transformation([2, 6, 1, 8, 5, 3, 8, 8]),
> Transformation([3, 7, 6, 4, 5, 2, 1, 8])];;
gap> s := Semigroup(gens);;
gap> i := MinimalIdeal(s);;
gap> MultiplicativeZero(s);
Transformation( [ 8, 8, 8, 8, 5, 8, 8, 8 ] )
gap> IsLeftZeroSemigroup(i);
true

# PropertiesTest28
gap> gens := [Transformation([2, 3, 4, 5, 6, 7, 8, 9, 1]),
> Transformation([4, 2, 3, 4, 5, 6, 7, 8, 9])];;
gap> s := Semigroup(gens);;
gap> i := MinimalIdeal(s);;
gap> Size(i);
81
gap> i := Semigroup(Generators(i), rec(small := true));;
gap> Size(i);
3
gap> IsLeftZeroSemigroup(i);
false
gap> IsSimpleSemigroup(i);
true
gap> IsRightZeroSemigroup(i);
false
gap> MultiplicativeZero(i);
fail
gap> One(i);
fail

# PropertiesTest29
gap> gens := [
> Transformation([1, 3, 4, 1]),
> Transformation([2, 4, 1, 2]),
> Transformation([3, 1, 1, 3]),
> Transformation([3, 3, 4, 1])];;
gap> s := Monoid(gens);;
gap> s := Semigroup(GeneratorsOfSemigroup(s));;
gap> IsMonoidAsSemigroup(s);
true
gap> IsMonoid(s);
true
gap> i := MinimalIdeal(s);;
gap> Size(i);
4
gap> IsLeftZeroSemigroup(i);
false
gap> IsRightZeroSemigroup(i);
true
gap> IsSynchronizingSemigroup(i);
true

# PropertiesTest30
gap> gens := [Transformation([2, 1, 4, 5, 3, 7, 8, 9, 10, 6]),
> Transformation([1, 2, 4, 3, 5, 6, 7, 8, 9, 10]),
> Transformation([1, 2, 3, 4, 5, 6, 10, 9, 8, 7]),
> Transformation([9, 1, 4, 3, 6, 9, 3, 4, 3, 9])];;
gap> s := Monoid(gens);;
gap> g := GroupOfUnits(s);;

# PropertiesTest31
gap> gens := [Transformation([4, 4, 4, 1, 1, 6, 7, 8, 9, 10, 11, 1]),
> Transformation([6, 6, 6, 7, 7, 1, 4, 8, 9, 10, 11, 7]),
> Transformation([8, 8, 8, 9, 9, 10, 11, 1, 4, 6, 7, 9]),
> Transformation([2, 2, 2, 4, 4, 6, 7, 8, 9, 10, 11, 4]),
> Transformation([1, 1, 1, 5, 5, 6, 7, 8, 9, 10, 11, 5]),
> Transformation([1, 1, 4, 4, 4, 6, 7, 8, 9, 10, 11, 1]),
> Transformation([1, 1, 7, 4, 4, 6, 7, 8, 9, 10, 11, 6])];;
gap> s := Semigroup(gens);;
gap> IsOrthodoxSemigroup(s);
true

# PropertiesTest32
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>   Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>   Transformation([4, 1, 8, 3, 5, 7, 3, 5]),
>   Transformation([4, 3, 4, 5, 6, 4, 1, 2]),
>   Transformation([5, 4, 8, 8, 5, 6, 1, 5]),
>   Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>   Transformation([7, 1, 2, 2, 2, 7, 4, 5]),
>   Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);;
gap> IsOrthodoxSemigroup(s);
false

# PropertiesTest33
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>   Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>   Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>   Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);;
gap> IsOrthodoxSemigroup(s);
false

# PropertiesTest34
gap> gens := [Transformation([2, 6, 7, 2, 6, 1, 1, 5]),
>   Transformation([3, 8, 1, 4, 5, 6, 7, 1]),
>   Transformation([4, 3, 2, 7, 7, 6, 6, 5]),
>   Transformation([7, 1, 7, 4, 2, 5, 6, 3])];;
gap> s := Monoid(gens);;
gap> IsOrthodoxSemigroup(s);
false

# PropertiesTest35
gap> gens := [Transformation([3, 4, 1, 2, 1]),
>   Transformation([4, 2, 1, 5, 5]),
>   Transformation([4, 2, 2, 2, 4])];;
gap> s := Semigroup(gens);;
gap> IsOrthodoxSemigroup(s);
false

# PropertiesTest36
gap> gens := [Transformation([1, 3, 2, 3]),
>  Transformation([1, 4, 1, 2]),
>  Transformation([3, 4, 2, 2]),
>  Transformation([4, 1, 2, 1])];;
gap> s := Monoid(gens);;
gap> IsOrthodoxSemigroup(s);
false

# PropertiesTest37
gap> gens := [Transformation([1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10]),
> Transformation([2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7])];;
gap> s := Monoid(gens);;
gap> IsOrthodoxSemigroup(s);
true

# PropertiesTest38
gap> gens := [Transformation([2, 3, 4, 5, 1, 8, 7, 6, 2, 7]),
> Transformation([3, 8, 7, 4, 1, 4, 3, 3, 7, 2])];;
gap> s := Monoid(gens);;
gap> i := MinimalIdeal(s);;
gap> IsRectangularBand(i);
true

# PropertiesTest39
gap> gens := [Transformation([1, 4, 6, 2, 5, 3, 7, 8]),
>   Transformation([6, 3, 2, 7, 5, 1, 8, 8])];
[ Transformation( [ 1, 4, 6, 2, 5, 3 ] ), 
  Transformation( [ 6, 3, 2, 7, 5, 1, 8, 8 ] ) ]
gap> s := Semigroup(gens);;
gap> i := MinimalIdeal(s);;
gap> IsRectangularBand(i);
true
gap> MultiplicativeZero(i);
Transformation( [ 8, 8, 8, 8, 5, 8, 8, 8 ] )

# PropertiesTest40
gap> gens := [Transformation([2, 8, 3, 7, 1, 5, 2, 6]),
>   Transformation([3, 5, 7, 2, 5, 6, 3, 8]),
>   Transformation([4, 1, 8, 3, 5, 7, 3, 5]),
>   Transformation([4, 3, 4, 5, 6, 4, 1, 2]),
>   Transformation([5, 4, 8, 8, 5, 6, 1, 5]),
>   Transformation([6, 7, 4, 1, 4, 1, 6, 2]),
>   Transformation([7, 1, 2, 2, 2, 7, 4, 5]),
>   Transformation([8, 8, 5, 1, 7, 5, 2, 8])];;
gap> s := Semigroup(gens);;
gap> i := MinimalIdeal(s);;
gap> IsRectangularBand(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsRectangularBand(i);
true
gap> IsRightZeroSemigroup(i);
true

# PropertiesTest41
gap> rms := ReesMatrixSemigroup(Group(()),
>                               List([1 .. 4], x -> List([1 .. 3], y -> ())));;
gap> s := IsomorphismTransformationSemigroup(rms);;
gap> s := Range(s);;
gap> IsRectangularBand(s);
true
gap> IsRegularSemigroup(s);
true

# PropertiesTest42
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5]),
>   Transformation([3, 1, 4, 2, 5, 2, 1, 6, 1, 7]),
>   Transformation([3, 8, 1, 9, 9, 4, 10, 5, 10, 6]),
>   Transformation([4, 7, 6, 9, 10, 1, 3, 6, 6, 2]),
>   Transformation([5, 9, 10, 9, 6, 3, 8, 4, 6, 5]),
>   Transformation([6, 2, 2, 7, 8, 8, 2, 10, 2, 4]),
>   Transformation([6, 2, 8, 4, 7, 5, 8, 3, 5, 8]),
>   Transformation([7, 1, 4, 3, 2, 7, 7, 6, 6, 5]),
>   Transformation([7, 10, 10, 1, 7, 9, 10, 4, 2, 10]),
>   Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9])];;
gap> s := Semigroup(gens, rec(acting := true));;
gap> IsRegularSemigroup(s);
false

# PropertiesTest43
gap> gens := [Transformation([2, 1, 4, 5, 3, 7, 8, 9, 10, 6]),
> Transformation([1, 2, 4, 3, 5, 6, 7, 8, 9, 10]),
> Transformation([1, 2, 3, 4, 5, 6, 10, 9, 8, 7]),
> Transformation([9, 1, 4, 3, 6, 9, 3, 4, 3, 9])];;
gap> s := Monoid(gens);;
gap> IsRegularSemigroup(s);
false

# PropertiesTest44
gap> gens := [Transformation([1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10]),
> Transformation([2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7])];;
gap> s := Monoid(gens);;
gap> IsInverseSemigroup(s);
false
gap> t := Semigroup(Idempotents(s));;
gap> IsSemilattice(t);
false
gap> IsBand(t);
true
gap> Size(t);
10
gap> IsOrthodoxSemigroup(t);
true

# PropertiesTest45
gap> gens := [Transformation([2, 3, 4, 5, 1, 8, 7, 6, 2, 7]),
> Transformation([2, 3, 4, 5, 6, 8, 7, 1, 2, 2])];;
gap> s := Monoid(gens);;
gap> s := Semigroup(Idempotents(Monoid(gens)));;
gap> IsSemilattice(s);
false
gap> IsBand(s);
true

# PropertiesTest46
gap> gens := [Transformation([5, 6, 7, 3, 1, 4, 2, 8]),
>   Transformation([3, 6, 8, 5, 7, 4, 2, 8])];
[ Transformation( [ 5, 6, 7, 3, 1, 4, 2 ] ), 
  Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] ) ]
gap> s := Semigroup(Idempotents(Monoid(gens)));;
gap> Size(s);
94
gap> IsSemilattice(s);
true

# PropertiesTest47
gap> s := FullTransformationSemigroup(3);;
gap> j := 0;;
gap> for f in s do
> for g in s do
> if IsSynchronizingSemigroup(Semigroup(f, g)) then j := j + 1; fi;
> od;
> od;
gap> j;
561

# PropertiesTest48
gap> gens := [Transformation([4, 6, 5, 2, 1, 3]),
>   Transformation([6, 3, 2, 5, 4, 1]),
>   Transformation([1, 2, 4, 3, 5, 6]),
>   Transformation([3, 5, 6, 1, 2, 3]),
>   Transformation([5, 3, 6, 6, 6, 2]),
>   Transformation([2, 3, 2, 6, 4, 6]),
>   Transformation([2, 1, 2, 2, 2, 4]),
>   Transformation([4, 4, 1, 2, 1, 2])];;
gap> s := Semigroup(gens);;
gap> g := Range(IsomorphismPermGroup(GroupOfUnits(s)));;
gap> IsZeroGroup(Range(InjectionZeroMagma(g)));
true
gap> IsZeroGroup(s);
false

# PropertiesTest49
gap> gens := List(Tuples([1, 2], 4), x ->
> TransformationNC(Concatenation([1, 1], x)));;
gap> s := Semigroup(gens);;
gap> IsZeroSemigroup(s);
true

# PropertiesTest50
gap> gens := [Transformation([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10]),
>  Transformation([3, 6, 9, 1, 4, 7, 2, 5, 8, 10, 10]),
>  Transformation([3, 6, 9, 7, 1, 4, 5, 8, 2, 10, 10]),
>  Transformation([8, 2, 5, 5, 4, 5, 5, 2, 8, 10, 10]),
>  Transformation([4, 4, 8, 4, 4, 2, 4, 4, 5, 10, 10])];;
gap> s := Semigroup(gens);;
gap> MultiplicativeNeutralElement(s);
Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10 ] )

# PropertiesTest51
gap> [Transformation([3, 6, 9, 1, 4, 7, 2, 5, 8]),
>   Transformation([3, 6, 9, 7, 1, 4, 5, 8, 2]),
>   Transformation([8, 2, 5, 5, 4, 5, 5, 2, 8]),
>   Transformation([4, 4, 8, 4, 4, 2, 4, 4, 5]),
>   Transformation([7, 5, 5, 7, 3, 7, 5, 5, 3]),
>   Transformation([7, 3, 3, 3, 3, 7, 5, 3, 3]),
>   Transformation([3, 5, 3, 3, 3, 3, 7, 7, 5]),
>   Transformation([3, 7, 3, 3, 5, 5, 7, 7, 7]),
>   Transformation([3, 3, 3, 7, 5, 5, 7, 5, 7]),
>   Transformation([3, 5, 5, 3, 7, 3, 7, 5, 7]),
>   Transformation([3, 3, 3, 5, 5, 3, 7, 5, 5]),
>   Transformation([5, 5, 7, 5, 7, 3, 5, 3, 7]),
>   Transformation([3, 5, 5, 3, 7, 3, 3, 5, 3]),
>   Transformation([7, 3, 7, 7, 7, 3, 3, 5, 7]),
>   Transformation([5, 3, 7, 3, 7, 5, 3, 5, 3]),
>   Transformation([5, 5, 7, 5, 7, 3, 7, 7, 5])];;
gap> s := Semigroup(last);;
gap> MultiplicativeNeutralElement(s);
IdentityTransformation

# PropertiesTest52: Checking E-unitary
gap> [PartialPerm([1, 2, 3, 4], [3, 1, 2, 5]),
>  PartialPerm([1, 2, 3, 4], [3, 2, 1, 4])];;
gap> s := InverseSemigroup(last);;
gap> IsEUnitaryInverseSemigroup(s);
true
gap> [PartialPerm([1, 2, 3, 4, 5], [1, 2, 5, 6, 3]),
>  PartialPerm([1, 2, 3, 4, 5], [3, 2, 1, 6, 5])];;
gap> s := InverseSemigroup(last);;
gap> IsEUnitaryInverseSemigroup(s);
true
gap> [PartialPerm([1, 2, 3, 4, 7], [2, 4, 6, 5, 7]),
>  PartialPerm([1, 2, 3, 4, 5, 6, 7], [6, 4, 7, 2, 3, 1, 8]),
>  PartialPerm([1, 2, 4, 5, 6, 7], [8, 6, 3, 5, 4, 1]),
>  PartialPerm([1, 2, 4, 5, 8], [1, 3, 8, 6, 2])];;
gap> s := InverseSemigroup(last);;
gap> IsEUnitaryInverseSemigroup(s);
false

# PropertiesTest53

#gap> gens := [ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
#>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
#>   Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ),
#>   Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ),
#>   Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ),
#>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
#>   Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ),
#>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
#gap> s:=Semigroup(gens);;
##gap> IsAbundantSemigroup(s);
##false
#
##
#gap> gens := [ Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ),
#>   Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ),
#>   Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ),
#>   Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] ) ];;
#gap> s:=Monoid(gens);;
##gap> IsAbundantSemigroup(s);
##false
#
##
#gap> gens := [ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
#>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
#>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
#>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
#gap> s:=Semigroup(gens);;
##gap> IsAbundantSemigroup(s);
##false
#
##
#gap> gens := [ Transformation( [ 3, 4, 1, 2, 1 ] ),
#>   Transformation( [ 4, 2, 1, 5, 5 ] ),
#>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
#gap> s:=Semigroup(gens);;
#gap> IsAbundantSemigroup(s);
#true
#
##
#gap> gens := [ Transformation( [ 1, 3, 4, 1 ] ),
#> Transformation( [ 2, 4, 1, 2 ] ),
#> Transformation( [ 3, 1, 1, 3 ] ),
#> Transformation( [ 3, 3, 4, 1 ] ) ];;
#gap> s:=Monoid(gens);;
#gap> IsAbundantSemigroup(s);
#false
#
##
#gap> gens := [ Transformation( [ 1, 3, 2, 3 ] ),
#>  Transformation( [ 1, 4, 1, 2 ] ),
#>  Transformation( [ 2, 4, 1, 1 ] ),
#>  Transformation( [ 3, 4, 2, 2 ] ) ];;
#gap> s:=Semigroup(gens);;
#gap> IsAbundantSemigroup(s);
#true
#gap> IsRegularSemigroup(s);
#false
#
##
#gap> gens := [ Transformation( [ 1, 3, 2, 3 ] ),
#>  Transformation( [ 1, 4, 1, 2 ] ),
#>  Transformation( [ 3, 4, 2, 2 ] ),
#>  Transformation( [ 4, 1, 2, 1 ] ) ];;
#gap> s:=Monoid(gens);;
#gap> IsAbundantSemigroup(s);
#true
#gap> IsRegularSemigroup(s);
#false
#
##
#gap> gens := [Transformation([2,1,4,5,3,7,8,9,10,6]),
#> Transformation([1,2,4,3,5,6,7,8,9,10]),
#> Transformation([1,2,3,4,5,6,10,9,8,7]),
#> Transformation([9,1,4,3,6,9,3,4,3,9])];;
#gap> s:=Monoid(gens);;
#gap> IsAbundantSemigroup(s);
#true
#gap> IsRegularSemigroup(s);
#false
#
##
#gap> gens := [Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
#> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
#gap> s:=Monoid(gens);;
#gap> IsAdequateSemigroup(s);
#false
#gap> gens := [Transformation([2,1,4,5,3,7,8,9,10,6]),
#> Transformation([1,2,4,3,5,6,7,8,9,10]),
#> Transformation([1,2,3,4,5,6,10,9,8,7]),
#> Transformation([9,1,4,3,6,9,3,4,3,9])];;
#gap> s:=Monoid(gens);;
#gap> IsAdequateSemigroup(s);
#false
#
# This is still part of PropertiesTest53
gap> s := Semigroup(
> [Transformation([1, 2, 3, 2]), Transformation([1, 2, 3, 3]),
>  Transformation([1, 2, 3, 4, 5, 7, 6]), Transformation([1, 2, 4, 3]),
>  Transformation([1, 2, 8, 3, 5, 6, 7, 8]),
>  Transformation([1, 6, 8, 8, 5, 7, 2, 8]),
>  Transformation([3, 8, 8, 8, 6, 2, 6, 7]),
>  Transformation([5, 2, 3, 4, 1]),
>  Transformation([6, 2, 3, 4, 7, 6, 7]),
>  Transformation([8, 8, 3, 4, 6, 7, 6, 2])]);;
gap> t := IdempotentGeneratedSubsemigroup(s);;
gap> Size(t);
105

# PropertiesTest54

#gap> gens := [ [ [ 2 ], [ 1 ], [ 4 ], [ 2 ], [ 3, 4 ] ], 
#>  [ [ 2, 3 ], [ 1, 2, 3, 4 ], [ 1 ], [ 1, 2, 4 ], [ 5 ] ], 
#>  [ [ 3 ], [ 1, 4 ], [ 1, 2, 3 ], [ 1, 3, 4 ], [ 2, 4, 5 ] ] ];;
#gap> s:=Semigroup(List(gens, BinaryRelationOnPoints));;
#gap> SetIsBinaryRelationSemigroup(s, true);;
#gap> Size(s);
#180
#gap> iso:=IsomorphismTransformationSemigroup(s);;
#gap> inv:=InverseGeneralMapping(iso);; t:=Range(iso);;
#gap> ForAll(s, x-> (x^iso)^inv=x);
#true
#gap> ForAll(t, x-> (x^inv)^iso=x);
#true
#gap> RespectsMultiplication(iso);
#true
#gap> Size(t);
#180
#
#
gap> S := Semigroup(Transformation([4, 2, 3, 3, 4]));;
gap> IsCongruenceFreeSemigroup(S);
true
gap> S := Semigroup(
>  Transformation([2, 2, 4, 4]),
>  Transformation([5, 3, 4, 4, 6, 6]));;
gap> IsCongruenceFreeSemigroup(S);
false

# PropertiesTest55: IsSynchronizingSemigroup
# for <IdentityTransformation>
gap> t := Transformation([1]);;
gap> s := Semigroup(t);
<trivial transformation group of degree 0 with 1 generator>
gap> IsSynchronizingSemigroup(s);
false
gap> IsSynchronizingSemigroup(s);
false
gap> IsSynchronizingSemigroup(s);
false

# PropertiesTest56: IsZeroSemigroup
gap> t := Transformation([1]);;

# For a trivial transformation semigroup
gap> s := Semigroup(t);
<trivial transformation group of degree 0 with 1 generator>
gap> IsZeroSemigroup(s);
true

# For a non-trivial zero semigroup of transformations & an ideal
gap> t := Transformation([1, 1, 2]);;
gap> s := Semigroup(t);
<commutative transformation semigroup of degree 3 with 1 generator>
gap> I := SemigroupIdeal(s, t ^ 2);;
gap> HasIsZeroSemigroup(s);
false
gap> IsZeroSemigroup(I);  # parent does not know it is zero
true
gap> HasIsZeroSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> I := SemigroupIdeal(s, t);;  # parent does know it is zero
gap> IsZeroSemigroup(I);
true
gap> I := SemigroupIdeal(s, t);;  # parent does know it is zero.
gap> GeneratorsOfSemigroup(I);;   # ideal now can use normal method
gap> IsZeroSemigroup(I);
true

# For a non-trivial transformation group (semigroup without a zero)
gap> t := Transformation([2, 1]);;
gap> s := Semigroup(t);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> IsZeroSemigroup(s);
false
gap> I := SemigroupIdeal(s, Transformation([1, 2]));
<commutative inverse transformation semigroup ideal of degree 2 with
  1 generator>
gap> IsZeroSemigroup(I);  # parent knows that it is not zero
false

# For a zero-group as a transformation semigroup
gap> s := Semigroup([
> Transformation([1, 3, 2, 3]),
> Transformation([1, 1, 1, 1])]);  # s is a 0-simple semigroup
<transformation semigroup of degree 4 with 2 generators>
gap> IsZeroSemigroup(s);
false
gap> IsZeroSimpleSemigroup(s);
true

# For a non-trivial inverse semigroup of partial perms (semigroup with a zero)
gap> s := InverseSemigroup([
> PartialPerm([1, 2], [3, 1]),
> PartialPerm([1, 2, 3], [1, 3, 4])]);
<inverse partial perm semigroup of rank 4 with 2 generators>
gap> MultiplicativeZero(s);
<empty partial perm>
gap> IsZeroSemigroup(s);
false
gap> s := InverseSemigroup(MultiplicativeZero(s));;
gap> IsZeroSemigroup(s);
true

# PropertiesTest57:
# IsZeroSemigroup: for a non-acting semigroup
# (Rees 0-matrix semigroup) and ideals
gap> s := ReesZeroMatrixSemigroup(Group(()), [[0]]);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> t := First(s, x -> not x = MultiplicativeZero(s));
(1,(),1)
gap> I := SemigroupIdeal(s, t);
<commutative Rees 0-matrix semigroup ideal with 1 generator>
gap> IsZeroSemigroup(I);
true
gap> HasIsZeroSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> I := SemigroupIdeal(s, t);;
gap> IsZeroSemigroup(I);
true
gap> s := ReesZeroMatrixSemigroup(Group(()), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> t := First(s, x -> not x = MultiplicativeZero(s));
(1,(),1)
gap> I := SemigroupIdeal(s, t);
<regular Rees 0-matrix semigroup ideal with 1 generator>
gap> IsZeroSemigroup(I);
false
gap> HasIsZeroSemigroup(s);
false
gap> IsZeroSemigroup(s);
false
gap> I := SemigroupIdeal(s, MultiplicativeZero(s));
<regular Rees 0-matrix semigroup ideal with 1 generator>
gap> IsZeroSemigroup(I);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/properties.tst");
