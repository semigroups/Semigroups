#############################################################################
##
#W  standard/ideals/acting.tst
#Y  Copyright (C) 2011-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local I, J, S, acting, an, x, y
gap> START_TEST("Semigroups package: standard/ideals/acting.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# MaximalDClasses, for an inverse acting semigroup ideal, 1
gap> S := InverseSemigroup(
>   Bipartition([[1, -2], [2, -6], [3, -7],
>      [4, 5, 7, 9, 10, -3, -4, -5, -8, -10], [6, -9], [8, -1]]),
>   Bipartition([[1, -5], [2, -3], [3, -8], [4, -1], [5, -9],
>      [6, 8, 10, -2, -7, -10], [7, -4], [9, -6]]));;
gap> x := Bipartition([[2, -9], [8, -4],
> [1, 3, 4, 5, 6, 7, 9, 10, -1, -2, -3, -5, -6, -7, -8, -10]]);;
gap> I := SemigroupIdeal(S, x);
<inverse bipartition semigroup ideal of degree 10 with 1 generator>
gap> MaximalDClasses(I);;
gap> x in last[1];
true

# MaximalDClasses, for an regular acting semigroup ideal, 1
gap> S := MotzkinMonoid(5);
<regular bipartition *-monoid of degree 5 with 10 generators>
gap> x := Bipartition([[1, -1], [2, -3], [3, -4], [4], [5, -5], [-2]]);;
gap> MaximalDClasses(SemigroupIdeal(S, x));
[ <Green's D-class: <bipartition: [ 1, -1 ], [ 2, -3 ], [ 3, -4 ], [ 4 ], 
      [ 5, -5 ], [ -2 ]>> ]

# DClassReps, for an regular acting semigroup ideal, 1
gap> S := MotzkinMonoid(5);
<regular bipartition *-monoid of degree 5 with 10 generators>
gap> x := Bipartition([[1, -1], [2, -3], [3, -4], [4], [5, -5], [-2]]);;
gap> Length(DClassReps(SemigroupIdeal(S, x))) = 5;
true

# NrDClasses, DClassReps, for an inverse acting smeigroups ideal, 1
gap> S := InverseSemigroup(
> Bipartition([[1, -3], [2, -5], [3, -8], [4, -6], [5, -2],
>      [6, 8, 9, 10, -1, -4, -9, -10], [7, -7]]),
>   Bipartition([[1, -1], [2, -7], [3, -6], [4, -2], [5, -8],
>      [6, -4], [7, 9, 10, -3, -9, -10], [8, -5]]),
>   Bipartition([[1, -2], [2, 4, 10, -5, -8, -10], [3, -4], [5, -7],
>      [6, -6], [7, -9], [8, -1], [9, -3]]),
>   Bipartition([[1, -6], [2, -5], [3, -7], [4, -8],
>      [5, 6, 7, 10, -2, -3, -4, -10], [8, -9], [9, -1]]),
>   Bipartition([[1, -9], [2, -3], [3, -1], [4, -4], [5, -2],
>      [6, -5], [7, 8, 10, -7, -8, -10], [9, -6]]));;
gap> x := Bipartition([
> [1, 3, 4, 5, 7, 8, 9, 10, -1, -2, -5, -6, -7, -8, -9, -10], [2, -3],
> [6, -4]]);;
gap> I := SemigroupIdeal(S, x, rec(acting := true));
<inverse bipartition semigroup ideal of degree 10 with 1 generator>
gap> NrDClasses(I);
3
gap> Length(DClassReps(I)) = 3;
true

# SemigroupData, for an acting semigroup ideal, 1
gap> S := Semigroup(Transformation([4, 3, 9, 7, 7, 8, 6, 8, 10, 4]),
>                   Transformation([6, 1, 6, 3, 1, 3, 6, 9, 9, 3]),
>                   rec(acting := true));;
gap> x := Transformation([6, 10, 4, 8, 8, 8, 8, 8, 7, 6]);;
gap> I := SemigroupIdeal(S, x, rec(acting := true));;
gap> SemigroupData(I);
<closed semigroup data with 33 reps, 26 lambda-values, 33 rho-values>

# SemigroupData, for an acting semigroup ideal, 2
gap> S := MotzkinMonoid(5);
<regular bipartition *-monoid of degree 5 with 10 generators>
gap> x := Bipartition([[1, -1], [2, -3], [3, -4], [4], [5, -5], [-2]]);;
gap> I := SemigroupIdeal(S, x, rec(acting := true));;
gap> SemigroupData(I);
<open semigroup ideal data with 0 reps, 0 lambda-values, 0 rho-values>

# SemigroupData, for an acting semigroup ideal, 3
gap> I := SemigroupIdeal(Semigroup(
>    [Transformation([2, 4, 2, 3, 3]), Transformation([3, 5, 5, 5, 3]),
>     Transformation([4, 4, 3, 3, 3]), Transformation([4, 5, 5, 1, 4]),
>     Transformation([5, 5, 2, 1, 5])]),
>    [Transformation([2, 4, 2, 3, 3]), Transformation([4, 5, 5, 1, 4])],
>   rec(acting := true));
<non-regular transformation semigroup ideal of degree 5 with 2 generators>
gap> SemigroupData(I);
<closed semigroup data with 12 reps, 17 lambda-values, 12 rho-values>

# GeneratorsOfSemigroup, for an acting semigroup ideal, 1
gap> S := Semigroup([PartialPerm([1, 2], [3, 1]),
>  PartialPerm([1, 2, 3], [4, 3, 1]),
>  PartialPerm([1, 2, 3], [5, 2, 1]),
>  PartialPerm([1, 2, 4], [4, 3, 2]),
>  PartialPerm([1, 2, 3, 5], [4, 3, 1, 2])]);;
gap> x := PartialPerm([3, 5], [3, 4]);
[5,4](3)
gap> I := SemigroupIdeal(S, x);;
gap> GeneratorsOfSemigroup(I);;

# GeneratorsOfSemigroup, for an acting semigroup ideal, 2
gap> S := Semigroup([PartialPerm([1, 2], [3, 1]),
>  PartialPerm([1, 2, 3], [4, 3, 1]),
>  PartialPerm([1, 2, 3], [5, 2, 1]),
>  PartialPerm([1, 2, 4], [4, 3, 2]),
>  PartialPerm([1, 2, 3, 5], [4, 3, 1, 2])]);;
gap> x := PartialPerm([2], [3]);;
gap> I := SemigroupIdeal(S, x);;
gap> GeneratorsOfSemigroup(I);;
gap> GeneratorsOfInverseSemigroup(I);;

# GeneratorsOfSemigroup, for an acting semigroup ideal, 3
gap> S := Semigroup([PartialPerm([1, 2], [3, 1]),
>  PartialPerm([1, 2, 3], [4, 3, 1]),
>  PartialPerm([1, 2, 3], [5, 2, 1]),
>  PartialPerm([1, 2, 4], [4, 3, 2]),
>  PartialPerm([1, 2, 3, 5], [4, 3, 1, 2])]);;
gap> x := PartialPerm([2], [3]);;
gap> I := SemigroupIdeal(S, x);;
gap> GeneratorsOfInverseSemigroup(I);;
gap> GeneratorsOfSemigroup(I);;

# GeneratorsOfSemigroup, for an acting semigroup ideal, 4
gap> S := Semigroup([Transformation([1, 4, 4, 3, 3]),
>  Transformation([3, 2, 4, 1]), Transformation([3, 4, 1, 2, 2]),
>  Transformation([3, 4, 5, 5, 1]), Transformation([5, 2, 5, 3, 5])]);;
gap> x := Transformation([1, 4, 4, 3, 3]);;
gap> I := SemigroupIdeal(S, x);;
gap> I = Semigroup(GeneratorsOfSemigroup(I));
true

# GeneratorsOfSemigroup, for an acting semigroup ideal, 5
gap> S := Semigroup(Transformation([3, 4, 3, 3, 1]),
>                   Transformation([4, 1, 2, 3, 4]),
>                   Transformation([5, 1, 1, 1, 4]));;
gap> x := Transformation([5, 1, 1, 1, 4]);;
gap> I := SemigroupIdeal(S, x);;
gap> I = Semigroup(GeneratorsOfSemigroup(I));
true

# GeneratorsOfSemigroup, for an acting semigroup ideal, 6
gap> S := InverseSemigroup(PartialPerm([1, 2, 3], [2, 1, 4]),
>                          PartialPerm([1, 3, 4], [1, 3, 2]),
>                          PartialPerm([1, 4], [3, 4]),
>                          PartialPerm([1, 2, 3, 4], [3, 4, 1, 2]),
>                          PartialPerm([1, 2, 3, 4], [4, 1, 2, 3]));;
gap> x := PartialPerm([1, 3], [1, 3]);;
gap> I := SemigroupIdeal(S, x);;
gap> I = InverseSemigroup(GeneratorsOfInverseSemigroup(I));
true

# GeneratorsOfSemigroup, for an acting semigroup ideal, 7
gap> S := InverseSemigroup(PartialPerm([1, 2, 3], [2, 1, 4]),
>                          PartialPerm([1, 3, 4], [1, 3, 2]),
>                          PartialPerm([1, 4], [3, 4]),
>                          PartialPerm([1, 2, 3, 4], [3, 4, 1, 2]),
>                          PartialPerm([1, 2, 3, 4], [4, 1, 2, 3]));;
gap> x := PartialPerm([1, 3], [1, 3]);;
gap> I := SemigroupIdeal(S, x);;
gap> I = Semigroup(GeneratorsOfSemigroup(I));
true

# Test GeneratorsOfInverseSemigroup
gap> S := InverseSemigroup(PartialPerm([1, 2, 3], [2, 1, 4]),
>                          PartialPerm([1, 3, 4], [1, 3, 2]),
>                          PartialPerm([1, 4], [3, 4]),
>                          PartialPerm([1, 2, 3, 4], [3, 4, 1, 2]),
>                          PartialPerm([1, 2, 3, 4], [4, 1, 2, 3]));;
gap> x := PartialPerm([1, 3], [1, 3]);;
gap> I := SemigroupIdeal(S, x);;
gap> GeneratorsOfSemigroup(I);;
gap> GeneratorsOfInverseSemigroup(I);;

# \in, for a regular acting semigroup ideal, wrong type, 1
gap> S := PartialTransformationMonoid(5);;
gap> x := Transformation([5, 6, 2, 3, 4, 6]);;
gap> I := SemigroupIdeal(S, x);;
gap> S.1 in I;
false
gap> PartialPerm([]) in I;
false
gap> Transformation([1, 1, 1, 1, 1, 1, 1]) in I;
false

# \in, for a regular acting semigroup ideal, too low rank, 2
gap> S := BrauerMonoid(5);
<regular bipartition *-monoid of degree 5 with 3 generators>
gap> x := Bipartition([[1, 5], [2, -1], [3, -4], [4, -5], [-2, -3]]);;
gap> I := SemigroupIdeal(S, x);;
gap> J := MinimalIdeal(I);
<simple bipartition *-semigroup ideal of degree 5 with 1 generator>
gap> Bipartition([[1, 2, 3, 4, 5], [-1, -2, -3, -4, -5]]) in I;
false

# \in, for a regular acting semigroup ideal, wrong lambda-val, 3
gap> S := BrauerMonoid(5);
<regular bipartition *-monoid of degree 5 with 3 generators>
gap> I := SemigroupIdeal(S, S.1);
<regular bipartition *-semigroup ideal of degree 5 with 1 generator>
gap> x := Bipartition([[1, 2, -1, -2], [3, -3], [4, -4], [5, -5]]);;
gap> x in I;
false

# \in, for a regular acting semigroup ideal, wrong lambda-val, 4
gap> S := BrauerMonoid(5);
<regular bipartition *-monoid of degree 5 with 3 generators>
gap> I := SemigroupIdeal(S, S.1);
<regular bipartition *-semigroup ideal of degree 5 with 1 generator>
gap> Size(I);;
gap> x := Bipartition([[1, 2, -1, -2], [3, -3], [4, -4], [5, -5]]);;
gap> x in I;
false

# \in, for a regular acting semigroup ideal, wrong lambda-val, 5
gap> S := BrauerMonoid(5);;
gap> I := SemigroupIdeal(S, S.1);;
gap> S.3 in I;
true
gap> Bipartition([[1, 2], [3, -5], [4, 5], [-1, -2], [-3, -4]])
> in I;
true

# \in, for a regular acting semigroup ideal, wrong lambda-val, 6
gap> S := Semigroup([Transformation([4, 3, 3, 3, 4]),
>  Transformation([5, 5, 1, 4, 5])]);;
gap> I := SemigroupIdeal(S, S.1);;
gap> S.2 in I;
false
gap> Transformation([3, 3, 4, 4, 4]) in I;
false

# \in, for a regular acting semigroup ideal, wrong lambda-val, 7
gap> S := Semigroup([Transformation([4, 3, 3, 3, 4]),
>  Transformation([5, 5, 1, 4, 5])]);;
gap> I := SemigroupIdeal(S, S.1);;
gap> Transformation([3, 3, 4, 4, 4]) in I;
false

# \in, for a regular acting semigroup ideal, looking in rho-orb, 8
gap> S := PartialTransformationMonoid(5);;
gap> x := Transformation([6, 2, 3, 4, 5, 6]);;
gap> I := SemigroupIdeal(S, x);;
gap> Transformation([5, 2, 4, 3, 2]) in I;
true

# \in, for a regular acting semigroup ideal, trivial Schutz gp 9
gap> S := JonesMonoid(5);;
gap> x := Bipartition([[1, -1], [2, 3], [4, -2], [5, -3], [-4, -5]]);;
gap> I := SemigroupIdeal(S, x);;
gap> RepresentativeOfMinimalIdeal(I) in I;
true

# \in, for a regular acting semigroup ideal, trivial Schutz gp 10
gap> S := JonesMonoid(5);;
gap> x := Bipartition([[1, -1], [2, 3], [4, -2], [5, -3], [-4, -5]]);;
gap> I := SemigroupIdeal(S, x);;
gap> y := Bipartition([[1, 2], [3, -5], [4, -1], [5, -2], [-3, -4]]);;
gap> y in I;
false

# ViewObj, for SemigroupIdealData 1
gap> S := Semigroup(Transformation([1, 3, 4, 1, 3]),
>                   Transformation([5, 5, 1, 1, 3]));
<transformation semigroup of degree 5 with 2 generators>
gap> x := Transformation([1, 3, 4, 1, 3]);;
gap> I := SemigroupIdeal(S, x, rec(acting := true));;
gap> SemigroupIdealData(I);
<closed semigroup ideal data with 10 reps, 10 lambda-values, 8 rho-values>

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/ideals/acting.tst");
