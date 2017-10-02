#############################################################################
##
#W  standard/attr.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/attr.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# AttributesTest1: MultiplicativeZero
# for a transformation semigroup/ideal
gap> t := Transformation([1]);;

# Trivial full transformation monoid T_1
# Previously this crashed: see issue #121 on Bitbucket
gap> s := Semigroup(t); # with displaying the semigroup
<trivial transformation group of degree 0 with 1 generator>
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true
gap> s := Semigroup(t);; # not displaying the semigroup
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true
gap> s := FullTransformationMonoid(1);;
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true

# Trivial transformation monoid with different rep.
gap> t := Transformation([2, 2, 3, 3]);;
gap> s := Semigroup(t); # with displaying the semigroup
<commutative transformation semigroup of degree 4 with 1 generator>
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true
gap> s := Semigroup(t);; # not displaying the semigroup
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true

# Issue #121 on Bitbucket (n x 1 rectangular band)
gap> s := Semigroup(Transformation([1, 2, 1]),
>                   Transformation([1, 2, 2]));;
gap> MultiplicativeZero(s);
fail
gap> Size(MinimalIdeal(s)) = 1;
false
gap> ForAny(s, x -> IsMultiplicativeZero(s, x));
false

# Other transformation semigroups
gap> s := Semigroup(FullTransformationMonoid(10), rec(acting := true));
<transformation monoid of degree 10 with 3 generators>
gap> MultiplicativeZero(s);
fail
gap> Size(MinimalIdeal(s)) = 1;
false

# Transformation semigroup ideal
gap> s := Semigroup([
> Transformation([2, 3, 4, 1]),
> Transformation([2, 1, 3, 4]),
> Transformation([3, 1, 1, 3])]);
<transformation semigroup of degree 4 with 3 generators>
gap> t := Transformation([1, 1, 1, 1]);;
gap> I := SemigroupIdeal(s, t);;
gap> HasMultiplicativeZero(s);
false
gap> MultiplicativeZero(I); # does not know whether parent has a zero
fail
gap> Size(MinimalIdeal(I)) = 1;
false
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s);
fail
gap> Size(MinimalIdeal(s)) = 1;
false
gap> I := SemigroupIdeal(s, t);;
gap> MultiplicativeZero(I); # does know whether parent has a zero
fail
gap> Size(MinimalIdeal(I)) = 1;
false

#T# AttributesTest2:
# MultiplicativeZero for a partial perm semigroup/ideal
gap> t := PartialPerm([], []);;

# For S = { <empty mapping> }
gap> s := Semigroup(t);;
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true
gap> s := SymmetricInverseMonoid(1);
<symmetric inverse monoid of degree 1>
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true

# For other trivial partial perm semigroups
gap> t := PartialPerm([2, 4], [2, 4]);;
gap> s := Semigroup(t);;
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true

# For a non-trivial partial perm semigroup
gap> s := Semigroup([PartialPerm([2], [1])]); # contains <empty pperm>
<commutative partial perm semigroup of rank 1 with 1 generator>
gap> MultiplicativeZero(s);
<empty partial perm>
gap> Size(MinimalIdeal(s)) = 1;
true
gap> s := Semigroup([
> PartialPerm([1, 2, 3], [1, 4, 2]),
> PartialPerm([1, 4], [1, 3])]); # does not contain <empty pperm>
<partial perm semigroup of rank 4 with 2 generators>
gap> MultiplicativeZero(s);
<identity partial perm on [ 1 ]>
gap> Size(MinimalIdeal(s)) = 1;
true
gap> s := InverseSemigroup([
> PartialPerm([1, 2, 3], [3, 4, 1]),
> PartialPerm([1, 2, 3, 4, 5], [3, 5, 1, 2, 4])]);
<inverse partial perm semigroup of rank 5 with 2 generators>
gap> MultiplicativeZero(s);
fail
gap> Size(MinimalIdeal(s)) = 1;
false

# For a partial perm semigroup ideal
gap> s := Semigroup([
> PartialPerm([1, 2, 3, 4], [2, 3, 4, 1]),
> PartialPerm([1, 2, 3, 4], [2, 1, 3, 4]),
> PartialPerm([1, 3], [2, 3])]);
<partial perm semigroup of rank 4 with 3 generators>
gap> t := PartialPerm([], []);;
gap> I := SemigroupIdeal(s, t);;
gap> HasMultiplicativeZero(s);
false
gap> MultiplicativeZero(I) = t; # does not know whether parent has a zero
true
gap> Size(MinimalIdeal(I)) = 1;
true
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true
gap> I := SemigroupIdeal(s, t);;
gap> MultiplicativeZero(I) = t; # does know whether parent has a zero
true
gap> Size(MinimalIdeal(I)) = 1;
true

#T# AttributesTest3:
# MultiplicativeZero for a bipartition semigroup/ideal
gap> s := PartitionMonoid(1);
<commutative bipartition monoid of degree 1 with 1 generator>
gap> MultiplicativeZero(s);
<bipartition: [ 1 ], [ -1 ]>
gap> Size(MinimalIdeal(s)) = 1;
true
gap> s := PartitionMonoid(2);
<regular bipartition *-monoid of size 15, degree 2 with 3 generators>
gap> MultiplicativeZero(s);
fail
gap> Size(MinimalIdeal(s)) = 1;
false
gap> s := PartitionMonoid(3);
<regular bipartition *-monoid of size 203, degree 3 with 4 generators>
gap> MultiplicativeZero(s);
fail
gap> Size(MinimalIdeal(s)) = 1;
false
gap> s := Semigroup([
> Bipartition([[1, 2, 3, 4, 5, -2], [-1], [-3], [-4], [-5]]),
> Bipartition([[1, 3, 5, -1], [2, 4, -2], [-3], [-4], [-5]])]);
<bipartition semigroup of degree 5 with 2 generators>
gap> MultiplicativeZero(s);
<bipartition: [ 1, 2, 3, 4, 5, -2 ], [ -1 ], [ -3 ], [ -4 ], [ -5 ]>
gap> Size(MinimalIdeal(s)) = 1;
true

# Ideals
gap> s := PartitionMonoid(3);;
gap> t := Bipartition([[1, -2], [2], [3, -3], [-1]]);;
gap> I := SemigroupIdeal(s, t);
<regular bipartition *-semigroup ideal of degree 3 with 1 generator>
gap> HasMultiplicativeZero(s);
false
gap> MultiplicativeZero(I);
fail
gap> Size(MinimalIdeal(I)) = 1;
false
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s);
fail
gap> Size(MinimalIdeal(s)) = 1;
false
gap> I := SemigroupIdeal(s, t);;
gap> MultiplicativeZero(I);
fail
gap> Size(MinimalIdeal(I)) = 1;
false
gap> t := Bipartition([[1], [-1]]);;
gap> s := Semigroup([t, Bipartition([[1, -1]])]);;
gap> I := SemigroupIdeal(s, t);;
gap> HasMultiplicativeZero(s);
false
gap> MultiplicativeZero(I);
<bipartition: [ 1 ], [ -1 ]>
gap> Size(MinimalIdeal(I)) = 1;
true
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s);
<bipartition: [ 1 ], [ -1 ]>
gap> Size(MinimalIdeal(s)) = 1;
true
gap> I := SemigroupIdeal(s, t);;
gap> MultiplicativeZero(I);
<bipartition: [ 1 ], [ -1 ]>
gap> Size(MinimalIdeal(I)) = 1;
true

#T# AttributesTest4:
# MultiplicativeZero for a block bijection inverse semigroup/ideal
gap> S := AsSemigroup(IsBlockBijectionSemigroup, SymmetricInverseMonoid(1));
<commutative inverse block bijection monoid of degree 2 with 1 generator>
gap> MultiplicativeZero(S);
<block bijection: [ 1, 2, -1, -2 ]>
gap> Size(MinimalIdeal(S)) = 1;
true
gap> S := AsSemigroup(IsBlockBijectionSemigroup, SymmetricInverseMonoid(4));
<inverse block bijection monoid of degree 5 with 3 generators>
gap> MultiplicativeZero(S);
<block bijection: [ 1, 2, 3, 4, 5, -1, -2, -3, -4, -5 ]>
gap> Size(MinimalIdeal(S)) = 1;
true
gap> s := InverseSemigroup([
> Bipartition([[1, -3], [2, -4], [3, -1], [4, 5, 6, -2, -5, -6]]),
> Bipartition([[1, -3], [2, -5], [3, -1], [4, -2], [5, -4],
> [6, -6]])]);
<inverse block bijection semigroup of degree 6 with 2 generators>
gap> MultiplicativeZero(s);
fail
gap> Size(MinimalIdeal(s)) = 1;
false

# Test MultiplicativeZero (for an infinite semigroup)
#gap> S := Semigroup([Matrix(IsMaxPlusMatrix, [[-2, 2, 0], [-1, 0, 0], [1, -3, 1]]),
#>  Matrix(IsMaxPlusMatrix, [[- infinity, 0, 0], [0, 1, 0], [1, -1, 0]])]);;
#gap> MultiplicativeZero(S); 
# FIXME Enters an infinite loop in a library method because S is not finite

# Ideals
gap> s := InverseSemigroup([
> Bipartition([[1, -1], [2, 6, -4, -6], [3, -5], [4, -2],
>  [5, -3]]),
> Bipartition([[1, -5], [2, -4], [3, -3], [4, -2], [5, -1],
>  [6, -6]])]);
<inverse block bijection semigroup of degree 6 with 2 generators>
gap> t := Bipartition(
> [[1, -1], [2, -2], [3, -3], [4, 6, -4, -6], [5, -5]]);;
gap> I := SemigroupIdeal(s, t);
<inverse bipartition semigroup ideal of degree 6 with 1 generator>
gap> HasMultiplicativeZero(s);
false
gap> MultiplicativeZero(I);
fail
gap> Size(MinimalIdeal(I)) = 1;
false
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s);
fail
gap> Size(MinimalIdeal(s)) = 1;
false
gap> I := SemigroupIdeal(s, t);;
gap> MultiplicativeZero(I);
fail
gap> Size(MinimalIdeal(I)) = 1;
false

#T# AttributesTest5:
# MultiplicativeZero where MinimalDClass is known
gap> s := Semigroup(FullTransformationMonoid(10), rec(acting := true));
<transformation monoid of degree 10 with 3 generators>
gap> MinimalDClass(s);;
gap> HasSize(last);
false
gap> MultiplicativeZero(s);
fail
gap> s := Semigroup(s, rec(acting := true));;
gap> HasMinimalDClass(s);
false
gap> Size(MinimalDClass(s));
10
gap> HasMinimalDClass(s) and HasSize(MinimalDClass(s));
true
gap> MultiplicativeZero(s);
fail
gap> gens := [
> Transformation([1, 13, 11, 4, 11, 12, 3, 1, 1, 1, 1, 4, 15, 2, 13]),
> Transformation([3, 11, 14, 4, 11, 13, 13, 5, 3, 11, 14, 14, 10, 15, 12]),
> Transformation([5, 13, 11, 4, 9, 13, 8, 1, 2, 12, 6, 12, 11, 8, 1])];;
gap> s := Semigroup(gens);
<transformation semigroup of degree 15 with 3 generators>
gap> HasMinimalDClass(s);
false
gap> MultiplicativeZero(s);
Transformation( [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 ] )
gap> s := Semigroup(gens, rec(acting := true));;
gap> MinimalDClass(s);
<Green's D-class: Transformation( [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
   4 ] )>
gap> HasSize(MinimalDClass(s));
false
gap> MultiplicativeZero(s);
Transformation( [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 ] )
gap> s := Semigroup(gens, rec(acting := true));;
gap> Size(MinimalDClass(s));
1
gap> MultiplicativeZero(s);
Transformation( [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 ] )

#T# attr: RightCayleyGraphSemigroup
gap> S := Semigroup(PartialPerm([1, 2, 3], [1, 3, 4]),
>                   PartialPerm([1, 2, 3], [2, 5, 3]),
>                   PartialPerm([1, 2, 3], [4, 1, 2]),
>                   PartialPerm([1, 2, 3, 4], [2, 4, 1, 5]),
>                   PartialPerm([1, 3, 5], [5, 1, 3]));;
gap> RightCayleyGraphSemigroup(S);;
gap> Length(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(last)) = NrRClasses(S);
true

#T# attr: RightCayleyGraphSemigroup, infinite
gap> RightCayleyGraphSemigroup(FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `CayleyGraphSemigroup' on 1 arguments

#T# attr: LeftCayleyGraphSemigroup
gap> S := Monoid(BooleanMat([[1, 1, 1, 1, 1], [1, 0, 1, 0, 0],
>                              [1, 1, 0, 1, 0], [1, 1, 1, 1, 1],
>                              [1, 1, 0, 0, 0]]),
>                BooleanMat([[0, 0, 1, 0, 0], [1, 0, 1, 1, 0],
>                              [1, 0, 1, 1, 1], [0, 1, 1, 1, 0],
>                              [0, 1, 1, 1, 0]]),
>                BooleanMat([[0, 0, 0, 1, 1], [0, 0, 1, 1, 0],
>                              [0, 0, 1, 1, 0], [1, 1, 0, 1, 0],
>                              [1, 0, 1, 0, 1]]),
>                BooleanMat([[0, 1, 1, 1, 0], [0, 0, 0, 1, 0],
>                              [1, 1, 1, 0, 1], [1, 0, 1, 0, 0],
>                              [1, 0, 1, 1, 0]]),
>                BooleanMat([[1, 0, 0, 0, 1], [1, 0, 0, 0, 1],
>                              [0, 0, 0, 0, 1], [0, 1, 1, 0, 1],
>                              [1, 1, 1, 0, 1]]));;
gap> LeftCayleyGraphSemigroup(S);;
gap> Length(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(last)) = NrLClasses(S);
true

#T# attr: RightCayleyGraphSemigroup, infinite
gap> LeftCayleyGraphSemigroup(FreeInverseSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `CayleyGraphDualSemigroup' on 1 argument\
s

#T# attr: IsomorphismReesMatrixSemigroup
gap> D := GreensDClassOfElement(Semigroup(
> Bipartition([[1, 2, 3, -3], [4, -4, -5], [5, -1], [-2]]),
> Bipartition([[1, 4, -2, -3], [2, 3, 5, -5], [-1, -4]]),
> Bipartition([[1, 5], [2, 4, -3, -5], [3, -1, -2], [-4]]),
> Bipartition([[1], [2], [3, 5, -1, -2], [4, -3], [-4, -5]]),
> Bipartition([[1], [2], [3], [4, -1, -4], [5], [-2, -3], [-5]])),
> Bipartition([[1], [2], [3], [4, -1, -4], [5], [-2, -3], [-5]]));;
gap> PrincipalFactor(D);
<Rees 0-matrix semigroup 12x15 over Group(())>

#T# attr: IsomorphismReesMatrixSemigroup, error, 1/1
gap> S := FullTransformationMonoid(3);;
gap> D := DClass(S, Transformation([1, 2, 1]));;
gap> IsomorphismReesMatrixSemigroup(D);
Error, Semigroups: IsomorphismReesMatrixSemigroup: usage,
the D-class is not a subsemigroup,
gap> D := MinimalDClass(S);;
gap> IsomorphismReesMatrixSemigroup(D);
MappingByFunction( <Green's D-class: Transformation( [ 1, 1, 1 ] )>, 
<Rees matrix semigroup 1x3 over Group(())>
 , function( x ) ... end, function( x ) ... end )

#T# attr: IrredundantGeneratingSubset, for a collection of elements
gap> G := CyclicGroup(3);;
gap> R := GF(2);;
gap> GR := GroupRing(R, G);;
gap> iso := IsomorphismTransformationSemigroup(GR);;
gap> S := Range(iso);;
gap> S := Semigroup(IrredundantGeneratingSubset(SmallGeneratingSet(S)));;

#T# attr: IrredundantGeneratingSubset: for a semigroup
gap> S := RandomMonoid(IsBooleanMatMonoid, 10, 3);;
gap> T := Semigroup(IrredundantGeneratingSubset(S));;
gap> S = T;
true

#T# attr: IrredundantGeneratingSubset: for a set with one element, 1
gap> IrredundantGeneratingSubset([RandomTransformation(10)]);;

#T# attr: IrredundantGeneratingSubset: for a set with one element, 2
gap> S := Monoid([Transformation([1, 1]), Transformation([2, 1]),
>  Transformation([2, 2])], rec(acting := true));
<transformation monoid of degree 2 with 3 generators>
gap> Size(IrredundantGeneratingSubset(S));
2

#T# attr: IrredundantGeneratingSubset: for a set with a single repeated
# element
gap> S := Semigroup([Transformation([1, 1]), Transformation([1, 1])]);
<transformation semigroup of degree 2 with 2 generators>
gap> Size(IrredundantGeneratingSubset(S));
1

#T# attr: IrredundantGeneratingSubset: test info statements
gap> S := MonogenicSemigroup(IsTransformationSemigroup, 4, 1);;
gap> S := Semigroup(Elements(S));
<transformation semigroup of degree 5 with 4 generators>
gap> x := InfoLevel(InfoSemigroups);;
gap> SetInfoLevel(InfoSemigroups, 3);
gap> IrredundantGeneratingSubset(S);
at 	1 of 	4 with 	0 redundant, 	0 non-redundant
at 	2 of 	4 with 	0 redundant, 	1 non-redundant
at 	3 of 	4 with 	1 redundant, 	1 non-redundant
at 	4 of 	4 with 	2 redundant, 	1 non-redundant

[ Transformation( [ 1, 1, 2, 3, 4 ] ) ]
gap> SetInfoLevel(InfoSemigroups, x);

#T# attr: PrincipalFactor: for a D-class
gap> D := GreensDClassOfElement(
>  Semigroup(
>    BooleanMat([[0, 1, 1, 0, 1, 0], [0, 1, 0, 1, 0, 0], [1, 1, 1, 0, 0, 0],
>                [0, 1, 1, 1, 1, 1], [1, 0, 1, 0, 0, 1], [1, 0, 1, 0, 1, 1]]),
>    BooleanMat([[1, 1, 1, 1, 1, 0], [0, 0, 0, 0, 1, 0], [0, 1, 0, 1, 1, 0],
>                [1, 0, 1, 1, 1, 0], [1, 1, 1, 0, 0, 1], [1, 1, 0, 0, 0, 0]])),
>  BooleanMat([[1, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1, 1],
>              [1, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1, 1], [1, 1, 1, 1, 1, 1]]));;
gap> PrincipalFactor(D);
<Rees matrix semigroup 1x1 over Group(())>

#T# attr: SmallSemigroupGeneratingSet: for a collection with > 1 elements
gap> SmallSemigroupGeneratingSet([
> Transformation([1, 1, 1, 1, 4]), Transformation([1, 2, 2, 1, 1]),
> Transformation([1, 2, 5, 4, 4]), Transformation([1, 3, 3, 5, 1]),
> Transformation([2, 2, 1, 2, 4]), Transformation([3, 2, 3, 3, 2]),
> Transformation([3, 5, 2, 4, 4]), Transformation([3, 5, 4, 5, 4]),
> Transformation([4, 4, 2, 5, 5]), Transformation([5, 2, 3, 5, 2])]);;

#T# attr: SmallSemigroupGeneratingSet: for a collection with 1 elements
gap> SmallSemigroupGeneratingSet([BooleanMat([[0, 1, 0, 0], [0, 1, 1, 0], [0,
> 1, 0, 0], [1, 0, 1, 1]])]);
[ Matrix(IsBooleanMat, [[0, 1, 0, 0], [0, 1, 1, 0], [0, 1, 0, 0], 
      [1, 0, 1, 1]]) ]

#T# attr: SmallSemigroupGeneratingSet: for a semigroup
gap> S := Semigroup([PartialPerm([1, 2], [3, 2]),
> PartialPerm([1, 2, 3], [2, 3, 4]),
> PartialPerm([1, 2, 3], [2, 5, 3]),
> PartialPerm([1, 4], [1, 3]),
> PartialPerm([1, 2, 3, 4], [3, 5, 1, 2]),
> PartialPerm([1, 2, 3, 4], [5, 4, 2, 1]),
> PartialPerm([1, 3, 5], [1, 4, 2]),
> PartialPerm([1, 2, 4, 5], [3, 2, 5, 1]),
> PartialPerm([1, 2, 4, 5], [3, 5, 1, 2]),
> PartialPerm([1, 3, 5], [4, 3, 1])]);;
gap> SmallSemigroupGeneratingSet(S);;

#T# attr: SmallMonoidGeneratingSet: for a singleton set 1/2
gap> SmallMonoidGeneratingSet([IdentityTransformation]);
[ IdentityTransformation ]

#T# attr: SmallMonoidGeneratingSet: for a singleton set 2/2
gap> SmallMonoidGeneratingSet([Transformation([2, 1, 2])]);
[ Transformation( [ 2, 1, 2 ] ) ]

#T# attr: SmallMonoidGeneratingSet: for a 0 generator monoid, 1
gap> S := Monoid(Bipartition([[1, -1]]));;
gap> SmallMonoidGeneratingSet(S);
[ <block bijection: [ 1, -1 ]> ]

#T# attr: SmallMonoidGeneratingSet: for a 0 generator monoid, 2
gap> S := FreeMonoid(0);;
gap> SmallMonoidGeneratingSet(S);
[  ]

#T# attr: SmallInverseSemigroupGeneratingSet: for collection > 1 element 
gap> SmallInverseSemigroupGeneratingSet(
> [PartialPerm([1, 2], [4, 1]),
>  PartialPerm([1, 2], [5, 2]), PartialPerm([1, 2, 3], [3, 2, 1]),
>  PartialPerm([1, 2, 3], [3, 2, 4]),
>  PartialPerm([1, 2, 3, 4], [1, 2, 3, 5]),
>  PartialPerm([1, 3, 4], [3, 2, 1]), PartialPerm([1, 2, 4], [3, 1, 2]),
>  PartialPerm([1, 2, 3, 4, 5], [3, 1, 5, 4, 2]),
>  PartialPerm([1, 2, 3, 5], [5, 4, 2, 3])]);;

#T# attr: SmallInverseSemigroupGeneratingSet: for collection 1 element 
gap> SmallInverseSemigroupGeneratingSet([PartialPerm([1, 2, 3, 7, 9, 10,
> 11, 12], [4, 6, 8, 12, 5, 9, 1, 3])]);
[ [2,6][7,12,3,8][10,9,5][11,1,4] ]

#T# attr: SmallInverseSemigroupGeneratingSet: for an inverse semigroup
gap> S :=
> InverseSemigroup([PartialPerm([1, 2], [1, 2]),
>   PartialPerm([1, 2, 4], [2, 3, 1]), PartialPerm([1, 3, 4], [3, 2, 4]),
>   PartialPerm([1, 2, 4, 5], [1, 3, 5, 4]),
>   PartialPerm([1, 2, 4, 5], [2, 1, 3, 5]),
>   PartialPerm([1, 3, 5], [3, 1, 2]),
>   PartialPerm([1, 2, 3, 5], [3, 1, 2, 5]),
>   PartialPerm([1, 2, 3, 4, 5], [3, 5, 1, 2, 4]),
>   PartialPerm([1, 3, 5], [4, 3, 2]),
>   PartialPerm([1, 2, 3, 5], [4, 1, 2, 3])]);;
gap> SmallInverseSemigroupGeneratingSet(S);;

#T# attr: SmallInverseMonoidGeneratingSet: for 0 generators, 1
gap> S := InverseMonoid(PartialPerm([1, 2, 3]));
<trivial partial perm group of rank 3 with 1 generator>
gap> SmallInverseMonoidGeneratingSet(S);
[ <identity partial perm on [ 1, 2, 3 ]> ]

#T# attr: SmallInverseMonoidGeneratingSet: for 0 generators, 2
gap> S := Group(IdentityTransformation);
<transformation group of degree 0 with 1 generator>
gap> S := Subgroup(S, []);
<trivial transformation group of degree 0 with 0 generators>
gap> IsInverseMonoid(S) and IsGeneratorsOfInverseSemigroup(S);
true
gap> SmallInverseMonoidGeneratingSet(S);
[  ]

#T# attr: SmallInverseMonoidGeneratingSet: for > 0 generators 1/2
gap> S := InverseMonoid([PartialPerm([1, 3], [2, 3]),
> PartialPerm([1, 3], [3, 1]),
> PartialPerm([1, 2, 3], [3, 2, 4]),
> PartialPerm([1, 4], [1, 3])]);;
gap> SmallInverseMonoidGeneratingSet(S);;

#T# attr: SmallInverseMonoidGeneratingSet: for > 0 generators 2/2
gap> Set(SmallInverseMonoidGeneratingSet(DualSymmetricInverseMonoid(3)));
[ <block bijection: [ 1, 2, -3 ], [ 3, -1, -2 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -1 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -3 ], [ 3, -1 ]> ]

#T# attr: SmallInverseSemigroupGeneratingSet: for a collection
gap> coll := [Bipartition([[1, -1], [2, -2], [3, -3], [4, -4], [5, -5]]),
> Bipartition([[1, -1], [2, -4], [3, -3], [4], [5], [-2], [-5]]),
> Bipartition([[1, -2], [2, -4], [3, -3], [4], [5], [-1], [-5]]),
> Bipartition([[1, -3], [2, -4], [3], [4, -1], [5], [-2], [-5]]),
> Bipartition([[1, -1], [2, -2], [3], [4, -4], [5, -3], [-5]]),
> Bipartition([[1, -1], [2, -5], [3, -4], [4], [5, -2], [-3]]),
> Bipartition([[1, -3], [2], [3, -5], [4, -2], [5, -4], [-1]]),
> Bipartition([[1, -3], [2, -1], [3, -5], [4], [5, -2], [-4]]),
> Bipartition([[1, -4], [2], [3], [4, -1], [5, -5], [-2], [-3]]),
> Bipartition([[1, -5], [2], [3, -1], [4, -2], [5, -3], [-4]]),
> Bipartition([[1, -5], [2, -3], [3], [4, -4], [5, -1], [-2]]),
> Bipartition([[1, -1], [2], [3, -3], [4, -2], [5], [-4], [-5]]),
> Bipartition([[1], [2, -1], [3, -3], [4, -2], [5], [-4], [-5]]),
> Bipartition([[1, -4], [2], [3, -1], [4, -2], [5], [-3], [-5]]),
> Bipartition([[1, -1], [2, -2], [3, -5], [4, -4], [5], [-3]]),
> Bipartition([[1, -1], [2, -5], [3], [4, -3], [5, -2], [-4]]),
> Bipartition([[1], [2, -4], [3, -1], [4, -5], [5, -3], [-2]]),
> Bipartition([[1, -3], [2, -4], [3, -5], [4], [5, -1], [-2]]),
> Bipartition([[1, -5], [2], [3, -2], [4, -4], [5, -1], [-3]])];;
gap> SmallInverseSemigroupGeneratingSet(coll);;

#T# attr: SmallInverseMonoidGeneratingSet: for a collection
gap> coll := [PartialPerm([1, 2, 3, 4, 5], [1, 2, 3, 4, 5]),
> PartialPerm([1, 2], [1, 4]), PartialPerm([1, 2, 3], [1, 4, 2]),
> PartialPerm([1, 2, 3], [3, 5, 2]),
> PartialPerm([1, 2, 3, 4], [1, 5, 4, 2]),
> PartialPerm([1, 2, 4], [2, 3, 1]), PartialPerm([1, 3, 4], [3, 2, 4]),
> PartialPerm([1, 2, 3, 4], [5, 2, 3, 4]),
> PartialPerm([1, 2, 4, 5], [1, 3, 5, 4]),
> PartialPerm([1, 3, 5], [3, 1, 2]),
> PartialPerm([1, 2, 4, 5], [5, 3, 2, 1]), PartialPerm([1, 4], [1, 2]),
> PartialPerm([1, 2, 4], [1, 3, 2]), PartialPerm([2, 3, 5], [3, 1, 2]),
> PartialPerm([1, 2, 4, 5], [1, 4, 3, 2]),
> PartialPerm([1, 2, 3], [4, 1, 2]), PartialPerm([2, 3, 4], [3, 1, 4]),
> PartialPerm([2, 3, 4, 5], [2, 3, 4, 1]),
> PartialPerm([1, 3, 4, 5], [1, 2, 5, 4]),
> PartialPerm([1, 2, 3], [3, 5, 1]),
> PartialPerm([1, 2, 3, 5], [5, 4, 2, 1])];;
gap> SmallInverseMonoidGeneratingSet(coll);;

#T# attr: SmallInverseMonoidGeneratingSet: for a collection of 1 element
gap> SmallInverseMonoidGeneratingSet([PartialPerm([1, 2, 4])]);
[ [3,4](1)(2) ]

#T# attr: SmallInverseSemigroupGeneratingSet: for non-inverse-op elements
gap> SmallInverseSemigroupGeneratingSet([RandomTransformation(10)]);
Error, Semigroups: SmallInverseSemigroupGeneratingSet: usage,
the argument must satisfy IsGeneratorsOfInverseSemigroup

#T# attr: SmallInverseMonoidGeneratingSet: for non-inverse-op elements
gap> SmallInverseMonoidGeneratingSet([RandomMatrix(IsBooleanMat, 10)]);
Error, Semigroups: SmallInverseMonoidGeneratingSet: usage,
the argument must satisfy IsGeneratorsOfInverseSemigroup

#T# attr: SmallInverseMonoidGeneratingSet: for One
gap> SmallInverseMonoidGeneratingSet([PartialPerm([1, 2, 3])]);
[  ]

#T# attr: SmallGeneratingSet: for an ideal
gap> S := SemigroupIdeal(Semigroup(
>     BooleanMat([[0, 1, 0], [1, 0, 0], [0, 0, 1]]),
>     BooleanMat([[0, 1, 0], [0, 0, 1], [1, 0, 0]]),
>     BooleanMat([[1, 0, 0], [0, 1, 0], [1, 0, 1]]),
>     BooleanMat([[1, 0, 0], [0, 1, 0], [0, 0, 0]])),
>     BooleanMat([[1, 0, 0], [0, 0, 0], [1, 1, 0]]));;
gap> SmallGeneratingSet(S);
[ Matrix(IsBooleanMat, [[1, 0, 0], [0, 0, 0], [1, 1, 0]]) ]

#T# attr: SmallGeneratingSet: for a group
gap> S := Group(IdentityTransformation);
<transformation group of degree 0 with 1 generator>
gap> SmallGeneratingSet(S);
[ IdentityTransformation ]

#T# attr: SmallGeneratingSet: for an inverse monoid
gap> S := InverseMonoid([PartialPerm([1, 2], [3, 2]),
> PartialPerm([1, 2, 4], [2, 3, 1]), PartialPerm([1, 2, 4], [3, 4, 2]),
> PartialPerm([1, 4], [4, 2])]);;
gap> SmallGeneratingSet(S);;

#T# attr: SmallGeneratingSet: for an inverse semigroup
gap> S := InverseSemigroup([PartialPerm([1, 2], [2, 3]),
>                             PartialPerm([1, 3], [3, 1]),
>                             PartialPerm([1, 2, 3], [4, 3, 2])]);;
gap> SmallGeneratingSet(S);;

#T# attr: SmallGeneratingSet: for a semigroup 
gap> S := Semigroup([Transformation([3, 1, 4, 1, 3]),
>                    Transformation([3, 5, 3, 2, 4])]);;
gap> SmallGeneratingSet(S);;

#T# attr: StructureDescription for a Brandt semigroup
gap> S := SemigroupIdeal(
> InverseSemigroup([
>   PartialPermNC([1, 2, 3, 4], [4, 1, 2, 6]),
>   PartialPermNC([1, 2, 4], [5, 2, 3]),
>   PartialPermNC([1, 2, 3, 6], [1, 3, 4, 5]),
>   PartialPermNC([1, 2, 3, 4, 6], [2, 4, 6, 1, 5]),
>   PartialPermNC([1, 2, 3, 6], [5, 1, 6, 3])]),
> [PartialPermNC([2], [2])]);;
gap> IsBrandtSemigroup(S);
true
gap> StructureDescription(S);
"B(1, 6)"

#T# attr: StructureDescription for a group as semigroup 1/3
gap> S := AsSemigroup(IsTransformationSemigroup, AlternatingGroup(5));;
gap> IsGroupAsSemigroup(S);
true
gap> StructureDescription(S);
"A5"

#T# attr: StructureDescription for a group as semigroup 2/3
gap> S := Semigroup(Transformation([2, 1, 1]));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> IsGroupAsSemigroup(S);
true
gap> StructureDescription(S);
"C2"

#T# attr: StructureDescription for a group as semigroup 3/3
gap> S := SymmetricGroup(3);;
gap> StructureDescription(S);
"S3"

#T# Test StructureDescription for a group as semigroup
gap> S := Monoid(IdentityTransformation);;
gap> StructureDescription(S);
"1"

#T# attr: IsGreensDGreaterThanFunc
gap> S := RegularBooleanMatMonoid(3);;
gap> foo := IsGreensDGreaterThanFunc(S);
function( x, y ) ... end
gap> x := BooleanMat([[1, 0, 1], [1, 1, 0], [1, 0, 1]]);;
gap> y := BooleanMat([[1, 0, 1], [0, 0, 0], [1, 0, 0]]);;
gap> foo(x, y);
true
gap> foo(y, x);
false
gap> z := RepresentativeOfMinimalIdeal(S);
Matrix(IsBooleanMat, [[0, 0, 0], [0, 0, 0], [0, 0, 0]])
gap> foo(x, z);
true
gap> foo(z, x);
false
gap> foo(z, y);
false
gap> foo(y, z);
true
gap> foo(z, z);
false

# Test IsGreensDGreaterThanFunc for an infinite enumerable semigroup
gap> S := Semigroup([Matrix(IsMaxPlusMatrix,
>                           [[-2, 2, 0], [-1, 0, 0], [1, -3, 1]]),
>                    Matrix(IsMaxPlusMatrix,
>                           [[-infinity, 0, 0], [0, 1, 0], [1, -1, 0]])]);;
gap> IsGreensDGreaterThanFunc(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `IsGreensDGreaterThanFunc' on 1 argument\
s

#T# attr: IsGreensDGreaterThanFunc, error
gap> IsGreensDGreaterThanFunc(FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `IsGreensDGreaterThanFunc' on 1 argument\
s

#T# attr: MaximalDClasses
gap> S := RegularBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 4 generators>
gap> MaximalDClasses(S);
[ <Green's D-class: Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]])> ]

#T# attr: StructureDescriptionMaximalSubgroups
gap> S := RegularBooleanMatMonoid(3);;
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2", "S3" ]

#T# attr: IdempotentGeneratedSubsemigroup, 1
gap> S := RegularBooleanMatMonoid(3);;
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> HasIsIdempotentGenerated(T) and IsIdempotentGenerated(T);
true
gap> Size(T);
381

#T# attr: IdempotentGeneratedSubsemigroup, 2
gap> S := SymmetricInverseMonoid(3);;
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> HasIsSemilattice(T) and IsSemilattice(T);
true
gap> S := AsSemigroup(IsTransformationSemigroup, S);;
gap> IsInverseSemigroup(S);
true
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> HasIsSemilattice(T) and IsSemilattice(T);
true
gap> S := Semigroup([
>  PartialPerm([1, 3], [5, 4]),
>  PartialPerm([1, 2, 5], [1, 4, 5]),
>  PartialPerm([1, 3, 4], [3, 4, 5])]);;
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> IsInverseSemigroup(S);
false
gap> HasIsSemilattice(T) and IsSemilattice(T);
true

#T# attr: InjectionPrincipalFactor
gap> S := Monoid([BooleanMat([[1, 0, 1], [0, 1, 0], [0, 0, 1]]),
>   BooleanMat([[1, 0, 0], [0, 1, 1], [0, 0, 1]]),
>   BooleanMat([[1, 0, 0], [0, 1, 0], [1, 0, 1]]),
>   BooleanMat([[1, 0, 0], [0, 1, 0], [0, 1, 1]]),
>   BooleanMat([[1, 0, 0], [1, 1, 0], [0, 0, 1]]),
>   BooleanMat([[1, 1, 0], [0, 1, 0], [0, 0, 1]]),
>   BooleanMat([[1, 1, 0], [0, 0, 0], [0, 1, 1]]),
>   BooleanMat([[1, 0, 1], [0, 1, 0], [0, 0, 0]]),
>   BooleanMat([[1, 0, 0], [0, 0, 1], [0, 0, 1]]),
>   BooleanMat([[0, 0, 0], [0, 1, 0], [0, 0, 1]]),
>   BooleanMat([[1, 0, 0], [0, 0, 0], [0, 0, 1]]),
>   BooleanMat([[1, 0, 0], [0, 1, 0], [0, 0, 0]])]);;
gap> D := DClass(S, BooleanMat([[1, 0, 1], [1, 1, 1], [1, 0, 1]]));;
gap> map := InjectionPrincipalFactor(D);
MappingByFunction( <Green's D-class: Matrix(IsBooleanMat, 
  [[1, 0, 1], [1, 1, 1], [1, 0, 1]])>, <Rees 0-matrix semigroup 12x12 over 
  Group(())>, function( x ) ... end, function( x ) ... end )
gap> inv := InverseGeneralMapping(map);;
gap> ForAll(D, x -> (x ^ map) ^ inv = x);
true
gap> MultiplicativeZero(Range(map)) ^ inv;
fail
gap> x := BooleanMat([[0, 0, 0], [1, 1, 0], [0, 0, 0]]);;
gap> x ^ map;
fail
gap> D := First(DClasses(S), x -> not IsRegularGreensClass(x));
<Green's D-class: Matrix(IsBooleanMat, [[1, 0, 1], [1, 1, 0], [0, 0, 1]])>
gap> InjectionPrincipalFactor(D);
Error, Semigroups: InjectionPrincipalFactor: usage,
the argument <D> must be a regular D-class,

#T# attr: MultiplicativeNeutralElement
gap> S := Semigroup([BooleanMat([[0, 0, 1], [0, 0, 1], [0, 1, 1]]),
>  BooleanMat([[1, 0, 0], [1, 1, 0], [0, 1, 1]])]);;
gap> MultiplicativeNeutralElement(S);
fail
gap> S := Semigroup(AsBooleanMat(Transformation([2, 1, 2]), 3));;
gap> Display(MultiplicativeNeutralElement(S));
1 0 0
0 1 0
1 0 0
gap> S := RegularBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> MultiplicativeNeutralElement(S);
Matrix(IsBooleanMat, [[1, 0], [0, 1]])
gap> S := Semigroup([Transformation([1, 2, 3, 3, 5, 5]),
> Transformation([3, 3, 5, 3, 5, 3])]);
<transformation semigroup of degree 6 with 2 generators>
gap> MultiplicativeNeutralElement(S);
fail
gap> S := Semigroup(Transformation([4, 5, 1, 3, 8, 5, 8, 2]),
>                   Transformation([4, 2, 3, 2, 8, 2, 8, 6]),
>                   Transformation([6, 8, 4, 2, 2, 8, 2, 6]),
>                   Transformation([4, 2, 6, 2, 8, 2, 8, 6]),
>                   Transformation([2, 8, 6, 4, 2, 8, 2, 4]),
>                   Transformation([4, 6, 6, 2, 4, 6, 4]),
>                   Transformation([2, 6, 2, 8, 2, 2, 2, 8]),
>                   Transformation([5, 4, 4, 5, 8, 8, 5, 5]),
>                   Transformation([2, 2, 6, 8, 6, 6, 6, 8]),
>                   Transformation([2, 8, 8, 6, 8, 8, 6, 6]),
>                   rec(acting := false));;
>  # acting := false is required to test a particular bit of code
gap> MultiplicativeNeutralElement(S);
fail

#T# attr: GroupOfUnits, for a finite semigroup 1/2
gap> S := RegularBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 4 generators>
gap> GroupOfUnits(S);
<group of 3x3 boolean matrices with 2 generators>
gap> StructureDescription(last);
"S3"

#T# attr: GroupOfUnits, fail 2/2
gap> S := Semigroup(
> BooleanMat([[1, 1, 0, 1], [0, 1, 1, 0], [1, 1, 0, 1], [1, 1, 0, 1]]),
> BooleanMat([[1, 1, 0, 1], [0, 1, 1, 1], [0, 1, 1, 1], [0, 1, 1, 0]]));;
gap> GroupOfUnits(S);
fail

#T# attr: GroupOfUnits, infinite 1/1
gap> GroupOfUnits(FreeInverseSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `GroupOfUnits' on 1 arguments

#T# attr: NrIdempotents, C++ 1/1
gap> S := RegularBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 4 generators>
gap> NrIdempotents(S);
123

#T# attr: NrIdempotents, non-C++ 1/1
gap> S := FreeBand(2);;
gap> NrIdempotents(S);
6

#T# attr: NrIdempotents, infinite 1/1
gap> NrIdempotents(FreeSemigroup(2));
Error, resulting list would be too large (length infinity)

#T# attr: RepresentativeOfMinimalIdeal, simple, 1/1
gap> S := MinimalIdeal(FreeBand(2));
<simple semigroup ideal with 1 generator>
gap> RepresentativeOfMinimalIdeal(S);
x1x2

#T# attr: MinimalIdeal, infinite, 1/1
gap> MinimalIdeal(FreeMonoid(3));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `MinimalIdeal' on 1 arguments

#T# attr: IdempotentGeneratedSubsemigroup, inverse op 1/1
gap> S := DualSymmetricInverseMonoid(2);;
gap> T := IdempotentGeneratedSubsemigroup(S);
<commutative inverse block bijection monoid of degree 2 with 1 generator>
gap> HasIsIdempotentGenerated(T) and IsIdempotentGenerated(T);
true

#T# attr: MultiplicativeZero, infinite, 1
#gap> MultiplicativeZero(FreeMonoid(2)); 
#FIXME this causes an infinite loop in the GAP library code

#T# attr: MultiplicativeZero, infinite, 2
gap> F := FreeSemigroup(2);;
gap> x := [[F.1 * F.1, F.1], [F.1 * F.2, F.1], [F.2 * F.1, F.1]];;
gap> T := F / x;
<fp semigroup on the generators [ s1, s2 ]>
gap> SetIsFinite(T, false);
gap> MultiplicativeZero(T);
s1

#T# attr: MaximalDClasses, infinite 1/1
gap> MaximalDClasses(FreeMonoid(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `MaximalDClasses' on 1 arguments

#T# attr: StructureDescriptionMaximalSubgroups, infinite 1/1
gap> StructureDescriptionMaximalSubgroups(FreeMonoid(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `StructureDescriptionMaximalSubgroups' o\
n 1 arguments

#T# attr: IdempotentGeneratedSubsemigroup, infinite 1/1
gap> IdempotentGeneratedSubsemigroup(FreeMonoid(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `IdempotentGeneratedSubsemigroup' on 1 a\
rguments

#T# attr: IdempotentGeneratedSubsemigroup, infinite, inverse-op 1/1
gap> IdempotentGeneratedSubsemigroup(FreeInverseSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 4th choice method found for `IdempotentGeneratedSubsemigroup' on 1 a\
rguments

#T# attr: MultiplicativeNeutralElement, infinite, 1
gap> MultiplicativeNeutralElement(FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `MultiplicativeNeutralElement' on 1 argu\
ments

#T# attr: MultiplicativeNeutralElement, infinite, 2
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]),
> Matrix(IsMaxPlusMatrix, [[0, 0], [1, -3]])]);
<semigroup of 2x2 max-plus matrices with 2 generators>
gap> MultiplicativeNeutralElement(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `MultiplicativeNeutralElement' on 1 argu\
ments

#T# attr: MultiplicativeNeutralElement, One, 1
gap> S := Semigroup(Transformation([1, 3, 2]));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> MultiplicativeNeutralElement(S);
IdentityTransformation

#T# attr: MultiplicativeNeutralElement, One, 2
gap> S := Semigroup(Transformation([3, 1, 3]));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> MultiplicativeNeutralElement(S);
fail

#T# attr: MultiplicativeNeutralElement, One, 3
gap> S := Semigroup(
> [BooleanMat([[true, false, false], [true, false, true], [true, true, true]]),
>  BooleanMat([[true, false, false], [true, true, true], [true, true, false]]),
>  BooleanMat([[false, true, false], [false, true, true], [true, true, true]]),
>  BooleanMat([[true, true, false], [true, true, true], [true, false, false]]),
>  BooleanMat([[false, true, false], [true, true, false], [true, true, true]]),
>  BooleanMat([[false, false, true], [true, false, true], [true, true, true]]),
>  BooleanMat([[true, true, false], [true, false, false], [true, true, true]]),
>  BooleanMat([[false, false, true], [false, true, true], [true, true, true]]),
>  BooleanMat([[true, true, true], [true, false, false], [true, true, false]]),
>  BooleanMat([[true, true, true], [true, true, false], [true, false, false]]),
>  BooleanMat([[true, false, false],
>              [true, true, false],
>              [false, false, false]])]);;
gap> MultiplicativeNeutralElement(S);
fail

#T# attr: MultiplicativeNeutralElement, One, 4
gap> S := Semigroup([PBR([[-2], [-1]], [[1], [2]])]);
<commutative pbr semigroup of degree 2 with 1 generator>
gap>  MultiplicativeNeutralElement(S);
PBR([ [ -1 ], [ -2 ] ], [ [ 1 ], [ 2 ] ])

#T# attr: RepresentativeOfMinimalIdeal, infinite 1/1
gap> RepresentativeOfMinimalIdeal(FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `RepresentativeOfMinimalIdeal' on 1 argu\
ments

#T# attr: RepresentativeOfMinimalIdeal, simple 1/1
gap> S := Semigroup(AsBooleanMat((1, 2)));
<commutative semigroup of 2x2 boolean matrices with 1 generator>
gap> IsSimpleSemigroup(S);
true
gap> RepresentativeOfMinimalIdeal(S);
Matrix(IsBooleanMat, [[0, 1], [1, 0]])

#T# attribute: NilpotencyDegree, 1/4
gap> S := Semigroup([
>  PartialPerm([2], [1]), PartialPerm([1, 2], [3, 1]),
>  PartialPerm([1, 2], [4, 1]), PartialPerm([1, 2], [5, 1]),
>  PartialPerm([3], [5]), PartialPerm([2, 3], [3, 5]),
>  PartialPerm([1, 3], [3, 5]), PartialPerm([1, 2, 3], [3, 1, 5]),
>  PartialPerm([1, 2, 3], [3, 4, 5]), PartialPerm([3, 4], [5, 3]),
>  PartialPerm([2, 4], [4, 5]), PartialPerm([2, 3, 4], [4, 5, 3]),
>  PartialPerm([1, 2, 4], [3, 1, 5]), PartialPerm([1, 2, 4], [4, 1, 5]),
>  PartialPerm([1, 2, 3, 4], [4, 1, 5, 3])]);
<partial perm semigroup of rank 4 with 15 generators>
gap> NilpotencyDegree(S);
5

#T# attribute: NilpotencyDegree, 2/4
gap> S := SymmetricGroup(2);
Sym( [ 1 .. 2 ] )
gap> NilpotencyDegree(S);
fail

#T# attribute: NilpotencyDegree, 3/4
gap> S := FullTransformationMonoid(1);
<full transformation monoid of degree 0>
gap> NilpotencyDegree(S);
1

#T# attribute: NilpotencyDegree, 4/4 
gap> S := Semigroup([
> Transformation([5, 2, 5, 3, 6, 6, 4, 6]),
> Transformation([6, 2, 5, 7, 5, 3, 7, 7]),
> Transformation([8, 4, 6, 4, 5, 6, 8, 1])]);
<transformation semigroup of degree 8 with 3 generators>
gap> NilpotencyDegree(S);
fail

#T# attribute: LengthOfLongestDClassChain, 1/4
gap> S := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> LengthOfLongestDClassChain(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `LengthOfLongestDClassChain' on 1 argume\
nts

#T# attribute: LengthOfLongestDClassChain, 2/4
gap> S := MonogenicSemigroup(8, 5);
<commutative non-regular transformation semigroup of size 12, degree 13 with 
 1 generator>
gap> LengthOfLongestDClassChain(S);
7

#T# attribute: LengthOfLongestDClassChain, 3/4
gap> S := Semigroup([
>  PartialPerm([2], [1]), PartialPerm([1, 2], [3, 1]),
>  PartialPerm([1, 2], [4, 1]), PartialPerm([1, 2], [5, 1]),
>  PartialPerm([3], [5]), PartialPerm([2, 3], [3, 5]),
>  PartialPerm([1, 3], [3, 5]), PartialPerm([1, 2, 3], [3, 1, 5]),
>  PartialPerm([1, 2, 3], [3, 4, 5]), PartialPerm([3, 4], [5, 3]),
>  PartialPerm([2, 4], [4, 5]), PartialPerm([2, 3, 4], [4, 5, 3]),
>  PartialPerm([1, 2, 4], [3, 1, 5]), PartialPerm([1, 2, 4], [4, 1, 5]),
>  PartialPerm([1, 2, 3, 4], [4, 1, 5, 3])]);
<partial perm semigroup of rank 4 with 15 generators>
gap> NilpotencyDegree(S);
5

#T# attribute: LengthOfLongestDClassChain, 4/4
gap> S := SymmetricGroup(5);
Sym( [ 1 .. 5 ] )
gap> LengthOfLongestDClassChain(S);
0

#T# attribute: NormalizedPrincipalFactor, 1
gap> S := FullTransformationMonoid(4);
<full transformation monoid of degree 4>
gap> S := NormalizedPrincipalFactor(DClass(S,
> Transformation([4, 4, 2, 3])));;
gap> Rows(S);
[ 1 .. 6 ]
gap> Columns(S);
[ 1 .. 4 ]
gap> StructureDescription(UnderlyingSemigroup(S));
"S3"

#T# attribute: InjectionNormalizedPrincipalFactor, 1
gap> S := ReesZeroMatrixSemigroup(Group(()), [[(), 0], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> InjectionNormalizedPrincipalFactor(DClass(S, RMSElement(S, 1, (), 1)));
MappingByFunction( <Green's D-class: (1,(),1)>, 
<Rees 0-matrix semigroup 2x2 over Group(())>
 , function( x ) ... end, function( x ) ... end )

#T# attribute: InjectionNormalizedPrincipalFactor, 2
gap> S := SymmetricInverseMonoid(4);
<symmetric inverse monoid of degree 4>
gap> InjectionNormalizedPrincipalFactor(DClass(S,
> PartialPerm([], [])));
MappingByFunction( <Green's D-class: <empty partial perm>>, 
<Rees matrix semigroup 1x1 over Group(())>
 , function( x ) ... end, function( x ) ... end )

#T# attribute: InjectionNormalizedPrincipalFactor, 2
gap> S := MonogenicSemigroup(4, 2);
<commutative non-regular transformation semigroup of size 5, degree 6 with 1 
 generator>
gap> InjectionNormalizedPrincipalFactor(DClass(S, S.1));
Error, Semigroups: InjectionNormalizedPrincipalFactor: usage,
the argument <D> must be a regular D-class,

#T# attrTest6:
# UnderlyingSemigroupOfSemigroupWithAdjoinedZero
gap> S := FullTransformationMonoid(10);;
gap> UnderlyingSemigroupOfSemigroupWithAdjoinedZero(S);
fail
gap> S := FullTransformationMonoid(4);;
gap> SetIsSemigroupWithAdjoinedZero(S, false);
gap> UnderlyingSemigroupOfSemigroupWithAdjoinedZero(S);
fail
gap> S := SymmetricInverseMonoid(5);;
gap> UnderlyingSemigroupOfSemigroupWithAdjoinedZero(S);
fail
gap> S := MonogenicSemigroup(4, 1);
<commutative non-regular transformation semigroup of size 4, degree 5 with 1 
 generator>
gap> UnderlyingSemigroupOfSemigroupWithAdjoinedZero(S);
fail
gap> S := Semigroup(Elements(S));
<transformation semigroup of degree 5 with 4 generators>
gap> UnderlyingSemigroupOfSemigroupWithAdjoinedZero(S);
fail
gap> S := Semigroup([PartialPerm([]), PartialPerm([1])]);
<partial perm monoid of rank 1 with 2 generators>
gap> UnderlyingSemigroupOfSemigroupWithAdjoinedZero(S);
<trivial partial perm group of rank 1 with 1 generator>

#T# attr: IrredundantGeneratingSubset: for a set with a single repeated
# element
gap> S := Semigroup([Transformation([1, 1]), Transformation([1, 1])]);
<transformation semigroup of degree 2 with 2 generators>
gap> Size(IrredundantGeneratingSubset(S));
1

#T# attr: Size: for a monogenic semigroup of special type with minimal
# generating set
gap> S := Semigroup(Transformation([10, 8, 4, 6, 4, 5, 3, 8, 8, 2]));
<commutative transformation semigroup of degree 10 with 1 generator>
gap> Size(S);
5
gap> S := Semigroup(PBR([
> [-3, -2, 3], [-4, -2, 3], [-4, -3, -2, 1, 2], [-4, 2, 3, 4]],
> [[-3, -2, -1, 2, 3, 4], [-3, -1, 1, 3, 4], [-4, -2, 2, 3], [-4, 1, 2, 3]]));;
gap> Size(S);
2
gap> S := Semigroup(Transformation([2, 5, 4, 1, 6, 3, 2]));
<commutative transformation semigroup of degree 7 with 1 generator>
gap> Size(S);
6
gap> S := Semigroup(Bipartition([[1], [2, -4], [3, -5], [4, -6], [5, -1],
> [6, -2], [-3]]));
<commutative bipartition semigroup of degree 6 with 1 generator>
gap> Size(S);
5
gap> S := Semigroup(Matrix(GF(2 ^ 2),
> [[Z(2 ^ 2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [Z(2 ^ 2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)]]));;
gap> Size(S);
6
gap> S := Semigroup(PartialPerm(
> [1, 2, 3, 4, 5, 7, 8, 10, 11, 12, 13, 14, 18, 20, 21, 22, 23, 25, 26, 29],
> [2, 20, 9, 6, 15, 12, 10, 3, 21, 17, 5, 1, 13, 7, 11, 24, 19, 4, 22, 30]));
<commutative partial perm semigroup of rank 20 with 1 generator>
gap> Size(S);
8

#T# attr: Size: for a monogenic monoid with minimal generating set
gap> S := Monoid(Transformation([7, 8, 1, 3, 5, 2, 4, 6]));
<commutative transformation monoid of degree 8 with 1 generator>
gap> Size(S);
12
gap> S := Monoid(Transformation([5, 6, 2, 1, 3, 4, 7, 7]));
<commutative transformation monoid of degree 8 with 1 generator>
gap> Size(S);
7
gap> S := Monoid(Transformation([4, 5, 5, 7, 1, 7, 4, 3]));
<commutative transformation monoid of degree 8 with 1 generator>
gap> Size(S);
6
gap> S := Monoid(Bipartition([[1], [2, -4], [3, -5], [4, -6], [5, -1],
> [6, -2], [-3]]));
<commutative bipartition monoid of degree 6 with 1 generator>
gap> Size(S);
6
gap> S := Monoid(Matrix(GF(2 ^ 2),
> [[Z(2 ^ 2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [Z(2 ^ 2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)]]));;
gap> Size(S);
7
gap> S := Monoid(PartialPerm(
> [1, 2, 3, 4, 5, 7, 8, 10, 11, 12, 13, 14, 18, 20, 21, 22, 23, 25, 26, 29],
> [2, 20, 9, 6, 15, 12, 10, 3, 21, 17, 5, 1, 13, 7, 11, 24, 19, 4, 22, 30]));
<commutative partial perm monoid of rank 27 with 1 generator>
gap> Size(S);
9

#T# attr: Size: for a monogenic semigroup/monoid of a type which does not
# have a special IndexPeriodOfSemigroup method
gap> x := PBR(
>  [[-75, -62, -55, -24, 15, 50, 61, 66],
>      [-61, -59, -54, -51, -32, -27, 16, 17, 18, 49, 67],
>      [-30, 5, 19, 29, 62, 67],
>      [-72, -63, -46, -28, -10, -7, -2, 9, 59, 73, 74],
>      [-65, -38, -31, -26, -24, -17, 4, 48, 57, 61],
>      [-69, -67, -62, -4, -3, 1, 2, 5, 26, 50],
>      [-73, -19, 5, 26, 55, 57, 62, 63], [-26, -5, 9, 45],
>      [-75, -71, -59, -53, -10, 20, 29, 62, 64, 69],
>      [-66, -59, -35, -32, 5, 23, 41, 66],
>      [-48, -47, -13, 24, 31, 46, 53],
>      [-75, -47, -34, -22, -14, -7, 10],
>      [-54, -49, -29, -26, -15, -8, 3, 15, 35, 67, 72],
>      [-68, -41, -40, -38, -16, 39, 53, 68], [-73, -62, -4, 20, 54],
>      [-44, -7, 9, 13, 29, 32, 39, 43, 45, 47, 63],
>      [-56, -45, -11, 12, 24, 28, 41],
>      [-72, -60, -38, -13, -11, 16, 42, 51, 59, 69],
>      [-61, 24, 44, 57, 63], [-70, -52, -20, 21, 38, 39, 61],
>      [-63, -39, -31, 28, 35, 75],
>      [-73, -58, -53, -36, -11, 25, 48, 54, 69],
>      [-75, -43, -32, -22, -2, 4, 67], [-32, 10, 27],
>      [-75, -66, -61, -36, -32, -27, -6, -2, 3, 21, 37, 57, 69],
>      [-46, 5, 33], [-73, -52, -26, -25, -3, 61, 72],
>      [-46, -38, -10, -1, 3, 43, 56],
>      [-26, -20, -12, -3, 32, 44, 53, 60, 61], [-37, -16, -8, 21, 39, 43],
>      [-75, -36, -32, 17, 32, 46, 54, 64], [-60, 45, 51, 54, 56],
>      [-66, -24, -15, 12, 21, 37, 53, 62, 72], [-50, -31, 49, 69],
>      [-69, -62, -41, 29, 32], [26, 60, 67], [-52, -37, -25, -2, 6, 19],
>      [-65, -44, -42, -35, -21, 14, 64], [-53, -51, -36, -25, -14, 45, 63],
>      [-72, -51, -21, -7, 6, 20, 61], [-67, -62, -4, 20],
>      [-28, -17, -13, 6, 8, 17, 36, 41, 71],
>      [-71, -57, -54, -43, -32, -12, 2, 15, 62, 64, 66, 67],
>      [-56, -37, 37, 38, 45, 50, 56, 63, 69], [-40, -32, 11, 32, 48, 51],
>      [-66, -60, -41, 23, 32, 65], [-72, -43, -41, 37, 41, 43, 55],
>      [-63, -61, -59, -8, -6, 7, 8, 27, 37, 44, 57, 72],
>      [-47, -45, -3, 11, 17, 19, 45, 57],
>      [-72, -51, -2, 17, 24, 29, 41, 59, 63],
>      [-11, -3, -1, 7, 8, 26, 48, 56, 64], [-56, -46, -22, 10, 11],
>      [65], [-69, -52, -44, -27, -25, -24, -11, -6, 10, 36],
>      [-62, -51, 2, 27, 61, 63], [-63, -34, -18, 19, 29, 44, 60],
>      [-57, 3, 27, 55, 58, 64], [-17, -16, -15, 8, 10, 14, 37, 68, 73],
>      [-13, 2, 5, 21, 31, 43, 53, 73],
>      [-51, -47, -42, -35, -18, -13, -12, 31, 36, 60],
>      [-72, -71, -38, -32, -9, 45], [-68, -35, -24, 15, 18, 50, 70],
>      [-47, -38, -22, -14, 48, 57], [-42, -34, -33, -15, 31, 46, 63],
>      [-74, -58, 9, 27], [-59, -53, -37, -32, -16, 14, 34, 60, 62],
>      [-55, -30, -27, -22, -19, 6, 37, 44, 52, 53],
>      [-75, -57, -20, -14, 2, 7, 8, 17], [-47, -44, -36, 13],
>      [-69, -47, -43, 4, 11, 17, 41, 56, 57], [-48, -35, 5, 35],
>      [-28, -9, 9, 12, 17, 30, 33], [-42, -15, -14, -12, -10],
>      [-68, -62, -48, -12, 3, 40, 56, 68, 73],
>      [-73, -55, -45, -33, -9, 21, 49, 58]],
>  [[-64, -63, -26, -2, 47, 63, 74], [-74, -65, -51, -18, 17, 19],
>      [-72, -67, -66, -48, -7, -5, 8, 21, 26, 35],
>      [-54, 6, 24, 42, 49, 68], [-71, -51, -37, -17, -4, 22],
>      [-73, -69, -33, -32, -7, -5, 19, 28, 31],
>      [-68, -61, -58, -22, 26, 73],
>      [-59, -53, -5, 10, 18, 28, 34, 35, 55, 56, 60],
>      [-67, -64, -51, -5, 31, 39, 52, 61, 71],
>      [-70, -63, -60, -13, 34, 51, 59, 73],
>      [-61, -56, -14, 17, 18, 20, 24, 41, 42, 67], [-52, -39, -23],
>      [-63, -30, -14, -10, 1, 22, 50, 60, 61, 69, 72],
>      [-75, -62, -41, 12, 19, 30, 58, 66, 71], [-66, -31, -3, 13, 26],
>      [-75, -69, -67, -46, -32, -23, -10, 24, 31, 36, 49],
>      [-68, -56, -14, -2, 3, 8],
>      [-75, -63, -50, -41, -22, -5, 9, 10, 30, 43, 52],
>      [-46, -39, -36, 11, 36, 45, 69], [-70, -60, -58, -48, -36, 3, 33],
>      [-71, -64, -60, -54, -53, -52, -41, -37, -14, -10, 18, 24, 39],
>      [-57, -55, -49, -45, -26, -20, -8, 9, 16, 19, 21, 27],
>      [-61, -50, -36, -31, -28, 12, 28, 37, 50, 51], [-73, -50, -46, -42],
>      [-75, 52, 55], [-62, -54, -12, 34, 48, 51, 61, 69],
>      [-67, -60, -10, -2, 10, 15, 22, 52], [-63, -46, 1, 30, 31, 42, 52],
>      [-16, -6, -2, 16, 26, 58, 68], [-62, -20, 7, 21, 33, 37, 53],
>      [-71, -43, -19, 24, 25, 52, 65, 68], [-13, 47, 52, 63],
>      [-68, -65, -51, -42, -17, 12, 15, 55], [-72, -71, -8, 14, 64],
>      [-74, -69, -56, -55, -36, -15, 11, 13, 20, 43], [-58, 28],
>      [-74, -71, -63, -49, -44, -11, 31, 50, 60],
>      [-65, -59, -58, -31, -22, 4, 19, 27, 69],
>      [-63, -57, -44, -31, -14, -8, 8, 26, 35, 38, 46, 61, 62],
>      [-69, -61, -51, -25, 13, 25, 27, 56],
>      [-67, -45, -31, -19, -17, 9, 29, 62], [-33, -20, -17, -2, 33, 60],
>      [-74, -72, -51, -38, -27],
>      [-31, -3, 30, 31, 37, 44, 55, 62, 64, 65, 72],
>      [-66, -64, -59, -57, -51, -45, 2, 30, 48, 67],
>      [-75, -71, -59, -45, -30, -11, 58, 66],
>      [-73, -71, -64, -44, -42, -3, 53, 60, 61], [-69, -45, -11, 41],
>      [-67, -46, -45, -44, -35, -31, -28, -27, 13, 33],
>      [-74, -69, -59, -42, -26, -12, 1, 26, 31, 34, 42, 49, 54, 63, 72],
>      [-47, -44, -23, 18, 22, 30], [-60, -56, -38, -10, 2],
>      [-29, -22, -11, -9, 12],
>      [-63, -36, -20, 3, 13, 27, 33, 40, 52, 55, 65],
>      [-64, -56, -14, -6, -4, 2, 6, 22, 37, 47, 52, 56, 73],
>      [-50, -18, 35, 72], [-56, -51, -43, -22, -18, 60],
>      [-71, -55, -47, -8, -3, 7, 35, 69, 74, 75], [-63, -49, 36, 55, 61],
>      [-75, -11, -8, 1, 23, 25, 63, 65, 75],
>      [-69, -56, -13, -10, -6, 13, 31, 49, 73],
>      [-20, -15, -9, 51, 62, 63], [-31, -7, 5, 43, 44, 49],
>      [-74, -64, -60, -50, -47, -2, 18, 45, 54, 66],
>      [-74, -38, 37, 38, 42, 57], [-73, -50, 34, 62, 63, 67],
>      [-54, -50, -15, -2, 3, 19, 48, 74],
>      [-75, -51, -44, -33, 24, 34, 44, 61], [-70, -49, 26, 35, 55],
>      [-15, -8, 6, 11, 20, 34, 67], [-54, -51, -30, 47],
>      [-64, -20, -5, 22, 34, 62], [-56, -39, -37, -19, 19, 22, 30, 56],
>      [-64, -57, -41, -10, -7, 8, 10, 29],
>      [-34, -13, -12, 12, 51, 57, 62]]);;
gap> Size(Semigroup(x));
3
gap> Size(Monoid(x));
4

# Test for Issue 218
gap> S := Semigroup(PlanarPartitionMonoid(5),
> AsBipartition((1, 2, 3, 4, 5)), rec(acting := true));;
gap> D := DClasses(S)[2];;
gap> x := Bipartition([[1, -1], [2, -2], [3, -3], [4, -4, -5], [5]]);;
gap> x in D;
true
gap> inj := InjectionPrincipalFactor(D);;
gap> x ^ inj;
(3,(1,2,3,4),4)

# Test GeneratorsSmallest
gap> S := Semigroup(IdentityTransformation);;
gap> GeneratorsSmallest(S);
[ IdentityTransformation ]
gap> S := FullBooleanMatMonoid(2);;
gap> GeneratorsSmallest(S);
[ Matrix(IsBooleanMat, [[0, 0], [0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0], [0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0], [1, 0]]), 
  Matrix(IsBooleanMat, [[0, 0], [1, 1]]), 
  Matrix(IsBooleanMat, [[0, 1], [0, 0]]), 
  Matrix(IsBooleanMat, [[0, 1], [0, 1]]), 
  Matrix(IsBooleanMat, [[0, 1], [1, 0]]), 
  Matrix(IsBooleanMat, [[0, 1], [1, 1]]) ]
gap> S = Semigroup(GeneratorsSmallest(S));
true
gap> IsSet(GeneratorsSmallest(S));
true

# Test SmallestElementSemigroup (for a semigroup)
gap> S := Semigroup([Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 1], [1, 0, 0]]),
>  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 1], [1, 1, 1]])]);;
gap> SmallestElementSemigroup(S);
Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 1], [1, 0, 0]])
gap> S := Semigroup([Matrix(IsMaxPlusMatrix,
>                           [[-2, 2, 0], [-1, 0, 0], [1, -3, 1]]),
>                    Matrix(IsMaxPlusMatrix,
>                           [[-infinity, 0, 0], [0, 1, 0], [1, -1, 0]])]);;
gap> SmallestElementSemigroup(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `SmallestElementSemigroup' on 1 argument\
s

# Test LargestElementSemigroup (for a semigroup)
gap> S := Semigroup([Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 1], [1, 0, 0]]),
>  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 1], [1, 1, 1]])]);;
gap> LargestElementSemigroup(S);
Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [1, 1, 1]])
gap> S := Semigroup([Matrix(IsMaxPlusMatrix,
>                           [[-2, 2, 0], [-1, 0, 0], [1, -3, 1]]),
>                    Matrix(IsMaxPlusMatrix,
>                           [[-infinity, 0, 0], [0, 1, 0], [1, -1, 0]])]);;
gap> LargestElementSemigroup(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `LargestElementSemigroup' on 1 arguments

#T# attr: InversesOfSemigroupElement, for a group as semigroup
gap> S := Semigroup(Transformation([2, 3, 1, 3, 3]));;
gap> IsGroupAsSemigroup(S);
true
gap> InversesOfSemigroupElement(S, S.1);
[ Transformation( [ 3, 1, 2, 1, 1 ] ) ]
gap> Inverse(S.1);
fail
gap> S := Semigroup(PartialPerm([2, 3, 1]), rec(acting := false));;
gap> IsGroupAsSemigroup(S);
true
gap> InversesOfSemigroupElement(S, S.1);
[ (1,3,2) ]
gap> S := SymmetricGroup(3);
Sym( [ 1 .. 3 ] )
gap> InversesOfSemigroupElement(S, (1, 3, 2));
[ (1,2,3) ]

#T# attr: InversesOfSemigroupElement, for a semigroup
#gap> S := Semigroup([
#>  Matrix(IsMaxPlusMatrix, [[-2, 2, 0], [-1, 0, 0], [1, -3, 1]]),
#>  Matrix(IsMaxPlusMatrix, [[- infinity, 0, 0], [0, 1, 0], [1, -1, 0]])]);;
#gap> InversesOfSemigroupElement(S, S.1);
# FIXME This test fails due to the library method
gap> S := Semigroup(Transformation([2, 3, 1, 3, 3]));;
gap> InversesOfSemigroupElement(S, Transformation([1, 3, 2]));
Error, Semigroups: InversesOfSemigroupElement: usage,
the second arg (a mult. element) must belong to the first arg (a semigroup),
gap> S := Semigroup([Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 1], [1, 0, 0]]),
>  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 1], [1, 1, 1]])]);;
gap> InversesOfSemigroupElement(S, S.1);
[  ]
gap> InversesOfSemigroupElement(S, S.1 * S.2 * S.1);
[ Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [0, 0, 1]]) ]

#T# InversesOfSemigroupElement, for an infinite semigroup, 1
gap> S := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> InversesOfSemigroupElement(S, IdentityTransformation);
Error, usage: the 2nd argument must be an element of the 1st,
gap> InversesOfSemigroupElementNC(S, S.1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `InversesOfSemigroupElementNC' on 2 argu\
ments

#T# attr: IdempotentGeneratedSubsemigroup, 2
gap> S := FullTransformationMonoid(3);;
gap> I := IdempotentGeneratedSubsemigroup(S);;
gap> HasIsIdempotentGenerated(I);
true
gap> IsIdempotentGenerated(I);
true

#T# attr: IdempotentGeneratedSubsemigroup, for an Rees matrix semigroup, 1
# TryNextMethod()

# Error: infinite
gap> S := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> R := ReesMatrixSemigroup(S, [[S.1]]);
<Rees matrix semigroup 1x1 over <free semigroup on the generators [ s1 ]>>
gap> IdempotentGeneratedSubsemigroup(R);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 4th choice method found for `IdempotentGeneratedSubsemigroup' on 1 a\
rguments

# not a Rees matrix semigroup
gap> x := Transformation([2, 2]);;
gap> R := ReesMatrixSemigroup(FullTransformationMonoid(2),
>                             [[IdentityTransformation, x],
>                              [x, x]]);
<Rees matrix semigroup 2x2 over <full transformation monoid of degree 2>>
gap> S := Semigroup(RMSElement(R, 1, IdentityTransformation, 1),
>                   RMSElement(R, 2, IdentityTransformation, 2));
<subsemigroup of 2x2 Rees matrix semigroup with 2 generators>
gap> IsReesMatrixSubsemigroup(S);
true
gap> IsReesMatrixSemigroup(S);
false
gap> I := IdempotentGeneratedSubsemigroup(S);;
gap> HasIsIdempotentGenerated(I);
true
gap> IsIdempotentGenerated(I);
true
gap> Size(I) = 5;
true
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true

# not over a group
gap> x := Transformation([2, 2]);;
gap> R := ReesMatrixSemigroup(FullTransformationMonoid(2),
>                             [[IdentityTransformation, x],
>                              [x, x]]);
<Rees matrix semigroup 2x2 over <full transformation monoid of degree 2>>
gap> I := IdempotentGeneratedSubsemigroup(R);;
gap> HasIsIdempotentGenerated(I);
true
gap> IsIdempotentGenerated(I);
true
gap> Size(I) = 9;
true
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true

#T# attr: IdempotentGeneratedSubsemigroup, for an Rees matrix semigroup, 2

# Rectangular bands
gap> R := RectangularBand(IsReesMatrixSemigroup, 1, 1);
<Rees matrix semigroup 1x1 over Group(())>
gap> I := IdempotentGeneratedSubsemigroup(R);
<subsemigroup of 1x1 Rees matrix semigroup with 1 generator>
gap> I = R;
true
gap> HasIsIdempotentGenerated(I);
true
gap> IsIdempotentGenerated(I);
true
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true
gap> R := RectangularBand(IsReesMatrixSemigroup, 3, 1);
<Rees matrix semigroup 3x1 over Group(())>
gap> I := IdempotentGeneratedSubsemigroup(R);
<subsemigroup of 3x1 Rees matrix semigroup with 3 generators>
gap> I = R;
true
gap> HasIsIdempotentGenerated(I);
true
gap> IsIdempotentGenerated(I);
true
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true
gap> R := RectangularBand(IsReesMatrixSemigroup, 1, 3);
<Rees matrix semigroup 1x3 over Group(())>
gap> I := IdempotentGeneratedSubsemigroup(R);
<subsemigroup of 1x3 Rees matrix semigroup with 3 generators>
gap> I = R;
true
gap> HasIsIdempotentGenerated(I);
true
gap> IsIdempotentGenerated(I);
true
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true
gap> R := RectangularBand(IsReesMatrixSemigroup, 2, 2);
<Rees matrix semigroup 2x2 over Group(())>
gap> I := IdempotentGeneratedSubsemigroup(R);
<subsemigroup of 2x2 Rees matrix semigroup with 2 generators>
gap> I = R;
true
gap> HasIsIdempotentGenerated(I);
true
gap> IsIdempotentGenerated(I);
true
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true

# Subsemigroup, giving non-standard matrix
gap> x := [[(2, 3, 5), (1, 2, 4)(3, 5), (1, 3, 5, 2, 4)],
>          [(1, 3, 5, 4), (2, 3, 5, 4), (1, 3, 5)(2, 4)],
>          [(2, 3, 5), (1, 5, 4, 3, 2), ()],
>          [(1, 4, 2, 5), (1, 2)(3, 5), (1, 5, 3, 2, 4)],
>          [(1, 4)(2, 3, 5), (1, 2)(4, 5), (1, 3, 4, 2, 5)]];;
gap> G := SymmetricGroup(5);;
gap> R := ReesMatrixSemigroup(G, x);
<Rees matrix semigroup 3x5 over Sym( [ 1 .. 5 ] )>
gap> S := ReesMatrixSubsemigroup(R, [2], G, [4]);
<Rees matrix semigroup 1x1 over Sym( [ 1 .. 5 ] )>
gap> I := IdempotentGeneratedSubsemigroup(S);
<subsemigroup of 3x5 Rees matrix semigroup with 1 generator>
gap> GeneratorsOfSemigroup(I);
[ (2,(1,2)(3,5),4) ]
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true
gap> S := ReesMatrixSubsemigroup(R, [2], G, [3, 4, 5]);
<Rees matrix semigroup 1x3 over Sym( [ 1 .. 5 ] )>
gap> I := IdempotentGeneratedSubsemigroup(S);
<subsemigroup of 3x5 Rees matrix semigroup with 3 generators>
gap> GeneratorsOfSemigroup(I);
[ (2,(1,2,3,4,5),3), (2,(1,2)(3,5),4), (2,(1,2)(4,5),5) ]
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true
gap> S := ReesMatrixSubsemigroup(R, [2, 3], G, [4]);
<Rees matrix semigroup 2x1 over Sym( [ 1 .. 5 ] )>
gap> I := IdempotentGeneratedSubsemigroup(S);
<subsemigroup of 3x5 Rees matrix semigroup with 2 generators>
gap> GeneratorsOfSemigroup(I);
[ (2,(1,2)(3,5),4), (3,(1,4,2,3,5),4) ]
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true
gap> S := ReesMatrixSubsemigroup(R, [2, 3], G, [4, 5]);
<Rees matrix semigroup 2x2 over Sym( [ 1 .. 5 ] )>
gap> I := IdempotentGeneratedSubsemigroup(S);
<subsemigroup of 3x5 Rees matrix semigroup with 2 generators>
gap> GeneratorsOfSemigroup(I);
[ (2,(1,2)(3,5),4), (3,(1,5,2,4,3),5) ]
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true

#T# attr: IdempotentGeneratedSubsemigroup, for an Rees 0-matrix semigroup, 1
# TryNextMethod()

# Error: infinite
gap> S := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> R := ReesZeroMatrixSemigroup(S, [[S.1]]);
<Rees 0-matrix semigroup 1x1 over <free semigroup on the generators [ s1 ]>>
gap> IdempotentGeneratedSubsemigroup(R);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 4th choice method found for `IdempotentGeneratedSubsemigroup' on 1 a\
rguments

# not a Rees matrix semigroup
gap> x := Transformation([2, 2]);;
gap> R := ReesZeroMatrixSemigroup(FullTransformationMonoid(2),
>                                 [[IdentityTransformation, 0],
>                                  [0, x]]);
<Rees 0-matrix semigroup 2x2 over <full transformation monoid of degree 2>>
gap> S := Semigroup(RMSElement(R, 1, IdentityTransformation, 1),
>                   RMSElement(R, 2, IdentityTransformation, 2));
<subsemigroup of 2x2 Rees 0-matrix semigroup with 2 generators>
gap> IsReesZeroMatrixSubsemigroup(S);
true
gap> IsReesZeroMatrixSemigroup(S);
false
gap> I := IdempotentGeneratedSubsemigroup(S);;
gap> HasIsIdempotentGenerated(I);
true
gap> IsIdempotentGenerated(I);
true
gap> Size(I) = 3;
true
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true

# not over a group
gap> x := Transformation([2, 2]);;
gap> R := ReesZeroMatrixSemigroup(FullTransformationMonoid(2),
>                                 [[IdentityTransformation, 0],
>                                  [x, x]]);
<Rees 0-matrix semigroup 2x2 over <full transformation monoid of degree 2>>
gap> I := IdempotentGeneratedSubsemigroup(R);;
gap> HasIsIdempotentGenerated(I);
true
gap> IsIdempotentGenerated(I);
true
gap> Size(I) = 10;
true
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true

#T# attr: IdempotentGeneratedSubsemigroup, for an Rees 0-matrix semigroup, 2

# Subsemigroup, giving non-standard matrix, 1
gap> R := ReesZeroMatrixSemigroup(Group(()), [[(), ()], [(), ()]]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> S := Semigroup(RMSElement(R, 2, (), 2), MultiplicativeZero(R));
<subsemigroup of 2x2 Rees 0-matrix semigroup with 2 generators>
gap> I := IdempotentGeneratedSubsemigroup(S);
<subsemigroup of 2x2 Rees 0-matrix semigroup with 2 generators>
gap> I = S;
true
gap> Elements(I);
[ 0, (2,(),2) ]

# Subsemigroup, giving non-standard matrix, 2
gap> x := [[(1, 3, 5)(2, 4), (1, 4, 3, 2, 5), (1, 3, 5)(2, 4)],
>          [(2, 4, 5, 3), 0, (1, 4, 5, 3)],
>          [(3, 5, 4), 0, (1, 4, 5)(2, 3)],
>          [0, 0, (1, 3, 4, 2)],
>          [(2, 3, 4), (1, 2, 5, 4, 3), (1, 5, 3)(2, 4)]];;
gap> G := SymmetricGroup(5);;
gap> R := ReesZeroMatrixSemigroup(G, x);
<Rees 0-matrix semigroup 3x5 over Sym( [ 1 .. 5 ] )>
gap> S := ReesZeroMatrixSubsemigroup(R, [2], G, [4]);
<Rees 0-matrix semigroup 1x1 over Sym( [ 1 .. 5 ] )>
gap> I := IdempotentGeneratedSubsemigroup(S);
<subsemigroup of 3x5 Rees 0-matrix semigroup with 1 generator>
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true
gap> I = Semigroup(Idempotents(S));
true
gap> S := ReesZeroMatrixSubsemigroup(R, [2], G, [3, 4, 5]);
<Rees 0-matrix semigroup 1x3 over Sym( [ 1 .. 5 ] )>
gap> I := IdempotentGeneratedSubsemigroup(S);
<subsemigroup of 3x5 Rees 0-matrix semigroup with 2 generators>
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true
gap> I = Semigroup(Idempotents(S));
true
gap> S := ReesZeroMatrixSubsemigroup(R, [2, 3], G, [4]);
<Rees 0-matrix semigroup 2x1 over Sym( [ 1 .. 5 ] )>
gap> I := IdempotentGeneratedSubsemigroup(S);
<subsemigroup of 3x5 Rees 0-matrix semigroup with 2 generators>
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true
gap> I = Semigroup(Idempotents(S));
true
gap> S := ReesZeroMatrixSubsemigroup(R, [2, 3], G, [4, 5]);
<Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 5 ] )>
gap> I := IdempotentGeneratedSubsemigroup(S);
<subsemigroup of 3x5 Rees 0-matrix semigroup with 3 generators>
gap> ForAll(GeneratorsOfSemigroup(I), IsIdempotent);
true
gap> I = Semigroup(Idempotents(S));
true

#T# IdempotentGeneratedSubsemigroup, for R(Z)MS where new method is much better

# RZMS: 2241 generators -> 384 generators
gap> R := PrincipalFactor(
>  DClass(FullTransformationMonoid(7),
>  Transformation([1, 1, 3, 4, 5, 4, 1])));;
gap> Length(Rows(R));
350
gap> Length(Columns(R));
35
gap> IdempotentGeneratedSubsemigroup(R);
<subsemigroup of 350x35 Rees 0-matrix semigroup with 384 generators>

# RMS: 7200 generators -> 90 generators
gap> R := RectangularBand(IsReesMatrixSemigroup, 80, 90);
<Rees matrix semigroup 80x90 over Group(())>
gap> IdempotentGeneratedSubsemigroup(R);
<subsemigroup of 80x90 Rees matrix semigroup with 90 generators>

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(D);
gap> Unbind(G);
gap> Unbind(GR);
gap> Unbind(I);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(coll);
gap> Unbind(foo);
gap> Unbind(gens);
gap> Unbind(inv);
gap> Unbind(iso);
gap> Unbind(map);
gap> Unbind(s);
gap> Unbind(t);
gap> Unbind(x);
gap> Unbind(y);
gap> Unbind(z);

#E#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attr.tst");
