#############################################################################
##
#W  standard/attributes/attr.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, F, G, GR, I, L, R, S, T, acting, an, coll, digraph, foo, gens, inj
#@local inv, iso, map, n, r, s, t, x, y, z
gap> START_TEST("Semigroups package: standard/attributes/attr.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# AttributesTest1: MultiplicativeZero
# for a transformation semigroup/ideal
gap> t := Transformation([1]);;

# Trivial full transformation monoid T_1
# Previously this crashed: see issue #121 on Bitbucket
gap> s := Semigroup(t);  # with displaying the semigroup
<trivial transformation group of degree 0 with 1 generator>
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true
gap> s := Semigroup(t);;  # not displaying the semigroup
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
gap> s := Semigroup(t);  # with displaying the semigroup
<commutative transformation semigroup of degree 4 with 1 generator>
gap> MultiplicativeZero(s) = t;
true
gap> Size(MinimalIdeal(s)) = 1;
true
gap> s := Semigroup(t);;  # not displaying the semigroup
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
gap> MultiplicativeZero(I);  # does not know whether parent has a zero
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
gap> MultiplicativeZero(I);  # does know whether parent has a zero
fail
gap> Size(MinimalIdeal(I)) = 1;
false

# AttributesTest2:
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
gap> s := Semigroup([PartialPerm([2], [1])]);  # contains < empty pperm >
<commutative partial perm semigroup of rank 1 with 1 generator>
gap> MultiplicativeZero(s);
<empty partial perm>
gap> Size(MinimalIdeal(s)) = 1;
true
gap> s := Semigroup([
> PartialPerm([1, 2, 3], [1, 4, 2]),
> PartialPerm([1, 4], [1, 3])]);  # does not contain <empty pperm>
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
gap> MultiplicativeZero(I) = t;  # does not know whether parent has a zero
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
gap> MultiplicativeZero(I) = t;  # does know whether parent has a zero
true
gap> Size(MinimalIdeal(I)) = 1;
true

# AttributesTest3:
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

# AttributesTest4:
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
# FIXME(later) Enters an infinite loop in a library method because S is not finite

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

# AttributesTest5:
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

# attr: RightCayleyDigraph
gap> S := Semigroup(PartialPerm([1, 2, 3], [1, 3, 4]),
>                   PartialPerm([1, 2, 3], [2, 5, 3]),
>                   PartialPerm([1, 2, 3], [4, 1, 2]),
>                   PartialPerm([1, 2, 3, 4], [2, 4, 1, 5]),
>                   PartialPerm([1, 3, 5], [5, 1, 3]));;
gap> digraph := RightCayleyDigraph(S);;
gap> Length(DigraphStronglyConnectedComponents(digraph).comps) 
> = NrRClasses(S);
true

# attr: RightCayleyDigraph, infinite
gap> RightCayleyDigraph(FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `RightCayleyDigraph' on 1 arguments

# attr: LeftCayleyDigraph
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
gap> digraph := LeftCayleyDigraph(S);;
gap> Length(DigraphStronglyConnectedComponents(digraph).comps) 
> = NrLClasses(S);
true

# attr: RightCayleyDigraph, infinite
gap> LeftCayleyDigraph(FreeInverseSemigroup(2));
Error, the argument (a semigroup) is not finite

# attr: IsomorphismReesMatrixSemigroup
gap> D := GreensDClassOfElement(Semigroup(
> Bipartition([[1, 2, 3, -3], [4, -4, -5], [5, -1], [-2]]),
> Bipartition([[1, 4, -2, -3], [2, 3, 5, -5], [-1, -4]]),
> Bipartition([[1, 5], [2, 4, -3, -5], [3, -1, -2], [-4]]),
> Bipartition([[1], [2], [3, 5, -1, -2], [4, -3], [-4, -5]]),
> Bipartition([[1], [2], [3], [4, -1, -4], [5], [-2, -3], [-5]])),
> Bipartition([[1], [2], [3], [4, -1, -4], [5], [-2, -3], [-5]]));;
gap> PrincipalFactor(D);
<Rees 0-matrix semigroup 12x15 over Group(())>

# attr: IsomorphismReesMatrixSemigroup, error, 1/1
gap> S := FullTransformationMonoid(3);;
gap> D := DClass(S, Transformation([1, 2, 1]));;
gap> IsomorphismReesMatrixSemigroup(D);
Error, the argument (a Green's D-class) is not a semigroup
gap> D := MinimalDClass(S);;
gap> IsomorphismReesMatrixSemigroup(D);
MappingByFunction( <Green's D-class: Transformation( [ 1, 1, 1 ] )>, 
<Rees matrix semigroup 1x3 over Group(())>
 , function( x ) ... end, function( x ) ... end )

# attr: IrredundantGeneratingSubset, for a collection of elements
gap> G := CyclicGroup(3);;
gap> R := GF(2);;
gap> GR := GroupRing(R, G);;
gap> iso := IsomorphismTransformationSemigroup(GR);;
gap> S := Range(iso);;
gap> S := Semigroup(IrredundantGeneratingSubset(SmallGeneratingSet(S)));;

# attr: IrredundantGeneratingSubset: for a semigroup
gap> S := RandomMonoid(IsBooleanMatMonoid, 10, 3);;
gap> T := Semigroup(IrredundantGeneratingSubset(S));;
gap> S = T;
true

# attr: IrredundantGeneratingSubset: for a set with one element, 1
gap> IrredundantGeneratingSubset([RandomTransformation(10)]);;

# attr: IrredundantGeneratingSubset: for a set with one element, 2
gap> S := Monoid([Transformation([1, 1]), Transformation([2, 1]),
>  Transformation([2, 2])], rec(acting := true));
<transformation monoid of degree 2 with 3 generators>
gap> Size(IrredundantGeneratingSubset(S));
2

# attr: IrredundantGeneratingSubset: for a set with a single repeated
# element
gap> S := Semigroup([Transformation([1, 1]), Transformation([1, 1])]);
<transformation semigroup of degree 2 with 2 generators>
gap> Size(IrredundantGeneratingSubset(S));
1

# attr: IrredundantGeneratingSubset: for a set containing identity and 
# a single generator, generators defined separately
gap> gens := [Transformation([2, 1]), IdentityTransformation];;
gap> Size(IrredundantGeneratingSubset(gens));
1

# attr: IrredundantGeneratingSubset: for a set containing identity and 
# a single generator, generators given directly
gap> IrredundantGeneratingSubset([Transformation([2, 1]), IdentityTransformation]);;

# attr: IrredundantGeneratingSubset: for a set containing elements
# of a cyclic semigroup along with a generator, generators defined separately
gap> gens := [Transformation([1, 1, 3, 1]),
> Transformation([3, 3, 1, 3])];;
gap> Size(IrredundantGeneratingSubset(gens));
1

# attr: IrredundantGeneratingSubset: for a set containing elements
# of a cyclic semigroup along with a generator, generators given directly
gap> IrredundantGeneratingSubset([Transformation([1, 1, 3, 1]),
> Transformation([3, 3, 1, 3])]);;

# attr: IrredundantGeneratingSubset: test info statements
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

# attr: PrincipalFactor: for a D-class
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

# attr: SmallSemigroupGeneratingSet: for a collection with > 1 elements
gap> SmallSemigroupGeneratingSet([
> Transformation([1, 1, 1, 1, 4]), Transformation([1, 2, 2, 1, 1]),
> Transformation([1, 2, 5, 4, 4]), Transformation([1, 3, 3, 5, 1]),
> Transformation([2, 2, 1, 2, 4]), Transformation([3, 2, 3, 3, 2]),
> Transformation([3, 5, 2, 4, 4]), Transformation([3, 5, 4, 5, 4]),
> Transformation([4, 4, 2, 5, 5]), Transformation([5, 2, 3, 5, 2])]);;

# attr: SmallSemigroupGeneratingSet: for a collection with 1 elements
gap> SmallSemigroupGeneratingSet([BooleanMat([[0, 1, 0, 0], [0, 1, 1, 0], [0,
> 1, 0, 0], [1, 0, 1, 1]])]);
[ Matrix(IsBooleanMat, [[0, 1, 0, 0], [0, 1, 1, 0], [0, 1, 0, 0], 
      [1, 0, 1, 1]]) ]

# attr: SmallSemigroupGeneratingSet: for a semigroup
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

# attr: SmallMonoidGeneratingSet: for a singleton set 1/2
gap> SmallMonoidGeneratingSet([IdentityTransformation]);
[ IdentityTransformation ]

# attr: SmallMonoidGeneratingSet: for a singleton set 2/2
gap> SmallMonoidGeneratingSet([Transformation([2, 1, 2])]);
[ Transformation( [ 2, 1, 2 ] ) ]

# attr: SmallMonoidGeneratingSet: for a 0 generator monoid, 1
gap> S := Monoid(Bipartition([[1, -1]]));;
gap> SmallMonoidGeneratingSet(S);
[ <block bijection: [ 1, -1 ]> ]

# attr: SmallMonoidGeneratingSet: for a 0 generator monoid, 2
gap> S := FreeMonoid(0);;
gap> SmallMonoidGeneratingSet(S);
[  ]

# attr: SmallInverseSemigroupGeneratingSet: for collection > 1 element 
gap> SmallInverseSemigroupGeneratingSet(
> [PartialPerm([1, 2], [4, 1]),
>  PartialPerm([1, 2], [5, 2]), PartialPerm([1, 2, 3], [3, 2, 1]),
>  PartialPerm([1, 2, 3], [3, 2, 4]),
>  PartialPerm([1, 2, 3, 4], [1, 2, 3, 5]),
>  PartialPerm([1, 3, 4], [3, 2, 1]), PartialPerm([1, 2, 4], [3, 1, 2]),
>  PartialPerm([1, 2, 3, 4, 5], [3, 1, 5, 4, 2]),
>  PartialPerm([1, 2, 3, 5], [5, 4, 2, 3])]);;

# attr: SmallInverseSemigroupGeneratingSet: for collection 1 element 
gap> SmallInverseSemigroupGeneratingSet([PartialPerm([1, 2, 3, 7, 9, 10,
> 11, 12], [4, 6, 8, 12, 5, 9, 1, 3])]);
[ [2,6][7,12,3,8][10,9,5][11,1,4] ]

# attr: SmallInverseSemigroupGeneratingSet: for an inverse semigroup
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

# attr: SmallInverseMonoidGeneratingSet: for 0 generators, 1
gap> S := InverseMonoid(PartialPerm([1, 2, 3]));
<trivial partial perm group of rank 3 with 1 generator>
gap> SmallInverseMonoidGeneratingSet(S);
[ <identity partial perm on [ 1, 2, 3 ]> ]

# attr: SmallInverseMonoidGeneratingSet: for 0 generators, 2
gap> S := Group(IdentityTransformation);;
gap> IsTransformationSemigroup(S) and IsGroup(S) and IsGroupAsSemigroup(S) 
> and IsTrivial(S);
true
gap> S := Subgroup(S, []);
<trivial transformation group of degree 0 with 0 generators>
gap> IsInverseMonoid(S) and IsGeneratorsOfInverseSemigroup(S);
true
gap> SmallInverseMonoidGeneratingSet(S);
[  ]

# attr: SmallInverseMonoidGeneratingSet: for > 0 generators 1/2
gap> S := InverseMonoid([PartialPerm([1, 3], [2, 3]),
> PartialPerm([1, 3], [3, 1]),
> PartialPerm([1, 2, 3], [3, 2, 4]),
> PartialPerm([1, 4], [1, 3])]);;
gap> SmallInverseMonoidGeneratingSet(S);;

# attr: SmallInverseMonoidGeneratingSet: for > 0 generators 2/2
gap> Set(SmallInverseMonoidGeneratingSet(DualSymmetricInverseMonoid(3)));
[ <block bijection: [ 1, 2, -3 ], [ 3, -1, -2 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -1 ], [ 3, -3 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -3 ], [ 3, -1 ]> ]

# attr: SmallInverseSemigroupGeneratingSet: for a collection
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

# attr: SmallInverseMonoidGeneratingSet: for a collection
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

# attr: SmallInverseMonoidGeneratingSet: for a collection of 1 element
gap> SmallInverseMonoidGeneratingSet([PartialPerm([1, 2, 4])]);
[ [3,4](1)(2) ]

# attr: SmallInverseSemigroupGeneratingSet: for non-inverse-op elements
gap> SmallInverseSemigroupGeneratingSet([RandomTransformation(10)]);
Error, the argument (a mult. elt. coll.) does not satisfy IsGeneratorsOfInvers\
eSemigroup

# attr: SmallInverseMonoidGeneratingSet: for non-inverse-op elements
gap> SmallInverseMonoidGeneratingSet([RandomMatrix(IsBooleanMat, 10)]);
Error, the argument (a mult. elt. coll.) do not satisfy IsGeneratorsOfInverseS\
emigroup

# attr: SmallInverseMonoidGeneratingSet: for One
gap> SmallInverseMonoidGeneratingSet([PartialPerm([1, 2, 3])]);
[  ]

# attr: SmallGeneratingSet: for an ideal
gap> S := SemigroupIdeal(Semigroup(
>     BooleanMat([[0, 1, 0], [1, 0, 0], [0, 0, 1]]),
>     BooleanMat([[0, 1, 0], [0, 0, 1], [1, 0, 0]]),
>     BooleanMat([[1, 0, 0], [0, 1, 0], [1, 0, 1]]),
>     BooleanMat([[1, 0, 0], [0, 1, 0], [0, 0, 0]])),
>     BooleanMat([[1, 0, 0], [0, 0, 0], [1, 1, 0]]));;
gap> SmallGeneratingSet(S);
[ Matrix(IsBooleanMat, [[1, 0, 0], [0, 0, 0], [1, 1, 0]]) ]

# attr: SmallGeneratingSet: for a group
gap> S := Group(IdentityTransformation);;
gap> IsTransformationSemigroup(S) and IsGroup(S) and IsGroupAsSemigroup(S) 
> and IsTrivial(S);
true
gap> SmallGeneratingSet(S);
[ IdentityTransformation ]

# attr: SmallGeneratingSet: for an inverse monoid
gap> S := InverseMonoid([PartialPerm([1, 2], [3, 2]),
> PartialPerm([1, 2, 4], [2, 3, 1]), PartialPerm([1, 2, 4], [3, 4, 2]),
> PartialPerm([1, 4], [4, 2])]);;
gap> SmallGeneratingSet(S);;

# attr: SmallGeneratingSet: for an inverse semigroup
gap> S := InverseSemigroup([PartialPerm([1, 2], [2, 3]),
>                             PartialPerm([1, 3], [3, 1]),
>                             PartialPerm([1, 2, 3], [4, 3, 2])]);;
gap> SmallGeneratingSet(S);;

# attr: SmallGeneratingSet: for a semigroup 
gap> S := Semigroup([Transformation([3, 1, 4, 1, 3]),
>                    Transformation([3, 5, 3, 2, 4])]);;
gap> SmallGeneratingSet(S);;

# attr: StructureDescription for a Brandt semigroup
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

# attr: StructureDescription for a group as semigroup 1/3
gap> S := AsSemigroup(IsTransformationSemigroup, AlternatingGroup(5));;
gap> IsGroupAsSemigroup(S);
true
gap> StructureDescription(S);
"A5"

# attr: StructureDescription for a group as semigroup 2/3
gap> S := Semigroup(Transformation([2, 1, 1]));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> IsGroupAsSemigroup(S);
true
gap> StructureDescription(S);
"C2"

# attr: StructureDescription for a group as semigroup 3/3
gap> S := SymmetricGroup(3);;
gap> StructureDescription(S);
"S3"

# Test StructureDescription for a group as semigroup
gap> S := Monoid(IdentityTransformation);;
gap> StructureDescription(S);
"1"

# Issue 393: StructureDescription method in Semigroups inappropriately selected
gap> F := FreeGroup("r", "s");;
gap> r := F.1;;
gap> s := F.2;;
gap> G := F / [s * r * s ^ (- 1) * r ^ (- 1)];;
gap> StructureDescription(G) in ["C0 x C0", "Z x Z"];
true

# attr: IsGreensDGreaterThanFunc
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

# Test IsGreensDGreaterThanFunc for an infinite CanUseFroidurePin semigroup
gap> S := Semigroup([Matrix(IsMaxPlusMatrix,
>                           [[-2, 2, 0], [-1, 0, 0], [1, -3, 1]]),
>                    Matrix(IsMaxPlusMatrix,
>                           [[-infinity, 0, 0], [0, 1, 0], [1, -1, 0]])]);;
gap> IsGreensDGreaterThanFunc(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `IsGreensDGreaterThanFunc' on 1 argument\
s

# attr: IsGreensDGreaterThanFunc, error
gap> IsGreensDGreaterThanFunc(FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `IsGreensDGreaterThanFunc' on 1 argument\
s

# attr: MaximalDClasses
gap> S := RegularBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 4 generators>
gap> MaximalDClasses(S);
[ <Green's D-class: Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 1]])> ]
gap> S := SingularTransformationMonoid(4);
<regular transformation semigroup ideal of degree 4 with 1 generator>
gap> x := MaximalDClasses(S);;
gap> Length(x) = 1 and x[1] = DClass(S, Transformation([1, 2, 3, 3]));
true
gap> S := ReesMatrixSemigroup(Group(()), [[()]]);;
gap> MaximalDClasses(S);
[ <Green's D-class: (1,(),1)> ]
gap> S := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> MaximalDClasses(S);
[ <Green's D-class: (1,(),1)> ]

# attr: StructureDescriptionMaximalSubgroups
gap> S := RegularBooleanMatMonoid(3);;
gap> StructureDescriptionMaximalSubgroups(S);
[ "1", "C2", "S3" ]

# attr: IdempotentGeneratedSubsemigroup, 1
gap> S := RegularBooleanMatMonoid(3);;
gap> T := IdempotentGeneratedSubsemigroup(S);;
gap> HasIsIdempotentGenerated(T) and IsIdempotentGenerated(T);
true
gap> Size(T);
381

# attr: IdempotentGeneratedSubsemigroup, 2
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

# attr: InjectionPrincipalFactor
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
Error, the argument (a Green's D-class) is not regular

# attr: MultiplicativeNeutralElement
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

# attr: GroupOfUnits, for a finite semigroup 1/2
gap> S := RegularBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 4 generators>
gap> GroupOfUnits(S);
<group of 3x3 boolean matrices with 2 generators>
gap> StructureDescription(last);
"S3"

# attr: GroupOfUnits, fail 2/2
gap> S := Semigroup(
> BooleanMat([[1, 1, 0, 1], [0, 1, 1, 0], [1, 1, 0, 1], [1, 1, 0, 1]]),
> BooleanMat([[1, 1, 0, 1], [0, 1, 1, 1], [0, 1, 1, 1], [0, 1, 1, 0]]));;
gap> GroupOfUnits(S);
fail

# attr: GroupOfUnits, infinite 1/1
gap> GroupOfUnits(FreeInverseSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `GroupOfUnits' on 1 arguments

# attr: NrIdempotents, C++ 1/1
gap> S := RegularBooleanMatMonoid(3);
<monoid of 3x3 boolean matrices with 4 generators>
gap> NrIdempotents(S);
123

# attr: NrIdempotents, non-C++ 1/1
gap> S := FreeBand(2);;
gap> NrIdempotents(S);
6

# attr: NrIdempotents, infinite 1/1
gap> NrIdempotents(FreeSemigroup(2));
Error, resulting list would be too large (length infinity)

# attr: RepresentativeOfMinimalIdeal, simple, 1/1
gap> S := MinimalIdeal(FreeBand(2));
<simple semigroup ideal with 1 generator>
gap> RepresentativeOfMinimalIdeal(S);
x1x2

# attr: MinimalIdeal, infinite, 1/1
gap> MinimalIdeal(FreeMonoid(3));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `MinimalIdeal' on 1 arguments

# attr: IdempotentGeneratedSubsemigroup, inverse op 1/1
gap> S := DualSymmetricInverseMonoid(2);;
gap> T := IdempotentGeneratedSubsemigroup(S);
<commutative inverse block bijection monoid of degree 2 with 1 generator>
gap> HasIsIdempotentGenerated(T) and IsIdempotentGenerated(T);
true

# attr: MultiplicativeZero, infinite, 1
#gap> MultiplicativeZero(FreeMonoid(2)); 
#FIXME(later) this causes an infinite loop in the GAP library code

# attr: MultiplicativeZero, infinite, 2
gap> F := FreeSemigroup(2);;
gap> x := [[F.1 * F.1, F.1], [F.1 * F.2, F.1], [F.2 * F.1, F.1]];;
gap> T := F / x;
<fp semigroup with 2 generators and 3 relations of length 11>
gap> IsFinite(T);
false
gap> MultiplicativeZero(T);
s1

# attr: MaximalDClasses, infinite 1/1
gap> MaximalDClasses(FreeMonoid(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `MaximalDClasses' on 1 arguments

# attr: StructureDescriptionMaximalSubgroups, infinite 1/1
gap> StructureDescriptionMaximalSubgroups(FreeMonoid(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `StructureDescriptionMaximalSubgroups' o\
n 1 arguments

# attr: IdempotentGeneratedSubsemigroup, infinite 1/1
gap> IdempotentGeneratedSubsemigroup(FreeMonoid(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `IdempotentGeneratedSubsemigroup' on 1 a\
rguments

# attr: IdempotentGeneratedSubsemigroup, infinite, inverse-op 1/1
gap> IdempotentGeneratedSubsemigroup(FreeInverseSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 4th choice method found for `IdempotentGeneratedSubsemigroup' on 1 a\
rguments

# attr: MultiplicativeNeutralElement, infinite, 1
gap> MultiplicativeNeutralElement(FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `MultiplicativeNeutralElement' on 1 argu\
ments

# attr: MultiplicativeNeutralElement, infinite, 2
gap> S := Semigroup([
> Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]),
> Matrix(IsMaxPlusMatrix, [[0, 0], [1, -3]])]);
<semigroup of 2x2 max-plus matrices with 2 generators>
gap> MultiplicativeNeutralElement(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `MultiplicativeNeutralElement' on 1 argu\
ments

# attr: MultiplicativeNeutralElement, One, 1
gap> S := Semigroup(Transformation([1, 3, 2]));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> MultiplicativeNeutralElement(S);
IdentityTransformation

# attr: MultiplicativeNeutralElement, One, 2
gap> S := Semigroup(Transformation([3, 1, 3]));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> MultiplicativeNeutralElement(S);
fail

# attr: MultiplicativeNeutralElement, One, 3
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

# attr: MultiplicativeNeutralElement, One, 4
gap> S := Semigroup([PBR([[-2], [-1]], [[1], [2]])]);
<commutative pbr semigroup of degree 2 with 1 generator>
gap>  MultiplicativeNeutralElement(S);
PBR([ [ -1 ], [ -2 ] ], [ [ 1 ], [ 2 ] ])

# attr: RepresentativeOfMinimalIdeal, infinite 1/1
gap> RepresentativeOfMinimalIdeal(FreeSemigroup(2));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `RepresentativeOfMinimalIdeal' on 1 argu\
ments

# attr: RepresentativeOfMinimalIdeal, simple 1/1
gap> S := Semigroup(AsBooleanMat((1, 2)));
<commutative semigroup of 2x2 boolean matrices with 1 generator>
gap> IsSimpleSemigroup(S);
true
gap> RepresentativeOfMinimalIdeal(S);
Matrix(IsBooleanMat, [[0, 1], [1, 0]])

# attribute: NilpotencyDegree, 1/4
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

# attribute: NilpotencyDegree, 2/4
gap> S := SymmetricGroup(2);
Sym( [ 1 .. 2 ] )
gap> NilpotencyDegree(S);
fail

# attribute: NilpotencyDegree, 3/4
gap> S := FullTransformationMonoid(1);
<full transformation monoid of degree 0>
gap> NilpotencyDegree(S);
1

# attribute: NilpotencyDegree, 4/4 
gap> S := Semigroup([
> Transformation([5, 2, 5, 3, 6, 6, 4, 6]),
> Transformation([6, 2, 5, 7, 5, 3, 7, 7]),
> Transformation([8, 4, 6, 4, 5, 6, 8, 1])]);
<transformation semigroup of degree 8 with 3 generators>
gap> NilpotencyDegree(S);
fail

# attribute: LengthOfLongestDClassChain, 1/4
gap> S := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> LengthOfLongestDClassChain(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `LengthOfLongestDClassChain' on 1 argume\
nts

# attribute: LengthOfLongestDClassChain, 2/4
gap> S := MonogenicSemigroup(8, 5);
<commutative non-regular transformation semigroup of size 12, degree 13 with 
 1 generator>
gap> LengthOfLongestDClassChain(S);
7

# attribute: LengthOfLongestDClassChain, 3/4
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

# attribute: LengthOfLongestDClassChain, 4/4
gap> S := SymmetricGroup(5);
Sym( [ 1 .. 5 ] )
gap> LengthOfLongestDClassChain(S);
0

# attribute: NormalizedPrincipalFactor, 1
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

# attribute: InjectionNormalizedPrincipalFactor, 1
gap> S := ReesZeroMatrixSemigroup(Group(()), [[(), 0], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> InjectionNormalizedPrincipalFactor(DClass(S, RMSElement(S, 1, (), 1)));
MappingByFunction( <Green's D-class: (1,(),1)>, 
<Rees 0-matrix semigroup 2x2 over Group(())>
 , function( x ) ... end, function( x ) ... end )

# attribute: InjectionNormalizedPrincipalFactor, 2
gap> S := SymmetricInverseMonoid(4);
<symmetric inverse monoid of degree 4>
gap> InjectionNormalizedPrincipalFactor(DClass(S,
> PartialPerm([], [])));
MappingByFunction( <Green's D-class: <empty partial perm>>, 
<Rees matrix semigroup 1x1 over Group(())>
 , function( x ) ... end, function( x ) ... end )

# attribute: InjectionNormalizedPrincipalFactor, 2
gap> S := MonogenicSemigroup(4, 2);
<commutative non-regular transformation semigroup of size 5, degree 6 with 1 
 generator>
gap> InjectionNormalizedPrincipalFactor(DClass(S, S.1));
Error, the argument (a Green's D-class) is not regular

# attrTest6:
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

# attr: IrredundantGeneratingSubset: for a set with a single repeated
# element
gap> S := Semigroup([Transformation([1, 1]), Transformation([1, 1])]);
<transformation semigroup of degree 2 with 2 generators>
gap> Size(IrredundantGeneratingSubset(S));
1

# attr: Size: for a monogenic semigroup of special type with minimal
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

# attr: Size: for a monogenic monoid with minimal generating set
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

# attr: Size: for a monogenic semigroup/monoid of a type which does not
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

# GeneratorsSmallest for CanUseGapFroidurePin
gap> S := FreeBand(3);
<free band on the generators [ x1, x2, x3 ]>
gap> Size(S);
159
gap> GeneratorsSmallest(S);
[ x1, x2x1x2, x3x2x3x1x3x2x3, x3x2x3x1x3x2, x3x2x3x1x2x3, x3x2x3x1x2x3x2, 
  x3x2x3x1x2x3x1x3, x3x2x3x1x2x1x3, x3x2x1x3x2x3, x3x1x3, x2x3x1x3x2x3, 
  x2x3x2x1x3x2x3, x2, x3x2x3, x3 ]

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

# attr: InversesOfSemigroupElement, for a group as semigroup
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

# attr: InversesOfSemigroupElement, for a semigroup
gap> S := Semigroup([
>  Matrix(IsMaxPlusMatrix, [[-2, 2, 0], [-1, 0, 0], [1, -3, 1]]),
>  Matrix(IsMaxPlusMatrix, [[- infinity, 0, 0], [0, 1, 0], [1, -1, 0]])]);;
gap> InversesOfSemigroupElement(S, S.1);
Error, the argument (a semigroup) is not finite
gap> S := Semigroup(Transformation([2, 3, 1, 3, 3]));;
gap> InversesOfSemigroupElement(S, Transformation([1, 3, 2]));
Error, the 2nd argument (a mult. element) must belong to the 1st argument (a s\
emigroup)
gap> S := Semigroup([Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 1], [1, 0, 0]]),
>  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 1], [1, 1, 1]])]);;
gap> InversesOfSemigroupElement(S, S.1);
[  ]
gap> InversesOfSemigroupElement(S, S.1 * S.2 * S.1);
[ Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [0, 0, 1]]) ]

# InversesOfSemigroupElement, for an infinite semigroup, 1
gap> S := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> InversesOfSemigroupElement(S, IdentityTransformation);
Error, usage: the 2nd argument must be an element of the 1st,
gap> InversesOfSemigroupElementNC(S, S.1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `InversesOfSemigroupElementNC' on 2 argu\
ments

# attr: OneInverseOfSemigroupElement, for a semigroup
gap> S := Semigroup([
>  Matrix(IsMaxPlusMatrix, [[-2, 2, 0], [-1, 0, 0], [1, -3, 1]]),
>  Matrix(IsMaxPlusMatrix, [[- infinity, 0, 0], [0, 1, 0], [1, -1, 0]])]);;
gap> OneInverseOfSemigroupElement(S, S.1);
Error, the semigroup is not finite
gap> S := Semigroup(Transformation([2, 3, 1, 3, 3]));;
gap> OneInverseOfSemigroupElement(S, Transformation([1, 3, 2]));
Error, the 2nd argument (a mult. element) must belong to the 1st argument (a s\
emigroup)
gap> S := Semigroup([Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 1], [1, 0, 0]]),
>  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 1], [1, 1, 1]])]);;
gap> OneInverseOfSemigroupElement(S, S.1);
fail
gap> OneInverseOfSemigroupElement(S, S.1 * S.2 * S.1);
Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [0, 0, 1]])

# OneInverseOfSemigroupElement, for a semigroup that cannot use Froidure-Pin
gap> S := Semigroup(SEMIGROUPS.UniversalFakeOne);;
gap> OneInverseOfSemigroupElement(S, S.1);
<universal fake one>

# OneInverseOfSemigroupElement, for an infinite semigroup, 1
gap> S := FreeSemigroup(1);
<free semigroup on the generators [ s1 ]>
gap> OneInverseOfSemigroupElementNC(S, S.1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `OneInverseOfSemigroupElementNC' on 2 ar\
guments

# attr: IdempotentGeneratedSubsemigroup, 2
gap> S := FullTransformationMonoid(3);;
gap> I := IdempotentGeneratedSubsemigroup(S);;
gap> HasIsIdempotentGenerated(I);
true
gap> IsIdempotentGenerated(I);
true

# attr: IdempotentGeneratedSubsemigroup, for an Rees matrix semigroup, 1
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

# attr: IdempotentGeneratedSubsemigroup, for an Rees matrix semigroup, 2

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

# attr: IdempotentGeneratedSubsemigroup, for an Rees 0-matrix semigroup, 1
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

# attr: IdempotentGeneratedSubsemigroup, for an Rees 0-matrix semigroup, 2

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

# IdempotentGeneratedSubsemigroup, for R(Z)MS where new method is much better

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

#  IndecomposableElements
gap> S := FullTransformationMonoid(3);
<full transformation monoid of degree 3>
gap> S := Semigroup(GeneratorsOfMonoid(S));
<transformation semigroup of degree 3 with 3 generators>
gap> HasIsSurjectiveSemigroup(S);
false
gap> IndecomposableElements(S);
[  ]
gap> S := Semigroup(S);
<transformation semigroup of degree 3 with 3 generators>
gap> IsMonoidAsSemigroup(S);
true
gap> HasIsSurjectiveSemigroup(S) and IsSurjectiveSemigroup(S);
true
gap> HasIndecomposableElements(S);
false
gap> IndecomposableElements(S);
[  ]
gap> S := MonogenicSemigroup(3, 2);;
gap> IndecomposableElements(S) = [S.1];
true

#  MinimalSemigroupGeneratingSet: for a monogenic semigroup, 1
gap> S := MonogenicSemigroup(IsTransformationSemigroup, 4, 5);
<commutative non-regular transformation semigroup of size 8, degree 9 with 1 
 generator>
gap> MinimalSemigroupGeneratingSet(S);
[ Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] ) ]
gap> x := MinimalSemigroupGeneratingSet(S)[1];
Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] )
gap> S := Semigroup(x, x ^ 2);
<transformation semigroup of degree 9 with 2 generators>
gap> x := MinimalSemigroupGeneratingSet(S);
[ Transformation( [ 2, 3, 4, 5, 1, 5, 6, 7, 8 ] ) ]
gap> Length(x);
1
gap> S = Semigroup(x);
true

#  MinimalSemigroupGeneratingSet: for a 2-generated semigroup, 1
gap> S := SymmetricInverseMonoid(1);
<symmetric inverse monoid of degree 1>
gap> x := MinimalSemigroupGeneratingSet(S);
[ <empty partial perm>, <identity partial perm on [ 1 ]> ]
gap> Length(x);
2
gap> S = Semigroup(x);
true

#  MinimalSemigroupGeneratingSet: for a semigroup with identity adjoined, 1
gap> S := Monoid(RectangularBand(IsBipartitionSemigroup, 2, 2));
<bipartition monoid of degree 2 with 2 generators>
gap> x := MinimalSemigroupGeneratingSet(S);;
gap> Length(x);
3
gap> S = Semigroup(x);
true

#  MinimalSemigroupGeneratingSet: for a semigroup with zero adjoined, 1
gap> S := ReesZeroMatrixSemigroup(Group(()), [[(), ()]]);
<Rees 0-matrix semigroup 2x1 over Group(())>
gap> x := MinimalSemigroupGeneratingSet(S);
[ 0, (1,(),1), (2,(),1) ]
gap> Length(x);
3
gap> S = Semigroup(x);
true

#  MinimalSemigroupGeneratingSet: decomposable elements, 1
gap> S := Semigroup(ZeroSemigroup(IsPartialPermSemigroup, 4));
<partial perm semigroup of rank 3 with 3 generators>
gap> x := MinimalSemigroupGeneratingSet(S);
[ [1,2], [3,4], [5,6] ]
gap> Length(x);
3
gap> S = Semigroup(x);
true
gap> S := Semigroup(Elements(S));
<partial perm semigroup of rank 3 with 4 generators>
gap> x := MinimalSemigroupGeneratingSet(S);
[ [1,2], [3,4], [5,6] ]
gap> Length(x);
3

#  MinimalSemigroupGeneratingSet: decomposable elements, 2
gap> S := Monoid([
>  Transformation([1, 1, 1, 2]),
>  Transformation([1, 1, 2, 1]),
>  Transformation([1, 1, 2, 2]),
>  Transformation([1, 1, 1, 1, 6, 5])]);
<transformation monoid of degree 6 with 4 generators>
gap> x := MinimalSemigroupGeneratingSet(S);
[ IdentityTransformation, Transformation( [ 1, 1, 1, 1, 6, 5 ] ), 
  Transformation( [ 1, 1, 1, 2 ] ), Transformation( [ 1, 1, 2, 1 ] ), 
  Transformation( [ 1, 1, 2, 2 ] ) ]
gap> Length(x);
5
gap> S = Semigroup(x);
true

#  MinimalSemigroupGeneratingSet: for a group as semigroup, 1
gap> S = Semigroup(x);
true
gap> S := Semigroup([
>  Transformation([1, 3, 2, 1]),
>  Transformation([2, 1, 3, 2]),
>  Transformation([3, 1, 2, 3])]);
<transformation semigroup of degree 4 with 3 generators>
gap> x := MinimalSemigroupGeneratingSet(S);;
gap> Length(x);
2
gap> S = Semigroup(x);
true

#  MinimalSemigroupGeneratingSet: for a monoid, 1
gap> S := FullTransformationMonoid(4);
<full transformation monoid of degree 4>
gap> x := MinimalSemigroupGeneratingSet(S);;
gap> Length(x);
3
gap> S = Semigroup(x);
true

#  MinimalSemigroupGeneratingSet: for a trivial semigroup, 1
gap> S := FreeSemigroup(1);;
gap> S := S / [[S.1 ^ 2, S.1]];
<fp semigroup with 1 generator and 1 relation of length 4>
gap> MinimalSemigroupGeneratingSet(S);
[ s1 ]

#  MinimalSemigroupGeneratingSet: for a D-trivial semigroup, 1
gap> n := 3;;
gap> S := UnitriangularBooleanMatMonoid(n);
<monoid of 3x3 boolean matrices with 3 generators>
gap> x := MinimalSemigroupGeneratingSet(S);;
gap> Length(x);
4
gap> S = Semigroup(x);
true

#  MinimalSemigroupGeneratingSet: not yet implemented, 1
gap> S := PartitionMonoid(4);
<regular bipartition *-monoid of size 4140, degree 4 with 4 generators>
gap> x := MinimalSemigroupGeneratingSet(S);
Error, no further methods for computing minimal generating sets are implemente\
d

#  MinimalMonoidGeneratingSet: for a trivial monoid, 1
gap> S := FreeMonoid(1);;
gap> S := S / [[S.1, S.1 ^ 0]];
<fp monoid with 1 generator and 1 relation of length 2>
gap> MinimalMonoidGeneratingSet(S);
[ <identity ...> ]

#  MinimalMonoidGeneratingSet: for a monoid, 1
gap> S := FullTransformationMonoid(3);;
gap> x := MinimalMonoidGeneratingSet(S);;
gap> Length(x);
3
gap> Monoid(x) = S;
true

#  MinimalMonoidGeneratingSet: for a monoid, 2
gap> S := SymmetricInverseMonoid(2);
<symmetric inverse monoid of degree 2>
gap> MinimalMonoidGeneratingSet(S);
[ <identity partial perm on [ 1 ]>, (1,2) ]
gap> S := AsSemigroup(IsBlockBijectionSemigroup, S);;
gap> MinimalMonoidGeneratingSet(S);
[ <block bijection: [ 1, -1 ], [ 2, 3, -2, -3 ]>, 
  <block bijection: [ 1, -2 ], [ 2, -1 ], [ 3, -3 ]> ]

#  MinimalMonoidGeneratingSet: for a monoid, 2
gap> S := SymmetricInverseMonoid(1);
<symmetric inverse monoid of degree 1>
gap> MinimalMonoidGeneratingSet(S);
[ <empty partial perm>, <identity partial perm on [ 1 ]> ]
gap> S := AsSemigroup(IsBlockBijectionSemigroup, S);;
gap> MinimalMonoidGeneratingSet(S);
[ <block bijection: [ 1, 2, -1, -2 ]> ]

#  MinimalMonoidGeneratingSet: for a monoid, 3
gap> x := Bipartition([[1, 3, -1, -2], [2, -3]]);;
gap> S := Monoid(x, x ^ 2);
<block bijection monoid of degree 3 with 2 generators>
gap> MinimalMonoidGeneratingSet(S) = [x];
true

#  NambooripadPartialOrder
gap> S := Semigroup([Transformation([3, 4, 4, 1]),
> Transformation([4, 1, 2, 1]), Transformation([3, 3, 3, 1]),
> Transformation([2, 1, 4, 1])]);;
gap> s := NambooripadPartialOrder(S);
[ [  ], [ 1, 32 ], [ 1, 47 ], [ 1, 78 ], [ 1, 32 ], [ 1, 47 ], [ 1, 78 ], 
  [ 1, 32 ], [ 1, 32 ], [ 1, 32 ], [ 1, 4, 9, 10, 24, 32, 33, 65, 78 ], 
  [ 1, 7, 8, 10, 24, 32, 34, 66, 78 ], [ 1, 47 ], [ 1, 47 ], [ 1, 47 ], 
  [ 1, 4, 14, 15, 24, 47, 48, 70, 78 ], [ 1, 7, 13, 15, 24, 47, 49, 71, 78 ], 
  [ 1, 78 ], [ 1, 5, 10, 18, 24, 32, 36, 73, 78 ], 
  [ 1, 6, 15, 18, 24, 47, 51, 74, 78 ], [ 1, 78 ], 
  [ 1, 2, 10, 21, 24, 32, 38, 76, 78 ], [ 1, 3, 15, 21, 24, 47, 53, 77, 78 ], 
  [ 1, 78 ], [ 1, 32 ], [ 1, 32 ], [ 1, 4, 25, 26, 32, 33, 39, 58, 78 ], 
  [ 1, 32 ], [ 1, 7, 25, 28, 32, 34, 39, 61, 78 ], [ 1, 32 ], [ 1, 32 ], 
  [  ], [ 32, 78 ], [ 32, 78 ], [ 1, 18, 25, 30, 32, 36, 39, 72, 78 ], 
  [ 32, 78 ], [ 1, 21, 25, 31, 32, 38, 39, 75, 78 ], [ 32, 78 ], [ 32, 78 ], 
  [ 1, 47 ], [ 1, 47 ], [ 1, 4, 40, 41, 47, 48, 54, 58, 78 ], [ 1, 47 ], 
  [ 1, 7, 40, 43, 47, 49, 54, 61, 78 ], [ 1, 47 ], [ 1, 47 ], [  ], 
  [ 47, 78 ], [ 47, 78 ], [ 1, 18, 40, 45, 47, 51, 54, 72, 78 ], [ 47, 78 ], 
  [ 1, 21, 40, 46, 47, 53, 54, 75, 78 ], [ 47, 78 ], [ 47, 78 ], [ 1, 78 ], 
  [ 1, 2, 26, 32, 55, 58, 64, 76, 78 ], [ 1, 3, 41, 47, 55, 58, 69, 77, 78 ], 
  [ 1, 78 ], [ 1, 5, 28, 32, 55, 61, 64, 73, 78 ], 
  [ 1, 6, 43, 47, 55, 61, 69, 74, 78 ], [ 1, 78 ], 
  [ 1, 8, 30, 32, 55, 64, 66, 72, 78 ], [ 1, 9, 31, 32, 55, 64, 65, 75, 78 ], 
  [ 32, 78 ], [ 32, 78 ], [ 32, 78 ], [ 1, 13, 45, 47, 55, 69, 71, 72, 78 ], 
  [ 1, 14, 46, 47, 55, 69, 70, 75, 78 ], [ 47, 78 ], [ 47, 78 ], [ 47, 78 ], 
  [ 1, 78 ], [ 32, 78 ], [ 47, 78 ], [ 1, 78 ], [ 32, 78 ], [ 47, 78 ], [  ] ]
gap> GR := Digraph(s);
<immutable digraph with 78 vertices, 316 edges>
gap> IsPartialOrderDigraph(DigraphReflexiveTransitiveClosure(GR));
true
gap> S := FullTransformationMonoid(4);;
gap> s := NambooripadPartialOrder(S);
[ [  ], [ 1, 86 ], [ 1, 171 ], [ 1, 256 ], [ 1, 86 ], [ 1, 86 ], 
  [ 1, 3, 5, 6, 11, 86, 87, 167, 171 ], [ 1, 4, 5, 6, 16, 86, 88, 248, 256 ], 
  [ 1, 171 ], [ 1, 2, 6, 9, 11, 86, 90, 170, 171 ], [ 1, 171 ], 
  [ 1, 4, 9, 11, 16, 171, 172, 252, 256 ], [ 1, 256 ], 
  [ 1, 2, 6, 13, 16, 86, 94, 254, 256 ], 
  [ 1, 3, 11, 13, 16, 171, 175, 255, 256 ], [ 1, 256 ], [ 1, 86 ], [ 1, 86 ], 
  [ 1, 3, 17, 18, 35, 86, 87, 155, 171 ], 
  [ 1, 4, 17, 18, 52, 86, 88, 224, 256 ], [ 1, 86 ], [ 1, 86 ], 
  [ 1, 3, 21, 22, 43, 86, 87, 151, 171 ], 
  [ 1, 4, 21, 22, 64, 86, 88, 216, 256 ], 
  [ 1, 9, 17, 21, 41, 86, 90, 155, 171 ], 
  [ 1, 9, 18, 22, 43, 86, 90, 154, 171 ], 
  [ 1, 11, 17, 22, 43, 86, 91, 155, 171 ], 
  [ 1, 4, 9, 11, 12, 16, 17, 18, 20, 21, 22, 24, 25, 26, 27, 32, 41, 43, 44, 
      52, 60, 64, 86, 88, 90, 91, 92, 96, 154, 155, 156, 171, 172, 188, 216, 
      220, 224, 236, 252, 256 ], [ 1, 13, 17, 21, 61, 86, 94, 224, 256 ], 
  [ 1, 13, 18, 22, 64, 86, 94, 222, 256 ], 
  [ 1, 3, 11, 13, 15, 16, 17, 18, 19, 21, 22, 23, 27, 29, 30, 32, 35, 43, 47, 
      61, 63, 64, 86, 87, 91, 94, 95, 96, 151, 155, 159, 171, 175, 191, 222, 
      223, 224, 239, 255, 256 ], [ 1, 16, 17, 22, 64, 86, 96, 224, 256 ], 
  [ 1, 171 ], [ 1, 2, 18, 33, 35, 86, 102, 170, 171 ], [ 1, 171 ], 
  [ 1, 4, 33, 35, 52, 171, 172, 240, 256 ], 
  [ 1, 5, 21, 33, 41, 86, 102, 167, 171 ], 
  [ 1, 6, 22, 33, 43, 86, 102, 166, 171 ], 
  [ 1, 5, 22, 35, 43, 86, 103, 167, 171 ], 
  [ 1, 4, 5, 6, 8, 16, 21, 22, 24, 33, 35, 36, 37, 38, 39, 41, 43, 44, 48, 
      52, 56, 64, 86, 88, 102, 103, 104, 120, 166, 167, 168, 171, 172, 176, 
      216, 232, 236, 240, 248, 256 ], [ 1, 171 ], 
  [ 1, 2, 22, 41, 43, 86, 106, 170, 171 ], [ 1, 171 ], 
  [ 1, 4, 41, 43, 64, 171, 172, 236, 256 ], 
  [ 1, 13, 33, 41, 61, 171, 175, 240, 256 ], 
  [ 1, 2, 6, 13, 14, 16, 18, 22, 30, 33, 34, 35, 38, 41, 42, 43, 45, 47, 48, 
      61, 62, 64, 86, 94, 102, 106, 110, 126, 166, 170, 171, 174, 175, 176, 
      222, 238, 239, 240, 254, 256 ], 
  [ 1, 13, 35, 43, 64, 171, 175, 239, 256 ], 
  [ 1, 16, 33, 43, 64, 171, 176, 240, 256 ], [ 1, 256 ], 
  [ 1, 2, 18, 49, 52, 86, 118, 254, 256 ], 
  [ 1, 3, 35, 49, 52, 171, 187, 255, 256 ], [ 1, 256 ], 
  [ 1, 5, 21, 49, 61, 86, 118, 248, 256 ], 
  [ 1, 6, 22, 49, 64, 86, 118, 246, 256 ], 
  [ 1, 3, 5, 6, 7, 11, 21, 22, 23, 35, 39, 43, 49, 51, 52, 53, 54, 56, 59, 
      61, 63, 64, 86, 87, 103, 118, 119, 120, 151, 167, 171, 183, 187, 191, 
      246, 247, 248, 251, 255, 256 ], [ 1, 5, 22, 52, 64, 86, 120, 248, 256 ],
  [ 1, 9, 41, 49, 61, 171, 187, 252, 256 ], 
  [ 1, 2, 6, 9, 10, 11, 18, 22, 26, 41, 42, 43, 49, 50, 52, 54, 57, 59, 60, 
      61, 62, 64, 86, 90, 106, 118, 122, 126, 154, 170, 171, 186, 187, 188, 
      246, 250, 251, 252, 254, 256 ], 
  [ 1, 11, 43, 49, 64, 171, 187, 251, 256 ], 
  [ 1, 9, 43, 52, 64, 171, 188, 252, 256 ], [ 1, 256 ], 
  [ 1, 2, 22, 61, 64, 86, 126, 254, 256 ], 
  [ 1, 3, 43, 61, 64, 171, 191, 255, 256 ], [ 1, 256 ], [ 1, 86 ], [ 1, 86 ], 
  [ 1, 3, 65, 66, 86, 87, 107, 131, 171 ], 
  [ 1, 4, 65, 66, 86, 88, 128, 196, 256 ], [ 1, 86 ], [ 1, 86 ], 
  [ 1, 3, 69, 70, 86, 87, 103, 139, 171 ], 
  [ 1, 4, 69, 70, 86, 88, 120, 208, 256 ], 
  [ 1, 9, 65, 69, 86, 90, 107, 137, 171 ], 
  [ 1, 9, 66, 70, 86, 90, 106, 139, 171 ], 
  [ 1, 11, 65, 70, 86, 91, 107, 139, 171 ], 
  [ 1, 4, 9, 11, 12, 16, 65, 66, 68, 69, 70, 72, 73, 74, 75, 80, 86, 88, 90, 
      91, 92, 96, 106, 107, 108, 120, 124, 128, 137, 139, 140, 171, 172, 188, 
      196, 204, 208, 236, 252, 256 ], [ 1, 13, 65, 69, 86, 94, 128, 205, 256 ]
    , [ 1, 13, 66, 70, 86, 94, 126, 208, 256 ], 
  [ 1, 3, 11, 13, 15, 16, 65, 66, 67, 69, 70, 71, 75, 77, 78, 80, 86, 87, 91, 
      94, 95, 96, 103, 107, 111, 126, 127, 128, 131, 139, 143, 171, 175, 191, 
      205, 207, 208, 239, 255, 256 ], [ 1, 16, 65, 70, 86, 96, 128, 208, 256 ]
    , [ 1, 86 ], [ 1, 86 ], [ 1, 3, 81, 82, 86, 87, 91, 163, 171 ], 
  [ 1, 4, 81, 82, 86, 88, 96, 244, 256 ], [ 1, 86 ], [  ], [ 86, 171 ], 
  [ 86, 256 ], [ 1, 9, 81, 85, 86, 90, 91, 169, 171 ], [ 86, 171 ], 
  [ 86, 171 ], [ 86, 88, 90, 91, 96, 171, 172, 252, 256 ], 
  [ 1, 13, 81, 85, 86, 94, 96, 253, 256 ], [ 86, 256 ], 
  [ 86, 87, 91, 94, 96, 171, 175, 255, 256 ], [ 86, 256 ], 
  [ 1, 33, 65, 81, 86, 102, 107, 161, 171 ], 
  [ 1, 33, 66, 82, 86, 102, 106, 163, 171 ], 
  [ 1, 35, 65, 82, 86, 103, 107, 163, 171 ], 
  [ 1, 4, 33, 35, 36, 52, 65, 66, 68, 81, 82, 84, 86, 88, 96, 97, 98, 99, 
      102, 103, 104, 106, 107, 108, 112, 116, 120, 128, 161, 163, 164, 171, 
      172, 176, 196, 228, 236, 240, 244, 256 ], 
  [ 1, 33, 69, 85, 86, 102, 103, 169, 171 ], [ 86, 171 ], [ 86, 171 ], 
  [ 86, 88, 102, 103, 120, 171, 172, 240, 256 ], 
  [ 1, 41, 65, 85, 86, 106, 107, 169, 171 ], [ 86, 171 ], [ 86, 171 ], 
  [ 86, 88, 106, 107, 128, 171, 172, 236, 256 ], 
  [ 1, 13, 33, 41, 45, 61, 65, 69, 77, 81, 85, 86, 93, 94, 96, 97, 101, 102, 
      103, 105, 106, 107, 110, 111, 112, 125, 126, 128, 161, 169, 171, 173, 
      175, 176, 205, 237, 239, 240, 253, 256 ], 
  [ 86, 94, 102, 106, 126, 171, 175, 240, 256 ], 
  [ 86, 94, 103, 107, 128, 171, 175, 239, 256 ], 
  [ 86, 96, 102, 107, 128, 171, 176, 240, 256 ], 
  [ 1, 49, 65, 81, 86, 118, 128, 241, 256 ], 
  [ 1, 49, 66, 82, 86, 118, 126, 244, 256 ], 
  [ 1, 3, 35, 49, 51, 52, 65, 66, 67, 81, 82, 83, 86, 87, 91, 99, 103, 107, 
      113, 114, 116, 118, 119, 120, 123, 126, 127, 128, 131, 163, 171, 179, 
      187, 191, 241, 243, 244, 251, 255, 256 ], 
  [ 1, 52, 65, 82, 86, 120, 128, 244, 256 ], 
  [ 1, 49, 69, 85, 86, 118, 120, 253, 256 ], [ 86, 256 ], 
  [ 86, 87, 103, 118, 120, 171, 187, 255, 256 ], [ 86, 256 ], 
  [ 1, 9, 41, 49, 57, 61, 65, 69, 73, 81, 85, 86, 89, 90, 91, 105, 106, 107, 
      113, 117, 118, 120, 122, 123, 124, 125, 126, 128, 137, 169, 171, 185, 
      187, 188, 241, 249, 251, 252, 253, 256 ], 
  [ 86, 90, 106, 118, 126, 171, 187, 252, 256 ], 
  [ 86, 91, 107, 118, 128, 171, 187, 251, 256 ], 
  [ 86, 90, 107, 120, 128, 171, 188, 252, 256 ], 
  [ 1, 61, 65, 85, 86, 126, 128, 253, 256 ], [ 86, 256 ], 
  [ 86, 87, 107, 126, 128, 171, 191, 255, 256 ], [ 86, 256 ], [ 1, 171 ], 
  [ 1, 2, 66, 86, 129, 131, 150, 170, 171 ], [ 1, 171 ], 
  [ 1, 4, 129, 131, 171, 172, 192, 196, 256 ], 
  [ 1, 5, 69, 86, 129, 137, 150, 167, 171 ], 
  [ 1, 6, 70, 86, 129, 139, 150, 166, 171 ], 
  [ 1, 5, 70, 86, 131, 139, 151, 167, 171 ], 
  [ 1, 4, 5, 6, 8, 16, 69, 70, 72, 86, 88, 120, 129, 131, 132, 133, 134, 135, 
      137, 139, 140, 144, 150, 151, 152, 166, 167, 168, 171, 172, 176, 184, 
      188, 192, 196, 200, 208, 216, 248, 256 ], [ 1, 171 ], 
  [ 1, 2, 70, 86, 137, 139, 154, 170, 171 ], [ 1, 171 ], 
  [ 1, 4, 137, 139, 171, 172, 188, 208, 256 ], 
  [ 1, 13, 129, 137, 171, 175, 192, 205, 256 ], 
  [ 1, 2, 6, 13, 14, 16, 66, 70, 78, 86, 94, 126, 129, 130, 131, 134, 137, 
      138, 139, 141, 143, 144, 150, 154, 158, 166, 170, 171, 174, 175, 176, 
      190, 191, 192, 205, 206, 208, 222, 254, 256 ], 
  [ 1, 13, 131, 139, 171, 175, 191, 208, 256 ], 
  [ 1, 16, 129, 139, 171, 176, 192, 208, 256 ], 
  [ 1, 17, 81, 86, 129, 150, 155, 161, 171 ], 
  [ 1, 18, 82, 86, 129, 150, 154, 163, 171 ], 
  [ 1, 17, 82, 86, 131, 151, 155, 163, 171 ], 
  [ 1, 4, 17, 18, 20, 52, 81, 82, 84, 86, 88, 96, 129, 131, 132, 145, 146, 
      147, 150, 151, 152, 154, 155, 156, 160, 161, 163, 164, 171, 172, 176, 
      180, 188, 192, 196, 212, 216, 224, 244, 256 ], 
  [ 1, 21, 85, 86, 129, 150, 151, 169, 171 ], [ 86, 171 ], [ 86, 171 ], 
  [ 86, 88, 150, 151, 171, 172, 192, 216, 256 ], 
  [ 1, 17, 85, 86, 137, 154, 155, 169, 171 ], [ 86, 171 ], [ 86, 171 ], 
  [ 86, 88, 154, 155, 171, 172, 188, 224, 256 ], 
  [ 1, 13, 17, 21, 29, 61, 81, 85, 86, 93, 94, 96, 129, 137, 141, 145, 149, 
      150, 151, 153, 154, 155, 158, 159, 160, 161, 169, 171, 173, 175, 176, 
      189, 191, 192, 205, 221, 222, 224, 253, 256 ], 
  [ 86, 94, 150, 154, 171, 175, 192, 222, 256 ], 
  [ 86, 94, 151, 155, 171, 175, 191, 224, 256 ], 
  [ 86, 96, 150, 155, 171, 176, 192, 224, 256 ], [ 1, 171 ], 
  [ 1, 2, 82, 86, 161, 163, 166, 170, 171 ], [ 1, 171 ], 
  [ 1, 4, 161, 163, 171, 172, 176, 244, 256 ], 
  [ 1, 5, 85, 86, 161, 166, 167, 169, 171 ], [ 86, 171 ], [ 86, 171 ], 
  [ 86, 88, 166, 167, 171, 172, 176, 248, 256 ], [ 1, 171 ], [ 86, 171 ], 
  [  ], [ 171, 256 ], [ 1, 13, 161, 169, 171, 175, 176, 253, 256 ], 
  [ 86, 94, 166, 170, 171, 175, 176, 254, 256 ], [ 171, 256 ], [ 171, 256 ], 
  [ 1, 49, 129, 161, 171, 187, 192, 241, 256 ], 
  [ 1, 2, 18, 49, 50, 52, 66, 82, 86, 114, 118, 126, 129, 130, 131, 146, 150, 
      154, 161, 162, 163, 166, 170, 171, 177, 179, 180, 182, 186, 187, 188, 
      190, 191, 192, 241, 242, 244, 246, 254, 256 ], 
  [ 1, 49, 131, 163, 171, 187, 191, 244, 256 ], 
  [ 1, 52, 129, 163, 171, 188, 192, 244, 256 ], 
  [ 1, 5, 21, 49, 53, 61, 69, 85, 86, 117, 118, 120, 129, 133, 137, 149, 150, 
      151, 161, 165, 166, 167, 169, 171, 177, 182, 183, 184, 185, 187, 188, 
      189, 191, 192, 241, 245, 246, 248, 253, 256 ], 
  [ 86, 118, 150, 166, 171, 187, 192, 246, 256 ], 
  [ 86, 118, 151, 167, 171, 187, 191, 248, 256 ], 
  [ 86, 120, 150, 167, 171, 188, 192, 248, 256 ], 
  [ 1, 49, 137, 169, 171, 187, 188, 253, 256 ], 
  [ 86, 118, 154, 170, 171, 187, 188, 254, 256 ], [ 171, 256 ], [ 171, 256 ], 
  [ 1, 61, 129, 169, 171, 191, 192, 253, 256 ], 
  [ 86, 126, 150, 170, 171, 191, 192, 254, 256 ], [ 171, 256 ], [ 171, 256 ], 
  [ 1, 256 ], [ 1, 2, 66, 86, 193, 196, 214, 254, 256 ], 
  [ 1, 3, 131, 171, 193, 196, 235, 255, 256 ], [ 1, 256 ], 
  [ 1, 5, 69, 86, 193, 205, 214, 248, 256 ], 
  [ 1, 6, 70, 86, 193, 208, 214, 246, 256 ], 
  [ 1, 3, 5, 6, 7, 11, 69, 70, 71, 86, 87, 103, 131, 135, 139, 151, 167, 171, 
      193, 195, 196, 197, 198, 200, 203, 205, 207, 208, 214, 215, 216, 231, 
      235, 239, 246, 247, 248, 251, 255, 256 ], 
  [ 1, 5, 70, 86, 196, 208, 216, 248, 256 ], 
  [ 1, 9, 137, 171, 193, 205, 235, 252, 256 ], 
  [ 1, 2, 6, 9, 10, 11, 66, 70, 74, 86, 90, 106, 137, 138, 139, 154, 170, 
      171, 193, 194, 196, 198, 201, 203, 204, 205, 206, 208, 214, 218, 222, 
      234, 235, 236, 246, 250, 251, 252, 254, 256 ], 
  [ 1, 11, 139, 171, 193, 208, 235, 251, 256 ], 
  [ 1, 9, 139, 171, 196, 208, 236, 252, 256 ], [ 1, 256 ], 
  [ 1, 2, 70, 86, 205, 208, 222, 254, 256 ], 
  [ 1, 3, 139, 171, 205, 208, 239, 255, 256 ], [ 1, 256 ], 
  [ 1, 17, 81, 86, 193, 214, 224, 241, 256 ], 
  [ 1, 18, 82, 86, 193, 214, 222, 244, 256 ], 
  [ 1, 3, 17, 18, 19, 35, 81, 82, 83, 86, 87, 91, 131, 147, 151, 155, 163, 
      171, 193, 195, 196, 209, 210, 212, 214, 215, 216, 219, 222, 223, 224, 
      227, 235, 239, 241, 243, 244, 251, 255, 256 ], 
  [ 1, 17, 82, 86, 196, 216, 224, 244, 256 ], 
  [ 1, 21, 85, 86, 193, 214, 216, 253, 256 ], [ 86, 256 ], 
  [ 86, 87, 151, 171, 214, 216, 235, 255, 256 ], [ 86, 256 ], 
  [ 1, 9, 17, 21, 25, 41, 81, 85, 86, 89, 90, 91, 137, 153, 154, 155, 169, 
      171, 193, 201, 205, 209, 213, 214, 216, 218, 219, 220, 221, 222, 224, 
      233, 235, 236, 241, 249, 251, 252, 253, 256 ], 
  [ 86, 90, 154, 171, 214, 222, 235, 252, 256 ], 
  [ 86, 91, 155, 171, 214, 224, 235, 251, 256 ], 
  [ 86, 90, 155, 171, 216, 224, 236, 252, 256 ], 
  [ 1, 17, 85, 86, 205, 222, 224, 253, 256 ], [ 86, 256 ], 
  [ 86, 87, 155, 171, 222, 224, 239, 255, 256 ], [ 86, 256 ], 
  [ 1, 33, 161, 171, 193, 235, 240, 241, 256 ], 
  [ 1, 2, 18, 33, 34, 35, 66, 82, 86, 98, 102, 106, 161, 162, 163, 166, 170, 
      171, 193, 194, 196, 210, 214, 222, 225, 227, 228, 230, 234, 235, 236, 
      238, 239, 240, 241, 242, 244, 246, 254, 256 ], 
  [ 1, 35, 163, 171, 193, 235, 239, 244, 256 ], 
  [ 1, 33, 163, 171, 196, 236, 240, 244, 256 ], 
  [ 1, 5, 21, 33, 37, 41, 69, 85, 86, 101, 102, 103, 161, 165, 166, 167, 169, 
      171, 193, 197, 205, 213, 214, 216, 225, 230, 231, 232, 233, 235, 236, 
      237, 239, 240, 241, 245, 246, 248, 253, 256 ], 
  [ 86, 102, 166, 171, 214, 235, 240, 246, 256 ], 
  [ 86, 103, 167, 171, 214, 235, 239, 248, 256 ], 
  [ 86, 102, 167, 171, 216, 236, 240, 248, 256 ], 
  [ 1, 41, 169, 171, 193, 235, 236, 253, 256 ], 
  [ 86, 106, 170, 171, 214, 235, 236, 254, 256 ], [ 171, 256 ], [ 171, 256 ], 
  [ 1, 33, 169, 171, 205, 239, 240, 253, 256 ], 
  [ 86, 102, 170, 171, 222, 239, 240, 254, 256 ], [ 171, 256 ], [ 171, 256 ], 
  [ 1, 256 ], [ 1, 2, 82, 86, 241, 244, 246, 254, 256 ], 
  [ 1, 3, 163, 171, 241, 244, 251, 255, 256 ], [ 1, 256 ], 
  [ 1, 5, 85, 86, 241, 246, 248, 253, 256 ], [ 86, 256 ], 
  [ 86, 87, 167, 171, 246, 248, 251, 255, 256 ], [ 86, 256 ], 
  [ 1, 9, 169, 171, 241, 251, 252, 253, 256 ], 
  [ 86, 90, 170, 171, 246, 251, 252, 254, 256 ], [ 171, 256 ], [ 171, 256 ], 
  [ 1, 256 ], [ 86, 256 ], [ 171, 256 ], [  ] ]
gap> S := InverseSemigroup([Bipartition([[1, -3], [2, -1], [3, 4, -2, -4]]),
> Bipartition([[1, -1], [2, -3], [3, -2], [4, -4]])]);
<inverse block bijection semigroup of degree 4 with 2 generators>
gap> s := NambooripadPartialOrder(S);
[ [  ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], [ 1 ], 
  [ 1, 2, 8 ], [ 1, 3, 8 ], [ 1, 4, 9 ], [ 1, 4, 10 ], [ 1, 6, 8 ], 
  [ 1, 5, 8 ], [ 1, 7, 9 ], [ 1, 7, 10 ], [ 1, 2, 6, 8, 11, 15 ], 
  [ 1, 3, 5, 8, 12, 16 ] ]
gap> s = NaturalPartialOrder(S);
true
gap> NambooripadLeqRegularSemigroup(S) = NaturalLeqInverseSemigroup(S);
true
gap> S := FreeSemigroup(3);;
gap> NambooripadPartialOrder(S);
Error, the argument (a semigroup) is not finite
gap> NambooripadLeqRegularSemigroup(S);
Error, the argument (a semigroup) is not finite
gap> S := ZeroSemigroup(5);;
gap> NambooripadPartialOrder(S);
Error, the argument (a semigroup) is not regular
gap> NambooripadLeqRegularSemigroup(S);
Error, the argument (a semigroup) is not regular

# Left/RightIdentity
gap> S := Semigroup(Transformation([2, 4, 3, 4]), 
>                   Transformation([3, 3, 2, 3, 3]),
>                   Transformation([5, 5, 5, 4, 4]), 
>                   Transformation([5, 1, 4, 1, 1]),
>                   Transformation([5, 3, 3, 4, 5]));;
gap> ForAll(S, x -> RightIdentity(S, x) = fail or x * RightIdentity(S, x) = x);
true
gap> ForAll(S, x -> RightIdentity(S, x) = fail or RightIdentity(S, x) in S);
true
gap> ForAll(S, x -> LeftIdentity(S, x) = fail or LeftIdentity(S, x) * x = x);
true
gap> ForAll(S, x -> LeftIdentity(S, x) = fail or LeftIdentity(S, x) in S);
true
gap> L := Filtered(S, x -> LeftIdentity(S, x) = fail);
[ Transformation( [ 2, 4, 3, 4 ] ), Transformation( [ 5, 5, 5, 4, 4 ] ), 
  Transformation( [ 5, 1, 4, 1, 1 ] ), Transformation( [ 5, 2, 4, 2, 2 ] ), 
  Transformation( [ 5, 4, 4, 4, 4 ] ), Transformation( [ 5, 3, 4, 3, 3 ] ) ]
gap> Length(L) = 6;
true
gap> ForAll(L, y -> ForAll(S, x -> x * y <> y));
true
gap> ForAll(L, y -> ForAll(S, x -> x * y <> y));
true
gap> R := Filtered(S, x -> RightIdentity(S, x) = fail);
[ Transformation( [ 2, 4, 3, 4 ] ), Transformation( [ 5, 1, 4, 1, 1 ] ), 
  Transformation( [ 5, 2, 4, 2, 2 ] ) ]
gap> Length(R) = 3;
true
gap> ForAll(R, y -> ForAll(S, x -> y * x <> y));
true

# Non-acting example
gap> S := Semigroup(
> [Matrix(IsBooleanMat, [[0, 1, 0, 0, 0], [0, 0, 0, 1, 0], [0, 0, 1, 0, 0], [0, 0, 0, 1, 0],
>       [0, 0, 0, 0, 1]]),
>  Matrix(IsBooleanMat, [[0, 0, 1, 0, 0], [0, 0, 1, 0, 0], [0, 1, 0, 0, 0],
>       [0, 0, 1, 0, 0], [0, 0, 1, 0, 0]]),
>  Matrix(IsBooleanMat, [[0, 0, 0, 0, 1], [0, 0, 0, 0, 1], [0, 0, 0, 0, 1], [0, 0, 0, 1, 0],
>       [0, 0, 0, 1, 0]]),
>  Matrix(IsBooleanMat, [[0, 0, 0, 0, 1], [1, 0, 0, 0, 0], [0, 0, 0, 1, 0],
>       [1, 0, 0, 0, 0], [1, 0, 0, 0, 0]]),
>  Matrix(IsBooleanMat, [[0, 0, 0, 0, 1], [0, 0, 1, 0, 0], [0, 0, 1, 0, 0], [0, 0, 0, 1, 0],
>       [0, 0, 0, 0, 1]])]);
<semigroup of 5x5 boolean matrices with 5 generators>
gap> ForAll(S, x -> RightIdentity(S, x) = fail or x * RightIdentity(S, x) = x);
true
gap> ForAll(S, x -> RightIdentity(S, x) = fail or RightIdentity(S, x) in S);
true
gap> ForAll(S, x -> LeftIdentity(S, x) = fail or LeftIdentity(S, x) * x = x);
true
gap> ForAll(S, x -> LeftIdentity(S, x) = fail or LeftIdentity(S, x) in S);
true
gap> L := Filtered(S, x -> LeftIdentity(S, x) = fail);
[ Matrix(IsBooleanMat, [[0, 1, 0, 0, 0], [0, 0, 0, 1, 0], [0, 0, 1, 0, 0], 
      [0, 0, 0, 1, 0], [0, 0, 0, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0, 0, 1], [0, 0, 0, 0, 1], [0, 0, 0, 0, 1], 
      [0, 0, 0, 1, 0], [0, 0, 0, 1, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0, 0, 1], [1, 0, 0, 0, 0], [0, 0, 0, 1, 0], 
      [1, 0, 0, 0, 0], [1, 0, 0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0, 0, 1], [0, 1, 0, 0, 0], [0, 0, 0, 1, 0], 
      [0, 1, 0, 0, 0], [0, 1, 0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0, 0, 1], [0, 0, 0, 1, 0], [0, 0, 0, 1, 0], 
      [0, 0, 0, 1, 0], [0, 0, 0, 1, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0, 0, 1], [0, 0, 1, 0, 0], [0, 0, 0, 1, 0], 
      [0, 0, 1, 0, 0], [0, 0, 1, 0, 0]]) ]
gap> Length(L) = 6;
true
gap> ForAll(L, y -> ForAll(S, x -> x * y <> y));
true
gap> ForAll(L, y -> ForAll(S, x -> x * y <> y));
true
gap> R := Filtered(S, x -> RightIdentity(S, x) = fail);
[ Matrix(IsBooleanMat, [[0, 1, 0, 0, 0], [0, 0, 0, 1, 0], [0, 0, 1, 0, 0], 
      [0, 0, 0, 1, 0], [0, 0, 0, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0, 0, 1], [1, 0, 0, 0, 0], [0, 0, 0, 1, 0], 
      [1, 0, 0, 0, 0], [1, 0, 0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0, 0, 1], [0, 1, 0, 0, 0], [0, 0, 0, 1, 0], 
      [0, 1, 0, 0, 0], [0, 1, 0, 0, 0]]) ]
gap> Length(R) = 3;
true
gap> ForAll(R, y -> ForAll(S, x -> y * x <> y));
true
gap> x := Matrix(IsBooleanMat, 
> [[0, 0, 1, 0, 0], 
>  [0, 1, 1, 0, 1], 
>  [1, 0, 0, 1, 0], 
>  [0, 1, 0, 0, 0], 
>  [1, 1, 1, 1, 1]]);
Matrix(IsBooleanMat, [[0, 0, 1, 0, 0], [0, 1, 1, 0, 1], [1, 0, 0, 1, 0], 
  [0, 1, 0, 0, 0], [1, 1, 1, 1, 1]])
gap> RightIdentity(S, x);
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
semigroup)
gap> LeftIdentity(S, x);
Error, the 2nd argument (a mult. elt.) does not belong to the 1st argument (a \
semigroup)
gap> S := Monoid(S);
<monoid of 5x5 boolean matrices with 5 generators>
gap> LeftIdentity(S, S.1) = One(S);
true
gap> RightIdentity(S, S.1) = One(S);
true
gap> S := Semigroup(Transformation([1, 2, 3, 3]), Transformation([2, 3, 1, 1]));;
gap> S := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of 4x4 boolean matrices with 2 generators>
gap> IsMonoidAsSemigroup(S);
true
gap> RightIdentity(S, Matrix(IsBooleanMat, [[0, 0, 1, 0], [1, 0, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0]])) = MultiplicativeNeutralElement(S);
true
gap> LeftIdentity(S, Matrix(IsBooleanMat, [[0, 0, 1, 0], [1, 0, 0, 0], [0, 1, 0, 0], [0, 1, 0, 0]])) = MultiplicativeNeutralElement(S);
true

# MaximalL/RClasses
gap> S := LeftZeroSemigroup(3);
<transformation semigroup of degree 4 with 3 generators>
gap> MaximalLClasses(S);
[ <Green's L-class: Transformation( [ 1, 2, 1, 1 ] )> ]
gap> MaximalRClasses(S);
[ <Green's R-class: Transformation( [ 1, 2, 1, 1 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 1, 2 ] )>, 
  <Green's R-class: Transformation( [ 1, 2, 2, 1 ] )> ]
gap> S := RightZeroSemigroup(3);
<transformation semigroup of degree 3 with 3 generators>
gap> MaximalLClasses(S);
[ <Green's L-class: Transformation( [ 1, 1, 1 ] )>, 
  <Green's L-class: Transformation( [ 2, 2, 2 ] )>, 
  <Green's L-class: Transformation( [ 3, 3, 3 ] )> ]
gap> MaximalRClasses(S);
[ <Green's R-class: Transformation( [ 1, 1, 1 ] )> ]
gap> S := FullPBRMonoid(1);
<pbr monoid of degree 1 with 4 generators>
gap> MaximalLClasses(S);
[ <Green's L-class: PBR([ [ -1 ] ], [ [ 1 ] ])> ]
gap> MaximalRClasses(S);
[ <Green's R-class: PBR([ [ -1 ] ], [ [ 1 ] ])> ]

# Issue 868 - IsMonoidAsSemigroup assumed HasMultiplicativeNeutralElement
gap> S := SemigroupByMultiplicationTable(
> [[1, 1, 1, 1, 5, 6],
>  [1, 1, 1, 2, 5, 6],
>  [3, 3, 3, 3, 5, 6],
>  [1, 2, 3, 4, 5, 6],
>  [5, 5, 5, 5, 5, 5],
>  [6, 6, 6, 6, 6, 6]]);;
gap> S := AsMonoid(IsFpMonoid, S);
<fp monoid with 5 generators and 25 relations of length 80>

# MinimalFaithfulTransformationDegree
gap> MinimalFaithfulTransformationDegree(RightZeroSemigroup(10));
7
gap> MinimalFaithfulTransformationDegree(LeftZeroSemigroup(10));
6

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/attr.tst");
