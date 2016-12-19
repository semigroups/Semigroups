#############################################################################
##
#W  standard/semiact.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semiact.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# ClosureSemigroupNC, for an acting semigroup
gap> S := Semigroup(IdentityTransformation);;
gap> Size(S);
1
gap> S := ClosureSemigroup(S, AsSet(FullTransformationMonoid(3)));;
gap> Size(S);
27
gap> S := Semigroup(S.3);;
gap> Size(S);
2
gap> S := ClosureSemigroup(S, AsSet(OrderEndomorphisms(5)));;
gap> Size(S);
526
gap> S := ClosureSemigroup(S, AsSet(OrderAntiEndomorphisms(5)));;
gap> Size(S);
1527
gap> S := Semigroup(IdentityTransformation);;
gap> S := ClosureSemigroup(S, Transformation([2, 1]));;
gap> S := AsSemigroup(IsTransformationSemigroup, 
>                     Semigroup(Matrix(IsBooleanMat, [[0, 0], [0, 0]])));;
gap> S := ClosureSemigroup(S, AsSemigroup(IsTransformationSemigroup,
>                                         RegularBooleanMatMonoid(3)));;
gap> # The next result is correct, since different embeddings are used
gap> # in the previous two lines.
gap> Size(S); 
27874
gap> S := ClosureSemigroup(S, AsSemigroup(IsTransformationSemigroup,
>                                         FullBooleanMatMonoid(3)));;
gap> Size(S);
27880
gap> S := InverseSemigroup(PartialPerm([1 .. 3]));;
gap> Size(S);
1
gap> S := ClosureInverseSemigroup(S, AsSet(SymmetricInverseMonoid(6)));;
gap> Size(S);
13327
gap> S := InverseSemigroup(S.3);;
gap> Size(S);
2
gap> S := ClosureInverseSemigroup(S, AsSet(POPI(6)));;
gap> Size(S);
13327
gap> S := InverseSemigroup(PartialPerm([1 .. 3]));;
gap> S := ClosureInverseSemigroup(S, PartialPerm([2, 1]));;
gap> I := SemigroupIdeal(S, S.1);;
gap> ClosureInverseSemigroupNC(I, [PartialPerm([1 .. 7])], rec());;

# Random, for an acting semigroup
gap> S := Semigroup(FullTransformationMonoid(4), rec(acting := true));;
gap> IsActingSemigroup(S);
true
gap> Random(S);;
gap> Size(S);;
gap> Random(S);;
gap> IsRegularSemigroup(S);
true
gap> I := SemigroupIdeal(S, S.1);;
gap> Random(I);;

# Random, for an regular acting semigroup rep
gap> S := OrderEndomorphisms(5);;
gap> IsRegularActingSemigroupRep(S); 
true
gap> Random(S);;
gap> NrRClasses(S);;
gap> Random(S);;
gap> NrLClasses(S);;
gap> Random(S);;
gap> I := SemigroupIdeal(S, S.1);;
gap> Random(I);;

# Random, for an inverse acting semigroup rep
gap> S := POPI(5);;
gap> IsInverseActingSemigroupRep(S); 
true
gap> Random(S);;
gap> NrRClasses(S);;
gap> Random(S);;
gap> NrLClasses(S);;
gap> Random(S);;
gap> I := SemigroupIdeal(S, S.1);;
gap> Random(I);;

# \in, for an regular acting semigroup rep
gap> S := Semigroup(OrderEndomorphisms(5), rec(regular := true));;
gap> IsRegularActingSemigroupRep(S); 
true
gap> Size(S);
126
gap> ConstantTransformation(6, 1) in S;
false
gap> PartialPerm([1]) in S;
false
gap> Enumerate(S);;
gap> S.1 in S;
true
gap> Transformation([2, 1]) in S;
false
gap> S := RegularSemigroup(Transformation([1, 1, 1]));;
gap> x := Transformation([1, 2, 2]);;
gap> x in S;
false
gap> S := RegularSemigroup(x);;
gap> MinimalIdeal(S);;
gap> Transformation([1, 1, 1]) in S;
false
gap> S := RegularSemigroup(x);;
gap> AsSet(S);;
gap> Transformation([1, 1, 1]) in S;
false
gap> S := RegularSemigroup(x);;
gap> Transformation([2, 3, 3]) in S;
false
gap> Transformation([1, 1, 2]) in S;
false
gap> x in S;
true
gap> Size(S);
1

# \in, for an inverse acting semigroup rep
gap> S := InverseSemigroup(POPI(5), rec(regular := true));;
gap> IsInverseActingSemigroupRep(S); 
true
gap> ConstantTransformation(6, 1) in S;
false
gap> PartialPerm([1]) in S;
true
gap> Enumerate(S);;
gap> S.1 in S;
true
gap> PartialPerm([3, 5]) in S;
true
gap> PartialPerm([5, 3]) in S;
true
gap> PartialPerm([1 .. 6]) in S;
false

#E#
gap> STOP_TEST("Semigroups package: standard/semiact.tst");
