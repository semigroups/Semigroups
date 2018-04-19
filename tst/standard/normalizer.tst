#############################################################################
##
#W  standard/normalizer.tst
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## TODO: remove acting := true from all of these, when there are methods
## available for Normalizer for non-acting semigroups.

gap> START_TEST("Semigroups package: standard/normalizer.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# normalizer: Normalizer, 1/3
gap> S := Semigroup([Transformation([1, 3, 4, 1]),
> Transformation([3, 1, 1, 3])], rec(acting := true));;
gap> Normalizer(S);
Group(())
gap> Normalizer(S, rec());
Group(())
gap> Normalizer(Group(()), S);
Group(())
gap> Normalizer(Group(()), S, rec(random := true));
Group(())
gap> SEMIGROUPS.NonDeterministicNormalizer(SymmetricGroup(3), S,
> rec());
Group(())

# normalizer: Normalizer, 2/3
gap> S := Semigroup([
> PartialPerm([1, 2, 3], [2, 4, 1]),
> PartialPerm([1, 3, 4], [3, 4, 1])], rec(acting := true));;
gap> Normalizer(S);
Group(())
gap> Normalizer(S, rec());
Group(())

# normalizer: Normalizer, 3/3
gap> S := Semigroup([Bipartition([[1, 2, -1], [3, -2], [4, -3, -4]]),
> Bipartition([[1, 4, -3, -4], [2, 3, -1], [-2]])], rec(acting := true));;
gap> Normalizer(S);
Group(())
gap> Normalizer(S, rec());
Group(())

# normalizer: SEMIGROUPS.NormalizerOptsRec, error, 1/?
gap> S := Semigroup(IdentityTransformation);;
gap> SEMIGROUPS.NormalizerOptsRec(S, rec(lambdastab := 1));
Error, Semigroups: SEMIGROUPS.NormalizerOptsRec: usage,
the component `lambdastab' must be a boolean,

# normalizer: SEMIGROUPS.NormalizerOptsRec, error, 2/?
gap> S := Semigroup(IdentityTransformation);;
gap> SEMIGROUPS.NormalizerOptsRec(S, rec(rhostab := 1));
Error, Semigroups: SEMIGROUPS.NormalizerOptsRec: usage,
the component `rhostab' must be a boolean,

# normalizer: SEMIGROUPS.NormalizerOptsRec, error, 3/?
gap> S := InverseSemigroup(PartialPerm([1, 4, 2]));;
gap> SEMIGROUPS.NormalizerOptsRec(S, rec(rhostab := 1));
rec( lambdastab := true, rhostab := false )

# normalizer: SEMIGROUPS.LambdaOrbForNormalizer, 1/1
gap> S := Semigroup(Transformation([1, 1, 1]), AsTransformation((1, 2, 3)),
> rec(acting := true));
<transformation semigroup of degree 3 with 2 generators>
gap> SEMIGROUPS.LambdaOrbForNormalizer(Group((1, 2, 3)), S,
> function(x, y)
>   return Length(x) < Length(y);
> end);
[ [ 1, 2, 3 ] ]

# normalizer: SEMIGROUPS.DeterministicNormalizer, error, 1/4
gap> S := Semigroup([
> BooleanMat([[true, false], [true, true]]),
> BooleanMat([[false, false], [false, true]])]);;
gap> Normalizer(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `Normalizer' on 1 arguments

# normalizer: SEMIGROUPS.DeterministicNormalizer, error, 2/4
gap> SEMIGROUPS.DeterministicNormalizer(Group((1, 2)), S, rec());
Error, Semigroups: SEMIGROUPS.DeterministicNormalizer: usage,
the second arg must be a semigroup of transformations,
partial perms or bipartitions,

# normalizer: SEMIGROUPS.DeterministicNormalizer, error, 3/4
gap> SEMIGROUPS.DeterministicNormalizer(1, S, rec());
Error, Semigroups: SEMIGROUPS.DeterministicNormalizer: usage,
the first arg must be a permutation group,

# normalizer: SEMIGROUPS.DeterministicNormalizer, error, 4/4
gap> SEMIGROUPS.DeterministicNormalizer(Group((1, 2)),
> Semigroup(IdentityTransformation), fail);
Error, Semigroups: SEMIGROUPS.DeterministicNormalizer: usage,
the third argument must be a record,

# normalizer: SEMIGROUPS.DeterministicNormalizer, works, 1/1
gap> S := Semigroup(FullTransformationMonoid(3), rec(acting := true));;
gap> SEMIGROUPS.DeterministicNormalizer(SymmetricGroup(3), S, rec());
Group([ (2,3), (1,2,3) ])
gap> SEMIGROUPS.DeterministicNormalizer(SymmetricGroup(3), S,
> rec(lambdastab := false));
Group([ (2,3), (1,2,3) ])
gap> SEMIGROUPS.DeterministicNormalizer(Group(()), S,
> rec(lambdastab := false));
Group(())

# normalizer: SEMIGROUPS.NonDeterministicNormalizer, error, 1/4
gap> S := Semigroup([
> BooleanMat([[true, false], [true, true]]),
> BooleanMat([[false, false], [false, true]])]);;
gap> Normalizer(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `Normalizer' on 1 arguments

# normalizer: SEMIGROUPS.NonDeterministicNormalizer, error, 2/4
gap> SEMIGROUPS.NonDeterministicNormalizer(Group((1, 2)), S, rec());
Error, Semigroups: SEMIGROUPS.NonDeterministicNormalizer: usage,
the second arg must be a semigroup of transformations,
partial perms or bipartitions,

# normalizer: SEMIGROUPS.NonDeterministicNormalizer, error, 3/4
gap> SEMIGROUPS.NonDeterministicNormalizer(1, S, rec());
Error, Semigroups: SEMIGROUPS.NonDeterministicNormalizer: usage,
the first arg must be a permutation group,

# normalizer: SEMIGROUPS.NonDeterministicNormalizer, error, 4/4
gap> SEMIGROUPS.NonDeterministicNormalizer(Group((1, 2)),
> Semigroup(IdentityTransformation), fail);
Error, Semigroups: SEMIGROUPS.NonDeterministicNormalizer: usage,
the third arg must be a record,

# normalizer: SEMIGROUPS.NonDeterministicNormalizer, trans, 1/1
gap> S := Semigroup(FullTransformationMonoid(3), rec(acting := true));;
gap> SEMIGROUPS.NonDeterministicNormalizer(SymmetricGroup(3), S, rec());
Group([ (1,3,2), (1,3) ])
gap> SEMIGROUPS.NonDeterministicNormalizer(SymmetricGroup(3), S,
> rec(lambdastab := false));
Group([ (1,3,2), (1,3) ])
gap> SEMIGROUPS.NonDeterministicNormalizer(Group(()), S,
> rec(lambdastab := false));
Group(())

# normalizer: SEMIGROUPS.NonDeterministicNormalizer, pperm, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(acting := true));;
gap> SEMIGROUPS.NonDeterministicNormalizer(SymmetricGroup(3), S, rec());
Group([ (1,3,2), (1,3) ])
gap> SEMIGROUPS.NonDeterministicNormalizer(SymmetricGroup(3), S,
> rec(lambdastab := false));
Group([ (1,2,3), (1,2) ])
gap> SEMIGROUPS.NonDeterministicNormalizer(Group(()), S,
> rec(lambdastab := false));
Group(())
gap> SEMIGROUPS.NonDeterministicNormalizer(SymmetricGroup(3), S,
> rec(rhostab := true, lambdastab := false));
Group([ (1,2,3), (1,2) ])
gap> S := Semigroup(PartialPerm([1]), PartialPerm([2]),
> PartialPerm([3]), rec(acting := true));;
gap> SEMIGROUPS.NonDeterministicNormalizer(SymmetricGroup(3), S, rec(rhostab :=
> true, lambdastab := false));
Group([ (2,3) ])

# normalizer: SEMIGROUPS.NonDeterministicNormalizer, pperm, 1/1
gap> S := Semigroup(PartitionMonoid(3), rec(acting := true));;
gap> SEMIGROUPS.NonDeterministicNormalizer(SymmetricGroup(3), S, rec());
Group([ (1,3,2), (2,3) ])
gap> SEMIGROUPS.NonDeterministicNormalizer(SymmetricGroup(3), S,
> rec(lambdastab := false));
Group([ (1,2,3), (1,2) ])
gap> SEMIGROUPS.NonDeterministicNormalizer(Group(()), S,
> rec(lambdastab := false));
Group(())

# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/normalizer.tst");
