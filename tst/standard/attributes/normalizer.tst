#############################################################################
##
#W  standard/attributes/normalizer.tst
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## TODO: remove generic := false from all of these!
gap> START_TEST("Semigroups package: standard/attributes/normalizer.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#T# normalizer: Normalizer, 1/3
gap> S := Semigroup( [ Transformation( [ 1, 3, 4, 1 ] ), 
> Transformation( [ 3, 1, 1, 3 ] ) ], rec(generic := false));;
gap> Normalizer(S);
Group(())
gap> Normalizer(S, rec());
Group(())
gap> Normalizer(Group(()), S);
Group(())
gap> Normalizer(Group(()), S, rec(random := true));
Group(())
gap> SEMIGROUPS_NonDeterministicNormalizer(SymmetricGroup(3), S, 
> rec());
Group(())

#T# normalizer: Normalizer, 2/3
gap> S := Semigroup( [ PartialPerm( [ 1, 2, 3 ], [ 2, 4, 1 ] ),
>   PartialPerm( [ 1, 3, 4 ], [ 3, 4, 1 ] ) ], rec(generic := false) );;
gap> Normalizer(S);
Group(())
gap> Normalizer(S, rec());
Group(())

#T# normalizer: Normalizer, 3/3
gap> S := Semigroup( [ Bipartition( [ [ 1, 2, -1 ], [ 3, -2 ], [ 4, -3, -4 ] ] ),
>  Bipartition( [ [ 1, 4, -3, -4 ], [ 2, 3, -1 ], [ -2 ] ] ) ], rec(generic := false) );;
gap> Normalizer(S);
Group(())
gap> Normalizer(S, rec());
Group(())

#T# normalizer: SEMIGROUPS_NormalizerOptsRec, error, 1/?
gap> S := Semigroup(IdentityTransformation);;
gap> SEMIGROUPS_NormalizerOptsRec(S, rec(lambdastab := 1));
Error, Semigroups: SEMIGROUPS_NormalizerOptsRec: usage,
the component `lambdastab' must be a boolean,

#T# normalizer: SEMIGROUPS_NormalizerOptsRec, error, 2/?
gap> S := Semigroup(IdentityTransformation);;
gap> SEMIGROUPS_NormalizerOptsRec(S, rec(rhostab := 1));
Error, Semigroups: SEMIGROUPS_NormalizerOptsRec: usage,
the component `rhostab' must be a boolean,

#T# normalizer: SEMIGROUPS_NormalizerOptsRec, error, 3/?
gap> S := InverseSemigroup(PartialPerm([1,4,2]));;
gap> SEMIGROUPS_NormalizerOptsRec(S, rec(rhostab := 1));
rec( lambdastab := true, rhostab := false )

#T# normalizer: SEMIGROUPS_LambdaOrbForNormalizer, 1/1
gap> S := Semigroup(Transformation([1,1,1]), AsTransformation((1,2,3)),
> rec(generic := false));
<transformation semigroup of degree 3 with 2 generators>
gap> SEMIGROUPS_LambdaOrbForNormalizer(Group((1,2,3)), S, 
> function(x, y)
>   return Length(x) < Length(y);
> end);
[ [ 1, 2, 3 ] ]

#T# normalizer: SEMIGROUPS_DeterministicNormalizer, error, 1/4
gap> S := Semigroup( [ BooleanMat([[true, false], [true, true]]),
>  BooleanMat([[false, false], [false, true]]) ] );;
gap> Normalizer(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `Normalizer' on 1 arguments

#T# normalizer: SEMIGROUPS_DeterministicNormalizer, error, 2/4
gap> SEMIGROUPS_DeterministicNormalizer(Group((1,2)), S, rec());
Error, Semigroups: SEMIGROUPS_DeterministicNormalizer: usage,
the second arg must be a semigroup of transformations,
partial perms or bipartitions,

#T# normalizer: SEMIGROUPS_DeterministicNormalizer, error, 3/4
gap> SEMIGROUPS_DeterministicNormalizer(1, S, rec());
Error, Semigroups: SEMIGROUPS_DeterministicNormalizer: usage,
the first arg must be a permutation group,

#T# normalizer: SEMIGROUPS_DeterministicNormalizer, error, 4/4
gap> SEMIGROUPS_DeterministicNormalizer(Group((1,2)),
> Semigroup(IdentityTransformation), fail);
Error, Semigroups: SEMIGROUPS_DeterministicNormalizer: usage,
the third argument must be a record,

#T# normalizer: SEMIGROUPS_DeterministicNormalizer, works, 1/1
gap> S := Semigroup(FullTransformationMonoid(3), rec(generic := false));;
gap> SEMIGROUPS_DeterministicNormalizer(SymmetricGroup(3), S, rec());
Group([ (2,3), (1,2,3) ])
gap> SEMIGROUPS_DeterministicNormalizer(SymmetricGroup(3), S, rec(lambdastab := false));
Group([ (2,3), (1,2,3) ])
gap> SEMIGROUPS_DeterministicNormalizer(Group(()), S, rec(lambdastab := false));
Group(())

#T# normalizer: SEMIGROUPS_NonDeterministicNormalizer, error, 1/4
gap> S := Semigroup( [ BooleanMat([[true, false], [true, true]]),
>  BooleanMat([[false, false], [false, true]]) ] );;
gap> Normalizer(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `Normalizer' on 1 arguments

#T# normalizer: SEMIGROUPS_NonDeterministicNormalizer, error, 2/4
gap> SEMIGROUPS_NonDeterministicNormalizer(Group((1,2)), S, rec());
Error, Semigroups: SEMIGROUPS_NonDeterministicNormalizer: usage,
the second arg must be a semigroup of transformations,
partial perms or bipartitions,

#T# normalizer: SEMIGROUPS_NonDeterministicNormalizer, error, 3/4
gap> SEMIGROUPS_NonDeterministicNormalizer(1, S, rec());
Error, Semigroups: SEMIGROUPS_NonDeterministicNormalizer: usage,
the first arg must be a permutation group,

#T# normalizer: SEMIGROUPS_NonDeterministicNormalizer, error, 4/4
gap> SEMIGROUPS_NonDeterministicNormalizer(Group((1,2)),
> Semigroup(IdentityTransformation), fail);
Error, Semigroups: SEMIGROUPS_NonDeterministicNormalizer: usage,
the third arg must be a record,

#T# normalizer: SEMIGROUPS_NonDeterministicNormalizer, trans, 1/1
gap> S := Semigroup(FullTransformationMonoid(3), rec(generic := false));;
gap> SEMIGROUPS_NonDeterministicNormalizer(SymmetricGroup(3), S, rec());
Group([ (1,3,2), (1,3) ])
gap> SEMIGROUPS_NonDeterministicNormalizer(SymmetricGroup(3), S, rec(lambdastab := false));
Group([ (1,3,2), (1,3) ])
gap> SEMIGROUPS_NonDeterministicNormalizer(Group(()), S, rec(lambdastab := false));
Group(())

#T# normalizer: SEMIGROUPS_NonDeterministicNormalizer, pperm, 1/1
gap> S := Semigroup(SymmetricInverseMonoid(3), rec(generic := false));;
gap> SEMIGROUPS_NonDeterministicNormalizer(SymmetricGroup(3), S, rec());
Group([ (1,3,2), (1,3) ])
gap> SEMIGROUPS_NonDeterministicNormalizer(SymmetricGroup(3), S, rec(lambdastab := false));
Group([ (1,2,3), (1,2) ])
gap> SEMIGROUPS_NonDeterministicNormalizer(Group(()), S, rec(lambdastab := false));
Group(())
gap> SEMIGROUPS_NonDeterministicNormalizer(SymmetricGroup(3), S, rec(rhostab := true, 
> lambdastab := false));
Group([ (1,2,3), (1,2) ])
gap> S := Semigroup(PartialPerm([1]), PartialPerm([2]),
> PartialPerm([3]), rec(generic := false));;
gap> SEMIGROUPS_NonDeterministicNormalizer(SymmetricGroup(3), S, rec(rhostab :=
> true, lambdastab := false));
Group([ (2,3) ])

#T# normalizer: SEMIGROUPS_NonDeterministicNormalizer, pperm, 1/1
gap> S := Semigroup(PartitionMonoid(3), rec(generic := false));;
gap> SEMIGROUPS_NonDeterministicNormalizer(SymmetricGroup(3), S, rec());
Group([ (1,2,3), (2,3) ])
gap> SEMIGROUPS_NonDeterministicNormalizer(SymmetricGroup(3), S, rec(lambdastab := false));
Group([ (1,3,2), (1,3) ])
gap> SEMIGROUPS_NonDeterministicNormalizer(Group(()), S, rec(lambdastab := false));
Group(())

#T# SEMIGROUPS_UnbindVariables

#E#
gap> STOP_TEST( "Semigroups package: standard/attributes/normalizer.tst");
