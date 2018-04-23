#############################################################################
##
#W  standard/trans.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/trans.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# Test CanonicalTransformation
gap> f := Transformation([10, 8, 4, 6, 4, 5, 3, 8, 8, 2]);;
gap> CanonicalTransformation(f);
Transformation( [ 10, 9, 10, 1, 2, 4, 9, 7, 8, 10 ] )
gap> CanonicalTransformation(CanonicalTransformation(f));
Transformation( [ 10, 9, 10, 1, 2, 4, 9, 7, 8, 10 ] )
gap> Number(Set(FullTransformationMonoid(5),
>               x -> CanonicalTransformation(x, 5)));
47
gap> CanonicalTransformation(IdentityTransformation);
IdentityTransformation
gap> CanonicalTransformation(IdentityTransformation, 2);
IdentityTransformation
gap> CanonicalTransformation(Transformation([1, 3, 2]), 2);
Error, Semigroups: CanonicalTransformation: usage,
the second argument (an integer) must be at least the degree of the first argu\
ment (a transformation),

# Test TransformationByImageAndKernel
gap> TransformationByImageAndKernel([1 .. 4],
>                                   [[1, 2, 3], [4, 5], [6, 8], [7]]);
Transformation( [ 1, 1, 1, 2, 2, 3, 4, 3 ] )
gap> TransformationByImageAndKernel([1 .. 4], [[1, 2], [4, 5], [6, 8], [7]]);
Error, Semigroups: TransformationByImageAndKernel: usage,
the union of the second argument (a partition) must be [1 .. 7],
gap> TransformationByImageAndKernel([1 .. 4],
> [[1, 2], [4, 5], [6, -1], [7]]);
Error, Semigroups: TransformationByImageAndKernel: usage,
the argument must be a list of lists of pos ints,
gap> TransformationByImageAndKernel([1 .. 4],
> [[1, 2], [4, 5], "a", [7]]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `TransformationByImageAndKernel' on 2 ar\
guments
gap> TransformationByImageAndKernel([1 .. 4], "a");
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `TransformationByImageAndKernel' on 2 ar\
guments
gap> TransformationByImageAndKernel([1 .. 4], [[1 .. 4]]);
fail

# SEMIGROUPS_UnbindVariables
gap> Unbind(M);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/trans.tst");
