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
Transformation( [ 6, 8, 2, 7, 4, 5, 5, 10, 10, 10 ] )
gap> CanonicalTransformation(CanonicalTransformation(f));
Transformation( [ 6, 8, 2, 7, 4, 5, 5, 10, 10, 10 ] )
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

# OnPosIntSetsTrans: for a transformation
gap> OnPosIntSetsTrans([], Transformation([1, 1]), 0);
[  ]
gap> OnPosIntSetsTrans([1, 2], Transformation([1, 1]), 0);
[ 1 ]
gap> OnPosIntSetsTrans([1, 2, 10], Transformation([1, 1]), 0);
[ 1, 10 ]
gap> OnPosIntSetsTrans([1, 2, 10], Transformation([65535], [65537]), 0);
[ 1, 2, 10 ]
gap> OnPosIntSetsTrans([1, 65535, 65538], Transformation([65535], [65537]), 0);
[ 1, 65537, 65538 ]
gap> OnPosIntSetsTrans([1, 2, 10, 65537], Transformation([65537], [1]), 0);
[ 1, 2, 10 ]
gap> OnPosIntSetsTrans([1, 2, 10, 65535], Transformation([65535], [5]), 0);
[ 1, 2, 5, 10 ]
gap> OnPosIntSetsTrans([1 .. 20], 
>                      Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9]), 0);
[ 1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 ]
gap> OnPosIntSetsTrans([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 19, 324, 4124, 123124],
>                      Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9]), 0);
[ 1, 5, 7, 8, 9, 10, 11, 19, 324, 4124, 123124 ]
gap> OnPosIntSetsTrans([], Transformation([1, 1]), 10);
[  ]
gap> OnPosIntSetsTrans([1, 2], Transformation([1, 1]), 10);
[ 1 ]
gap> OnPosIntSetsTrans([1, 2, 10], Transformation([1, 1]), 10);
[ 1, 10 ]
gap> OnPosIntSetsTrans([1, 2, 10], Transformation([65535], [65537]), 10);
[ 1, 2, 10 ]
gap> OnPosIntSetsTrans([1, 65535, 65538], Transformation([65535], [65537]), 10);
[ 1, 65537, 65538 ]
gap> OnPosIntSetsTrans([1, 2, 10, 65537], Transformation([65537], [1]), 12);
[ 1, 2, 10 ]
gap> OnPosIntSetsTrans([1, 2, 10, 65535], Transformation([65535], [5]), 130);
[ 1, 2, 5, 10 ]
gap> OnPosIntSetsTrans([1 .. 20], 
>                      Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9]), 10);
[ 1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 ]
gap> OnPosIntSetsTrans([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 19, 324, 4124, 123124],
>                      Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9]), 10);
[ 1, 5, 7, 8, 9, 10, 11, 19, 324, 4124, 123124 ]
gap> OnPosIntSetsTrans([0],
>           Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9]), 0);
[  ]
gap> OnPosIntSetsTrans([0],
>           Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9]), 10);
[ 1, 5, 7, 8, 9, 10 ]
gap> OnPosIntSetsTrans([0],
>           Transformation([10, 7, 10, 8, 8, 7, 5, 9, 1, 9]), 20);
[ 1, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 ]
gap> OnPosIntSetsTrans([1], "a", 20);
Error, OnPosIntSetsTrans: <f> must be a transformation (not a list (string))
gap> OnPosIntSetsTrans([0], "a", 20);
Error, OnPosIntSetsTrans: <f> must be a transformation (not a list (string))
gap> OnPosIntSetsTrans(1, "a", 20);
Error, OnPosIntSetsTrans: <f> must be a transformation (not a list (string))

# SEMIGROUPS_UnbindVariables
gap> Unbind(M);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/trans.tst");
