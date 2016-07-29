#############################################################################
##
#W  standard/congfinite.tst
#Y  Copyright (C) 2016                                      Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/congfinite.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# Test multithreading in TC
gap> S := DirectProduct(FullTransformationMonoid(6), Semigroup(Transformation([2, 1])));
<transformation semigroup of degree 8 with 4 generators>
gap> pairs := [[Transformation([1, 1, 1, 1, 1, 1, 8, 7]), Transformation([1, 1,
> 1, 1, 1, 1])],
> [Transformation([1, 1, 1, 1, 1, 1]), Transformation([2, 2, 2, 2, 2, 2])],
> [Transformation([1, 1, 1, 1, 1, 1]), Transformation([3, 3, 3, 3, 3, 3])],
> [Transformation([1, 1, 1, 1, 1, 1]), Transformation([4, 4, 4, 4, 4, 4])],
> [Transformation([1, 1, 1, 1, 1, 1]), Transformation([5, 5, 5, 5, 5, 5])],
> [Transformation([1, 1, 1, 1, 1, 1]), Transformation([6, 6, 6, 6, 6, 6])],
> [Transformation([1, 1, 1, 1, 1, 1, 8, 7]), Transformation([1, 2, 3, 4, 5, 6,
> 8, 7])]];;
gap> cong2 := RightSemigroupCongruence(S, pairs);;
gap> NrEquivalenceClasses(cong2);
1

#T# Test multithreading in TC
gap> S := DirectProduct(FullTransformationMonoid(6),
>                       Semigroup(Transformation([2, 1])));;
gap> pair1 := [Transformation([1, 2, 3, 4, 5, 6, 8, 7]),
>              IdentityTransformation];;
gap> cong1 := RightSemigroupCongruence(S, pair1);;
gap> NrEquivalenceClasses(cong1);
46656

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(cong1);
gap> Unbind(cong2);
gap> Unbind(pair1);
gap> Unbind(pairs);

#E#
gap> STOP_TEST("Semigroups package: standard/congfinite.tst");
