############################################################################
##
#W  extreme/attributes.tst
#Y  Copyright (C) 2016                                    Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: extreme/attributes.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

#T# SmallerDegreeTransformationRepresentation: Partial perm semigroup
gap> S := Semigroup( [ PartialPerm( [ 1, 2 ], [ 2, 1 ] ),
>                      PartialPerm( [ 1, 2 ], [ 3, 1 ] ) ] );;
gap> iso := SmallerDegreeTransformationRepresentation(S);;
gap> T := Range(iso);
<transformation semigroup of degree 4 with 2 generators>
gap> IsIsomorphicSemigroup(S, T);
true

#T# SmallerDegreeTransformationRepresentation: PBR semigroup
gap> S := Semigroup(
> [ PBR([ [ -3, -2, -1, 1, 2, 3 ], [ -3, 1, 3 ], [ -3, 2 ] ],
>       [ [ -3, -2, 1, 2, 3 ], [ -3, -2, -1, 2, 3 ], [ -2, -1, 1, 3 ] ]),
>   PBR([ [ -3, 2 ], [ 3 ], [ -3, -2, -1 ] ],
>       [ [ -3 ], [ -3, -2 ], [ -3, -2, 1, 2 ] ]) ] );;
gap> iso := SmallerDegreeTransformationRepresentation(S);;
gap> T := Range(iso);
<transformation semigroup of degree 7 with 2 generators>
gap> IsIsomorphicSemigroup(S, T);
true

#T# SmallerDegreeTransformationRepresentation: Partition monoid
gap> p := PartitionMonoid(2);;
gap> iso := SmallerDegreeTransformationRepresentation(p);;
gap> q := Range(iso);
<transformation monoid of degree 7 with 3 generators>
gap> IsIsomorphicSemigroup(p, q);
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(iso);
gap> Unbind(p);
gap> Unbind(q);

#E#
gap> STOP_TEST("Semigroups package: extreme/attributes.tst");
