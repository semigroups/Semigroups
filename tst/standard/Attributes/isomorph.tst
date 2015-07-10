#%T##########################################################################
##
#W  standard/attributes/isomorph.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#
gap> START_TEST("Semigroups package: standard/attributes/isomorph.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

# isomorph: SmallestMultiplicationTable, 1/2
gap> S := DualSymmetricInverseMonoid(2);
<inverse bipartition monoid of degree 2 with 2 generators>
gap> Size(S);
3
gap> SmallestMultiplicationTable(S);
[ [ 1, 2, 3 ], [ 2, 1, 3 ], [ 3, 3, 3 ] ]

# isomorph: SmallestMultiplicationTable, 2/2
gap> S := Semigroup(
> [ PBR([ [ -4, 1, 2, 3 ], [ -4, 1, 2, 4 ], [ -2, -1, 1 ], [ -4, -1, 1, 2, 4 ] ],
>        [ [ -4, -3, 1, 4 ], [ -3, -1, 3 ], [ -4, -1, 1, 2, 4 ], [ -4, -3, -2, 3, 4 ] ]),
>  PBR([ [ -3, -2, -1, 1, 3, 4 ], [ -4, -3, -2, -1, 2, 3 ], [ -3, -2, -1, 1 ], 
>        [ -1, 1, 2 , 3 ] ], [ [ -3, -2, -1, 2, 3, 4 ], [ -3, 1, 4 ], 
>        [ -3, 1, 2 ], [ -4, -3, 1, 2, 3, 4 ] ]),
>  PBR([ [ -3, -1, 1, 3 ], [ -1, 1, 2 ], [ -2, -1, 1 ], [ -4, -3, -1, 1, 2, 3, 4 ] ],
>        [ [ -4, -3, -2 ], [ ], [ -4, -1, 1, 2 ], [ -4, -3, -2, -1, 2, 3, 4 ] ]),
>  PBR( [ [ -3, -2, -1, 2, 3 ], [ -2, -1, 2, 4 ], [ -3, -2, 1, 3 ], 
>         [ -4, -3, -2, -1, 1, 2 , 3, 4 ] ], 
>       [ [ -4, -2, -1, 4 ], [ -4, -3, -2, 2, 3 ], [ -3, -2, -1, 1, 3, 4 ], [ -3, 2 ] ]),
>  PBR( [ [ -4, -3, -2, -1, 2, 3 ], [ -4, -1, 1, 2, 3, 4 ], [ -3, 1, 2, 3 ], 
>         [ -4, -3, -2, -1, 1, 4 ] ],
>        [ [ -4, -3, -1, 2, 4 ], [ -3, -2, 2, 3, 4 ], [ -4, -2, -1, 1 ], 
>          [ -4, -2, 1, 4 ] ]),
>  PBR([ [ -4, -2, -1, 2, 3 ], [ -4, -3, -1, 1, 3 ], [ -4, 2, 4 ], [ -3, -1, 1 ] ],
>        [ [ -4, -3, -1, 1, 3 ], [ -4, -3, 2, 3 ], [ -4, -3, -2, -1, 2, 4 ], 
>          [ -4, -1, 1 , 3, 4 ] ]) ] );
<pbr semigroup of degree 4 with 6 generators>
gap> Size(S);
11
gap> SmallestMultiplicationTable(S);
[ [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], 
  [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], 
  [ 1, 1, 1, 1, 1, 6, 6, 7, 9, 9, 11 ], [ 1, 1, 1, 11, 1, 6, 6, 6, 9, 9, 11 ],
  [ 1, 1, 1, 11, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 11, 1, 6, 6, 6, 9, 9, 11 ]
    , [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], 
  [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ], [ 1, 1, 1, 1, 1, 6, 6, 6, 9, 9, 11 ] ]

# isomorph: IsIsomorphicSemigroup, 1/2
gap> S := DualSymmetricInverseMonoid(2);;
gap> T := Semigroup( [ Transformation( [ 2, 1, 2, 2 ] ), 
>                      Transformation( [ 1, 2, 3, 3 ] ) ] );;
gap> IsIsomorphicSemigroup(S, T);
false

# isomorph: IsIsomorphicSemigroup, 2/2
gap> S := Semigroup( [ NaturalMatrixNC([[0, 1, 2], [4, 3, 0], [0, 2, 0]], 9, 4),
>  NaturalMatrixNC([[1, 1, 0], [4, 1, 1], [0, 0, 0]], 9, 4) ] );
<semigroup of 3x3 natural matrices with 2 generators>
gap> IsIsomorphicSemigroup(S, S);
true
gap> T := AsTransformationSemigroup(S);
<transformation semigroup of degree 47 with 2 generators>
gap> IsIsomorphicSemigroup(S, T);
Error, Semigroups: IsIsomorphicSemigroup:
not yet implemented,
gap> S := Semigroup(IdentityTransformation);
<trivial transformation group of degree 0 with 0 generators>
gap> T := Semigroup(PartialPerm([]));
<trivial partial perm group of rank 0 with 0 generators>
gap> IsIsomorphicSemigroup(S, T);
true
gap> T := JonesMonoid(4);
<regular bipartition monoid of degree 4 with 3 generators>
gap> IsIsomorphicSemigroup(S, T);
false

#T# SEMIGROUPS_UnbindVariables

#E#
gap> STOP_TEST("Semigroups package: standard/attributes/isomorph.tst");
