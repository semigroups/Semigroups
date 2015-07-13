#############################################################################
##
#W  standard/attributes/properties.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/attributes/properties.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#T# properties: IsBand, for a semigroup, 1/2
gap> S := Semigroup(
> [ Bipartition( [ [ 1, 2, 7, -1 ], [ 3, 4, 5, -2, -3 ], [ 6, -5 ], [ -4 ],
>      [ -6, -7 ] ] ),
>   Bipartition( [ [ 1, 3, 4, 7 ], [ 2, 6 ], [ 5, -3, -4, -5 ], [ -1, -2, -6 ],
>      [ -7 ] ] ), Bipartition( [ [ 1, 4, 6 ], [ 2, 3, 5, 7, -1, -4 ],
>      [ -2, -5, -7 ], [ -3, -6 ] ] ),
>   Bipartition( [ [ 1, 6, -1, -3, -5, -6 ], [ 2, 3, 4, 7, -2 ],
>      [ 5, -4, -7 ] ] ),
>   Bipartition( [ [ 1, 4, 5, -2 ], [ 2, 7, -5 ], [ 3, 6, -7 ], [ -1 ],
>      [ -3, -4, -6 ] ] ),
>   Bipartition( [ [ 1, 5 ], [ 2, 4, -1, -3 ], [ 3, 6, 7, -4, -5 ], [ -2, -7 ],
>      [ -6 ] ] ) ] );;
gap> IsBand(S);
false

#T# properties: IsBand, for an ideal, 2/2
gap> S := Semigroup(
>  PBR([ [ ], [ -2, 2 ], [ -3, -1, 2 ] ],
>        [ [ -3, -2, 3 ], [ -2, 2 ], [ -3, -2, 2 ] ]),
>  PBR([ [ -2, -1, 1, 2, 3 ], [ -3, -1, 1, 3 ], [ -2, -1, 1 ] ],
>        [ [ -2, -1 ], [ -3, -1 ], [ -2, -1, 2 ] ]),
>  PBR([ [ -3, -2, -1, 1, 2, 3 ], [ -2, 1 ], [ -3, -2, 3 ] ],
>        [ [ -2, -1, 1, 2, 3 ], [ -3, -1, 1, 2, 3 ], [ -3, -2, 1, 3 ] ]),
>  PBR([ [ -2, 1, 2, 3 ], [ -3, -2, -1, 2, 3 ], [ -3, -1, 1, 2 ] ],
>        [ [ -1, 3 ], [ -2, -1, 2, 3 ], [ 3 ] ]),
>  PBR([ [ -3, -1, 1, 2 ], [ -2, -1, 2, 3 ], [ -1, 1 ] ],
>        [ [ 1, 3 ], [ -1, 1 ], [ -3, -2, 1, 2, 3 ] ]),
>  PBR([ [ -2, 1, 2 ], [ -3, -2, -1, 2 ], [ -3, -2, 2, 3 ] ],
>        [ [ -3, -2, 1, 2, 3 ], [ 1, 2, 3 ], [ -2, -1, 1 ] ]),
>  PBR([ [ -2, 1, 2 ], [ -3, 2 ], [ -3, -2, 1, 2 ] ],
>        [ [ 2, 3 ], [ -2, -1, 1, 2, 3 ], [ -3, -2, -1, 1 ] ]),
>  PBR([ [ -3, -1, 1, 3 ], [ -2 ], [ -3, 1, 3 ] ],
>        [ [ -3, -1, 1, 2, 3 ], [ -2, -1, 3 ], [ -1, 1, 2 ] ]),
>  PBR([ [ -2, 2, 3 ], [ -3, -1, 1, 2, 3 ], [ -3, 1, 2 ] ],
>        [ [ -3, -2, -1, 1, 3 ], [ -3, -1, 3 ], [ -3, -1, 1, 2 ] ]),
>  PBR([ [ -3, 2, 3 ], [ -3, -1, 2 ], [ -3, 3 ] ],
>        [ [ -2, -1, 3 ], [ -2, 1, 3 ], [ -2, 1, 3 ] ]),
>  PBR([ [ -2, -1 ], [ -2, 1, 2 ], [ -3, -1, 1 ] ],
>        [ [ -1, 1, 2, 3 ], [ -3, -1, 2, 3 ], [ -3, 2, 3 ] ]) );;
gap> IsBand(S);
false
gap> I := SemigroupIdeal(S, 
> PBR([ [ -3, -2, 2, 3 ], [ -3, -2, 1, 2, 3 ], [ -3, -2, 1, 2, 3 ] ],
>     [ [ -3, -2 ], [ -3, -2, -1 ], [ -3, -2, -1 ] ]));
<pbr semigroup ideal of degree 3 with 1 generator>
gap> IsBand(I);
true
gap> J := SemigroupIdeal(I, 
> PBR([ [ -3, -2, 2, 3 ], [ -3, -2, 1, 2, 3 ], [ -3, -2, 1, 2, 3 ] ],
>     [ [ -3, -2 ], [ -3, -2, -1 ], [ -3, -2, -1 ] ]));
<regular pbr semigroup ideal of degree 3 with 1 generator>
gap> IsBand(J);
true

#T# properties: IsBand, for an inverse semigroup, 1/1
gap> S := InverseSemigroup(
> [ Bipartition( [ [ 1, -6 ], [ 2, -2 ], [ 3, -1 ], [ 4, -4 ], [ 5 ], [ 6 ],
>      [ -3 ], [ -5 ] ] ), Bipartition( [ [ 1, -1 ], [ 2, -6 ], [ 3, -3 ],
>      [ 4, -2 ], [ 5, -4 ], [ 6 ], [ -5 ] ] ),
>   Bipartition( [ [ 1, -1 ], [ 2, -6 ], [ 3, -5 ], [ 4 ], [ 5, -2 ], [ 6 ],
>      [ -3 ], [ -4 ] ] ), Bipartition( [ [ 1, -1 ], [ 2, -3 ], [ 3, -5 ],
>      [ 4 ], [ 5 ], [ 6, -6 ], [ -2 ], [ -4 ] ] ),
>   Bipartition( [ [ 1, -3 ], [ 2 ], [ 3 ], [ 4, -6 ], [ 5, -4 ], [ 6, -5 ],
>      [ -1 ], [ -2 ] ] ),
>   Bipartition( [ [ 1, -3 ], [ 2, -5 ], [ 3 ], [ 4, -4 ], [ 5, -1 ],
>      [ 6, -2 ], [ -6 ] ] ) ] );;
gap> IsBand(S);
false
gap> IsBand(IdempotentGeneratedSubsemigroup(S));
true

#T# properties: IsBlockGroup, 1/?
gap> S := Semigroup( [ Transformation( [ 6, 2, 8, 8, 7, 8, 4, 8 ] ),
>   Transformation( [ 6, 7, 4, 2, 8, 1, 5, 8 ] ) ] );
<transformation semigroup of degree 8 with 2 generators>
gap> IsBlockGroup(S);
true
gap> I := SemigroupIdeal(S, Transformation( [ 1, 8, 8, 8, 8, 8, 5, 8 ] ));;
gap> IsBlockGroup(I);
true

#T# properties: IsBlockGroup, 2/?
gap> S := JonesMonoid(3);
<regular bipartition monoid of degree 3 with 2 generators>
gap> IsInverseSemigroup(S);
false
gap> IsBlockGroup(S);
false

#T# properties: IsBlockGroup, 3/?
gap> S := Semigroup( [ BooleanMat([[true, true], [true, true]]),
>  BooleanMat([[false, false], [false, true]]) ] );
<semigroup of 2x2 boolean matrices with 2 generators>
gap> IsBlockGroup(S);
false

#T# properties: IsBlockGroup, 4/?
gap> S := Semigroup(
>  TropicalMaxPlusMatrixNC([[0, 1, 1, 2, 0, 3, 0, -infinity],
>      [5, 1, 1, 2, -infinity, -infinity, 0, 2],
>      [0, 3, -infinity, 1, -infinity, -infinity, 2, 1],
>      [-infinity, -infinity, -infinity, 0, -infinity, 3, 1, 1],
>      [1, 2, 1, 2, 1, 1, 2, 1], [1, 2, 5, 2, -infinity, 2, 2, 1],
>      [1, 0, -infinity, 2, -infinity, 0, 0, 2],
>      [2, -infinity, 5, 2, 4, 1, 3, 3]], 6),
>  TropicalMaxPlusMatrixNC([[1, 3, 0, 4, -infinity, 2, 1, -infinity],
>      [2, 5, 2, 5, -infinity, 0, 1, -infinity],
>      [2, -infinity, -infinity, 1, 3, 2, 2, 1],
>      [1, 4, 1, 3, -infinity, 1, 1, 3],
>      [3, 0, 1, 1, 1, 2, -infinity, -infinity],
>      [1, 1, -infinity, 1, 2, 0, 1, 2], [3, 0, 1, 1, 1, 1, 1, 2],
>      [0, -infinity, 0, 3, 1, 1, 2, 1]], 6),
>  TropicalMaxPlusMatrixNC([[1, 4, -infinity, 2, 1, 3, 2, 1],
>      [-infinity, 1, 2, 0, 1, 1, 2, 1],
>      [1, 2, 0, -infinity, 0, 1, -infinity, -infinity],
>      [-infinity, 3, 1, -infinity, 2, 0, 2, 1],
>      [1, -infinity, 2, 2, -infinity, 5, 2, 0],
>      [-infinity, 0, -infinity, 0, -infinity, 1, 1, -infinity],
>      [-infinity, 1, 0, 1, 3, 2, 1, 1], [3, 2, -infinity, 0, 2, 2, 2, 1]], 6),
>  TropicalMaxPlusMatrixNC([[2, 3, 3, 0, -infinity, 1, 1, 2],
>      [-infinity, 2, 0, -infinity, -infinity, 0, -infinity, -infinity],
>      [3, 0, 4, -infinity, -infinity, -infinity, -infinity, 6],
>      [1, 0, 0, -infinity, 0, 2, 1, 3], [2, 3, 5, 2, 3, 0, -infinity, 0],
>      [0, -infinity, 2, -infinity, 0, 1, 2, -infinity],
>      [0, 0, 1, 0, -infinity, 2, 2, 4], [0, 3, 1, -infinity, 3, 1, 1, 1]], 6),
>  TropicalMaxPlusMatrixNC([[3, -infinity, -infinity, 1, 3, 1, 2, 1],
>      [4, 1, 1, 2, -infinity, 3, 1, 5], [2, 3, 1, 2, 3, 2, 1, 1],
>      [2, 2, -infinity, 3, 3, 3, 1, 0], [-infinity, 2, 1, 1, 2, 1, 1, 0],
>      [2, 1, -infinity, 2, 2, 1, 1, -infinity],
>      [1, 3, 2, 0, -infinity, 2, 4, 1],
>      [1, 2, -infinity, 1, -infinity, -infinity, -infinity, -infinity]], 6),
>  TropicalMaxPlusMatrixNC([[3, 0, 1, 1, 0, 0, 2, 3],
>      [4, 1, 0, 0, 3, 1, 2, 2], [2, -infinity, 0, 2, -infinity, 1, 2, 3],
>      [3, 0, 2, 1, -infinity, -infinity, 3, -infinity],
>      [2, 0, -infinity, 1, -infinity, 3, -infinity, -infinity],
>      [3, 1, 2, 2, 1, -infinity, 1, 0], [2, 3, -infinity, 1, 3, 1, 2, 0],
>      [2, 2, 1, 0, 3, 1, 1, 1]], 6),
>  TropicalMaxPlusMatrixNC([[3, 0, 2, -infinity, 0, 2, 0, -infinity],
>      [2, 4, 0, 1, -infinity, 1, 2, 1],
>      [-infinity, 3, 0, 3, -infinity, 3, -infinity, 2],
>      [4, -infinity, 2, -infinity, -infinity, 1, 1, 3],
>      [-infinity, 3, 0, 6, 2, 0, 0, 1], [-infinity, 2, 2, 1, 2, 0, 0, 0],
>      [5, 3, 2, 0, -infinity, 0, 5, 1],
>      [-infinity, 1, 4, 4, 2, -infinity, 4, 3]], 6),
>  TropicalMaxPlusMatrixNC([[3, 3, 1, 0, 2, 4, 1, -infinity],
>      [3, 0, 0, 0, 0, 5, -infinity, 2],
>      [-infinity, -infinity, 1, 4, 4, 4, 1, 2], [0, 1, 1, 0, 1, 2, 0, 0],
>      [3, 0, 1, 5, -infinity, 0, 1, 2],
>      [-infinity, 0, -infinity, 2, 1, 3, 1, 0],
>      [4, 2, 2, -infinity, 1, 2, -infinity, 2],
>      [1, -infinity, 2, 4, 1, 0, 4, 1]], 6) );
<semigroup of 8x8 tropical max-plus matrices with 8 generators>
gap> IsBlockGroup(S);
true

#T# properties: IsBrandtSemigroup, 1/1
gap> S := Semigroup( [ Transformation( [ 2, 1, 5, 5, 5 ] ),
> Transformation( [ 4, 5, 3, 1, 5 ] ) ] );
<transformation semigroup of degree 5 with 2 generators>
gap> IsBrandtSemigroup(S);
false
gap> S := Semigroup(S);;
gap> x := Transformation( [ 5, 5, 5, 4, 5 ] );;
gap> I := SemigroupIdeal(S, x);;
gap> IsBrandtSemigroup(I);
true

#T# properties: IsZeroSimpleSemigroup, bug, 1/1
gap> IsZeroSimpleSemigroup(ZeroSemigroup(2));
false

#T# properties: IsCongruenceFreeSemigroup, trivial, 1/6
gap> IsCongruenceFreeSemigroup(TrivialSemigroup());
true

#T# properties: IsCongruenceFreeSemigroup, group, 2/6
gap> S := AsTransformationSemigroup(AlternatingGroup(5));
<transformation semigroup of size 60, degree 5 with 2 generators>
gap> IsCongruenceFreeSemigroup(S);
true

#T# properties: IsCongruenceFreeSemigroup, group, 3/6
gap> IsCongruenceFreeSemigroup(AlternatingGroup(5));
true

#T# properties: IsCongruenceFreeSemigroup, 4/6
gap> S := FullTransformationMonoid(3);;
gap> D := PrincipalFactor(DClass(S, S.3));
<Rees 0-matrix semigroup 3x3 over Group([ (1,2) ])>
gap> IsCongruenceFreeSemigroup(D);
false

#T# properties: IsCongruenceFreeSemigroup, 5/6
gap> R := ReesZeroMatrixSemigroup( Group( [ () ] ),
> [ [ (), (), 0 ], [ (), 0, () ], [ 0, (), () ] ] );;
gap> IsCongruenceFreeSemigroup(R);
true

#T# properties: IsCongruenceFreeSemigroup, 6/6
gap> R := ReesZeroMatrixSemigroup( Group( [ () ] ),
> [ [ (), (), 0 ], [ (), (), 0 ], [ 0, (), () ] ] );;
gap> IsCongruenceFreeSemigroup(R);
false

#T# properties: IsCliffordSemigroup, ideal, 1/?
gap> I := SemigroupIdeal( Semigroup(
>    [ Transformation( [ 1, 4, 3, 2 ] ), Transformation( [ 2, 1, 4, 3 ] ), Tran\
> sformation( [ 3, 2, 1, 3 ] ), Transformation( [ 3, 3, 1 ] ), Transformation( [\
> 4, 4, 4, 3 ] ) ] ), [ Transformation( [ 4, 3, 4, 4 ] ) ] );;
gap> IsCliffordSemigroup(I);
false

#T# properties: IsCliffordSemigroup, parent, 2/?
gap> S := IdempotentGeneratedSubsemigroup(SymmetricInverseMonoid(3));;
gap> IsCliffordSemigroup(S);
true
gap> I := SemigroupIdeal(S, PartialPerm( [1, 2, 3], [1, 2, 3]));
<inverse partial perm semigroup ideal of rank 3 with 1 generator>
gap> IsCliffordSemigroup(I);
true
gap> I := SemigroupIdeal(S, PartialPerm( [1, 2, 3], [1, 2, 3]));
<inverse partial perm semigroup ideal of rank 3 with 1 generator>
gap> GeneratorsOfSemigroup(I);;
gap> IsCliffordSemigroup(I);
true

#T# properties: IsCliffordSemigroup, non-inverse, 3/?
gap> S := ZeroSemigroup(2);;
gap> IsInverseSemigroup(S);
false
gap> IsCliffordSemigroup(S);
false

#T# properties: IsCliffordSemigroup, non-completely regular, 4/?
gap> S := ZeroSemigroup(2);;
gap> IsCompletelyRegularSemigroup(S);
false
gap> IsCliffordSemigroup(S);
false

#T# properties: IsCliffordSemigroup, group, 5/?
gap> S := AsPartialPermSemigroup(Group((1,2,3)));
<commutative inverse partial perm semigroup of rank 3 with 1 generator>
gap> IsCliffordSemigroup(S);
true

#T# properties: IsCliffordSemigroup, non-regular, 6/?
gap> S := ZeroSemigroup(2);;
gap> IsCliffordSemigroup(S);
false

#T# SEMIGROUPS_UnbindVariables

#E#
gap> STOP_TEST( "Semigroups package: standard/attributes/properties.tst");
