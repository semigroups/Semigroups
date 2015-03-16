#############################################################################
##
#W  matrix.tst
#Y  Copyright (C) 2015                                    Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: matrix.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#
gap> M := NewMatrix(IsPlistMatrixRep, GF(2), 16,
> [[ 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0 ],
> [ 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0 ],
> [ Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0 ],
> [ 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2) ],
> [ 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0 ],
> [ 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2) ],
> [ 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2) ],
> [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2) ],
> [ Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0 ],
> [ 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0 ],
> [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2) ],
> [ 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0 ],
> [ Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0 ],
> [ 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0 ],
> [ 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2) ],
> [ Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0 ]
> ]);;
gap> S := Semigroup(M);
<semigroup of 16x16 matrices over GF(2) with 1 generator>
gap> Size(S);
7161

#
gap> S := Semigroup(
> [ NewMatrix(IsPlistMatrixRep,GF(3),5,
> [ [ Z(3), Z(3), Z(3)^0, Z(3), Z(3)^0 ],
>   [ 0*Z(3), 0*Z(3), Z(3), 0*Z(3), Z(3)^0 ],
>   [ Z(3), Z(3), Z(3)^0, 0*Z(3), Z(3) ],
>   [ Z(3)^0, Z(3)^0, Z(3), Z(3), 0*Z(3) ],
>   [ Z(3), Z(3)^0, Z(3), Z(3)^0, 0*Z(3) ] ]),
> NewMatrix(IsPlistMatrixRep,GF(3),5,
> [ [ 0*Z(3), Z(3)^0, 0*Z(3), Z(3), Z(3) ],
>   [ Z(3), 0*Z(3), Z(3)^0, 0*Z(3), Z(3)^0 ],
>   [ Z(3), Z(3), Z(3)^0, Z(3), Z(3) ],
>   [ Z(3), Z(3), 0*Z(3), Z(3), Z(3)^0 ],
>   [ Z(3), Z(3), Z(3)^0, Z(3)^0, Z(3) ] ]) ]);;
gap> Size(S);
170080803
gap> DClasses(S);
[ {NewMatrix(IsPlistMatrixRep,GF(3),5,[ [ Z(3), Z(3), Z(3)^0, Z(3), Z(3)^0 ], 
      [ 0*Z(3), 0*Z(3), Z(3), 0*Z(3), Z(3)^0 ], 
      [ Z(3), Z(3), Z(3)^0, 0*Z(3), Z(3) ], 
      [ Z(3)^0, Z(3)^0, Z(3), Z(3), 0*Z(3) ], 
      [ Z(3), Z(3)^0, Z(3), Z(3)^0, 0*Z(3) ] ])}, 
  {NewMatrix(IsPlistMatrixRep,GF(3),5,
    [ [ 0*Z(3), Z(3), Z(3), 0*Z(3), 0*Z(3) ], 
      [ Z(3), 0*Z(3), Z(3)^0, Z(3), Z(3)^0 ], 
      [ Z(3), 0*Z(3), 0*Z(3), Z(3)^0, 0*Z(3) ], 
      [ 0*Z(3), Z(3), Z(3), 0*Z(3), 0*Z(3) ], 
      [ Z(3), Z(3)^0, Z(3), Z(3), Z(3)^0 ] ])}, 
  {NewMatrix(IsPlistMatrixRep,GF(3),5,
    [ [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
      [ Z(3), Z(3)^0, Z(3)^0, Z(3)^0, 0*Z(3) ], 
      [ 0*Z(3), 0*Z(3), Z(3), Z(3), Z(3) ], 
      [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
      [ Z(3), Z(3)^0, Z(3)^0, Z(3)^0, 0*Z(3) ] ])}, 
  {NewMatrix(IsPlistMatrixRep,GF(3),5,
    [ [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
      [ 0*Z(3), 0*Z(3), Z(3)^0, Z(3)^0, Z(3)^0 ], 
      [ 0*Z(3), 0*Z(3), Z(3)^0, Z(3)^0, Z(3)^0 ], 
      [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
      [ 0*Z(3), 0*Z(3), Z(3)^0, Z(3)^0, Z(3)^0 ] ])}, 
  {NewMatrix(IsPlistMatrixRep,GF(3),5,
    [ [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
      [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
      [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
      [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
      [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ] ])} ]

#
gap> S := Semigroup(
> [ NewMatrix(IsPlistMatrixRep,GF(3),3,
>    [ [ Z(3), Z(3), Z(3)^0 ], [ 0*Z(3), Z(3), Z(3) ], 
>      [ Z(3), 0*Z(3), Z(3)^0 ] ]), NewMatrix(IsPlistMatrixRep,GF(3),3,
>    [ [ Z(3), Z(3), 0*Z(3) ], [ Z(3)^0, Z(3)^0, 0*Z(3) ], 
>      [ Z(3)^0, Z(3)^0, 0*Z(3) ] ]) ]);;
gap> Size(S);
137
gap> NrIdempotents(S);
42
gap> MinimalIdeal(S);
<ideal of semigroup of 3x3 matrices over GF(3) with 1 generator>
gap> Size(last);
1
gap> MultiplicativeZero(S);
<immutable 3x3-matrix over GF(3)>
gap> MinimalDClass(S);
<Green's D-class: <immutable 3x3-matrix over GF(3)>>
gap> MaximalSubsemigroups(S);
[ <semigroup of 3x3 matrices over GF(3) with 4 generators>, 
  <semigroup of 3x3 matrices over GF(3) with 2 generators> ]
gap> time;
4281
gap> List(last2, U-> IsMaximalSubsemigroup(S, U));
[ true, true ]

#
gap> STOP_TEST("Semigroups package: matrix.tst");
