#############################################################################
##
#W  semimat.tst
#Y  Copyright (C) 2015                                    Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: semimat.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#T# MatrixSemigroupTest1: Create and Size
gap> M := NewSMatrix(IsPlistSMatrixRep, GF(2), 16,
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
<semigroup of 16x16 s-matrices over GF(2) with 1 generator>
gap> Size(S);
7161
gap> NrDClasses(S);
1
gap> PartialOrderOfDClasses(S);
[ [ 1 ] ]
gap> StructureDescriptionSchutzenbergerGroups(S);
[ "C7161" ]
gap> T := AsTransformationSemigroup(S);
<commutative transformation semigroup on 65536 pts with 1 generator>
gap> Size(T);
7161
gap> Size(S) = Size(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true

#T# MatrixSemigroupTest2: Create and Size
gap> S := Semigroup(
> [ NewSMatrix(IsPlistSMatrixRep,GF(3),5,
> [ [ Z(3), Z(3), Z(3)^0, Z(3), Z(3)^0 ],
>   [ 0*Z(3), 0*Z(3), Z(3), 0*Z(3), Z(3)^0 ],
>   [ Z(3), Z(3), Z(3)^0, 0*Z(3), Z(3) ],
>   [ Z(3)^0, Z(3)^0, Z(3), Z(3), 0*Z(3) ],
>   [ Z(3), Z(3)^0, Z(3), Z(3)^0, 0*Z(3) ] ]),
> NewSMatrix(IsPlistSMatrixRep,GF(3),5,
> [ [ 0*Z(3), Z(3)^0, 0*Z(3), Z(3), Z(3) ],
>   [ Z(3), 0*Z(3), Z(3)^0, 0*Z(3), Z(3)^0 ],
>   [ Z(3), Z(3), Z(3)^0, Z(3), Z(3) ],
>   [ Z(3), Z(3), 0*Z(3), Z(3), Z(3)^0 ],
>   [ Z(3), Z(3), Z(3)^0, Z(3)^0, Z(3) ] ]) ]);;
gap> Size(S);
170080803
gap> NrIdempotents(S);
43844
gap> PartialOrderOfDClasses(S);
[ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ], [ 5 ] ]
gap> S := Semigroup(GeneratorsOfSemigroup(S));
<semigroup of 5x5 s-matrices over GF(3) with 2 generators>
gap> PartialOrderOfDClasses(S);
[ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ], [ 5 ] ]

#T# MatrixSemigroupTest3: Create, Size, MinimalIdeal
gap> S := Semigroup(
> [ NewSMatrix(IsPlistSMatrixRep,GF(3),3,
>    [ [ Z(3), Z(3), Z(3)^0 ],
>      [ 0*Z(3), Z(3), Z(3) ], 
>      [ Z(3), 0*Z(3), Z(3)^0 ] ]),
>   NewSMatrix(IsPlistSMatrixRep,GF(3),3,
>    [ [ Z(3), Z(3), 0*Z(3) ],
>      [ Z(3)^0, Z(3)^0, 0*Z(3) ], 
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
<s-matrix of degree 3 over GF(3)>
gap> MinimalDClass(S);
<Green's D-class: <s-matrix of degree 3 over GF(3)>>
gap> MaximalSubsemigroups(S);
[ <semigroup of 3x3 matrices over GF(3) with 4 generators>, 
  <semigroup of 3x3 matrices over GF(3) with 2 generators> ]
gap> time;
4281
gap> List(last2, U-> IsMaximalSubsemigroup(S, U));
[ true, true ]

#T# MatrixSemigroups3: Upper triangular matrices, SubsemigroupByProperty
# This fails and gets a semigroup that is too small!
upper := function(mat)
>   local zero, n, i, j;
>   zero := Zero(BaseDomain(mat));
>   n := DegreeOfSMatrix(mat);
>   for i in [2 .. n] do 
>     for j in [1 .. i - 1] do 
>       if mat!.mat[i][j] <> zero then 
>         return false;
>       fi;
>     od;
>   od;
>   return true;
> end;;
gap> S := GeneralLinearSemigroup(3,3);;
gap> T := SubsemigroupByProperty(S, upper);
<monoid of 3x3 s-matrices over GF(3) with 12 generators>
gap> Size(T);
729

#T# MatrixSemigroups4: ClosureSemigroup
# This *also* fails
gap> elms := Filtered(Elements(GLS(3,3)), upper);;
gap> S := Semigroup(elms[1]);;
gap> for i in [2..Length(elms)] do
  S := ClosureSemigroup(S, elms[i]);
od;
gap> S;
<monoid of 3x3 s-matrices over GF(3) with 53 generators>
gap> Size(S);
729

#T# MatrixSemigroups5:  
gap> func := IsGreensDLeq(S);
function( x, y ) ... end
gap> x := Random(S);
<3x3-matrix over GF(3)>
gap> y := Random(S);
<3x3-matrix over GF(3)>
gap> func(x, y);        
false
gap> func(y, x);
true
gap> DClass(S, x) = DClass(S, y);
true
gap> Print(x);
NewSMatrix(IsPlistSMatrixRep,GF(3),3,
[ [ Z(3), Z(3)^0, Z(3)^0 ], [ Z(3)^0, Z(3), Z(3) ], [ Z(3)^0, Z(3), Z(3) ] ])
gap> Print(y);
NewSMatrix(IsPlistSMatrixRep,GF(3),3,
[ [ 0*Z(3), 0*Z(3), Z(3) ], [ 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3) ] ])

#T# MatrixSemigroups6:
gap> T := Semigroup(Transformation( [ 1, 2, 7, 3, 2, 1, 4, 3 ] ), Transformation( [ 5, 7, 8, 2, 7, 3, 8, 5 ] ));
<transformation semigroup on 8 pts with 2 generators>
gap> Size(T);
416
gap> S := AsMatrixSemigroup(T);
<semigroup of 8x8 s-matrices over GF(2) with 2 generators>
gap> Size(S);
416
gap> Size(S) = Size(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true

#T# MatrixSemigroups7:
gap> S := Semigroup( 
> NewSMatrix(IsPlistSMatrixRep, GF(2), 16, 
>    [ [ Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0 ],
>      [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2) ],
>      [ Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2) ], 
>      [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2) ],
>      [ Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0 ],
>      [ Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0 ], 
>      [ Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2) ],
>      [ Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2) ],
>      [ Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2) ], 
>      [ 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0 ],
>      [ 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2) ],
>      [ Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2) ], 
>      [ 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2) ],
>      [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2) ],
>      [ 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0 ], 
>      [ Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0 ] ]),
> NewSMatrix(IsPlistSMatrixRep, GF(2), 16, 
>    [ [ Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2) ],
>      [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0 ],
>      [ 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0 ], 
>      [ Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0 ],
>      [ Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0 ],
>      [ 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0 ], 
>      [ 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2) ],
>      [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0 ],
>      [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2) ], 
>      [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2) ],
>      [ 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0 ],
>      [ Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2) ], 
>      [ 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0 ],
>      [ 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0 ],
>      [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0 ], 
>      [ 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2) ] ]) );
<monoid of 16x16 s-matrices over GF(2) with 2 generators>
gap> Size(S);
21392255076846796801
gap> IsGroupAsSemigroup(S);
false
gap> NrIdempotents(S);
5
gap> NrRClasses(S);
3
gap> NrLClasses(S);
3
gap> SchutzenbergerGroup(DClasses(S)[2]);
<group of 8x8 s-matrices over GF(2) with 3 generators>
gap> PartialOrderOfDClasses(S);
[ [ 1, 2 ], [ 2 ] ]
#gap> StructureDescriptionOfSchutzenbergerGroups(S); 
#T This takes ages, and this is probably due to the
#T StructureDescription for s-matrix groups not being
#T very efficient.
#T It seems to be going through permutation groups
#T Making a Schutzenberger group into a normal GAP
#T Matrix group yields a result instantly:
gap> G := Group(List(GeneratorsOfGroup(SchutzenbergerGroup(DClasses(S)[2])), AsMatrix));
<matrix group with 3 generators>
gap> Size(G);
5348063769211699200
gap> StructureDescription(G);
"PSL(8,2)"


#T# 
gap> S := AsMatrixSemigroup(Semigroup([Z(4) * [[1,0,0], [1,1,0], [0,1,0]],
>                         Z(4) * [[0,0,0],[0,0,1],[0,1,0]]]));

#E#
gap> STOP_TEST("Semigroups package: semimat.tst");
