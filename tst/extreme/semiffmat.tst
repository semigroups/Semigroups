#############################################################################
##
#W  extreme/semiffmat.tst
#Y  Copyright (C) 2015                                    Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local G, M, S, T, acting, elms, func, i, j, n, upper, x, y, zero
gap> START_TEST("Semigroups package: extreme/semiffmat.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# MatrixSemigroupTest1: Create and Size
gap> M := Matrix(GF(2),
> [[0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>   Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>   0 * Z(2), Z(2) ^ 0],
> [0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2),
>  Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0,
>  Z(2) ^ 0, Z(2) ^ 0],
> [Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>  Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0,
>  Z(2) ^ 0, Z(2) ^ 0],
> [0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0,
>  0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>  0 * Z(2), 0 * Z(2)],
> [0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>  0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>  0 * Z(2), Z(2) ^ 0],
> [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2),
>  0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>  0 * Z(2), 0 * Z(2)],
> [0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>  0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>  Z(2) ^ 0, 0 * Z(2)],
> [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>  0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>  Z(2) ^ 0, 0 * Z(2)],
> [Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2),
>  Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>  0 * Z(2), Z(2) ^ 0],
> [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2),
>  0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>  Z(2) ^ 0, Z(2) ^ 0],
> [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0,
>  Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0,
>  Z(2) ^ 0, 0 * Z(2)],
> [0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>  Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2),
>  0 * Z(2), Z(2) ^ 0],
> [Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>  Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>  0 * Z(2), Z(2) ^ 0],
> [0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0,
>  0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>  0 * Z(2), Z(2) ^ 0],
> [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>  Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>  0 * Z(2), 0 * Z(2)],
> [Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>  0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>  Z(2) ^ 0, Z(2) ^ 0]]);;
gap> S := Semigroup(M);
<commutative semigroup of 16x16 matrices over GF(2) with 1 generator>
gap> Size(S);
7161
gap> NrDClasses(S);
1
gap> PartialOrderOfDClasses(S);
<immutable empty digraph with 1 vertex>
gap> StructureDescriptionSchutzenbergerGroups(S);
[ "C7161" ]
gap> T := AsSemigroup(IsTransformationSemigroup, S);
<commutative transformation semigroup of size 7161, degree 7161 with 1 
 generator>
gap> Size(T);
7161
gap> Size(S) = Size(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true

# MatrixSemigroupTest2: Create and Size
gap> S := Semigroup(
> [Matrix(GF(3),
> [[Z(3), Z(3), Z(3) ^ 0, Z(3), Z(3) ^ 0],
>   [0 * Z(3), 0 * Z(3), Z(3), 0 * Z(3), Z(3) ^ 0],
>   [Z(3), Z(3), Z(3) ^ 0, 0 * Z(3), Z(3)],
>   [Z(3) ^ 0, Z(3) ^ 0, Z(3), Z(3), 0 * Z(3)],
>   [Z(3), Z(3) ^ 0, Z(3), Z(3) ^ 0, 0 * Z(3)]]),
> Matrix(GF(3),
> [[0 * Z(3), Z(3) ^ 0, 0 * Z(3), Z(3), Z(3)],
>   [Z(3), 0 * Z(3), Z(3) ^ 0, 0 * Z(3), Z(3) ^ 0],
>   [Z(3), Z(3), Z(3) ^ 0, Z(3), Z(3)],
>   [Z(3), Z(3), 0 * Z(3), Z(3), Z(3) ^ 0],
>   [Z(3), Z(3), Z(3) ^ 0, Z(3) ^ 0, Z(3)]])]);;
gap> Size(S);
170080803
gap> NrIdempotents(S);
43844
gap> PartialOrderOfDClasses(S);
<immutable digraph with 5 vertices, 4 edges>
gap> S := Semigroup(GeneratorsOfSemigroup(S));
<semigroup of 5x5 matrices over GF(3) with 2 generators>
gap> PartialOrderOfDClasses(S);
<immutable digraph with 5 vertices, 4 edges>

# MatrixSemigroupTest3: Create, Size, MinimalIdeal
gap> S := Semigroup(
> [Matrix(GF(3),
>    [[Z(3), Z(3), Z(3) ^ 0],
>      [0 * Z(3), Z(3), Z(3)],
>      [Z(3), 0 * Z(3), Z(3) ^ 0]]),
>   Matrix(GF(3),
>    [[Z(3), Z(3), 0 * Z(3)],
>      [Z(3) ^ 0, Z(3) ^ 0, 0 * Z(3)],
>      [Z(3) ^ 0, Z(3) ^ 0, 0 * Z(3)]])]);;
gap> Size(S);
137
gap> NrIdempotents(S);
42
gap> MinimalIdeal(S);
<simple semigroup ideal of 3x3 matrices over GF(3) with 1 generator>
gap> Size(last);
1
gap> MultiplicativeZero(S);
[ [ 0*Z(3), 0*Z(3), 0*Z(3) ], [ 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3) ] ]
gap> MinimalDClass(S);
<Green's D-class: <matrix object of dimensions 3x3 over GF(3)>>
gap> M := MaximalSubsemigroups(S);;
gap> List(M, Size);
[ 133, 9 ]
gap> List(M, U -> IsMaximalSubsemigroup(S, U));
[ true, true ]

# MatrixSemigroups3: Upper triangular matrices, SubsemigroupByProperty
gap> upper := function(mat)
>   local zero, n, i, j;
>   zero := Zero(BaseDomain(mat));
>   n := NrRows(mat);
>   for i in [2 .. n] do
>     for j in [1 .. i - 1] do
>       if mat[i, j] <> zero then
>         return false;
>       fi;
>     od;
>   od;
>   return true;
> end;;
gap> S := GeneralLinearMonoid(3, 3);
<general linear monoid 3x3 over GF(3)>
gap> T := SubsemigroupByProperty(S, upper);
<monoid of size 729, 3x3 matrices over GF(3) with 21 generators>
gap> Size(T);
729

# MatrixSemigroups4: ClosureSemigroup
gap> upper := function(mat)
>   local zero, n, i, j;
>   zero := Zero(BaseDomain(mat));
>   n := NrRows(mat);
>   for i in [2 .. n] do
>     for j in [1 .. i - 1] do
>       if mat[i][j] <> zero then
>         return false;
>       fi;
>     od;
>   od;
>   return true;
> end;;
gap> elms := Filtered(Elements(GLM(3, 3)), upper);;
gap> S := Semigroup(elms[1]);;
gap> for i in [2 .. Length(elms)] do
>  S := ClosureSemigroup(S, elms[i]);
> od;;
gap> S;
<monoid of 3x3 matrices over GF(3) with 64 generators>
gap> Size(S);
729

# MatrixSemigroups5:  
gap> func := IsGreensDGreaterThanFunc(S);
function( x, y ) ... end
gap> x := Random(S);;
gap> y := Random(S);;
gap> func(x, y);;
gap> func(y, x);;

# MatrixSemigroups6:
gap> T := Semigroup(Transformation([1, 2, 7, 3, 2, 1, 4, 3]),
>                   Transformation([5, 7, 8, 2, 7, 3, 8, 5]));
<transformation semigroup of degree 8 with 2 generators>
gap> Size(T);
416
gap> S := AsSemigroup(IsMatrixOverFiniteFieldSemigroup, T);
<semigroup of 8x8 matrices over GF(2) with 2 generators>
gap> Size(S);
416
gap> Size(S) = Size(T);
true
gap> NrIdempotents(S) = NrIdempotents(T);
true

# MatrixSemigroups7:
gap> S := Semigroup(
> Matrix(GF(2),
>  [[Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0],
>   [Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>    Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2),
>    Z(2) ^ 0, 0 * Z(2)],
>   [Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2),
>    0 * Z(2), 0 * Z(2)],
>   [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>    0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    0 * Z(2), 0 * Z(2)],
>   [Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0,
>    0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>    Z(2) ^ 0, Z(2) ^ 0],
>   [Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>    Z(2) ^ 0, Z(2) ^ 0],
>   [Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0,
>    Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0,
>    0 * Z(2), 0 * Z(2)],
>   [Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>    Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    0 * Z(2), 0 * Z(2)],
>   [Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>    0 * Z(2), 0 * Z(2)],
>   [0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>    0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>    Z(2) ^ 0, Z(2) ^ 0],
>   [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0,
>    0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>    Z(2) ^ 0, 0 * Z(2)],
>   [Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2),
>    0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, 0 * Z(2)],
>   [0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2),
>    Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>    Z(2) ^ 0, 0 * Z(2)],
>   [Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, 0 * Z(2)],
>   [0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0,
>    0 * Z(2), Z(2) ^ 0],
>   [Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0,
>    Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, Z(2) ^ 0]]),
> Matrix(GF(2),
>  [[Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2),
>    Z(2) ^ 0, 0 * Z(2)],
>   [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>    0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, Z(2) ^ 0],
>   [0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>    Z(2) ^ 0, Z(2) ^ 0],
>   [Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>    Z(2) ^ 0, Z(2) ^ 0],
>   [Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>    0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0,
>    Z(2) ^ 0, Z(2) ^ 0],
>   [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2),
>    0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>    0 * Z(2), Z(2) ^ 0],
>   [0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>    0 * Z(2), 0 * Z(2)],
>   [Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>    Z(2) ^ 0, Z(2) ^ 0],
>   [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>    Z(2) ^ 0, 0 * Z(2)],
>   [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>    Z(2) ^ 0, 0 * Z(2)],
>   [0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0],
>   [Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0,
>    0 * Z(2), 0 * Z(2)],
>   [0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0,
>    Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0,
>    0 * Z(2), Z(2) ^ 0],
>   [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0,
>    0 * Z(2), Z(2) ^ 0],
>   [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>    0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0],
>   [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>    0 * Z(2), Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2),
>    0 * Z(2), 0 * Z(2)]]));
<semigroup of 16x16 matrices over GF(2) with 2 generators>
gap> Size(S);
21392255076846796800
gap> IsGroupAsSemigroup(S);
false
gap> NrIdempotents(S);
4
gap> NrRClasses(S);
2
gap> NrLClasses(S);
2
gap> SchutzenbergerGroup(DClasses(S)[1]);
<matrix group of size 5348063769211699200 with 3 generators>
gap> PartialOrderOfDClasses(S);
<immutable empty digraph with 1 vertex>

#FIXME(later)
#gap> StructureDescriptionOfSchutzenbergerGroups(S); 
#T This takes ages, and this is probably due to the
#T StructureDescription for matrix over finite field
#T groups not being very efficient.
#T It seems to be going through permutation groups
#T Making a Schutzenberger group into a normal GAP
#T Matrix group yields a result instantly:
gap> G := Group(List(GeneratorsOfGroup(SchutzenbergerGroup(DClasses(S)[1])),
> AsList));
<matrix group with 3 generators>
gap> Size(G);
5348063769211699200
gap> StructureDescription(G);
"PSL(8,2)"

# 
gap> S := AsSemigroup(IsMatrixOverFiniteFieldSemigroup,
>                     Semigroup(Z(4) * [[1, 0, 0], [1, 1, 0], [0, 1, 0]],
>                               Z(4) * [[0, 0, 0], [0, 0, 1], [0, 1, 0]]));
<semigroup of 3x3 matrices over GF(2^2) with 2 generators>
gap> Size(S);
27

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/semiffmat.tst");
