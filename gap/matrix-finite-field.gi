############################################################################
##
#W  matrix-finite-field.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains an implementation of matrices over finite fields.

InstallMethod(TypeViewStringOfMatrixOverSemiring, "for a matrix over finite field",
[IsMatrixOverFiniteField], x -> "finite field");

#TODO make it non-NC

InstallMethod(TypePrintStringOfMatrixOverSemiring, "for a matrix over finite field",
[IsMatrixOverFiniteField], x -> "MatrixOverFiniteFieldNC");

InstallGlobalFunction(MatrixOverFiniteFieldNC,
x -> Objectify(MatrixOverFiniteFieldType, x));

InstallMethod(BaseField, "for a matrix over finite field",
[IsMatrixOverFiniteField], x -> GF(x![DimensionOfMatrixOverSemiring(x) + 1]));


InstallMethod(\*, "for matrices over a finite field", 
[IsMatrixOverFiniteField, IsMatrixOverFiniteField],
function(x, y)
  local n, xy, i, j, k;

  n := DimensionOfMatrixOverSemiring(x);
  xy := List([1 .. n], x -> EmptyPlist(n));
  xy[n + 1] := Size(BaseField(x));

  for i in [1 .. n] do
    for j in [1 .. n] do
      xy[i][j] := 0;
      for k in [1 .. n] do
        xy[i][j] := xy[i][j] + x![i][k] * y![k][j];
      od;
    od;
  od;
  return MatrixOverFiniteFieldNC(xy);
end);

InstallMethod(OneImmutable, "for a matrix over a finite field",
[IsMatrixOverFiniteField],
function(x)
  local n, id, i;

  n := DimensionOfMatrixOverSemiring(x);
  id := List([1 .. n], y -> [1 .. n] * Zero(BaseField(x)));
  id[n + 1] := Size(BaseField(x));
  for i in [1 .. n] do
    id[i][i] := One(BaseField(x));
  od;
  return MatrixOverFiniteFieldNC(id);
end);

InstallMethod(OneMutable, "for a matrix over a finite field",
[IsMatrixOverFiniteField], OneImmutable);

InstallMethod(RandomMatrixOverFiniteField, "for a pos int and a finite field", 
[IsPosInt, IsFinite and IsField],
function(n, field)
  local xy, i, j;
  xy := List([1 .. n], x -> EmptyPlist(n));
  xy[n + 1] := Size(field);
  for i in [1 .. n] do
    for j in [1 .. n] do
      xy[i][j] := Random(field);
    od;
  od;
  return MatrixOverFiniteFieldNC(xy);
end);

InstallMethod(RandomMatrixOverFiniteField, "for pos int, prime int, pos int", 
[IsPosInt, IsPosInt, IsPosInt],
function(n, p, d)
  return RandomMatrixOverFiniteField(n, GF(p, d));
end);

InstallMethod(Display, "for a matrix over a finite field",
[IsMatrixOverFiniteField],
function(x)
  Display(AsMatrix(x));
end);

# special methods

InstallMethod(AsMatrix, "for a matrix over finite field",
[IsMatrixOverFiniteField], 
function(x)
  return List([1 .. DimensionOfMatrixOverSemiring(x)], i -> x![i]);
end);

InstallMethod(AsMatrixOverFiniteField, 
"for a finite field and matrix with entries in a finite field",
[IsFinite and IsField, IsMatrix and IsFFECollColl], 
function(field, x)
  local n, i, j;
  
  n := Length(x);

  for i in [1 .. n] do
    for j in [1 .. n] do 
      if not x[i][j] in field then
        Error("Semigroups: AsMatrixOverFiniteField: usage,\n", 
              "the entry ", x[i][j], " does not belong to ", field, ",");
      fi;
    od;
  od;

  x := MutableCopyMat(x);
  Apply(x, PlainListCopy);
  x[Length(x) + 1] := Size(field);

  return MatrixOverFiniteFieldNC(x);
end);

InstallMethod(AsMatrixOverFiniteField, 
"for a pos int and matrix with entries in finite field",
[IsPosInt, IsMatrix and IsFFECollColl], 
function(p, x)
  if not IsPrimePowerInt(p) then 
    Error();
    return;
  fi;

  return MatrixOverFiniteField(GF(p), x);
end);

InstallMethod(AsMatrixOverFiniteField, 
"for a prime power int and matrix of integers",
[IsPosInt, IsMatrix and IsCyclotomicCollColl],
function(p, x)
  local n, i, j;
  
  if not IsPrimePowerInt(p) then 
    Error();
    return;
  fi;
  return AsMatrixOverFiniteFieldNC(p, x);
end);

InstallMethod(AsMatrixOverFiniteFieldNC, 
"for a prime power int and matrix of integers",
[IsPosInt, IsObject],
function(p, x)
  local n, prim, i, j;

  n := Length(x);
  prim := PrimitiveRoot(GF(p));

  for i in [1 .. n] do
    for j in [1 .. n] do
      x[i][j] := x[i][j] * prim;
    od;
  od;
  Apply(x, PlainListCopy);
  x[n + 1] := p;
  return Objectify(MatrixOverFiniteFieldType, x);
end);

