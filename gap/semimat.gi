############################################################################r
##
#W  semimat.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(BaseDomain, "for a matrix semigroup",
[IsMatrixSemigroup], S -> BaseDomain(Representative(S)));

InstallMethod(DegreeOfMatrixSemigroup, "for a matrix semigroup",
[IsMatrixSemigroup], S -> DegreeOfSMatrix(Representative(S)));

InstallMethod(DegreeOfSMatrixCollection, "for a matrix semigroup",
[IsMatrixSemigroup], DegreeOfMatrixSemigroup);

InstallMethod(IsMatrixSemigroupGreensClass, "for a Green's class",
[IsGreensClass], C -> IsMatrixSemigroup(Parent(C)));

InstallTrueMethod(IsGeneratorsOfSemigroup, IsSMatrixCollection);

InstallMethod(IsGeneratorsOfInverseSemigroup,
"for an s-matrix collection",
[IsSMatrixCollection],
function(coll)
  return ForAll(coll, x -> x^(-1) <> fail);
end);

#T can we fold this into SemigroupByGenerators maybe?
InstallGlobalFunction(MatrixSemigroup,
function(arg)
  local gens, ring, d;

  if IsHomogeneousList(arg) and IsFFECollCollColl(arg) then
    gens := arg;
  elif Length(arg) = 2 and IsField(arg[2]) then
    gens := arg[1];
    ring := arg[2];
  else
    Error("Usage: MatrixSemigroup either takes a list",
          "of matrices, or a list of matrices and a ring",
          "as arguments");
  fi;
  
  
  if not IsBound(ring) then
     ring := DefaultFieldOfMatrix(Product(gens));
  fi;
 
  d := Length(gens[1]);

  gens := List(gens, x->NewSMatrix(IsPlistSMatrixRep, ring, d, x));
  return Semigroup(gens);
end);

#T Why?
InstallMethod(OneMutable, "for an s-matrix collection",
[IsSMatrixCollection],
coll -> One(Representative(coll)));

#T is it inconsistent to have the filter first for NewSMatrix
#T but last for isomorphism?
#T Why does it not seem to be possible to do the same as for
#T constructors? IsObject seems a bit out there
InstallOtherMethod(IsomorphismMatrixSemigroup,
"for a semigroup of matrices and a constructing filter",
[IsSemigroup and HasGeneratorsOfSemigroup and IsFFECollCollColl, IsObject],
function(S, filter)
  local gens, dom, deg, iso, T;
  dom := DefaultFieldOfMatrix(Representative(S));
  deg := Length(Representative(S));
  iso := x -> NewSMatrix(filter, dom, deg, x);
  gens := List(GeneratorsOfSemigroup(S), iso);
  T := Semigroup(gens);
  return MagmaIsomorphismByFunctionsNC(S, Semigroup(gens), iso, AsMatrix);
end);

# This chooses as default representation to be IsPlistSMatrixRep
InstallMethod(IsomorphismMatrixSemigroup,
"for a semigroup of matrices",
[IsSemigroup and HasGeneratorsOfSemigroup and IsFFECollCollColl],
function(S)
  return IsomorphismMatrixSemigroup(S, IsPlistSMatrixRep);
end);

InstallMethod(IsomorphismMatrixSemigroup,
"for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(S)
    return IsomorphismMatrixSemigroup(S, GF(2));
end);

InstallMethod(IsomorphismMatrixSemigroup,
"for a semigroup and a ring",
[IsTransformationSemigroup, IsRing],
function(S, R)
  local n, basis, iso, gens;

  n := DegreeOfTransformationSemigroup(S);
  basis := NewIdentityMatrix(IsPlistMatrixRep, R, n);
  iso := x -> NewSMatrix(IsPlistSMatrixRep, R, n,
                         basis{ImageListOfTransformation(x, n)});
  gens := List(GeneratorsOfSemigroup(S), iso);

  return MagmaIsomorphismByFunctionsNC(S, 
                                       Semigroup(gens),
                                       iso,
                                       x -> 
                                         Transformation(List(x, PositionNonZero)));
end);

InstallMethod(IsomorphismMatrixSemigroup,
"for a matrix semigroup and a ring",
[IsMatrixSemigroup, IsRing],
function(S, R)
    local f, g;
    if BaseDomain(Representative(S)) = R then
      return MagmaIsomorphismByFunctionsNC(S, S, IdFunc, IdFunc);
    else
      # This is obviously not ideal!
      f := IsomorphismTransformationSemigroup(S);
      g := IsomorphismMatrixSemigroup(Range(f), R);
      return f * g;
    fi;
end);

InstallMethod(IsomorphismMatrixSemigroup,
"for a semigroup and a ring",
[IsSemigroup, IsRing],
function(S, R)
  local map;
  map := IsomorphismTransformationSemigroup(S);
  return map * IsomorphismMatrixSemigroup(Range(map), R);
end);

InstallMethod(AsMatrixSemigroup, "for a semigroup", [IsSemigroup],
function(S)
  return Range(IsomorphismMatrixSemigroup(S));
end);

InstallMethod(AsMatrixSemigroup, "for a semigroup and a ring",
[IsSemigroup, IsRing],
function(S, R)
  return Range(IsomorphismMatrixSemigroup(S, R));
end);

#############################################################################
##
## Methods for acting semigroups setup
##
#############################################################################

InstallOtherMethod(FakeOne, "for an s-matrix collection",
[IsSMatrixCollection],
function(coll)
  if IsGeneratorsOfActingSemigroup(coll) then
    return One(Representative(coll));
  fi;
  return fail;
end);

InstallGlobalFunction(SMatrixRowSpaceRightAction,
function(s, vsp, m)
  local basis, nvsp, i, n, deg;

  # This takes care of the token element
  if Rank(vsp) > DegreeOfSMatrix(m) then
    return RowSpaceBasis(m);
  elif Rank(vsp) = 0 then
    return vsp;
  else
    nvsp := SEMIGROUPS_MutableCopyMat(vsp!.rows * m!.mat);
  fi;
  TriangulizeMat(nvsp);

  deg := Length(nvsp);
  for i in [deg,deg - 1 .. 1] do
    if IsZero(nvsp[i]) then
      Remove(nvsp, i);
    fi;
  od;

  return NewSRowBasis(IsPlistSRowBasisRep, BaseDomain(vsp), nvsp);
end);

InstallGlobalFunction(SMatrixLocalRightInverse,
function(S, V, mat)
  local W, im, se, Vdims, mdims, n, k, i, j, u, zv, nonheads;

  n := DegreeOfSMatrix(mat);
  k := Rank(V);

  if n = 0 or k = 0 then
    Error("nullspace");
  fi;

  W := SEMIGROUPS_MutableCopyMat( V!.rows * mat );

  for i in [1 .. k] do
    Append(W[i], V!.rows[i]);
  od;
  se := SemiEchelonMat(W);
  # If the matrix does not act injectively on V,
  # then there is no right inverse
  # FIXME: I think we can now simplify things below
  if Number(se.heads{[1..n]}, IsZero) > n - k then
    return fail;
  fi;

  for i in [1 .. Length(se.vectors)] do
    W[i] := ShallowCopy(se.vectors[i]);
  od;
  
  zv := [1..2*n] * Zero(BaseDomain(mat));
  for i in [1..n-Length(W)] do
    Add(W, ShallowCopy(zv));
  od;

  # add missing heads
  u := One(BaseDomain(mat));
  j := k + 1;
  for i in [1 .. n] do
    if se.heads[i] = 0 then
      W[j][i] := u;
      W[j][n + i] := u;
      j := j + 1;
    fi;
  od;
  TriangulizeMat(W);
 
  return AsSMatrix(mat, W{[1 .. n]}{[n + 1 .. 2 * n]});
end);

#T returns an invertible matrix
#T make pretty and efficient (in that order)
#T In particular the setup for the matrix should be much more
#T efficient.
InstallGlobalFunction(SMatrixSchutzGrpElement,
function(S, x, y)
  local deg, n, eqs, sch, res, idx, row, col,
        check1, check2;

#  check1 := SEMIGROUPS_MutableCopyMat(x!.mat);
#  check2 := SEMIGROUPS_MutableCopyMat(y!.mat);
  deg := DegreeOfSMatrix(x);
  n := RowRank(x);

  if n = 0 then
    return NewIdentitySMatrix(ConstructingFilter(x), BaseDomain(x),
                              n);
    Error();
    res := x;
  else
    eqs := TransposedMatMutable(
      Concatenation(TransposedMat(x!.mat),
                    TransposedMat(y!.mat)));
    TriangulizeMat(eqs);

    idx := [];
    col := 1;
    row := 1;

    while col <= deg do
      while IsZero(eqs[row][col]) and col <= deg do
        col := col + 1;
      od;
      if col <= deg then
        Add(idx, col);
        row := row + 1;
        col := col + 1;
      fi;
    od;
    res := NewSMatrix(ConstructingFilter(x), BaseDomain(x),
                      n, eqs{[1 .. n]}{idx + deg});

  if res^(-1) = fail then
    Error("Schutz element not invertible");
  fi;
 
  fi;
  return res;
end);

## StabilizerAction
InstallGlobalFunction(SMatrixStabilizerAction,
function(S, x, m)
  local rsp, g, coeff, i, n, k, zv;
  if IsZero(x) then 
    return x;
  fi;
  n := DegreeOfSMatrix(m);
  k := RowRank(x);
  rsp := ShallowCopy(m!.mat * RowSpaceBasis(x)!.rows);

  zv := [1 .. n] * Zero(BaseDomain(x)); 
  for i in [1 .. n - k] do
    Add(rsp, ShallowCopy(zv));
  od;

  return AsSMatrix(x,RowSpaceTransformationInv(x) * rsp);
end);

# This should be doable in a much more efficient way
InstallGlobalFunction(SMatrixLambdaConjugator,
function(S, x, y)
  local res, zero, xse, h, p, yse, q, i;

  if IsZero(x) then
    res := NewZeroSMatrix(ConstructingFilter(x), BaseDomain(x), Rank(RowSpaceBasis(x)));
  else
    xse := SemiEchelonMat(SEMIGROUPS_MutableCopyMat(x!.mat));
    h := Filtered(xse.heads, x -> x <> 0);
    p := NewSMatrix(ConstructingFilter(x), BaseDomain(x), Length(h),
                    One(BaseDomain(x)) * PermutationMat(SortingPerm(h),
                                             Length(h), BaseDomain(x)));

    yse := SemiEchelonMat(SEMIGROUPS_MutableCopyMat(y!.mat));
    h := Filtered(yse.heads, x -> x <> 0);
    q := NewSMatrix(ConstructingFilter(y), BaseDomain(y), Length(h),
                    One(BaseDomain(y)) * PermutationMat(SortingPerm(h),
                                           Length(h), BaseDomain(y)));

    res := p * q^(-1);
  fi;
  return res;
end);

#T is there a complete direct way of testing whether
#T this idempotent exists (without constructing it)?
#T the method below is already pretty efficient

# TODO: remove redundant S as an argument here.
InstallGlobalFunction(SMatrixIdempotentTester,
function(S, x, y)
    return SMatrixIdempotentCreator(S,x,y) <> fail;
end);

# Attempt to construct an idempotent m with RowSpace(m) = x
# ColumnSpace(m) = y

InstallGlobalFunction(SMatrixIdempotentCreator,
function(S, x, y)
  local m, inv;
    
  if Rank(x) = 0 then
    return NewZeroSMatrix(ConstructingFilter(Representative(S)),
                          BaseDomain(S), DegreeOfMatrixSemigroup(S)); 
  else
    m := AsSMatrix(Representative(S), TransposedMat(y!.rows) * x!.rows);
    inv := SMatrixLocalRightInverse(S, x, m);
    if inv = fail then 
      return fail;
    else
      return m * inv;
    fi;
  fi;
end);

InstallMethod(ViewString,
"for an s-matrix semigroup with generators",
[ IsMatrixSemigroup and HasGeneratorsOfSemigroup ],
function(S)
  local gens, deg, res;
  if HasIsMonoid(S) and IsMonoid(S) then
    gens := GeneratorsOfMonoid(S);
    deg := DegreeOfSMatrix(gens[1]);
    res := "<monoid of ";
    Append(res, Concatenation(String(deg), "x", String(deg)));
    Append(res, " s-matrices over ");
    Append(res, String(BaseDomain(S)));
    Append(res, Concatenation(" with ", Length(gens), " generator"));
  else
    gens := GeneratorsOfSemigroup(S);
    deg := DegreeOfSMatrix(gens[1]);
    res := "<semigroup of ";
    Append(res, Concatenation(String(deg), "x", String(deg)));
    Append(res, " s-matrices over ");
    Append(res, String(BaseDomain(S)));
    Append(res, Concatenation(" with ", Length(gens), " generator"));
  fi;
  if Length(gens) > 1 then
    Append(res, "s");
  fi;
  Append(res,">");
  return res;
end);

InstallMethod(ViewObj,
"for an s-matrix semigroup with generators",
[ IsMatrixSemigroup and HasGeneratorsOfSemigroup ],
function(S)
  local gens, deg;
  if HasIsMonoid(S) and IsMonoid(S) then
    gens := GeneratorsOfMonoid(S);
    deg := DegreeOfSMatrix(gens[1]);
    Print("<monoid of ");
    Print(deg, "x", deg);
    Print(" s-matrices over ", BaseDomain(S));
    Print(" with ", Length(gens), " generator");
  else
    gens := GeneratorsOfSemigroup(S);
    deg := DegreeOfSMatrix(gens[1]);
    Print("<semigroup of ");
    Print(deg, "x", deg);
    Print(" s-matrices over ", BaseDomain(S));
    Print(" with ", Length(gens), " generator");
  fi;
  if Length(gens) > 1 then
    Print("s");
  fi;
  Print(">");
end);

InstallMethod(PrintObj, "for a matrix semigroup with generators",
[IsMatrixSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local l;
  l := GeneratorsOfSemigroup(S);
  if Length(l) = 0 then
    Print("Semigroup([])");
  else
    Print("Semigroup(",l,")");
  fi;
end);

InstallMethod(ViewObj,
"for a matrix semigroup ideal with generators of semigroup ideal",
[IsMatrixSemigroup and IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(S)
  local deg, gens;
  gens := GeneratorsOfSemigroupIdeal(S);
  deg := DegreeOfSMatrix(gens[1]);
  Print("<ideal of semigroup of ");
  Print(deg, "x", deg);
  Print(" s-matrices over ", BaseDomain(gens[1]));
  Print(" with ", Length(gens), " generator");
  
  if Length(gens) > 1 then
    Print("s");
  fi;
  Print(">");
end);

InstallMethod(IsGeneratorsOfSemigroup, "for an s-matrix collection", 
[IsSMatrixCollection], 
function(coll) 
  if ForAny(coll, x -> DegreeOfSMatrix(x) <> DegreeOfSMatrix(coll[1]) 
                       or BaseDomain(x) <> BaseDomain(coll[1])) then
    return false;
  fi;
  return true;
end);
