#############################################################################
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

InstallMethod(OneMutable, "for an smatrix", [IsSMatrixCollection],
coll -> One(Representative(coll)));

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
  local n, basis, gens;

  n := DegreeOfTransformationSemigroup(S);

  basis := NewIdentityMatrix(IsPlistMatrixRep, R, n);

  gens := List(GeneratorsOfSemigroup(S),
   x -> basis{ImageListOfTransformation(x, n)});

  return MagmaIsomorphismByFunctionsNC(S, Semigroup(gens),
   x -> basis{ImageListOfTransformation(x, n)},
   x -> Transformation(List(x, PositionNonZero)));
end);

InstallMethod(IsomorphismMatrixSemigroup,
"for a matrix semigroup and a ring",
[IsMatrixSemigroup, IsRing],
function(S, R)
    local f, g;
    if BaseDomain(Representative(S)) = R then
      return MagmaIsomorphismByFunctionsNC(S, S, x -> x, x -> x);
    else
      # This is obviously not ideal!
      f := IsomorphismTransformationSemigroup(S);
      g := IsomorphismMatrixSemigroup(Range(f),R);
      return f * g;
    fi;
end);

InstallMethod(IsomorphismMatrixSemigroup,
"for a semigroup and a ring",
[IsSemigroup, IsRing],
function(S, R)
  return IsomorphismMatrixSemigroup(AsTransformationSemigroup(S), R);
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

# from here 
#InstallMethod(ImagesRepresentative, "for a general mapping and null map",
#[IsGeneralMapping, IsNullMapMatrix], 
#function(map, g)
#  return g;
#end);
#
#InstallImmediateMethod(IsNullMapMatrixGroup, IsGroup and HasGeneratorsOfGroup,
#0, 
#function(G)
#  if Length(GeneratorsOfGroup(G)) = 1 and
#      IsNullMapMatrix(GeneratorsOfGroup(G)[1]) then 
#    return true;
#  else
#    return false;
#  fi;
#end);
#
#InstallMethod(\^, "for a null mat matrix group and matrix",
#[IsNullMapMatrixGroup, IsMatrix], 
#function(x, mat)
#  return x;
#end);
#
#InstallMethod(\/, "for a matrix and null map matrix", 
#[IsMatrix, IsNullMapMatrix],
#function(mat, null)
#  return null;
#end);
#
#InstallMethod(\*, "for a right coset and null map matrix",
#[IsRightCoset, IsNullMapMatrix], 
#function(coset, x)
#  return coset;
#end);
#
#InstallMethod(\^, "for a null map matrix and int",
#[IsNullMapMatrix, IsInt], 
#function(x, n)
#  return x;
#end);
#
#InstallMethod(\^, "for a null map matrix and matrix",
#[IsNullMapMatrix, IsMatrix], 
#function(x, m)
#  return x;
#end);
#
#InstallMethod(InverseMutable, "for a null map matrix",
#[IsNullMapMatrix], IdFunc);
#
#InstallMethod(IsGeneratorsOfMagmaWithInverses, "for a list",
#[IsList], 
#function(list) 
#  if Length(list) = 1 and IsNullMapMatrix(list[1]) then 
#    return true;
#  fi;
#  TryNextMethod();
#end);
#
#InstallTrueMethod(IsOne, IsNullMapMatrix);
#
#InstallMethod(One, "for a null map matrix", 
#[IsNullMapMatrix], IdFunc);

# to here is a hack to make it possible to make a group consisting of the null
# map matrix . . . 


#T Are these still needed
#T This returns immutable

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

  deg := DegreeOfSMatrix(deg);
  # This takes care of the token element
  if Length(vsp) > DegreeOfSMatrix(m) then
    nvsp := m!.mat;
  else
    nvsp := vsp * m!.mat;
  fi;
  TriangulizeMat(nvsp);

  for i in [deg,deg - 1 .. 1] do
    if IsZero(nvsp[i]) then
      Remove(nvsp, i);
    fi;
  od;

  return nvsp;
end);


# 
# If dim(V * mat) = dim(V) compute a right inverse for mat, otherwise
# return fail.
#
#T do this "by hand", and in place, or find a
#T kernel function that does this.
#
InstallGlobalFunction(SMatrixLocalRightInverse,
function( S, V, mat )
        local W, im, se, Vdims, mdims, n, k, i, j, u, zv, nonheads;

        # We assume that mat is quadratic but we don't check this.
        # If a semigroup decides to have non-quadratic matrices in it,
        # something is seriously wrong anyway.
        n := DimensionsMat(mat)[1];
        k := DimensionsMat(V)[1];

        W := ZeroMatrix(n, 2 * n, mat);

        #T I think this should all be done in place
        CopySubMatrix( V * mat, W, [1 .. k], [1 .. k], [1 .. n], [1 .. n]);
        CopySubMatrix( V, W, [1 .. k], [1 .. k], [1 .. n], [n + 1 .. 2 * n]);

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

        # add missing heads
        u := One(BaseDomain(W));
        j := k + 1;
        for i in [1 .. n] do
            if se.heads[i] = 0 then
                W[j][i] := u;
                W[j][n + i] := u;
                j := j + 1;
            fi;
        od;

        TriangulizeMat(W);

        return ExtractSubMatrix(W, [1 .. n], [n + 1 .. 2 * n]);
end);

#T returns an invertible matrix
#T make pretty and efficient (in that order)
#T In particular the setup for the matrix should be much more
#T efficient.
InstallGlobalFunction(SMatrixSchutzGrpElement,
function(S, x, y)
    local eqs, sch, res, n, k, idx, row, col;

    k := DimensionsMat(x)[2];
    n := LambdaRank(S)(x);

    if IsZero(x) then
        res := [[One(BaseDomain(x))]];
    else
        eqs := MutableCopyMat(TransposedMat(Concatenation(TransposedMat(x),
                TransposedMat(y))));
	TriangulizeMat(eqs);

	idx := [];
        col := 1;
        row := 1;

	while col <= k do
		while IsZero(eqs[row][col]) and col <= k do
			col := col + 1;
		od;
		if col <= k then
			Add(idx, col);
			row := row + 1;
			col := col + 1;
		fi;
	od;
	return ExtractSubMatrix(eqs, [1 .. n], idx + k);
    fi;

    return res;
end);

## StabilizerAction
InstallGlobalFunction(SMatrixStabilizerAction,
function(S, x, m)
    local rsp, g, coeff, i, n, k;
    
    if IsZero(x) then 
      return x;
    fi;
    n := DimensionsMat(x)[1];
    k := LambdaRank(S)(x);
    g := NewMatrix(IsPlistMatrixRep, BaseDomain(x), Length(m), m);
    rsp := g * RowSpaceBasis(x);

    for i in [1 .. n - k] do
        Add(rsp, ZeroVector(n, rsp));
    od;

    return RowSpaceTransformationInv(x) * rsp;
end);

InstallGlobalFunction(SMatrixLambdaConjugator,
function(S, x, y)
  local res, zero, xse, h, p, yse, q, i;

    if x ^ -1 <> fail then
        res := List(x ^ -1 * y, List);
    else
      # FIXME this is totally fucked up, IsZero is a property which get stored
      # as false for some element in the semigroup
      # S := Semigroup(
      # [ NewMatrix(IsPlistMatrixRep,GF(3),3,
      #     [ [ Z(3), Z(3), Z(3)^0 ], [ 0*Z(3), Z(3), Z(3) ], 
      #       [ Z(3), 0*Z(3), Z(3)^0 ] ]), NewMatrix(IsPlistMatrixRep,GF(3),3,
      #     [ [ Z(3), Z(3), 0*Z(3) ], [ Z(3)^0, Z(3)^0, 0*Z(3) ], 
      #       [ Z(3)^0, Z(3)^0, 0*Z(3) ] ]) ]);;
      # then the matrix "becomes" zero, and the value of IsZero is false. 
      # I don't know how this could happend. Change this back to IsZero and
      # then run MaximalSubsemigroups on the semigroup S above to see what I
      # mean. Then quit from the break loop and do MaximalSubsemigroups again,
      # and watch GAP seg fault. 
      zero := true;
      for i in [1..Length(x![ROWSPOS])] do
          if not IsZero(x![ROWSPOS][i]) then
              zero := false;
          fi;
      od;
      if zero then
        res := [[One(BaseDomain(x))]];
      else
        xse := SemiEchelonMat(x);
        h := Filtered(xse.heads, x -> x <> 0);
        p := Matrix(One(BaseDomain(x)) * PermutationMat(SortingPerm(h),
             Length(h), BaseDomain(x)), x);

        yse := SemiEchelonMat(y);
        h := Filtered(yse.heads, x -> x <> 0);
        q := Matrix(One(BaseDomain(y)) * PermutationMat(SortingPerm(h),
             Length(h), BaseDomain(y)), y);

        res := List(p * q ^ ( - 1), List);
      fi;
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
    
    m := TransposedMat(y) * x;
    inv := SMatrixLocalRightInverse(S, x, m);
    if inv = fail then 
      return fail;
    else
      return m * inv;
    fi;
end);

# TODO ViewString

InstallMethod(ViewObj,
"for a matrix semigroup with generators",
[ IsMatrixSemigroup and HasGeneratorsOfSemigroup ],
function(S)
  local gens, deg;
  if HasIsMonoid(S) and IsMonoid(S) then
    gens := GeneratorsOfMonoid(S);
    deg := DegreeOfSMatrix(gens[1]);
    Print("<monoid of ");
    Print(deg, "x", deg);
    Print(" matrices over ", BaseDomain(S));
    Print(" with ", Length(gens), " generator");
  else
    gens := GeneratorsOfSemigroup(S);
    deg := DegreeOfSMatrix(gens[1]);
    Print("<semigroup of ");
    Print(deg, "x", deg);
    Print(" matrices over ", BaseDomain(S));
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
  local gens, dims;
  gens := GeneratorsOfSemigroupIdeal(S);
  dims := DimensionsMat(gens[1]);
  Print("<ideal of semigroup of ");
  Print(dims[1], "x", dims[2]);
  Print(" matrices over ", BaseDomain(gens[1]));
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
