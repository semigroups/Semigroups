#############################################################################
##
#W  semimat.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(IsGeneratorsOfSemigroup, [IsFFECollCollColl],
        function(L)
#T do checking of dimensions
    return ForAll(L, IsMatrixObj);
end);

#T Are these still needed
#T This returns immutable
InstallMethod(OneMutable,
    "for ring element coll coll coll",
    [IsRingElementCollCollColl],
    x-> One(Representative(x)));

InstallMethod(IsGroupAsSemigroup,
    "for a matrix semigroup",
    [IsMatrixSemigroup],
    s-> IsGroupAsSemigroup(Range(IsomorphismTransformationSemigroup(s))));

#############################################################################
##
## Methods for acting semigroups setup
##
#############################################################################


InstallOtherMethod(FakeOne,
    "for a list of matrices (hack)",
    [IsHomogeneousList and IsRingElementCollCollColl],
function(elts)
    if IsGeneratorsOfActingSemigroup(elts) then
        return One(elts[1]);
    else
        TryNextMethod();
    fi;
end);

InstallGlobalFunction(MatrixObjRowSpaceRightAction,
  function(s, vsp, mat)
    local basis, nvsp, i, n;

      # This takes care of the token element
      if DimensionsMat(vsp)[1] > DimensionsMat(mat)[1] then
          nvsp := mat;
      else
          nvsp := vsp * mat;
      fi;
      #T WHY? This is not correct, i think
      nvsp := MutableCopyMat(nvsp);
      TriangulizeMat(nvsp);
      n := DimensionsMat(nvsp)[1];
      for i in [n,n-1..1] do
        if IsZero(nvsp[i]) then
          Remove(nvsp, i);
        fi;
      od;

      return nvsp;
end);


# Under the assumption that rank mat >= dim V compute
# a matrix N such that mat * N = id_V
#
#T do this "by hand", and in place, or find a
#T kernel function that does this.
InstallGlobalFunction(MatrixObjLocalRightInverse,
function( S, V, mat )
        local W, im, se, Vdims, mdims, n, k, i, j, u, zv, nonheads;

        # We assume that mat is quadratic but we don't check this.
        # If a semigroup decides to have non-quadratic matrices in it,
        # something is seriously wrong anyway.
        n := DimensionsMat(mat)[1];
        k := DimensionsMat(V)[1];

        W := ZeroMatrix(n, 2 * n, mat);

        #T I think this should all be done in place
        CopySubMatrix( V * mat, W, [1..k], [1..k], [1..n], [1..n]);
        CopySubMatrix( V, W, [1..k], [1..k], [1..n], [n+1..2*n]);

        se := SemiEchelonMat(W);

        for i in [1..Length(se.vectors)] do
            W[i] := ShallowCopy(se.vectors[i]);
        od;

        # add missing heads
        u := One(BaseDomain(W));
        j := k+1;
        for i in [1..n] do
            if se.heads[i] = 0 then
                W[j][i] := u;
                W[j][n+i] := u;
                j := j+1;
            fi;
        od;

        TriangulizeMat(W);

        return ExtractSubMatrix(W, [1..n], [n+1..2*n]);
end);

#T returns an invertible matrix
#T make pretty and efficient (in that order)
#T In particular the setup for the matrix should be much more
#T efficient.
InstallGlobalFunction(MatrixObjSchutzGrpElement,
function(S, x, y)
    local eqs, sch, res, n, k, idx, row, col;

    k := DimensionsMat(x)[2];
	n := LambdaRank(S)(x);

    if IsZero(x) then
        res := [[One(BaseDomain(x))]];
    else
		eqs := MutableCopyMat(TransposedMat(Concatenation(TransposedMat(x),TransposedMat(y))));
		TriangulizeMat(eqs);
	
		idx := [];

		col := 1; row := 1;
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
		sch := ExtractSubMatrix(eqs, [1..n], idx + k);

        res := List(sch, List);
    fi;

    return res;
end);

InstallGlobalFunction(MatrixObjLambdaConjugator,
function(S, x, y)
     local xse, xhe, yse, yhe, he, h, p, q, i, RemoveZeroRows, res;

    if x^(-1) <> fail then
        res := List(x^(-1) * y, List);
    elif IsZero(x) then
        res := [[One(BaseDomain(x))]];
    else
        xse := SemiEchelonMat(x);
        h := Filtered(xse.heads, x->x<>0);
        p := Matrix(One(BaseDomain(x)) * PermutationMat(SortingPerm(h), Length(h), BaseDomain(x)), x);

        yse := SemiEchelonMat(y);
        h := Filtered(yse.heads, x->x<>0);
        q := Matrix(One(BaseDomain(y)) * PermutationMat(SortingPerm(h), Length(h), BaseDomain(y)), y);

        res := List(p * q^(-1), List);
    fi;
    return res;
end);

#T is there a complete direct way of testing whether
#T this idempotent exists (without constructing it)?
#T the method below is already pretty efficient
InstallGlobalFunction(MatrixObjIdempotentTester,
function(S, x, y)
    return MatrixObjIdempotentCreator(S,x,y) <> fail;
end);

# Attempt to construct an idempotent m with RowSpace(m) = x
# ColumnSpace(m) = y
InstallGlobalFunction(MatrixObjIdempotentCreator,
function(S, x, y)
    local m, m2, p1, p2, f;
    m := TransposedMat(y) * x;

    m2 := m * m;
    p1 := PositionNonZero(m2);
    p2 := PositionNonZero(m2[p1]);

    f := m[p1][p2] / m2[p1][p2];

    m := f * m;

    if f * m2 = m then
        return m;
    else
        return fail;
    fi;
end);

#############################################################################
##
#M  ViewObj( <matsemigrp> )
##

InstallMethod( ViewObj,
    "for a matrix semigroup with generators",
    [ IsMatrixSemigroup and HasGeneratorsOfSemigroup ],
function(S)
    local gens, dims;
        gens:=GeneratorsOfSemigroup(S);
        dims:=DimensionsMat(gens[1]);
        Print("<semigroup of ");
        #if HasSize(S) then
        #   Print(" of size ",Size(S));
        #fi;
        Print(dims[1], "x", dims[2]);
        Print(" matrices over ", BaseDomain(gens[1]));
        Print(" with ", Length(gens), " generator");
        if Length(gens)>1 then
          Print("s");
        fi;
        Print(">");
end);

#############################################################################
##
#M  PrintObj( <matsemigrp> )
##
InstallMethod( PrintObj,"for a matrix semigroup",
    [ IsMatrixSemigroup ],
function(S)
    local l;
    l := GeneratorsOfSemigroup(S);
    if Length(l) = 0 then
        Print("Semigroup([])");
    else
        Print("Semigroup(",l,")");
    fi;
end);

# Note that this method assumes that the object is
# IsPlistMatrixRep and that we are using a position
# in the positionalobjectrep for storing the row space (this
# is mainly because MatrixObj, PlistMatrixObj are not
# in IsAttributeStoringRep
#
InstallMethod( CanonicalRowSpace,
        "for a matrix object in PlistMatrixRep over a finite field",
        [ IsMatrixObj
          and IsPlistMatrixRep
          and IsFFECollColl ],
function( m )
    local i, n;

    if not IsBound(m![5]) then
        Info(InfoMatrixSemigroups, 2, "CanonicalRowSpace called");

        m![5] := MutableCopyMat(m);
        TriangulizeMat(m![5]);

        n := DimensionsMat(m![5])[1];

        for i in [n,n-1..1] do
            if IsZero(m![5][i]) then
                Remove(m![5], i);
            fi;
        od;
    fi;

    return m![5];
end);

#############################################################################
##
#M  MatrixSemigroup(gens, [F, n] )
##
#############################################################################
##
## This is the kitchen-sink matrix semigroup creation tool
##
## This needs more/better checking since GAP does not have any means
## of determining _whether there is_ a common scalar domain for
## a list of matrices for example, it just complains with a
## "no method found" error if handed a list of matrices over different
## incompatible domains.
##
InstallGlobalFunction(MatrixSemigroup,
function(arg)
    local gens, field, n, ListOfGens, ListOfGensAndField;

    if Length(arg) > 0 then
        if IsHomogeneousList(arg) and IsFFECollCollColl(arg) then # Just the generators
            gens := arg;
        elif Length(arg) = 1 then # List of generators
            gens := arg[1];
        elif Length(arg) = 2 then # List of generators, field
            gens := arg[1];
            field := arg[2];
        elif Length(arg) = 3 then # List of generators, field, matrix dimensions
            gens := arg[1];
            field := arg[2];
            n := arg[3];
        else                      # just the generators
            Error("usage: MatrixSemigroup(gens [, F, n])");
        fi;
    else
        Error("usage: MatrixSemigroup(gens [, F, n])");
    fi;

    if Length(gens) = 0 then
        Error("empty generating sets are not supported");
    fi;

    if (not IsFFECollCollColl(gens)) or
       (not IsHomogeneousList(gens)) then
        Error("only matrices over finite fields are supported as generating sets");
    fi;

    if not IsBound(field) then
        field := DefaultScalarDomainOfMatrixList(gens);
    fi;

    if not IsBound(n) then
        n := Length(gens[1]);
    fi;

    gens := List(gens, x -> NewMatrix(IsPlistMatrixRep, field, n, x));

    return Semigroup(gens);
end);

InstallMethod(IsomorphismMatrixSemigroup, 
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, basis, gens;

  n := DegreeOfTransformationSemigroup(S);

  basis := NewIdentityMatrix(IsPlistMatrixRep, GF(2), n);

  gens := List(GeneratorsOfSemigroup(S), 
   x -> basis{ ImageListOfTransformation(x, n) });

  return MagmaIsomorphismByFunctionsNC(S, SemigroupByGenerators(gens), 
   x -> basis{ ImageListOfTransformation(x, n) },
   x -> Transformation(List(x, PositionNonZero)));

end);

InstallMethod(AsMatrixSemigroup, "for a semigroup", [IsSemigroup],
function(S)
  return Range(IsomorphismMatrixSemigroup(AsTransformationSemigroup(S)));
end);
