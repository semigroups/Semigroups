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
      #T WHY?
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

InstallMethod(LambdaBound,
	"for a matrix semigroup",
        [IsMatrixSemigroup],
	S -> function(r)
  	    local f;

            if r < 100 then
		    f := Representative(S);
		    return Size(GL(DimensionsMat(f)[1], BaseDomain(f)));
            else
                    return infinity;
            fi;
        end);

InstallMethod(LambdaIdentity,
	"for a matrix semigroup",
        [IsMatrixSemigroup],
	S -> function(r)
  	    local f, one;

	    f := Representative(S);
            one := IdentityMat(r, BaseDomain(f));

            return one;
        end);

#T returns an invertible matrix
#T make pretty and efficient (in that order)
InstallGlobalFunction(MatrixObjSchutzGrpElement,
function(S, x, y)
    local xse, xhe, yse, yhe, he, p, q, i, RemoveZeroRows, res;
        
    RemoveZeroRows := function(mat)
        local i, n;
            
        n := DimensionsMat(mat)[1];

        for i in [n,n-1..1] do
            if IsZero(mat[i]) then
                Remove(mat,i);
            fi;
        od;
    end;
        
    if x^(-1) <> fail then
        res := List(x^(-1) * y, List);
    elif IsZero(x) then
        res := [[One(BaseDomain(x))]];
    else 
        xse := SemiEchelonMatTransformation(x);
        p := MutableCopyMat(TransposedMat(Matrix(xse.coeffs, Length(xse.heads), x)));
        RemoveZeroRows(p);
        p := TransposedMat(p);
        p := Matrix(PermutationMat(SortingPerm(Filtered(xse.heads, x -> x <> 0)), DimensionsMat(p)[1], BaseDomain(p)), p) * p;

        yse := SemiEchelonMatTransformation(y);
        q := MutableCopyMat(TransposedMat(Matrix(yse.coeffs, Length(yse.heads), y)));
        RemoveZeroRows(q);
        q := TransposedMat(q);
        q := Matrix(One(BaseDomain(q)) * PermutationMat(SortingPerm(Filtered(yse.heads, x -> x <> 0)), DimensionsMat(q)[1], BaseDomain(q)), q) * q;

        res := List(p * q^(-1), List);
    fi;

    return res;
end);

InstallGlobalFunction(MatrixObjLambdaConjugator,
function(S, x, y)
    
    Error("not implemented yet");
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
            if IsZero(m![5]) then
                Remove(m![5], i);
            fi;
        od;
    fi;
    return m![5];
end);

