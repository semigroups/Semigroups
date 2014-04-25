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

InstallMethod(FakeOne, "for an FFE coll coll coll",
[IsFFECollCollColl], One);

# Under the assumption that rank mat >= dim V compute
# a matrix N such that mat * N = id_V
#
#T maybe do this "by hand"
#T in particular we can probably not rely
#T on SemiEchelonMatDestructive actually leaving
#T the semiechelon form in W
InstallMethod(LambdaInverse,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    return function( V, mat )
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
    end;
end);

InstallMethod(RhoInverse,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

#T returns an invertible matrix
InstallMethod(LambdaPerm,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    return function(x, y)
        local xse, xhe, yse, yhe, he, p, q, i, RemoveZeroRows;
        
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
            p := List(x^(-1) * y, List);
            Print("p is ", IsMatrixObj(p), "\n");
            return List(x^(-1) * y, List);
        fi;
        
        if IsZero(x) then
            return [[One(BaseDomain(x))]];
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

            p := List(p*q^(-1), List);
            Print("p is ", IsMatrixObj(p), "\n");
            return List(p*q^(-1), List);
        fi;
    end;
end);

InstallMethod(LambdaConjugator,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error
      ("not implemented yet\n");
end);

InstallMethod(IdempotentTester,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

InstallMethod(IdempotentCreator,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

InstallMethod(StabilizerAction,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

#############################################################################
##
#M  ViewObj( <matgrp> )
##
#T Print additional info about base domain and matrix size

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
#M  PrintObj( <matgrp> )
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

InstallMethod( CanonicalRowSpace,
        "for a matrix object over a finite field",
        [ IsMatrixObj and IsFFECollColl ],
function( m )
    local i, n;

    if not IsBound(m![5]) then
        Info(InfoMatrixSemigroups, 2, "CanonicalRowSpace called\n");
        
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

