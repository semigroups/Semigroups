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

#InstallMethod(IsMatrixObjCollection, "for a homo. list ",
#[IsHomogeneousList and IsRingElementCollCollColl],
#function(l)
#  if IsMatrixObj(l[1]) then 
#    if IsAssociativeElement(l[1]) then 
#      SetFilterObj(l, IsAssociativeElementCollection);
#    fi;
#    return true;
#  fi;
#  return false; 
#end);

# This certainly is a collection of associative elements under the assumption
# that the multiplication only has to be associative if it works.
#InstallTrueMethod(IsAssociativeElementCollection
#        , IsMatrixSemigroupElementCollection);

#InstallMethod(SemigroupByGenerators,
#        "for a list of matrices",
#        [IsHomogeneousList and IsRingElementCollCollColl],
#function(gens)
#    if IsGeneratorsOfActingSemigroup(gens) then
#        return InternalSemigroupByGenerators(gens, SemigroupsOptionsRec);
#    else
#        TryNextMethod();
#    fi;
#end);

# Note that for technical reasons we cannot
# use IsMatrixObjCollection here.
#InstallMethod(SemigroupByGenerators,
#        "for a list of matrices",
#        [IsHomogeneousList and IsRingElementCollCollColl, IsRecord],
#function( gens, opt )
#    if IsMatrixObj(gens[1]) and
#       IsAssociativeElement(gens[1]) then
#        # Make a matrix semigroup
#        Info(InfoSemigroups, 2, "creating matrix semigroup");
#        return InternalSemigroupByGenerators(gens, opt);
#    else
#        TryNextMethod();
#    fi;
#end);

#T Hack?
#InstallTrueMethod(IsAssociativeElementCollection, IsMatrixSemigroup);

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

## Degree is the number of rows/columns.
#T It is not checked yet that the dimensions
#T of the matrix match
#InstallMethod(ActionDegree,
#    "for a matrix object",
#    # MatrixObj are not per default IsAssociativeElement
#    [IsMatrixObj and IsAssociativeElement],
#    RowLength);

#InstallOtherMethod(ActionDegree,
#    "for a matrix object collection",
#    # note that we have to ensure associativity here
#    # and the type of collection
#    [IsHomogeneousList and IsRingElementCollCollColl],
#function(coll)
#    if IsGeneratorsOfActingSemigroup(coll) then
#        return RowLength(coll[1]);
#    else
#        #T Error() here?
#        return fail;
#    fi;
#end);
#InstallMethod(ActionDegree,
#        "for a matrix semigroup with generators",
#        [IsMatrixSemigroup],
#function(S)
#    return ActionDegree(GeneratorsOfSemigroup(S));
#end);



InstallMethod(MinActionRank,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        x -> 0);
InstallMethod(LambdaOrbOpts,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        s -> rec());
InstallMethod(RhoOrbOpts,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        s -> rec());

InstallMethod(LambdaAct,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
function(S)
    # returns the right action on subspaces of F^n by right multiplication
    return
      function(vsp, mat)
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
      end;
end);

InstallMethod(RhoAct,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
function(S)
    return
      #T I checked, these arguments should be this way around
      function(vsp, mat)
        local basis, nvsp;
        
        if RowLength(vsp) > RowLength(mat) then
            nvsp := mat;
        else
            nvsp := vsp * TransposedMat(mat);
        fi;
        nvsp := MutableCopyMat(nvsp);
        
        SemiEchelonMatDestructive(nvsp);
        return nvsp;
      end;
end);

InstallMethod(LambdaOrbSeed,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        # row space of matrix
        # This is still a hack.
function(s)
    local rep;
    rep := Representative(s);
    
    return NewZeroMatrix(IsPlistMatrixRep, BaseDomain(rep), RowLength(rep)+1, RowLength(rep)+1);
end);

InstallMethod(RhoOrbSeed,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
function(s)
    local rep;
    rep := Representative(s);
    
    return NewZeroMatrix(IsPlistMatrixRep, BaseDomain(rep), RowLength(rep)+1, RowLength(rep)+1);
end);

InstallMethod(LambdaFunc,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    # a function that returns the row space
    return
      function(mat)
        local i, n, nvsp;

        Info(InfoMatrixSemigroups, 2, "LambdaFunc for IsMatrixSemigroup called\n");
        
        nvsp := MutableCopyMat(mat);
        TriangulizeMat(nvsp);
        
        n := DimensionsMat(nvsp)[1];
        
        for i in [n,n-1..1] do
            if IsZero(nvsp[i]) then
                Remove(nvsp, i);
            fi;
        od;
        return nvsp;
      end;
end);

InstallMethod(RhoFunc,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    # a function that returns the column space
    return
      function(mat)
        local i, n, nvsp, se;
        
        Info(InfoMatrixSemigroups, 2, "RhoFunc for IsMatrixSemigroup called\n");
        
        return LambdaFunc(S)(TransposedMat(mat));
      end;
end);

InstallMethod(LambdaRank,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    # returns a function that
    # returns the row rank of
    # it's input
    return (x -> Length(SemiEchelonMat(x).heads));
end);

InstallMethod(RhoRank,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    # rank of column space
    return (x -> Length(SemiEchelonMat(x).heads));
end);


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
        Print("<matrix semigroup ");
        #if HasSize(S) then
        #   Print(" of size ",Size(S));
        #fi;
        Print(dims[1], "x", dims[2]);
        Print(" over ", BaseDomain(gens[1]));
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
