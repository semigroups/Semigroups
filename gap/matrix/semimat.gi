############################################################################
##
#W  semimat.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#
#T This will be moved to a more appropriate place
#

DeclareGlobalFunction( "SEMIG_HashFunctionForPlistVects" );
DeclareGlobalFunction( "SEMIG_HashFunctionForPlistMats" );

#T these are basically stolen from cvec
InstallGlobalFunction( SEMIG_HashFunctionForPlistVects,
function(x,data)
    return ORB_HashFunctionForPlainFlatList(x, data);
end);

InstallGlobalFunction( SEMIG_HashFunctionForPlistMats,
function(x,data)
    local i,res;
    res := 0;
    #T This looks magic bus isn't, see matobjplist.gd
    #T the entries of PlistMatrixRep and PlistVectorRep are defined there
    for i in [1..x![3]] do
        res := (res * 1001 + SEMIG_HashFunctionForPlistVects(x![4][i]![2],data[1]))
             mod data[1]+1;
    od;
    return res;
end );

InstallMethod( ChooseHashFunction, "for plain list vector objects",
    [IsPlistVectorRep, IsInt],
function(vec, hashlen)
    return rec( func := ORB_HashFunctionForPlainFlatList,
                data := hashlen );
end);

InstallMethod( ChooseHashFunction, "for plain list matrix objects",
    [IsPlistMatrixRep, IsInt],
function(mat, hashlen)
    local bytelen;
    bytelen := (GAPInfo.BytesPerVariable * (mat![3] + 1) mod hashlen) + 1;
    return rec( func := SEMIG_HashFunctionForPlistMats,
                data := [hashlen,bytelen] );
end );

# This certainly is a collection of associative elements under the assumption
# that the multiplication only has to be associative if it works.
InstallTrueMethod(IsAssociativeElementCollection
        , IsMatrixSemigroupElementCollection);

InstallMethod(SemigroupByGenerators,
        "for a list of matrices",
        [IsHomogeneousList and IsRingElementCollCollColl],
function(gens)
    if IsGeneratorsOfActingSemigroup(gens) then
        return InternalSemigroupByGenerators(gens, SemigroupsOptionsRec);
    else
        TryNextMethod();
    fi;
end);

# Note that for technical reasons we cannot
# use IsMatrixObjCollection here.
InstallMethod(SemigroupByGenerators,
        "for a list of matrices",
        [IsHomogeneousList and IsRingElementCollCollColl, IsRecord],
function( gens, opt )
    if IsMatrixObj(gens[1]) and
       IsAssociativeElement(gens[1]) then
        # Make a matrix semigroup
        Info(InfoSemigroups, 2, "creating matrix semigroup");
        return InternalSemigroupByGenerators(gens, opt);
    else
        TryNextMethod();
    fi;
end);

#T Hack?
InstallTrueMethod(IsAssociativeElementCollection, IsMatrixSemigroup);

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


#############################################################################
##
#M IsGeneratorsOfActingSemigroups
##
#############################################################################
##
## A homogenous collection of matrices of the same size
##
## An empty list does not generate a very well defined
## matrix semigroup in this setting so we don't allow
## empty generating sets
#
InstallMethod(IsGeneratorsOfActingSemigroup,
    "for a list of matrices (special case)",
    [IsHomogeneousList and IsRingElementCollCollColl],
function( gens )
    local m, dims;

    if (Length(gens) > 0) and
       (IsMatrixObj(gens[1])) and
       (IsAssociativeElement(gens[1])) then
        dims := DimensionsMat(gens[1]);

        if dims[1] <> dims[2] then return false; fi;
        for m in gens do
            if DimensionsMat(m) <> dims then
                return false;
            fi;
        od;
        return true;
    else
        return false;
    fi;
end);

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

InstallMethod(FakeOne,
    "for a collection of elements of a matrix semigroup",
    [IsMatrixSemigroupElementCollection],
    One);

## Degree is the number of rows/columns.
#T It is not checked yet that the dimensions
#T of the matrix match
InstallMethod(ActionDegree,
    "for a matrix object",
    # MatrixObj are not per default IsAssociativeElement
    [IsMatrixObj and IsAssociativeElement],
    RowLength);
InstallOtherMethod(ActionDegree,
    "for a matrix object collection",
    # note that we have to ensure associativity here
    # and the type of collection
    [IsHomogeneousList and IsRingElementCollCollColl],
function(coll)
    if IsGeneratorsOfActingSemigroup(coll) then
        return RowLength(coll[1]);
    else
        #T Error() here?
        return fail;
    fi;
end);
InstallMethod(ActionDegree,
        "for a matrix semigroup with generators",
        [IsMatrixSemigroup],
function(S)
    return ActionDegree(GeneratorsOfSemigroup(S));
end);

InstallMethod(ActionRank,
        "for a matrix object and a positive integer",
        [IsMatrixObj and IsAssociativeElement, IsInt],
function(x, y)
    #T I assume that y is supposed to be a larger enclosing
    ## thing in certain cases (transformations on n points)?
    Error("not correctly implemetned");
end);
InstallMethod(ActionRank,
        "for a matrix semigroup with generators",
        [IsMatrixSemigroup and HasGeneratorsOfSemigroup],
function( s )
    Error("not correctly implemetned");
end);

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
        local basis;
        
        if vsp = [[]] then
            basis := Rows(mat);
        else
            #T is CanonicalBasis better here?
            basis := BasisVectors(Basis(vsp));
        
            #T hack.
            if basis = [] then
                basis := [Zero(vsp)];
            fi;
        fi;
        return VectorSpace(BaseDomain(mat), basis * mat);
      end;
end);

InstallMethod(RhoAct,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
function(S)
    return
      #T I checked, these arguments should be this way around
      function(vsp, mat)
        local basis;
        
         if vsp = [[]] then
            basis := Rows(mat);
        else
            #T is CanonicalBasis better here?
            basis := BasisVectors(Basis(vsp));
        
            #T hack.
            if basis = [] then
                basis := [Zero(vsp)];
            fi;
        fi;
        return VectorSpace(BaseDomain(mat), basis * TransposedMat(mat));
      end;
end);

InstallMethod(LambdaOrbSeed,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        # row space of matrix
        s -> [ [] ] );

InstallMethod(RhoOrbSeed,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        s -> [ [] ] );

InstallMethod(LambdaFunc,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    # a function that returns the row space
    return
      function(mat)
        #T This will only work for fields 
        #T at the moment the Matrix functions
        #T Don't check that their entries actually
        #T lie in the base domain. This leads to
        #T problems. Best way of solving this is 
        #T to implement checks.
        return(VectorSpace(BaseDomain(mat), Rows(mat)));
      end;
end);

InstallMethod(RhoFunc,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    # a function that returns the column space
    return
      function(mat)
        #T This will only work for fields 
        #T at the moment the Matrix functions
        #T Don't check that their entries actually
        #T lie in the base domain. This leads to
        #T problems. Best way of solving this is 
        #T to implement checks.
        return(VectorSpace(BaseDomain(mat), Rows(mat)));
      end;
end);

InstallMethod(LambdaRank,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    # returns a function that
    # returns the row rank of
    # it's input
    return (x -> Dimension(x));
end);

InstallMethod(RhoRank,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    # rank of column space
    Error("not implemented yet\n");
end);

InstallMethod(LambdaInverse,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    # Returns a function that for 
    Error("not implemented yet\n");
end);

InstallMethod(RhoInverse,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

#T returns a permutation
InstallMethod(LambdaPerm,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    return function(x, y)
        return ();
    end;
end);

InstallMethod(LambdaConjugator,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
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
    "for a matrix semigroup with stored generators",
    [ IsMatrixSemigroup and HasGeneratorsOfSemigroup ],
function(S)
    local gens, dims;
        gens := GeneratorsOfSemigroup(S);
        Print("<matrix semigroup");
        if HasSize(S) then
            Print(" of size ",Size(S));
        fi;
        if IsMatrixObj(gens[1]) then
            dims := DimensionsMat(gens[1]);
            Print(" generated by ", Length(GeneratorsOfSemigroup(S)));
            Print(" ", dims[1], "x", dims[2]);
            Print(" matrices over ", BaseDomain(gens[1]));
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
