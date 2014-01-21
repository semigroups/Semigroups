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

# This certainly is a collection of associative elements under the assumption
# that the multiplication only has to be associative if it works.
InstallTrueMethod(IsAssociativeElementCollection
        , IsMatrixSemigroupElementCollection);

# Note that for technical reasons we cannot
# use IsMatrixObjCollection here.
InstallMethod(SemigroupByGenerators,
        "for a list of matrices",
        [IsHomogeneousList and IsRingElementCollCollColl],
function( gens )
    if Length(gens) > 0 and
       IsMatrixObj(gens[1]) and
       IsAssociativeElement(gens[1]) then
        # Make a matrix semigroup
        Info(InfoSemigroups, 2, "creating matrix semigroup");
        return InternalSemigroupByGenerators(gens, SemigroupsOptionsRec);
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
## This is InstallOtherMethod because a list of matrices
## is not in IsAssociativeElementCollection
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
        return One;
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
    return RankMat(x);
end);
InstallMethod(ActionRank,
        "for a matrix semigroup with generators",
        [IsMatrixSemigroup and HasGeneratorsOfSemigroup],
function( s )
    return RankMat;
end);

InstallMethod(MinActionRank,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        x -> 0);
InstallMethod(LambdaOrbOpts,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        s -> rec(forflatplainlists := true));
InstallMethod(RhoOrbOpts,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        s -> rec(forflatplainlists := true));

InstallMethod(LambdaAct,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
function(S)
    Error("not implemented yet\n");
end);

InstallMethod(RhoAct,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
function(S)
    Error("not implemented yet\n");
end);

InstallMethod(LambdaOrbSeed,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        s -> [0]);

InstallMethod(RhoOrbSeed,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        s -> [0]);

InstallMethod(LambdaFunc,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

InstallMethod(RhoFunc,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

InstallMethod(LambdaRank,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

InstallMethod(RhoRank,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

InstallMethod(LambdaInverse,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

InstallMethod(RhoInverse,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
end);

InstallMethod(LambdaPerm,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        function(S)
    Error("not implemented yet\n");
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
