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
        Error("not implemented\n");
    else
        TryNextMethod();
    fi;
end);

InstallMethod(OneMutable, "for ring element coll coll coll",
[IsRingElementCollCollColl], x-> One(Representative(x)));

InstallMethod(IsGroupAsSemigroup, "for a matrix semigroup",
[IsMatrixSemigroup],
s-> IsGroupAsSemigroup(Range(IsomorphismTransformationSemigroup(s))));



#############################################################################
##
#M  Methods for acting semigroups setup
##
#############################################################################
##

InstallTrueMethod(IsMatrixSemigroup, IsActingSemigroup);
#InstallMethod(IsGeneratorsOfActingSemigroup,
#        "for a matrix semigroup",
#        [ IsMatrixSemigroupElementCollection ],
#        function(coll) return true; end);

#T What is a sensible notion of degree here?
InstallMethod(ActionDegree,
        "for a matrix object",
        # MatrixObj are not per default IsAssociativeElement
        [IsMatrixObj and IsAssociativeElement],
        x -> 42);
InstallOtherMethod(ActionDegree,
        "for a matrix object collection",
        # note that we have to ensure associativity here
        # and the type of collection
        [IsHomogeneousList],
        x->42);
#InstallMethod(ActionDegree,
#        "for a matrix semigroup with generators",
#        [IsMatrixSemigroup and HasGeneratorsOfSemigroup],
#        x->42);

InstallMethod(ActionRank,
        "for a matrix object",
        [IsMatrixObj and IsAssociativeElement, IsInt],
function(x,y) 
    Error("not implemented\n");
end);
InstallMethod(ActionRank,
        "for a matrix semigroup with generators",
        [IsMatrixSemigroup and HasGeneratorsOfSemigroup],
function( S )
    Error("not implemented\n");
end);

InstallMethod(MinActionRank,
        "for a matrix semigroup",
        [IsMatrixSemigroup],
        x -> 1);
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
    local gens;
        gens := GeneratorsOfSemigroup(S);
        Print("<matrix semigroup");
        if HasSize(S) then
            Print(" of size ",Size(S));
        fi;
        Print(" with ",Length(GeneratorsOfSemigroup(S)),
              " generators>");
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


