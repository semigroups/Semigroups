############################################################################
##
#W  semimat.gi
#Y  Copyright (C) 2013                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## Some of this code is taken from grpmat.gi from the GAP library

InstallMethod(OneMutable, "for ring element coll coll coll",
[IsRingElementCollCollColl], x-> One(Representative(x)));

InstallMethod(IsGroupAsSemigroup, "for a matrix semigroup",
[IsMatrixSemigroup],
s-> IsGroupAsSemigroup(Range(IsomorphismTransformationSemigroup(s))));



#############################################################################
##
#M  ViewObj( <matgrp> )
##

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


