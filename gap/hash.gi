############################################################################
##
#W  hash.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#T these are basically stolen from cvec
InstallGlobalFunction( SEMIG_HashFunctionForPlistVects,
function(x,data)
    return ORB_HashFunctionForPlainFlatList(x![2], data);
end);

InstallGlobalFunction( SEMIG_HashFunctionForPlistMats,
function(x,data)
    local i,res;
    if DimensionsMat(x)[1] = 0 then
        return 1;
    fi;
    res := 0;
    for i in [1 .. DimensionsMat(x)[1]] do
        res := (res * 1001 + SEMIG_HashFunctionForPlistVects(x[i], data))
             mod data + 1;
    od;
    return res;
end );

InstallGlobalFunction( SEMIG_HashFunctionForFiniteDimensionalVectorSpaces,
function(x,data)
    local i,basis,res;
    #T This is an attribute and is thus only computed once
    #T If we cannot compute a canonical basis we are stuffed atm.
    basis := CanonicalBasis(x);
    res := 0;
    #T This looks magic bus isn't, see matobjplist.gd
    #T the entries of PlistMatrixRep and PlistVectorRep are defined there
    if Length(basis) = 0 then
        res := 1;
    else
        for i in [1 .. Length(basis)] do
            res := (res * 1001 + SEMIG_HashFunctionForPlistVects(basis[i],data))
                mod data + 1;
        od;
    fi;

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
                data := hashlen );
end );

# Note that this can only be used to hash subspaces of F^n for some
# previously fixed F and n
InstallMethod( ChooseHashFunction, "for finite dimensional vector spaces",
    [IsVectorSpace, IsInt],
function(vsp, hashlen)
    return rec( func := SEMIG_HashFunctionForFiniteDimensionalVectorSpaces,
                data := hashlen );
end);

