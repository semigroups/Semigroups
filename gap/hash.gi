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

InstallGlobalFunction( SEMIGROUPS_HashFunctionForPlistSMatrices,
function(x, data)
    local i,res;
    if DegreeOfSMatrix(x) = 0 then
        return 1;
    fi;
    res := 0;
    for i in [1 .. DegreeOfSMatrix(x)] do
        res := (res * 1001 + ORB_HashFunctionForPlainFlatList(x!.mat[i], data))
             mod data + 1;
    od;
    return res;
end);

InstallMethod( ChooseHashFunction, "for plist s-matrices",
[IsPlistSMatrixRep, IsInt],
function(m, hashlen)
  return rec( func := SEMIGROUPS_HashFunctionForPlistSMatrices,
              data := hashlen );
end);
