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

InstallGlobalFunction(SEMIGROUPS_HashFunctionForPlistMatricesOverFiniteField,
function(x, data)
  local i, res;
  if DegreeOfMatrixOverFiniteField(x) = 0 then
    return 1;
  fi;
  res := 0;
  for i in [1 .. DegreeOfMatrixOverFiniteField(x)] do
    res := (res * 1001
            + ORB_HashFunctionForPlainFlatList(AsPlist(x!.mat[i]), data))
            mod data + 1;
  od;
  return res;
end);

InstallGlobalFunction(SEMIGROUPS_HashFunctionForPlistRowBasisOverFiniteField,
function(x, data)
  local i, res;
  if Rank(x) = 0 then
    return 1;
  fi;
  res := 0;
  for i in [1 .. Rank(x)] do
    res := (res * 1001
            + ORB_HashFunctionForPlainFlatList(AsPlist(x!.rows[i]), data))
            mod data + 1;
  od;
  return res;
end);

InstallGlobalFunction(SEMIGROUPS_HashFunctionForPlistVectorOverFiniteFields,
function(x, data)
  local i, res;
  if Length(x) = 0 then
    return 1;
  fi;
  res := 0;
  for i in [1 .. Length(x)] do
    res := (res * 1001
            + ORB_HashFunctionForPlainFlatList(AsPlist(x[i]!.vec), data))
            mod data + 1;
  od;
  return res;
end);

InstallGlobalFunction(SEMIGROUPS_HashFunctionForFFECollColl,
function(x, data)
  local i, res;
  if Length(x) = 0 then
    return 1;
  fi;
  res := 0;
  for i in [1 .. Length(x)] do
    res := (res * 1001 + ORB_HashFunctionForPlainFlatList(AsPlist(x[i]), data))
           mod data + 1;
  od;
  return res;
end);

InstallMethod(ChooseHashFunction,
"for collections of plist vector over finite fields",
[IsVectorOverFiniteFieldCollection, IsInt],
function(m, hashlen)
  return rec(func := SEMIGROUPS_HashFunctionForPlistVectorOverFiniteFields,
             data := hashlen);
end);

InstallMethod(ChooseHashFunction,
"for collections of ffeplist vector over finite fields",
[IsFFECollColl, IsInt],
function(m, hashlen)
  return rec(func := SEMIGROUPS_HashFunctionForFFECollColl,
             data := hashlen);
end);

InstallMethod(ChooseHashFunction, "for plist matrices over finite fields",
[IsPlistMatrixOverFiniteFieldRep, IsInt],
function(m, hashlen)
  return rec(func := SEMIGROUPS_HashFunctionForPlistMatricesOverFiniteField,
             data := hashlen);
end);

InstallMethod(ChooseHashFunction, "for plist rowbasis over finite fields",
[IsPlistRowBasisOverFiniteFieldRep, IsInt],
function(b, hashlen)
  return rec(func := SEMIGROUPS_HashFunctionForPlistRowBasisOverFiniteField,
             data := hashlen);
end);
