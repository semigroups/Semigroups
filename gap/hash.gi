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
  local h;

  if DimensionOfMatrixOverSemiring(x) = 0 then
    return 1;
  elif IsInt(data.data) then 
    h := ChooseHashFunction(AsList(x), data.data);
    data.func := h.func;
    data.data := h.data;
  fi;

  return data.func(AsList(x), data.data);
end);

InstallGlobalFunction(SEMIGROUPS_HashFunctionForPlistRowBasisOverFiniteField,
function(x, data)
  local h;

  if Rank(x) = 0 then
    return 1;
  elif IsInt(data.data) then 
    h := ChooseHashFunction(x!.rows, data.data);
    data.func := h.func;
    data.data := h.data;
  fi;
  return data.func(x!.rows, data.data);
end);

InstallMethod(ChooseHashFunction, "for plist matrices over finite fields",
[IsPlistMatrixOverFiniteFieldRep, IsInt],
function(x, hashlen)
  local data;

  if DimensionOfMatrixOverSemiring(x) <> 0 then 
    data := ChooseHashFunction(AsList(x), hashlen);
  else 
    data := hashlen;
  fi;
  return rec(func := SEMIGROUPS_HashFunctionForPlistMatricesOverFiniteField,
             data := ChooseHashFunction(AsList(x), hashlen));
end);

InstallMethod(ChooseHashFunction, "for plist rowbasis over finite fields",
[IsPlistRowBasisOverFiniteFieldRep, IsInt],
function(x, hashlen)
  local data;

  if Rank(x) <> 0 then 
    data := ChooseHashFunction(x!.rows, hashlen);
  else 
    data := hashlen;
  fi;
  return rec(func := SEMIGROUPS_HashFunctionForPlistRowBasisOverFiniteField,
             data := data);
end);
