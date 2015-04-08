############################################################################
##
#W  reesmat.gi
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(IsGeneratorsOfInverseSemigroup,
"for a collection of Rees 0-matrix semigroup elements",
[IsReesZeroMatrixSemigroupElementCollection], ReturnFalse);

#

InstallMethod(ViewString,
"for a Rees 0-matrix subsemigroup ideal with ideal generators",
[IsReesZeroMatrixSubsemigroup and IsSemigroupIdeal and
HasGeneratorsOfSemigroupIdeal],
function(I)
  local str, nrgens;

  str := "\><";

  if HasIsTrivial(I) and IsTrivial(I) then
    Append(str, "\>trivial\< ");
  else
    if HasIsCommutative(I) and IsCommutative(I) then
      Append(str, "\>commutative\< ");
    fi;
  fi;

  if HasIsTrivial(I) and IsTrivial(I) then
  elif HasIsZeroSimpleSemigroup(I) and IsZeroSimpleSemigroup(I) then
    Append(str, "\>0-simple\< ");
  elif HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I) then
    Append(str, "\>simple\< ");
  fi;

  if HasIsInverseSemigroup(I) and IsInverseSemigroup(I) then
    Append(str, "\>inverse\< ");
  elif HasIsRegularSemigroup(I)
   and not (HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I)) then
    if IsRegularSemigroup(I) then
      Append(str, "\>regular\< ");
    else
      Append(str, "\>non-regular\< ");
    fi;
  fi;

  Append(str, "\>Rees\< \>0-matrix\< \>semigroup\< \>ideal\< ");
  Append(str, "\<with\> ");

  nrgens := Length(GeneratorsOfSemigroupIdeal(I));
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens > 1 or nrgens = 0 then
    Append(str, "s\<");
  else
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

#

InstallMethod(MatrixEntries, "for a Rees matrix semigroup",
[IsReesMatrixSemigroup],
function(R)
  return Union(Matrix(R){Columns(R)}{Rows(R)});
  # in case R is a proper subsemigroup of another RMS
end);

InstallMethod(MatrixEntries, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  return Union(Matrix(R){Columns(R)}{Rows(R)});
end);

#

InstallMethod(GreensHClassOfElement, "for a RZMS, pos int, and pos int",
[IsReesZeroMatrixSemigroup, IsPosInt, IsPosInt],
function(R, i, j)
  local rep;

  rep := RMSElement(R, i, Representative(UnderlyingSemigroup(R)), j);
  return GreensHClassOfElement(R, rep);
end);

#

if not IsGrapeLoaded then
  InstallMethod(RZMSGraph, "for a RZMS", [IsReesZeroMatrixSemigroup],
  function(R)
    Info(InfoWarning, 1, GrapeIsNotLoadedString);
    return fail;
  end);

else

  InstallMethod(RZMSGraph, "for a RZMS", [IsReesZeroMatrixSemigroup],
  function(R)
    local mat, n, m, adj;

    mat := Matrix(R);
    n := Length(mat);
    m := Length(mat[1]);

    adj := function(x, y)
      if x <= m and y > m then
        return not mat[y - m][x] = 0;
      elif x > m and y <= m then
        return not mat[x - m][y] = 0;
      else
        return false;
      fi;
    end;

    return Graph(Group(()), [1 .. n + m], OnPoints, adj, true);
  end);

fi;

#

InstallMethod(ZeroSemigroupCons,
"for a filter and a positive integer",
[IsReesZeroMatrixSemigroup and IsFinite, IsPosInt],
function(filter, n)
  local mat;

  if n = 1 then
    Error("Semigroups: ZeroSemigroupCons: usage:\n",
          "there is no Rees 0-matrix semigroup of order 1,");
    return;
  fi;
  mat := [[1 .. n - 1] * 0];
  return ReesZeroMatrixSemigroup(Group(()), mat);
end);

InstallMethod(IsInverseSemigroup,
"for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  local U, mat, n, seen, mat_elts, G, i, j;

  U := UnderlyingSemigroup(R);
  mat := Matrix(R);

  if HasIsInverseSemigroup(U) and not IsInverseSemigroup(U) then
    return false;
  fi;

  if HasIsRegularSemigroup(U) and not IsRegularSemigroup(U) then
    return false;
  fi;

  if HasIsMonoidAsSemigroup(U) and (not IsMonoidAsSemigroup(U)) then
    return false;
  fi;

  if HasGroupOfUnits(U) and GroupOfUnits(U) = fail then
    return false;
  fi;

  # Check that the matrix is square
  n := Length(mat);
  if Length(mat[1]) <> n then
    return false;
  fi;

  # Check that each column of mat contains exactly one non-zero entry
  # Also collect the non-zero entries of mat
  seen := BlistList([1 .. n], []);
  mat_elts := [];
  for i in [1 .. n] do
    for j in [1 .. n] do
      if mat[j][i] <> 0 then
        if seen[i] then
          return false;
        else
          seen[i] := true;
          AddSet(mat_elts, mat[j][i]);
        fi;
      fi;
    od;
    if not seen[i] then
      return false;
    fi;
  od;

  # Check that each row of mat contains exactly one non-zero entry
  seen := BlistList([1 ..n], []);
  for j in [1 .. n] do
    for i in [1 .. n] do
      if mat[j][i] <> 0 then
        if seen[j] then
          return false;
        else
          seen[j] := true;
        fi;
      fi;
    od;
    if not seen[j] then
      return false;
    fi;
  od;

  # Get the group of units
  G := GroupOfUnits(U);
  if G = fail then
    return false;
  fi;

  # Get that mat is over G^0
  if ForAny(mat_elts, x -> not x in G) then
    return false;
  fi;

  return IsInverseSemigroup(U);
end);
