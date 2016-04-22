############################################################################
##
#W  reesmat.gi
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# TODO a proper method here

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
  local mat, elt, zero, i, j;

  mat := Matrix(R);
  elt := [];
  zero := false;

  for i in Rows(R) do
    for j in Columns(R) do
      if mat[j][i] = 0 then
        zero := true;
      else
        AddSet(elt, mat[j][i]);
      fi;
    od;
  od;

  if zero then
    return Concatenation([0], elt);
  fi;
  return elt;
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
    Error("Semigroups: ZeroSemigroupCons: usage,\n",
          "there is no Rees 0-matrix semigroup with only 1 element,");
    return;
  fi;
  mat := [[1 .. n - 1] * 0];
  return ReesZeroMatrixSemigroup(Group(()), mat);
end);

InstallMethod(RectangularBandCons,
"for a filter and a positive integer and positive integer",
[IsReesMatrixSemigroup and IsFinite, IsPosInt, IsPosInt],
function(filter, m, n)
  local id, mat;

  id := ();
  mat := List([1 .. n], x -> List([1 .. m], y -> id));
  return ReesMatrixSemigroup(Group(id), mat);
end);

#

InstallMethod(IsInverseSemigroup,
"for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(R)
  local U, mat, G, seen_col, mat_elts, seen_row_i, i, j;

  if not IsReesZeroMatrixSemigroup(R) then
    TryNextMethod();
  fi;

  U := UnderlyingSemigroup(R);
  mat := Matrix(R);

  if IsGroup(U) or HasIsGroupAsSemigroup(U) and IsGroupAsSemigroup(U) then
    G := U;
  else
    if HasIsInverseSemigroup(U) and not IsInverseSemigroup(U) then
      return false;
    fi;

    if HasIsRegularSemigroup(U) and not IsRegularSemigroup(U) then
      return false;
    fi;

    if HasIsMonoidAsSemigroup(U) and not IsMonoidAsSemigroup(U)
        and not IsMonoid(U) then
      return false;
    fi;

    if HasGroupOfUnits(U) and GroupOfUnits(U) = fail then
      return false;
    fi;
  fi;

  # Check that the matrix is square
  if Length(Columns(R)) <> Length(Rows(R)) then
    return false;
  fi;

  # Check that each row and column of mat contains exactly one non-zero entry
  # Also collect the non-zero entries of mat
  seen_col := BlistList([1 .. Length(mat[1])], []);
  mat_elts := [];
  for i in Columns(R) do
    seen_row_i := false;
    for j in Rows(R) do
      if mat[i][j] <> 0 then
        if seen_row_i or seen_col[j] then
          return false;
        fi;
        seen_row_i := true;
        seen_col[j] := true;
        AddSet(mat_elts, mat[i][j]);
      fi;
    od;
    if not seen_row_i then
      return false;
    fi;
  od;

  # Get the group of units
  if not IsBound(G) then
    G := GroupOfUnits(U);
    if G = fail then
      return false;
    fi;
  fi;

  # Get that mat is over G^0
  if ForAny(mat_elts, x -> not x in G) then
    return false;
  fi;

  return IsInverseSemigroup(U);
end);

InstallMethod(Idempotents,
"for an inverse Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup and IsInverseSemigroup],
function(R)
  local mat, I, J, star, e, k, out, i, j, x;

  if not IsReesZeroMatrixSemigroup(R) then
    TryNextMethod();
  fi;

  mat := Matrix(R);
  I := Rows(R);
  J := Columns(R);
  star := EmptyPlist(Length(I));
  for i in I do
    for j in J do
      if mat[j][i] <> 0 then
        star[i] := j;
        break;
      fi;
    od;
  od;

  e := Idempotents(UnderlyingSemigroup(R));
  k := 1;
  out := EmptyPlist(NrIdempotents(R));
  out[k] := MultiplicativeZero(R);

  for i in I do
    for x in e do
      k := k + 1;
      out[k] := RMSElement(R, i, x * mat[star[i]][i] ^ -1, star[i]);
    od;
  od;

  return out;
end);

# The following works for RZMS's over groups

InstallMethod(Idempotents,
"for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(R)
  local U, iso, inv, out, mat, i, j;

  if not IsReesZeroMatrixSemigroup(R) then
    TryNextMethod();
  fi;

  U := UnderlyingSemigroup(R);
  if IsGroup(U) then
    iso := IdentityMapping(U);
    inv := iso;
  elif IsGroupAsSemigroup(U) then
    iso := IsomorphismPermGroup(U);
    inv := InverseGeneralMapping(iso);
  else
    TryNextMethod();
  fi;

  out := EmptyPlist(NrIdempotents(R));
  out[1] := MultiplicativeZero(R);

  mat := Matrix(R);
  for i in Rows(R) do
    for j in Columns(R) do
      if mat[j][i] <> 0 then
        Add(out, RMSElement(R, i, ((mat[j][i] ^ iso) ^ -1) ^ inv, j));
      fi;
    od;
  od;

  return out;
end);

#

InstallMethod(NrIdempotents,
"for an inverse Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup and IsInverseSemigroup],
function(R)
  if not IsReesZeroMatrixSemigroup(R) then
    TryNextMethod();
  fi;
  return NrIdempotents(UnderlyingSemigroup(R)) * Length(Rows(R)) + 1;
end);

# The following works for RZMS's over groups

InstallMethod(NrIdempotents,
"for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(R)
  local U, count, mat, i, j;

  if not IsReesZeroMatrixSemigroup(R) then
    TryNextMethod();
  fi;

  U := UnderlyingSemigroup(R);
  if not IsGroup(U) and not IsGroupAsSemigroup(U) then
    TryNextMethod();
  fi;

  count := 1;

  mat := Matrix(R);
  for i in Rows(R) do
    for j in Columns(R) do
      if mat[j][i] <> 0 then
        count := count + 1;
      fi;
    od;
  od;

  return count;
end);
