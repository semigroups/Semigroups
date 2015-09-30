############################################################################
##
#W  reesmat.gi
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains methods for every operation/attribute/property that is
# specific to Rees 0-matrix semigroups.

InstallGlobalFunction(RMSElementNC,
function(R, i, g, j)
  return Objectify(TypeReesMatrixSemigroupElements(R),
                   [i, g, j, Matrix(R)]);
end);

InstallImmediateMethod(IsFinite, IsReesZeroMatrixSubsemigroup, 0,
function(R)
  if ElementsFamily(FamilyObj(R))!.IsFinite then
    return true;
  fi;
  TryNextMethod();
end);

InstallMethod(ViewString, "for a Rees matrix semigroup element",
[IsReesMatrixSemigroupElement],
function(x)
  return Concatenation("(", String(x![1]), ",", String(x![2]), ",",
                       String(x![3]), ")");
end);

#

InstallMethod(ViewString, "for a Rees 0-matrix semigroup element",
[IsReesZeroMatrixSemigroupElement],
function(x)
  if x![1] = 0 then
    return "0";
  fi;
  return Concatenation("(", ViewString(x![1]), ",", ViewString(x![2]), ",",
                       ViewString(x![3]), ")");
end);

InstallMethod(MultiplicativeZero, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSubsemigroup],
function(R)
  local rep, zero;

  rep := Representative(R);
  zero := MultiplicativeZero(ReesMatrixSemigroupOfFamily(FamilyObj(rep)));
  if IsReesMatrixSemigroup(R) or zero in R then
    return zero;
  fi;
  return fail;
end);

# same method for ideals

InstallMethod(IsomorphismPermGroup,
"for a subsemigroup of a Rees 0-matrix semigroup",
[IsReesZeroMatrixSubsemigroup],
function(S)
  local rep;

  if not IsGroupAsSemigroup(S)  then
    ErrorMayQuit("Semigroups: IsomorphismPermGroup: usage,\n",
                 "the argument <S> must be a subsemigroup of a Rees 0-matrix ",
                 "semigroup satisfying IsGroupAsSemigroup,");
  fi;

  rep := Representative(S);
  if rep![1] = 0 then # special case for the group consisting of 0
    return MagmaIsomorphismByFunctionsNC(S, Group(()), x -> (), x -> rep);
  fi;

  # gaplint: ignore 4
  return MagmaIsomorphismByFunctionsNC(S,
           Group(List(GeneratorsOfSemigroup(S), x -> x![2])),
           x -> x![2],
           x -> RMSElement(S, rep![1], x, rep![3]));
end);

# same method for ideals

InstallMethod(GroupOfUnits, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(S)
  local R, G, i, j, U;

  if MultiplicativeNeutralElement(S) = fail then
    return fail;
  fi;

  R := GreensRClassOfElementNC(S, MultiplicativeNeutralElement(S));
  G := SchutzenbergerGroup(R);
  i := MultiplicativeNeutralElement(S)![1];
  j := MultiplicativeNeutralElement(S)![3];

  U := Semigroup(List(GeneratorsOfGroup(G), x -> RMSElement(S, i, x, j)));

  SetIsGroupAsSemigroup(U, true);
  UseIsomorphismRelation(U, G);

  return U;
end);

# this method is better than the generic one for acting semigroups
#FIXME double check this isn't already in the library

InstallMethod(Random, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
3, # to beat the method for regular acting semigroups
function(R)
  return Objectify(TypeReesMatrixSemigroupElements(R),
                   [Random(Rows(R)), Random(UnderlyingSemigroup(R)),
                    Random(Columns(R)), Matrix(ParentAttr(R))]);
end);

# this method is just a copy of the library method in GAP 4.7.5 with the extra
# line GeneratorsOfSemigroup, so that the correct (i.e. acting) methods
# are used for ReesZeroMatrixSemigroups when the package is loaded.

#FIXME double check this isn't already in the library
#FIXME check this is still necessary, are RZM semigroups acting?
InstallMethod(ReesZeroMatrixSemigroup, "for a semigroup and a dense list",
[IsSemigroup, IsDenseList],
function(S, mat)
  local fam, R, type, x;

  if not ForAll(mat, x -> IsDenseList(x) and Length(x) = Length(mat[1])) then
    ErrorMayQuit("Semigroups: ReesZeroMatrixSemigroup: usage,\n",
                 "<mat> must be a list of dense lists of equal length,");
  fi;

  for x in mat do
    if ForAny(x, s -> not (s = 0 or s in S)) then
      ErrorMayQuit("Semigroups: ReesZeroMatrixSemigroup: usage,\n",
                   "the entries of <mat> must be 0 or belong to <S>,");
    fi;
  od;

  fam := NewFamily("ReesZeroMatrixSemigroupElementsFamily",
                   IsReesZeroMatrixSemigroupElement);

  if HasIsFinite(S) then
    fam!.IsFinite := IsFinite(S);
  else
    fam!.IsFinite := false;
  fi;

  # create the Rees matrix semigroup
  R := Objectify(NewType(CollectionsFamily(fam),
                 IsWholeFamily
                 and IsReesZeroMatrixSubsemigroup
                 and IsAttributeStoringRep), rec());

  # store the type of the elements in the semigroup
  type := NewType(fam, IsReesZeroMatrixSemigroupElement);

  fam!.type := type;
  SetTypeReesMatrixSemigroupElements(R, type);
  SetReesMatrixSemigroupOfFamily(fam, R);

  SetMatrix(R, mat);
  SetUnderlyingSemigroup(R, S);
  SetRows(R, [1 .. Length(mat[1])]);
  SetColumns(R, [1 .. Length(mat)]);
  SetMultiplicativeZero(R,
                        Objectify(TypeReesMatrixSemigroupElements(R), [0]));

  # cannot set IsZeroSimpleSemigroup to be <true> here since the matrix may
  # contain a row or column consisting entirely of 0s!

  #if HasIsFinite(S) then
  #  SetIsFinite(R, IsFinite(S));
  #fi;

  GeneratorsOfSemigroup(R);
  SetIsSimpleSemigroup(R, false);
  return R;
end);

#

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
[IsReesZeroMatrixSemigroup], x -> Union(Matrix(x){Columns(x)}{Rows(x)}));

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
    ErrorMayQuit("Semigroups: ZeroSemigroupCons: usage:\n",
                 "there is no Rees 0-matrix semigroup of order 1,");
  fi;
  mat := [[1 .. n - 1] * 0];
  return ReesZeroMatrixSemigroup(Group(()), mat);
end);

InstallMethod(IsInverseSemigroup,
"for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(R)
  local U, mat, seen_col, mat_elts, seen_row_i, G, i, j;

  if not IsReesZeroMatrixSemigroup(R) then
    TryNextMethod();
  fi;

  U := UnderlyingSemigroup(R);

  if (HasIsInverseSemigroup(U) and not IsInverseSemigroup(U))
      or (HasIsRegularSemigroup(U) and not IsRegularSemigroup(U))
      or (HasIsMonoidAsSemigroup(U) and (not IsMonoidAsSemigroup(U)))
      or (HasGroupOfUnits(U) and GroupOfUnits(U) = fail)
      or Length(Columns(R)) <> Length(Rows(R)) then
    return false;
  fi;

  # Check each row and column of mat contains *exactly* one non-zero entry
  mat := Matrix(R);
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

  G := GroupOfUnits(U);
  return G <> fail
      and ForAll(mat_elts, x -> x in G)
      and IsInverseSemigroup(U);
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
  elif IsGroupAsSemigroup(U) <> fail and IsGroupAsSemigroup(U) then
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
  if not IsGroup(U)
      and (IsGroupAsSemigroup(U) = fail or not IsGroupAsSemigroup(U)) then
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
