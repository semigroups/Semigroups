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

  if not IsGroup(U) then
    SetIsGroupAsSemigroup(U, true);
  fi;
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
  # WW Also S might not be a simple semigroup (which is necessary)!

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

InstallMethod(RZMSDigraph,
"for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  local mat, n, m, out, i, j;

  mat := Matrix(R);
  n := Length(mat);
  m := Length(mat[1]);
  out := List([1 .. m + n], x -> []);
  for i in [1 .. m] do
    for j in [1 .. n] do
      if mat[j][i] <> 0 then
        Add(out[j + m], i);
        Add(out[i], j + m);
      fi;
    od;
  od;
  return DigraphNC(out);
end);

InstallMethod(RZMSConnectedComponents,
"for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  local comps, new, mat, n, m, i;

  comps := DigraphConnectedComponents(RZMSDigraph(R));
  new := List([1 .. Length(comps.comps)], x -> [[], []]);
  mat := Matrix(R);
  n := Length(mat);
  m := Length(mat[1]);
  for i in [1 .. m] do
    Add(new[comps.id[i]][1], i);
  od;
  for i in [m + 1 .. m + n] do
    Add(new[comps.id[i]][2], i - m);
  od;
  return new;
end);

InstallMethod(RZMSNormalization,
"for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  local mat, n, m, comp, perm, norm, size, rows, cols, l, x, next_cols,
  row_perm, col_perm, next_rows, new, out, iso, inv, hom, k, j, i;

  if not IsGroup(UnderlyingSemigroup(R)) then
    ErrorMayQuit("Semigroups: RZMSNormalization: usage,\n",
                 "not yet implemented for when the underlying semigroup is ",
                 "not IsGroup,");
  fi;
  mat := Matrix(R);
  n := Length(mat);
  m := Length(mat[1]);
  comp := ShallowCopy(RZMSConnectedComponents(R));
  perm := rec();
  norm := rec();

  # Sort components by size descending
  size := List(comp, x -> Length(x[1]) * Length(x[2]));
  SortParallel(size, comp, function(x, y)
                             return LT(y, x);
                           end);

  perm.row := [1 .. m];
  perm.col := [1 .. n];
  norm.row := List(Rows(R), x -> ());
  norm.col := List(Columns(R), x -> ());
  for k in [1 .. Number(size, x -> x <> 0)] do
    rows := ShallowCopy(comp[k][1]); # Rows of RZMS, not rows of matrix
    cols := ShallowCopy(comp[k][2]); # Cols of RZMS, not cols of matrix

    # Pick the column with the maximum number of non-zero entries to go first
    l := List(cols, y -> Number(rows, z -> mat[y][z] <> 0));
    x := Position(l, Maximum(l));
    next_cols := [cols[x]];
    Remove(cols, x);

    row_perm := EmptyPlist(Length(rows));
    col_perm := EmptyPlist(Length(cols));
    while not IsEmpty(next_cols) do
      Append(col_perm, next_cols);
      next_rows := [];
      for j in next_cols do
        for i in rows do
          if mat[j][i] <> 0 then
            Add(next_rows, i);
            norm.row[i] := norm.col[j] * mat[j][i];
          fi;
        od;
        rows := Difference(rows, next_rows);
      od;
      Append(row_perm, next_rows);
      next_cols := [];
      for i in next_rows do
        for j in cols do
          if mat[j][i] <> 0 then
            Add(next_cols, j);
            norm.col[j] := norm.row[i] * mat[j][i] ^ -1;
          fi;
        od;
        cols := Difference(cols, next_cols);
      od;
    od;

    # Store the perm which sorts rows and columns within component <k>
    perm.row{comp[k][1]} := row_perm;
    perm.col{comp[k][2]} := col_perm;
  od;

  norm.col_inv := List(norm.col, x -> x ^ -1);
  norm.row_inv := List(norm.row, x -> x ^ -1);

  # Create the perms to sort rows and columns within components
  perm.row := PermList(perm.row);
  perm.col := PermList(perm.col);

  # Combine with perms which will sort components by size
  perm.row := PermList(OnTuples(Concatenation(List(comp, x -> x[1])),
                                perm.row));
  perm.col := PermList(OnTuples(Concatenation(List(comp, x -> x[2])),
                                perm.col));
  perm.row_inv := perm.row ^ -1;
  perm.col_inv := perm.col ^ -1;

  # Construct new matrix
  new := List([1 .. n], x -> EmptyPlist(m));
  for j in Columns(R) do
    for i in Rows(R) do
      if mat[j][i] = 0 then
        new[j ^ perm.col_inv][i ^ perm.row_inv] := 0;
      else
        new[j ^ perm.col_inv][i ^ perm.row_inv] := norm.col[j] * mat[j][i] *
                                                   norm.row_inv[i];
      fi;
    od;
  od;

  out := ReesZeroMatrixSemigroup(UnderlyingSemigroup(R), new);

  iso := function(x)
    if x![1] = 0 then
      return MultiplicativeZero(out);
    fi;
    return RMSElement(out,
                      x![1] ^ perm.row_inv,
                      norm.row[x![1]] * x![2] * norm.col_inv[x![3]],
                      x![3] ^ perm.col_inv);
  end;
  inv := function(x)
    if x![1] = 0 then
      return MultiplicativeZero(R);
    fi;
    return RMSElement(R,
                      x![1] ^ perm.row,
                      norm.row_inv[x![1] ^ perm.row] * x![2]
                        * norm.col[x![3] ^ perm.col],
                      x![3] ^ perm.col);
  end;

  hom := MappingByFunction(R, out, iso, inv);
  SetIsInjective(hom, true);
  SetIsSurjective(hom, true);
  SetIsTotal(hom, true);
  return hom;
end);

InstallMethod(RMSNormalization,
"for a Rees matrix semigroup",
[IsReesMatrixSemigroup],
function(R)
  local G, mat, id, r, c, new, S, iso, inv, hom, i, j;

  G := UnderlyingSemigroup(R);
  if not IsGroup(G) or IsGroupAsSemigroup(G) then
    ErrorMayQuit("Semigroups: RMSNormalization: usage,\n",
                 "the underlying semigroup of the Rees matrix semigroup <R> ",
                 "must be a group,");
  fi;

  mat := Matrix(R);
  id := Identity(G);
  r := EmptyPlist(Length(Rows(R)));

  r[1] := id;
  for i in [2 .. Length(Rows(R))] do
    r[i] := mat[1][i] ^ -1 * mat[1][1];
  od;
  c := List(Columns(R), j -> mat[j][1] ^ -1);

  # Construct new RMS
  new := List(Columns(R), x -> EmptyPlist(Length(Rows(R))));
  for i in Rows(R) do
    for j in Columns(R) do
      new[j][i] := c[j] * mat[j][i] * r[i];
    od;
  od;
  S := ReesMatrixSemigroup(G, new);

  iso := function(x)
    if x![1] = 0 then
      return MultiplicativeZero(S);
    fi;
    return RMSElement(S, x![1], r[x![1]] ^ -1 * x![2] * c[x![3]] ^ -1, x![3]);
  end;

  inv := function(x)
    if x![1] = 0 then
      return MultiplicativeZero(R);
    fi;
    return RMSElement(R, x![1], r[x![1]] * x![2] * c[x![3]], x![3]);
  end;

  hom := MappingByFunction(R, S, iso, inv);
  SetIsInjective(hom, true);
  SetIsSurjective(hom, true);
  SetIsTotal(hom, true);
  return hom;
end);
