############################################################################
##
##  reesmat.gi
##  Copyright (C) 2014-16                                James D. Mitchell
##                                                          Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains methods for every operation/attribute/property that is
# specific to Rees 0-matrix semigroups.

#############################################################################
## Random semigroups
#############################################################################

InstallMethod(SEMIGROUPS_ProcessRandomArgsCons,
[IsReesMatrixSemigroup, IsList],
function(filt, params)
  local order, i;
  if Length(params) < 1 then  # rows I
    params[1] := Random([1 .. 100]);
  elif not IsPosInt(params[1]) then
    return "the second argument (number of rows) must be a pos int,";
  fi;
  if Length(params) < 2 then  # cols J
    params[2] := Random([1 .. 100]);
  elif not IsPosInt(params[2]) then
    return "the third argument (number of columns) must be a pos int,";
  fi;
  if Length(params) < 3 then  # group
    order := Random([1 .. 2047]);
    i := Random([1 .. NumberSmallGroups(order)]);
    params[3] := Range(IsomorphismPermGroup(SmallGroup(order, i)));
  elif not IsPermGroup(params[3]) then
    return "the fourth argument must be a perm group,";
  fi;
  if Length(params) > 3 then
    return "there must be at most four arguments,";
  fi;

  return params;
end);

InstallMethod(SEMIGROUPS_ProcessRandomArgsCons,
[IsReesZeroMatrixSemigroup, IsList],
function(filt, params)
  return SEMIGROUPS_ProcessRandomArgsCons(IsReesMatrixSemigroup, params);
end);

InstallMethod(SEMIGROUPS_ProcessRandomArgsCons,
[IsReesZeroMatrixSemigroup and IsRegularSemigroup, IsList],
function(filt, params)
  return SEMIGROUPS_ProcessRandomArgsCons(IsReesMatrixSemigroup, params);
end);

InstallMethod(RandomSemigroupCons,
"for IsReesZeroMatrixSemigroup and list",
[IsReesZeroMatrixSemigroup, IsList],
function(filt, params)
  local I, G, J, mat, i, j;

  I := [1 .. params[1]];
  J := [1 .. params[2]];
  G := params[3];
  # could add nr connected components

  mat := List(J, x -> I * 0);

  for i in I do
    for j in J do
      if Random([1, 2]) = 1 then
        mat[j][i] := Random(G);
      fi;
    od;
  od;

  return ReesZeroMatrixSemigroup(G, mat);
end);

InstallMethod(RandomSemigroupCons,
"for IsReesMatrixSemigroup and list",
[IsReesMatrixSemigroup, IsList],
function(filt, params)
  local I, G, J, mat, i, j;

  I := [1 .. params[1]];
  J := [1 .. params[2]];
  G := params[3];

  mat := List(J, x -> List(I, x -> ()));

  for i in I do
    for j in J do
      if Random([1, 2]) = 1 then
        mat[j][i] := Random(G);
      fi;
    od;
  od;

  return ReesMatrixSemigroup(G, mat);
end);

InstallMethod(RandomSemigroupCons,
"for IsReesZeroMatrixSemigroup and IsRegularSemigroup and list",
[IsReesZeroMatrixSemigroup and IsRegularSemigroup, IsList],
function(filt, params)
  local I, G, J, mat, i, j;

  I := [1 .. params[1]];
  J := [1 .. params[2]];
  G := params[3];
  # could add nr connected components

  mat := List(J, x -> I * 0);

  if I > J then
    for i in J do
      mat[i][i] := ();
    od;
    for i in [params[2] + 1 .. params[1]] do
      mat[1][i] := ();
    od;
  else
    for i in I do
      mat[i][i] := ();
    od;
    for i in [params[1] + 1 .. params[2]] do
      mat[i][1] := ();
    od;
  fi;

  for i in I do
    for j in J do
      if Random([1, 2]) = 1 then
        mat[j][i] := Random(G);
      fi;
    od;
  od;

  return ReesZeroMatrixSemigroup(G, mat);
end);

#############################################################################
## Isomorphisms
#############################################################################

InstallMethod(AsMonoid, "for a Rees matrix semigroup",
[IsReesMatrixSemigroup], ReturnFail);

InstallMethod(AsMonoid, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup], ReturnFail);

InstallMethod(IsomorphismSemigroup,
"for IsReesMatrixSemigroup and a semigroup",
[IsReesMatrixSemigroup, IsSemigroup],
function(filt, S)
  return IsomorphismReesMatrixSemigroup(S);
end);

InstallMethod(IsomorphismSemigroup,
"for IsReesZeroMatrixSemigroup and a semigroup",
[IsReesZeroMatrixSemigroup, IsSemigroup],
function(filt, S)
  return IsomorphismReesZeroMatrixSemigroup(S);
end);

InstallMethod(IsomorphismReesMatrixSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  local D, iso, inv;

  if not IsFinite(S) then
    TryNextMethod();
  elif not IsSimpleSemigroup(S) then
    ErrorNoReturn("Semigroups: IsomorphismReesMatrixSemigroup: usage,\n",
                  "the argument must be a simple semigroup,");
    # TODO is there another method? I.e. can we turn non-simple/non-0-simple
    # semigroups into Rees (0-)matrix semigroups over non-groups?
  fi;

  D := GreensDClasses(S)[1];
  iso := IsomorphismReesMatrixSemigroup(D);
  inv := InverseGeneralMapping(iso);
  UseIsomorphismRelation(S, Range(iso));

  return MagmaIsomorphismByFunctionsNC(S,
                                       Range(iso),
                                       x -> x ^ iso,
                                       x -> x ^ inv);
end);

InstallMethod(IsomorphismReesZeroMatrixSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  local D, map, inj, inv;

  if not IsFinite(S) then
    TryNextMethod();
  elif not IsZeroSimpleSemigroup(S) then
    ErrorNoReturn("Semigroups: IsomorphismReesZeroMatrixSemigroup: usage,\n",
                  "the argument must be a 0-simple semigroup,");
    # TODO is there another method? I.e. can we turn non-simple/non-0-simple
    # semigroups into Rees (0-)matrix semigroups over non-groups?
  fi;

  D := First(GreensDClasses(S),
             x -> not IsMultiplicativeZero(S, Representative(x)));
  map := SEMIGROUPS.InjectionPrincipalFactor(D, ReesZeroMatrixSemigroup);

  # the below is necessary since map is not defined on the zero of S
  inj := function(x)
    if x = MultiplicativeZero(S) then
      return MultiplicativeZero(Range(map));
    fi;
    return x ^ map;
  end;

  inv := function(x)
    if x = MultiplicativeZero(Range(map)) then
      return MultiplicativeZero(S);
    fi;
    return x ^ InverseGeneralMapping(map);
  end;

  return MagmaIsomorphismByFunctionsNC(S,
                                       Range(map),
                                       inj,
                                       inv);
end);

InstallGlobalFunction(RMSElementNC,
function(R, i, g, j)
  return Objectify(TypeReesMatrixSemigroupElements(R),
                   [i, g, j, Matrix(R)]);
end);

InstallMethod(MultiplicativeZero, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSubsemigroup],
function(R)
  local super, x, gens, row, col, i;

  super := ReesMatrixSemigroupOfFamily(FamilyObj(Representative(R)));
  x := MultiplicativeZero(super);
  if (HasIsReesZeroMatrixSemigroup(R) and IsReesZeroMatrixSemigroup(R))
      or x in R then
    return x;
  fi;

  gens := GeneratorsOfSemigroup(R);
  row := RowOfReesZeroMatrixSemigroupElement(gens[1]);
  col := ColumnOfReesZeroMatrixSemigroupElement(gens[1]);
  for i in [2 .. Length(gens)] do
    x := gens[i];
    if RowOfReesZeroMatrixSemigroupElement(x) <> row or
        ColumnOfReesZeroMatrixSemigroupElement(x) <> col then
      return fail;
    fi;
  od;

  TryNextMethod();
end);

# same method for ideals

InstallMethod(IsomorphismPermGroup,
"for a subsemigroup of a Rees 0-matrix semigroup",
[IsReesZeroMatrixSubsemigroup],
function(S)
  local r, mat, G, iso;

  if not IsGroupAsSemigroup(S) then
    ErrorNoReturn("Semigroups: IsomorphismPermGroup: usage,\n",
                  "the argument <S> must be a subsemigroup of a Rees 0-matrix ",
                  "semigroup satisfying IsGroupAsSemigroup,");
  fi;

  r := Representative(S);
  if r![1] = 0 then  # special case for the group consisting of 0
    return MagmaIsomorphismByFunctionsNC(S, Group(()), x -> (), x -> r);
  fi;

  mat := Matrix(ReesMatrixSemigroupOfFamily(FamilyObj(r)));
  G := Group(List(GeneratorsOfSemigroup(S), x -> x![2] * mat[x![3]][x![1]]));
  UseIsomorphismRelation(S, G);

  iso := MagmaIsomorphismByFunctionsNC;
  return iso(S, G,
             x -> x![2] * mat[x![3]][x![1]],
             x -> RMSElement(S, r![1], x * mat[r![3]][r![1]] ^ -1, r![3]));
end);

################################################################################
# Attributes, operations, and properties
################################################################################

# This method is only required because of some problems in the library code for
# Rees (0-)matrix semigroups.

InstallMethod(Representative,
"for a Rees 0-matrix subsemigroup with rows, columns and matrix",
[IsReesZeroMatrixSubsemigroup and HasRows and HasColumns and HasMatrix],
function(R)
  return Objectify(TypeReesMatrixSemigroupElements(R),
                   [Rows(R)[1],
                    Representative(UnderlyingSemigroup(R)),
                    Columns(R)[1],
                    Matrix(R)]);
end);

# This method is only required because of some problems in the library code for
# Rees (0-)matrix semigroups.

InstallMethod(Representative,
"for a Rees matrix subsemigroup with rows, columns, and matrix",
[IsReesMatrixSubsemigroup and HasRows and HasColumns and HasMatrix],
function(R)
  return Objectify(TypeReesMatrixSemigroupElements(R),
                   [Rows(R)[1],
                    Representative(UnderlyingSemigroup(R)),
                    Columns(R)[1],
                    Matrix(R)]);
end);

# same method for ideals

InstallMethod(GroupOfUnits, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(S)
  local x, U;

  x := MultiplicativeNeutralElement(S);

  if MultiplicativeNeutralElement(S) = fail then
    return fail;
  elif IsMultiplicativeZero(S, x) then
    U := Semigroup(x);
  else
    U := Semigroup(RClassNC(S, x), rec(small := true));
  fi;
  SetIsGroupAsSemigroup(U, true);
  return U;
end);

# This method is better than the one for acting semigroups.

InstallMethod(Random, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
3,  # to beat the method for regular acting semigroups
function(R)
  return Objectify(TypeReesMatrixSemigroupElements(R),
                   [Random(Rows(R)), Random(UnderlyingSemigroup(R)),
                    Random(Columns(R)), Matrix(ParentAttr(R))]);
end);

# TODO a proper method here

InstallMethod(IsGeneratorsOfInverseSemigroup,
"for a collection of Rees 0-matrix semigroup elements",
[IsReesZeroMatrixSemigroupElementCollection], ReturnFalse);

InstallMethod(ViewString,
"for a Rees 0-matrix subsemigroup ideal with ideal generators",
[IsReesZeroMatrixSubsemigroup and IsSemigroupIdeal and
 HasGeneratorsOfSemigroupIdeal],
5,  # to beat ViewString for a semigroup ideal with ideal generators
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

InstallMethod(GreensHClassOfElement, "for a RZMS, pos int, and pos int",
[IsReesZeroMatrixSemigroup, IsPosInt, IsPosInt],
function(R, i, j)
  local rep;

  rep := RMSElement(R, i, Representative(UnderlyingSemigroup(R)), j);
  return GreensHClassOfElement(R, rep);
end);

InstallMethod(RZMSDigraph, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  local rows, cols, mat, n, m, nredges, out, gr, i, j;

  rows := Rows(R);
  cols := Columns(R);
  mat := Matrix(R);
  n := Length(Columns(R));
  m := Length(Rows(R));
  nredges := 0;
  out := List([1 .. m + n], x -> []);
  for i in [1 .. m] do
    for j in [1 .. n] do
      if mat[cols[j]][rows[i]] <> 0 then
        nredges := nredges + 2;
        Add(out[j + m], i);
        Add(out[i], j + m);
      fi;
    od;
  od;
  gr := DigraphNC(out);
  SetIsBipartiteDigraph(gr, true);
  SetDigraphHasLoops(gr, false);
  SetDigraphBicomponents(gr, [[1 .. m], [m + 1 .. m + n]]);
  SetDigraphNrEdges(gr, nredges);
  # SetDigraphVertexLabels(gr, Concatenation(rows, cols));
  return gr;
end);

InstallMethod(RZMSConnectedComponents,
"for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  local gr, comps, new, n, m, i;

  gr := RZMSDigraph(R);
  comps := DigraphConnectedComponents(gr);
  new := List([1 .. Length(comps.comps)], x -> [[], []]);
  n := Length(Columns(R));
  m := Length(Rows(R));

  for i in [1 .. m] do
    Add(new[comps.id[i]][1], Rows(R)[i]);
  od;
  for i in [m + 1 .. m + n] do
    Add(new[comps.id[i]][2], Columns(R)[i - m]);
  od;
  return new;
end);

#############################################################################
## Methods related to inverse semigroups
#############################################################################

InstallMethod(IsInverseSemigroup,
"for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(R)
  local U, elts, mat, row, seen, G, j, i;

  if not IsReesZeroMatrixSemigroup(R) then
    TryNextMethod();
  fi;

  U := UnderlyingSemigroup(R);

  if (HasIsInverseSemigroup(U) and not IsInverseSemigroup(U))
      or (HasIsRegularSemigroup(U) and not IsRegularSemigroup(U))
      or (HasIsMonoidAsSemigroup(U) and not IsMonoidAsSemigroup(U))
      or (HasGroupOfUnits(U) and GroupOfUnits(U) = fail)
      or Length(Columns(R)) <> Length(Rows(R)) then
    return false;
  fi;

  # Check each row and column of mat contains *exactly* one non-zero entry
  elts := [];
  mat := Matrix(R);
  row := BlistList([1 .. Maximum(Rows(R))], []);  # <row[i]> is <true> iff found
                                                 # a non-zero entry in row <i>
  for j in Columns(R) do
    seen := false;  # <seen>: <true> iff found a non-zero entry in the col <j>
    for i in Rows(R) do
      if mat[j][i] <> 0 then
        if seen or row[i] then
          return false;
        fi;
        seen := true;
        row[i] := true;
        AddSet(elts, mat[j][i]);
      fi;
    od;
    if not seen then
      return false;
    fi;
  od;

  # Check that non-zero matrix entries are units of the inverse monoid <U>
  G := GroupOfUnits(U);

  return G <> fail and ForAll(elts, x -> x in G) and IsInverseSemigroup(U);
end);

InstallMethod(Idempotents,
"for an inverse Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup and IsInverseSemigroup],
function(R)
  local mat, I, star, e, out, k, i, j, x;

  if not IsReesZeroMatrixSemigroup(R) then
    TryNextMethod();
  fi;

  mat := Matrix(R);
  I := Rows(R);
  star := EmptyPlist(Maximum(I));
  for i in I do
    for j in Columns(R) do
      if mat[j][i] <> 0 then
        star[i] := j;  # <star[i]> is index such that mat[star[i]][i] <> 0
        break;
      fi;
    od;
  od;

  e := Idempotents(UnderlyingSemigroup(R));
  out := EmptyPlist(NrIdempotents(R));
  out[1] := MultiplicativeZero(R);
  k := 1;
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
RankFilter(IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup) -
RankFilter(IsReesZeroMatrixSubsemigroup) + 1,
function(R)
  local G, group, iso, inv, mat, out, k, i, j;

  if not IsReesZeroMatrixSemigroup(R)
      or not IsGroupAsSemigroup(UnderlyingSemigroup(R)) then
    TryNextMethod();
  fi;

  G := UnderlyingSemigroup(R);
  group := IsGroup(G);
  if not IsGroup(R) then
    iso := IsomorphismPermGroup(G);
    inv := InverseGeneralMapping(iso);
  fi;

  mat := Matrix(R);
  out := EmptyPlist(NrIdempotents(R));
  out[1] := MultiplicativeZero(R);
  k := 1;
  for i in Rows(R) do
    for j in Columns(R) do
      if mat[j][i] <> 0 then
        k := k + 1;
        if group then
          out[k] := RMSElement(R, i, mat[j][i] ^ -1, j);
        else
          out[k] := RMSElement(R, i, ((mat[j][i] ^ iso) ^ -1) ^ inv, j);
        fi;
      fi;
    od;
  od;

  return out;
end);

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
  local count, mat, i, j;

  if not IsReesZeroMatrixSemigroup(R)
      or not IsGroupAsSemigroup(UnderlyingSemigroup(R)) then
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

#############################################################################
## Normalizations
#############################################################################

InstallMethod(RZMSNormalization,
"for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  local T, rows, cols, I, L, lookup_cols, lookup_rows, mat, group, one, r, c,
  invert, GT, comp, size, p, rows_unsorted, cols_unsorted, x, j, cols_todo,
  rows_todo, new, S, iso, inv, i, k;

  T := UnderlyingSemigroup(R);

  rows := Rows(R);
  cols := Columns(R);
  I := Length(rows);
  L := Length(cols);

  lookup_cols := EmptyPlist(Maximum(cols));
  for i in [1 .. L] do
    lookup_cols[cols[i]] := i;
  od;
  lookup_rows := EmptyPlist(Maximum(rows));
  for i in [1 .. I] do
    lookup_rows[rows[i]] := i;
  od;

  mat := Matrix(R);
  group := IsGroupAsSemigroup(T);

  if group then
    one := MultiplicativeNeutralElement(T);
    r := ListWithIdenticalEntries(I, one);
    c := ListWithIdenticalEntries(L, one);
    # Not every IsGroupAsSemigroup has the usual inverse operation
    if IsGroup(T) then
      invert := InverseOp;
    else
      invert := x -> InversesOfSemigroupElement(T, x)[1];
    fi;
  fi;

  GT := function(x, y)
    return LT(y, x);
  end;

  # Sort the connected components of <R> by size (#rows * #colums) descending.
  # This also sends any zero-columns or zero-rows to the end of the new matrix.
  comp := ShallowCopy(RZMSConnectedComponents(R));
  size := List(comp, x -> Length(x[1]) * Length(x[2]));
  SortParallel(size, comp, GT);  # comp[1] defines largest connected component.
  p := rec(row := EmptyPlist(I), col := EmptyPlist(I));

  for k in [1 .. Length(comp)] do
    if size[k] = 0 then
      Append(p.row, comp[k][1]);
      Append(p.col, comp[k][2]);
      continue;
    fi;
    # The rows and columns contained in the <k>^th largest conn. component.
    rows := comp[k][1];
    cols := comp[k][2];
    rows_unsorted := BlistList(rows, rows);
    cols_unsorted := BlistList(cols, cols);

    # Choose a column with the max number of non-zero entries to go first
    x := List(cols, j -> Number(rows, i -> mat[j][i] <> 0));
    x := Position(x, Maximum(x));
    j := cols[x];
    cols_todo := [j];
    cols_unsorted[x] := false;
    Add(p.col, j);

    # At each step, for the columns which have just been sorted:
    # 1. It is necessary to identify <rows_todo>, those unsorted rows which have
    #    an idempotent in any of the just-sorted columns.
    # 2. When doing this we also order these rows such that they are contiguous.
    # 3. If <T> is a group, then for each of these rows <i> we define the
    #    element <r[i]> of <T> which will, in the normalization, ensure that the
    #    first non-zero entry of that row is <one>, the identity of <T>.
    # It is necessary to do the same procedure again, swapping rows and columns.

    while not IsEmpty(cols_todo) do
      rows_todo := [];
      for j in cols_todo do
        x := ListBlist(rows, rows_unsorted);
        for i in x do
          if mat[j][i] <> 0 then
            Add(rows_todo, i);
            rows_unsorted[PositionSorted(rows, i)] := false;
            if group then
              r[i] := c[j] * mat[j][i];
            fi;
            Add(p.row, i);
          fi;
        od;
      od;
      cols_todo := [];
      for i in rows_todo do
        x := ListBlist(cols, cols_unsorted);
        for j in x do
          if mat[j][i] <> 0 then
            Add(cols_todo, j);
            cols_unsorted[PositionSorted(cols, j)] := false;
            if group then
              c[j] := r[i] * invert(mat[j][i]);
            fi;
            Add(p.col, j);
          fi;
        od;
      od;
    od;
  od;

  rows := Rows(R);
  cols := Columns(R);

  p.row := List(p.row, x -> lookup_rows[x]);
  p.col := List(p.col, x -> lookup_cols[x]);

  # p.row takes a row of <R> and gives the corresponding row of <S>.
  # p.row_i is the inverse of this (to avoid inverting repeatedly).
  p.row_i := PermList(p.row);
  p.col_i := PermList(p.col);
  p.row := p.row_i ^ -1;
  p.col := p.col_i ^ -1;

  # Construct the normalized RZMS <S>.
  new := List(cols, x -> EmptyPlist(I));
  for j in cols do
    for i in rows do
      if mat[j][i] <> 0 and group then
        new[lookup_cols[j] ^ p.col][lookup_rows[i] ^ p.row] :=
          c[j] * mat[j][i] * invert(r[i]);
      else
        new[lookup_cols[j] ^ p.col][lookup_rows[i] ^ p.row] := mat[j][i];
      fi;
    od;
  od;
  S := ReesZeroMatrixSemigroup(T, new);

  # Construct isomorphisms between <R> and <S>.
  # iso: (i, g, l) -> (i ^ p.row, r[i] * g * c[l] ^ -1, l ^ p.col)
  iso := function(x)
    if x![1] = 0 then
      return MultiplicativeZero(S);
    fi;
    return RMSElement(S, lookup_rows[x![1]] ^ p.row,
                         r[x![1]] * x![2] * invert(c[x![3]]),
                         lookup_cols[x![3]] ^ p.col);
  end;
  # inv: (i, g, l) -> (a, r[a] ^ -1 * g * c[b], b)
  #      where a = i ^ (p.row ^ -1) and b = j ^ (r.col ^ -1)
  inv := function(x)
    if x![1] = 0 then
      return MultiplicativeZero(R);
    fi;
    return RMSElement(R,
                      rows[x![1] ^ p.row_i],
                      invert(r[rows[x![1] ^ p.row_i]]) * x![2]
                      * c[cols[x![3] ^ p.col_i]],
                      cols[x![3] ^ p.col_i]);
  end;

  return MagmaIsomorphismByFunctionsNC(R, S, iso, inv);
end);

# Makes entries in the first row and col of the matrix equal to identity

InstallMethod(RMSNormalization,
"for a Rees matrix semigroup",
[IsReesMatrixSemigroup],
function(R)
  local G, invert, new, r, c, S, lookup_rows, lookup_cols, iso, inv, j, i;

  G := UnderlyingSemigroup(R);
  if not IsGroupAsSemigroup(G) then
    ErrorNoReturn("Semigroups: RMSNormalization: usage,\n",
                  "the underlying semigroup <G> of the Rees matrix semigroup ",
                  "<R> must be a group,");
  fi;

  if IsGroup(G) then
    invert := InverseOp;
  else
    invert := x -> InversesOfSemigroupElement(G, x)[1];
  fi;

  new := ShallowCopy(Matrix(R)){Columns(R)}{Rows(R)};

  # Construct the normalized RMS <S>
  r := List([1 .. Length(Rows(R))], i -> invert(new[1][i]) * new[1][1]);
  c := List([1 .. Length(Columns(R))], j -> invert(new[j][1]));
  for j in [1 .. Length(Columns(R))] do
    new[j] := ShallowCopy(new[j]);
    for i in [1 .. Length(Rows(R))] do
      new[j][i] := c[j] * new[j][i] * r[i];
    od;
  od;
  S := ReesMatrixSemigroup(G, new);

  lookup_rows := EmptyPlist(Maximum(Rows(R)));
  lookup_cols := EmptyPlist(Maximum(Columns(R)));
  for i in [1 .. Length(Rows(R))] do
    lookup_rows[Rows(R)[i]] := i;
  od;
  for j in [1 .. Length(Columns(R))] do
    lookup_cols[Columns(R)[j]] := j;
  od;

  # Construct isomorphisms between <R> and <S>
  iso := x -> RMSElement(S, lookup_rows[x![1]],
                            invert(r[lookup_rows[x![1]]]) * x![2] *
                            invert(c[lookup_cols[x![3]]]),
                            lookup_cols[x![3]]);
  inv := x -> RMSElement(R, Rows(R)[x![1]],
                            r[x![1]] * x![2] * c[x![3]],
                            Columns(R)[x![3]]);

  return MagmaIsomorphismByFunctionsNC(R, S, iso, inv);
end);

InstallMethod(IsIdempotentGenerated, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(R)
  local U, N;
  if not IsReesZeroMatrixSemigroup(R) then
    TryNextMethod();
  elif not IsConnectedDigraph(RZMSDigraph(R)) then
    return false;
  fi;
  U := UnderlyingSemigroup(R);
  if not IsGroupAsSemigroup(U) then
    TryNextMethod();
  fi;
  N := Range(RZMSNormalization(R));
  return Semigroup(Filtered(MatrixEntries(N), x -> x <> 0)) = U;
end);

InstallMethod(IsIdempotentGenerated, "for a Rees matrix subsemigroup",
[IsReesMatrixSubsemigroup],
function(R)
  local U, N;
  if not IsReesMatrixSemigroup(R) then
    TryNextMethod();
  fi;
  U := UnderlyingSemigroup(R);
  if not IsGroupAsSemigroup(U) then
    TryNextMethod();
  fi;
  N := Range(RMSNormalization(R));
  return Semigroup(MatrixEntries(N)) = U;
end);

# The next two methods are just copies of the methods in the library but with
# the rank increased so they are used in favour of the method for
# IsEnumerableSemigroupRep

InstallMethod(Size, "for a Rees matrix semigroup",
[IsReesMatrixSemigroup],
RankFilter(IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup),
function(R)
  # This is unreachable
  # if Size(UnderlyingSemigroup(R)) = infinity then
  #   return infinity;
  # fi;
  return Length(Rows(R)) * Size(UnderlyingSemigroup(R)) * Length(Columns(R));
end);

InstallMethod(Size, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
RankFilter(IsEnumerableSemigroupRep and HasGeneratorsOfSemigroup),
function(R)
  # This is unreachable
  # if Size(UnderlyingSemigroup(R)) = infinity then
  #   return infinity;
  # fi;
  return Length(Rows(R)) * Size(UnderlyingSemigroup(R)) * Length(Columns(R))
         + 1;
end);

#############################################################################
# Pickler
#############################################################################

InstallMethod(IO_Pickle, "for a Rees matrix semigroup",
[IsFile, IsReesMatrixSemigroup],
function(file, x)
  if IO_Write(file, "RMSX") = fail then
    return IO_Error;
  fi;
  if IO_Pickle(file, [UnderlyingSemigroup(x), Matrix(x)]) = IO_Error then
    return IO_Error;
  fi;
  return IO_OK;
end);

IO_Unpicklers.RMSX := function(file)
  local x;
  x := IO_Unpickle(file);
  if x = IO_Error then
    return IO_Error;
  fi;
  return ReesMatrixSemigroup(x[1], x[2]);
end;

InstallMethod(IO_Pickle, "for a Rees 0-matrix semigroup",
[IsFile, IsReesZeroMatrixSemigroup],
function(file, x)
  if IO_Write(file, "RZMS") = fail then
    return IO_Error;
  fi;
  if IO_Pickle(file, [UnderlyingSemigroup(x), Matrix(x)]) = IO_Error then
    return IO_Error;
  fi;
  return IO_OK;
end);

IO_Unpicklers.RZMS := function(file)
  local x;
  x := IO_Unpickle(file);
  if x = IO_Error then
    return IO_Error;
  fi;
  return ReesZeroMatrixSemigroup(x[1], x[2]);
end;

