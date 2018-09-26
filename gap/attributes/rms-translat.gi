#############################################################################
##
# W  rms-translat.gi
# Y  Copyright (C) 2016-18                                         Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
#############################################################################
## This file contains special methods for translations semigroups and
## translational hulls of completely simple and 0-simple semigroups.
##
## These methods are based on the constructions given in
## Petrich, M. (1968)
## ‘The translational hull of a completely 0-simple semigroup’,
## Glasgow Mathematical Journal, 9(01), p. 1.
## doi: 10.1017/s0017089500000239
##
## A.H Clifford, Mario Petrich, (1977)
## Some classes of completely regular semigroups,
## Journal of Algebra, Volume 46, Issue 2, 1977, Pages 462-480,
## http://dx.doi.org/10.1016/0021-8693(77)90383-0.
#############################################################################
# TODO: make sure you can't mix translations of different types in one semigroup

#############################################################################
# 1. Internal Functions
#############################################################################

# This should go somewhere else
SEMIGROUPS.IsNormalRMSOverGroup := function(S)
  local mat, T, one;

  if not IsReesMatrixSemigroup(S) then
    return false;
  fi;

  T := UnderlyingSemigroup(S);

  if not IsGroupAsSemigroup(T) then
    return false;
  fi;

  mat := Matrix(S);
  one := MultiplicativeNeutralElement(T);
  return ForAll(mat[1], x -> x = one) and
          ForAll(mat, x -> x[1] = one);
end;

# Hash translations by their underlying transformations
  SEMIGROUPS.HashFunctionForRMSTranslations := function(x, data)
    return ORB_HashFunctionForTransformations(x![2], data);
  end;

# Hash linked pairs as sum of hashes
  SEMIGROUPS.HashFunctionForRMSBitranslations := function(x, data)
      return (SEMIGROUPS.HashFunctionForRMSTranslations(x![1], data)
        + SEMIGROUPS.HashFunctionForRMSTranslations(x![2], data)) mod data + 1;
  end;

# Finds the transformations on the indices of a finite 0-simple semigroup
# which are candidates for translations, when combined with a function from
# the index sets to the group.
# TODO: swap rows/columns if more convenient.
SEMIGROUPS.FindTranslationTransformations := function(S)
  local iso, reesmatsemi, mat, simplerows, nrrows, nrcols, translist,
  simplecols, ispartialsuccess, extend, reject, bt, v, partcols, x,
  linkedtranslist, t, cols, col, possiblecols, i, j, k;

  iso          := IsomorphismReesZeroMatrixSemigroup(S);
  reesmatsemi  := Range(iso);
  mat          := Matrix(reesmatsemi);
  simplerows   := List(mat, ShallowCopy);
  nrrows       := Length(simplerows);
  nrcols       := Length(simplerows[1]);
  translist    := [];
  for i in [1 .. Length(simplerows)] do
    for j in [1 .. Length(simplerows[i])] do
      if simplerows[i][j] <> 0 then
        simplerows[i][j] := 1;
      fi;
    od;
  od;
  simplecols := TransposedMat(simplerows);

  ispartialsuccess := function(x)
    local foundcol, noclash, pc;
    for pc in partcols do
      # Only check the columns which contain a 1 since
      # all-zero column can be made by column not in domain of transformation.
      # No ones in one partial matrix but not in the other...
      if 1 in pc then
        foundcol := false;
        for col in simplecols do
          noclash := true;
          for i in [1 .. Length(pc)] do
            if pc[i] + col[i] = 1 then
              noclash := false;
              break;
            fi;
          od;
          if noclash then
            foundcol := true;
            break;
          fi;
        od;
        if not foundcol then
          return false;
        fi;
      fi;
    od;
    return true;
  end;

  extend := function(w)
    Add(w, 1);
    for i in [1 .. nrcols] do
      partcols[i][Length(w)] := simplerows[1][i];
    od;
  end;

  reject := function(q)
    # return the next list to consider
    k := Length(q);
    if q[k] <= nrrows then
      q[k] := q[k] + 1;
      for i in [1 .. nrcols] do
        if q[k] <= nrrows then
          partcols[i][k] := simplerows[q[k]][i];
        else
          partcols[i][k] := 0;
        fi;
      od;
    elif k > 1 then
      for i in [1 .. nrcols] do
        Remove(partcols[i]);
      od;
      q := reject(q{[1 .. k - 1]});
    else return 0;
    fi;
    return q;
  end;

  bt := function(x)
    if x = 0 then
      return 0;
    fi;
    while not ispartialsuccess(x) do
      x := reject(x);
      if x = 0 then
        return 0;
      fi;
    od;
    if Length(x) = nrrows then
      return x;
    else
      extend(x);
    fi;
    return bt(x);
  end;

  v         := 1;
  partcols  := List([1 .. nrcols], i -> []);
  x         := [];
  translist := [bt(x)];
  while translist[v] <> 0 do
    v            := v + 1;
    translist[v] := bt(reject(ShallowCopy(translist[v - 1])));
  od;

  linkedtranslist := [];
  # Given each row transformation, look for matching column transformations.
  # Last element of translist will be 0, ignore.
  for k in [1 .. Length(translist) - 1] do
    t     := translist[k];
    cols  := [];
    for i in [1 .. Length(simplerows[1])] do
      col := [];
      for j in [1 .. nrrows] do
        if t[j] = nrrows + 1 then
          col[j] := 0;
        else
          col[j] := simplerows[t[j]][i];
        fi;
      od;
      Add(cols, col);
    od;
    possiblecols := [];
    for i in [1 .. nrcols] do
      possiblecols[i] := [];
      for j in [1 .. nrcols] do
        if not 1 in cols[i] + simplecols[j] then
          Add(possiblecols[i], j);
        fi;
        if not 1 in cols[i] then
          Add(possiblecols[i], nrcols + 1);
        fi;
      od;
    od;
    linkedtranslist[k] := IteratorOfCartesianProduct(possiblecols);
  od;
  return [translist{[1 .. Length(translist) - 1]}, linkedtranslist];
end;

# For a pair of transformations on the indices of a completely 0-simple
# semigroup, determine the functions to the group associated with the RMS
# representation which will form translations when combined with the
# transformations.  Connected components here means points in the domains which
# are related through the linked pair condition given by Petrich.
SEMIGROUPS.FindTranslationFunctionsToGroup :=
function(S, t1, t2)
  local reesmatsemi, mat, nrrows, nrcols, gplist, gpsize, invmat,
  rowtransformedmat, zerorow, zerocol, rels, rowrels, edges, digraph, cc,
  relpoints, fillin, relssatisfied, funcsfromrelpointvals, foundfuncs, iterator,
  funcs, i, j, k;

  reesmatsemi := Range(IsomorphismReesZeroMatrixSemigroup(S));
  mat         := Matrix(reesmatsemi);
  nrrows      := Length(mat);
  nrcols      := Length(mat[1]);

  if ValueOption("SEMIGROUPS_gp_is_trivial_perm_gp") = true then
      return [[List([1 .. nrrows], x -> ()), List([1 .. nrcols], y -> ())]];
  fi;

  gplist := AsList(UnderlyingSemigroup(reesmatsemi));
  gpsize := Size(gplist);
  invmat := List(mat, ShallowCopy);
  for i in [1 .. nrrows] do
    for j in [1 .. nrcols] do
      invmat[i][j] := Inverse(invmat[i][j]);
    od;
  od;
  rowtransformedmat := [];
  zerorow           := [];
  zerocol           := [];
  for i in [1 .. nrcols] do
    zerorow[i] := 0;
  od;
  for i in [1 .. nrrows] do
    zerocol[i] := 0;
  od;
  for i in [1 .. nrrows] do
    if t1[i] <> nrrows + 1 then
      rowtransformedmat[i] := mat[t1[i]];
    else
      rowtransformedmat[i] := zerorow;
    fi;
  od;

  # for ease of checking the constraints, set up lists of linked indices
  rels := [];
  for i in [1 .. nrrows] do
    for j in [1 .. nrcols] do
      if rowtransformedmat[i][j] <> 0 then
        Add(rels, [i, j]);
      fi;
    od;
  od;
  rowrels := [];
  for i in [1 .. nrrows] do
    rowrels[i] := [];
    for j in [1 .. nrrows] do
      for k in [1 .. nrcols] do
        if i <> j and [i, k] in rels and [j, k] in rels then
          rowrels[i][j] := k;
          break;
        fi;
      od;
    od;
  od;

  # get connected components
  edges   := List(rels, r -> [r[1], r[2] + nrrows]);
  digraph := DigraphSymmetricClosure(DigraphByEdges(edges, nrrows + nrcols));
  cc      := DigraphConnectedComponents(digraph);

  # only deal with enough elements to hit each relation
  relpoints := [];
  for cc in cc.comps do
    # if cc[1] is in the domain
    if cc[1] <= nrrows and t1[cc[1]] <= nrrows then
      Add(relpoints, cc[1]);
    elif cc[1] > nrrows
         and cc[1] <= nrrows + nrcols
         and t2[cc[1] - nrrows] <= nrcols then
      Add(relpoints, cc[1]);
    fi;
  od;

  fillin := function(funcs)
    local x, y, queue, n, q, j, r;
    # given a choice of relpoints, fill in everything else possible
    x := ShallowCopy(funcs[1]);
    y := ShallowCopy(funcs[2]);
    for r in relpoints do
      queue := [r];
      n     := 1;
      repeat
        q := queue[n];
        # the digraph is bipartite
        for j in OutNeighbours(digraph)[q] do
          if j > nrrows then
            j := j - nrrows;
            if not IsBound(y[j]) then
              y[j] := invmat[q][t2[j]] * x[q] * rowtransformedmat[q][j];
              Add(queue, nrrows + j);
            fi;
          else
            if not IsBound(x[j]) then
              x[j] := mat[j][t2[q - nrrows]]
                      * y[q - nrrows]
                      * invmat[t1[j]][q - nrrows];
              Add(queue, j);
            fi;
          fi;
        od;
        n := n + 1;
      until n = Size(queue) + 1;
    od;
    return [x, y];
  end;

  relssatisfied := function(funcs)
    local x, y, rel;
    # check whether the relations are satisfied
    x := funcs[1];
    y := funcs[2];
    for rel in rels do
      i := rel[1];
      j := rel[2];
      if IsBound(x[i]) and IsBound(y[j]) and not
          x[i] * rowtransformedmat[i][j] = mat[i][t2[j]] * y[j] then
          return false;
      fi;
    od;
    return true;
  end;

  funcsfromrelpointvals := function(relpointvals)
    local x, y;
    # given values for the representative points, create the functions
    x := [1 .. nrrows];
    y := [1 .. nrcols];

    for i in [1 .. nrrows] do
      Unbind(x[i]);
    od;
    for i in [1 .. nrcols] do
      Unbind(y[i]);
    od;
    for i in [1 .. Length(relpoints)] do
      x[relpoints[i]] := gplist[relpointvals[i]];
    od;
    return fillin([x, y]);
  end;

  foundfuncs := [];
  iterator   := IteratorOfCartesianProduct(List(relpoints, i -> [1 .. gpsize]));

  # If S is a commutative group and fails on one choice of relpointvals,
  # it will fail on all of them.
  if IsDoneIterator(iterator) then
    return [];
  else
    funcs := funcsfromrelpointvals(NextIterator(iterator));
    if not relssatisfied(funcs) then
      if IsCommutative(S) then
        return fail;
      fi;
    else
      Add(foundfuncs, funcs);
    fi;
  fi;

  while not IsDoneIterator(iterator) do
    funcs := funcsfromrelpointvals(NextIterator(iterator));
    if relssatisfied(funcs) then
      Add(foundfuncs, funcs);
    fi;
  od;

  return foundfuncs;
end;

# Combine the previous methods to form the translational hull
# Performance suffers greatly as the size of the group increases.
SEMIGROUPS.BitranslationsOfZeroSimple := function(H)
  local S, iso, inv, reesmatsemi, gplist, nrrows, nrcols, zero, L, R, tt,
  linkedpairs, linkedpairfromfuncs, fr, l, r, t1, iterator, t2, transfuncs, fx,
  fy, unboundpositions, c, partialfunciterator, funcvals, i, funcs, j;

  S := UnderlyingSemigroup(H);
  if not (IsFinite(S) and IsZeroSimpleSemigroup(S)) then
    TryNextMethod();
  fi;
  iso         := IsomorphismReesZeroMatrixSemigroup(S);
  inv         := InverseGeneralMapping(iso);
  reesmatsemi := Range(iso);
  gplist      := AsList(UnderlyingSemigroup(reesmatsemi));
  nrrows      := Length(Matrix(reesmatsemi));
  nrcols      := Length(Matrix(reesmatsemi)[1]);
  zero        := MultiplicativeZero(reesmatsemi);
  L           := LeftTranslationsSemigroup(S);
  R           := RightTranslationsSemigroup(S);
  tt          := SEMIGROUPS.FindTranslationTransformations(S);
  linkedpairs := [];

  linkedpairfromfuncs := function(t1, t2, fx, fy)
    local fl, fr;
    # given transformations on the indices and fns to the group,
    # create a bitranslation
    fl := function(x)
      if x = zero then
        return zero;
      fi;
      if t2[x[1]] <> nrcols + 1 then
        return RMSElement(reesmatsemi, t2[x[1]], fy[x[1]] * x[2], x[3]);
      else
        return zero;
      fi;
    end;

    fr := function(x)
      if x = zero then
        return zero;
      fi;
      if t1[x[3]] <> nrrows + 1 then
        return RMSElement(reesmatsemi, x[1], x[2] * fx[x[3]], t1[x[3]]);
      else
        return zero;
      fi;
    end;

    l := LeftTranslationNC(L, CompositionMapping(inv, MappingByFunction(
      reesmatsemi, reesmatsemi, fl), iso));

    r := RightTranslationNC(R, CompositionMapping(inv, MappingByFunction(
      reesmatsemi, reesmatsemi, fr), iso));

    return BitranslationNC(H, l, r);
  end;

  for i in [1 .. Length(tt[1])] do
    t1        := tt[1][i];
    iterator  := tt[2][i];
    while not IsDoneIterator(iterator) do
      t2 := NextIterator(iterator);
      if Size(UnderlyingSemigroup(S)) = 1 then
        transfuncs := SEMIGROUPS.FindTranslationFunctionsToGroup(S,
                                                                 t1,
                                                                 t2 :
                                      SEMIGROUPS_gp_is_trivial_perm_gp);
      else
        transfuncs := SEMIGROUPS.FindTranslationFunctionsToGroup(S, t1, t2);
      fi;
      if not transfuncs = fail then
        for funcs in transfuncs do
          fx                := funcs[1];
          fy                := funcs[2];
          unboundpositions  := [];
          for j in [1 .. nrcols] do
            if not IsBound(fy[j]) then
              Add(unboundpositions, j);
            fi;
          od;
          if Length(unboundpositions) > 0 then
            c := List([1 .. Length(unboundpositions)],
                      i -> [1 .. Length(gplist)]);
            partialfunciterator := IteratorOfCartesianProduct(c);
            while not IsDoneIterator(partialfunciterator) do
              funcvals := NextIterator(partialfunciterator);
              for j in [1 .. Length(unboundpositions)] do
                fy[unboundpositions[j]] := gplist[funcvals[j]];
              od;
              Add(linkedpairs, linkedpairfromfuncs(t1, t2, fx, fy));
            od;
          else
            Add(linkedpairs, linkedpairfromfuncs(t1, t2, fx, fy));
          fi;
        od;
      fi;
    od;
  od;

  return Set(linkedpairs);
end;

# Finds the bitranslations of a normalised RMS over a group
# Uses the characterisation of Theorem 1 in
# Clifford and Petrich, 'Some Classes of Completely Regular Semigroups'
# Journal of Algebra 46, 1977
SEMIGROUPS.BitranslationsOfNormalRMS := function(H)
  local S, P, m, n, G, triples, extendf, extendg, nextf, nextg, partialcheckrow,
  partialcheckcol, reject, bt, k, f, g, a;

  S       := UnderlyingSemigroup(H);
  if not SEMIGROUPS.IsNormalRMSOverGroup(S) then
    ErrorNoReturn("Semigroups: SEMIGROUPS.BitranslationsOfNormalRMS: \n",
                  "the argument must be a normalised RMS over a group,");
  fi;
  P       := Matrix(S);
  m       := Size(P);
  n       := Size(P[1]);
  G       := UnderlyingSemigroup(S);
  triples := [];

  extendf := function(k)
    f[k] := 1;
  end;

  extendg := function(k)
    g[k] := 1;
  end;

  nextf := function(k)
    if f[k] = m then
      return fail;
    fi;
    f[k] := f[k] + 1;
    return k;
  end;

  nextg := function(k)
    if g[k] = n then
      return fail;
    fi;
    g[k] := g[k] + 1;
    return k;
  end;

  partialcheckrow := function(k)
    local j;
    for j in [1 .. Minimum(k - 1, n)] do
      if not P[k][g[j]] * a * P[f[1]][j] = P[k][g[1]] * a * P[f[k]][j] then
        return false;
      fi;
    od;
    return true;
  end;

  partialcheckcol := function(k)
    local i;
    for i in [1 .. Minimum(k, m)] do
      if not P[i][g[k]] * a * P[f[1]][k] = P[i][g[1]] * a * P[f[i]][k] then
        return false;
      fi;
    od;
    return true;
  end;

  reject := function(k)
    while k > n do
      if nextf(k) = fail then
        Unbind(f[k]);
        k := k - 1;
      else
        return k;
      fi;
    od;

    while k > m do
      if nextg(k) = fail then
        Unbind(g[k]);
        k := k - 1;
      else
        return k;
      fi;
    od;

    if not IsBound(g[k]) then
      if nextf(k) = fail then
        Unbind(f[k]);
        k := k - 1;
      else
        return k;
      fi;
    fi;

    while k > 0 and nextg(k) = fail do
      Unbind(g[k]);
      if nextf(k) = fail then
        Unbind(f[k]);
        k := k - 1;
      else
        return k;
      fi;
    od;
    return k;
  end;

  # TODO: add flag to not partial check row/col as appropriate
  bt := function(k)
    if k = 0 then
      return 0;
    fi;

    if k > m then
      if partialcheckcol(k) then
        if k = n then
          Add(triples, [a, ShallowCopy(f), ShallowCopy(g)]);
          k := reject(k);
          return bt(k);
        fi;
        k := k + 1;
        extendg(k);
        return bt(k);
      fi;
      k := reject(k);
      return bt(k);
    fi;

    if k > n then
      if partialcheckrow(k) then
        if k = m then
          Add(triples, [a, ShallowCopy(f), ShallowCopy(g)]);
          k := reject(k);
          return bt(k);
        fi;
        k := k + 1;
        extendf(k);
        return bt(k);
      fi;
      k := reject(k);
      return bt(k);
    fi;

    if partialcheckrow(k) then

      if not IsBound(g[k]) then
        extendg(k);
        return bt(k);
      fi;

      if partialcheckcol(k) then
        if k = m and k = n then
          Add(triples, [a, ShallowCopy(f), ShallowCopy(g)]);
          k := reject(k);
          return bt(k);
        fi;

        if k = m then
          k := k + 1;
          extendg(k);
          return bt(k);
        fi;

        k := k + 1;
        extendf(k);
        return bt(k);
      fi;

      k := reject(k);
      return bt(k);
    fi;

    k := reject(k);
    return bt(k);
  end;

  for a in G do
    k := 1;
    f := [1];
    g := [];
    bt(k);
  od;

  # careful with the order!
  Apply(triples, x -> SEMIGROUPS.BitranslationOfNormalRMSByTripleNC(H,
                                                 x![1],
                                                 Transformation(x![3]),
                                                 Transformation(x![2])));
  return triples;
end;

SEMIGROUPS.FamOfRMSLeftTranslationsByTriple := function()
  local fam, type;

  fam       := NewFamily("LeftTranslationsSemigroupElementsFamily",
                          IsLeftTranslationOfNormalRMS);
  type      := NewType(fam, IsLeftTranslationOfNormalRMS);
  fam!.type := type;
  return fam;
end;

SEMIGROUPS.FamOfRMSRightTranslationsByTriple := function()
  local fam, type;

  fam       := NewFamily("RightTranslationsSemigroupElementsFamily",
                          IsRightTranslationOfNormalRMS);
  type      := NewType(fam, IsRightTranslationOfNormalRMS);
  fam!.type := type;
  return fam;
end;

SEMIGROUPS.FamOfRMSBitranslationsByTriple := function()
  local fam, type;

  fam       := NewFamily("BitranslationsSemigroupElementsFamily",
                          IsBitranslationOfNormalRMS);
  type      := NewType(fam, IsBitranslationOfNormalRMS);
  fam!.type := type;
  return fam;
end;

SEMIGROUPS.BitranslationOfNormalRMSByTripleNC := function(H, a, transI, transJ)
  local S, L, R, P, I, J, leftgpfunc, rightgpfunc, l, r;

  S := UnderlyingSemigroup(H);
  L := LeftTranslationsSemigroup(S);
  R := RightTranslationsSemigroup(S);
  P := Matrix(S);
  I := Size(P[1]);
  J := Size(P);

  leftgpfunc  := List([1 .. I], i -> a * P[1 ^ transJ][i]);
  rightgpfunc := List([1 .. J], j -> P[j][1 ^ transI] * a);

  l := LeftTranslationOfNormalRMS(L, leftgpfunc, transI);
  r := RightTranslationOfNormalRMS(R, rightgpfunc, transJ);

  return BitranslationOfNormalRMS(H, l, r);
end;

#############################################################################
# 2. Methods for (zero) simple semigroups
#############################################################################

# For zero simple, don't calculate AsList when LeftTranslations is called
# Just get generators
InstallMethod(LeftTranslations, "for a zero simple semigroup",
[IsEnumerableSemigroupRep and IsFinite and IsZeroSimpleSemigroup],
function(S)
  local L;

  L := LeftTranslationsSemigroup(S);
  GeneratorsOfSemigroup(L);

  return L;
end);

# For zero simple, don't calculate AsList when RightTranslations is called
# Just get generators
InstallMethod(RightTranslations, "for a zero simple semigroup",
[IsEnumerableSemigroupRep and IsFinite and IsZeroSimpleSemigroup],
function(S)
  local R;

  R := RightTranslationsSemigroup(S);
  GeneratorsOfSemigroup(R);

  return R;
end);

# For simple semigroups, don't calculate AsList when LeftTranslations is called
# Just get generators
InstallMethod(LeftTranslations, "for a RMS semigroup",
[IsEnumerableSemigroupRep and IsFinite and IsSimpleSemigroup],
function(S)
  local L;

  L := LeftTranslationsSemigroup(S);
  GeneratorsOfSemigroup(L);

  return L;
end);

# For simple semigroups, don't calculate AsList when RightTranslations is called
# Just get generators
InstallMethod(RightTranslations, "for a RMS semigroup",
[IsEnumerableSemigroupRep and IsFinite and IsSimpleSemigroup],
function(S)
  local R;

  R := RightTranslationsSemigroup(S);
  GeneratorsOfSemigroup(R);

  return R;
end);

# The generators are generators of partial transformation monoid to act on the
# index sets, together with functions to the generators of the group.
InstallMethod(GeneratorsOfSemigroup,
"for the semigroup of left/right translations of a finite 0-simple semigroup",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  local S, iso, inv, reesMatSemi, zero, L, n, gens, G, groupgens, f, fa, t, a;

  S := UnderlyingSemigroup(T);
  if not (IsZeroSimpleSemigroup(S) and IsFinite(S)) then
    TryNextMethod();
  fi;

  iso         := IsomorphismReesZeroMatrixSemigroup(S);
  inv         := InverseGeneralMapping(iso);
  reesMatSemi := Range(iso);
  zero        := MultiplicativeZero(reesMatSemi);
  L           := IsLeftTranslationsSemigroup(T);
  if L then
    n := Length(Rows(reesMatSemi));
  else
    n := Length(Columns(reesMatSemi));
  fi;

  gens      := [];
  G         := UnderlyingSemigroup(reesMatSemi);
  groupgens := GeneratorsOfGroup(G);

  # It would be safer (and more correct) to use GeneratorsOfSemigroup here
  # but the semigroup generated is the same in either case,
  # from fewer generators in this case.
  # If the generators of the partial transformation monoid are changed
  # in the future, this could fail but would be an easy fix
  # (Monoid -> Semigroup)
  for t in GeneratorsOfMonoid(PartialTransformationMonoid(n)) do
    if L then
      f := function(x)
        if (x = zero or x[1] ^ t = n + 1) then
          return zero;
        fi;
        return ReesMatrixSemigroupElement(reesMatSemi, x[1] ^ t,
            x[2], x[3]);
      end;
      Add(gens, LeftTranslation(T, CompositionMapping(
        inv, MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    else
      f := function(x)
        if (x = zero or x[3] ^ t = n + 1) then
          return zero;
        fi;
        return ReesMatrixSemigroupElement(reesMatSemi, x[1],
          x[2], x[3] ^ t);
      end;
      Add(gens, RightTranslation(T, CompositionMapping(
        inv, MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    fi;
  od;

  for a in groupgens do
    fa := function(x)
      if x = 1 then
        return a;
      fi;
      return MultiplicativeNeutralElement(G);
    end;
    if L then
      f := function(x)
        if x = zero then
          return zero;
        fi;
        return ReesMatrixSemigroupElement(reesMatSemi, x[1],
            fa(x[1]) * x[2], x[3]);
      end;
      Add(gens, LeftTranslationNC(T, CompositionMapping(
        inv, MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    else
      f := function(x)
        if x = zero then
          return zero;
        fi;
        return ReesMatrixSemigroupElement(reesMatSemi, x[1],
          x[2] * fa(x[3]), x[3]);
      end;
      Add(gens, RightTranslationNC(T, CompositionMapping(
        inv, MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    fi;
  od;
  return gens;
end);

# The generators are generators of a full transformation monoid to act on the
# index sets, together with functions to the generators of the group.
InstallMethod(GeneratorsOfSemigroup,
"for the semigroup of left/right translations of a finite simple semigroup",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  local S, iso, inv, reesMatSemi, L, n, gens, G, groupgens, IsNRMS, idgpfunc, f,
  fa, t, a;

  S := UnderlyingSemigroup(T);
  if not (IsSimpleSemigroup(S) and IsFinite(S)) then
    TryNextMethod();
  fi;

  iso         := IsomorphismReesMatrixSemigroup(S);
  inv         := InverseGeneralMapping(iso);
  reesMatSemi := Range(iso);
  L           := IsLeftTranslationsSemigroup(T);

  if L then
    n := Length(Rows(reesMatSemi));
  else
    n := Length(Columns(reesMatSemi));
  fi;
  gens      := [];
  G         := UnderlyingSemigroup(reesMatSemi);
  groupgens := GeneratorsOfGroup(G);

  IsNRMS := SEMIGROUPS.IsNormalRMSOverGroup(S);
  if IsNRMS then
    idgpfunc := List([1 .. n], i -> MultiplicativeNeutralElement(G));
  fi;

  # It would be safer (and more correct) to use GeneratorsOfSemigroup here
  # but the semigroup generated is the same in either case,
  # from fewer generators in this case.
  # If the generators of the full transformation monoid are changed
  # in the future, this could fail but would be an easy fix
  # (Monoid -> Semigroup)
  for t in GeneratorsOfMonoid(FullTransformationMonoid(n)) do
    if L then
      if IsNRMS then
        Add(gens, LeftTranslationOfNormalRMS(T, idgpfunc, t));
      else
        f := function(x)
          return ReesMatrixSemigroupElement(reesMatSemi, x[1] ^ t,
                                            x[2], x[3]);
        end;
        Add(gens, LeftTranslation(T, CompositionMapping(
                                        inv,
                                        MappingByFunction(reesMatSemi,
                                                          reesMatSemi, f),
                                        iso)));
      fi;
    else
      if IsNRMS then
        Add(gens, RightTranslationOfNormalRMS(T, idgpfunc, t));
      else
        f := function(x)
          return ReesMatrixSemigroupElement(reesMatSemi, x[1],
            x[2], x[3] ^ t);
        end;
        Add(gens, RightTranslation(T, CompositionMapping(
                                        inv,
                                        MappingByFunction(reesMatSemi,
                                                          reesMatSemi, f),
                                        iso)));
      fi;
    fi;
  od;

  for a in groupgens do
    fa := function(x)
      if x = 1 then
        return a;
      fi;
      return MultiplicativeNeutralElement(G);
    end;
    if L then
      if IsNRMS then
        Add(gens, LeftTranslationOfNormalRMS(T,
                                             List([1 .. n], i -> fa(i)),
                                             IdentityTransformation));
      else
        f := function(x)
          return ReesMatrixSemigroupElement(reesMatSemi, x[1],
              fa(x[1]) * x[2], x[3]);
        end;
        Add(gens, LeftTranslation(T, CompositionMapping(
                                        inv,
                                        MappingByFunction(reesMatSemi,
                                                          reesMatSemi, f),
                                        iso)));
      fi;
    else
      if IsNRMS then
        Add(gens, RightTranslationOfNormalRMS(T,
                                             List([1 .. n], i -> fa(i)),
                                             IdentityTransformation));
      else
        f := function(x)
          return ReesMatrixSemigroupElement(reesMatSemi, x[1],
            x[2] * fa(x[3]), x[3]);
        end;
        Add(gens, RightTranslation(T, CompositionMapping(
                                        inv,
                                        MappingByFunction(reesMatSemi,
                                                          reesMatSemi, f),
                                        iso)));
      fi;
    fi;
  od;
  return gens;
end);

InstallMethod(Size,
"for the semigroup of translations of a completely 0-simple semigroup",
[IsTranslationsSemigroup and IsWholeFamily], 1,
function(T)
  local S, G, reesMatSemi, n;
  S := UnderlyingSemigroup(T);
  if not (IsZeroSimpleSemigroup(S) and IsFinite(S)) then
    TryNextMethod();
  fi;
  reesMatSemi := Range(IsomorphismReesZeroMatrixSemigroup(S));
  G           := UnderlyingSemigroup(reesMatSemi);
  if IsLeftTranslationsSemigroup(T) then
    n := Length(Rows(reesMatSemi));
  else
    n := Length(Columns(reesMatSemi));
  fi;
  return (n * Size(G) + 1) ^ n;
end);

InstallMethod(Size,
"for the semigroup of translations of a completely simple semigroup",
[IsTranslationsSemigroup and IsWholeFamily], 1,
function(T)
  local S, G, reesMatSemi, n;
  S := UnderlyingSemigroup(T);
  if not (IsSimpleSemigroup(S) and IsFinite(S)) then
    TryNextMethod();
  fi;
  reesMatSemi := Range(IsomorphismReesMatrixSemigroup(S));
  G           := UnderlyingSemigroup(reesMatSemi);
  if IsLeftTranslationsSemigroup(T) then
    n := Length(Rows(reesMatSemi));
  else
    n := Length(Columns(reesMatSemi));
  fi;
  return n ^ n * Size(G) ^ n;
end);

# Create a left translation of an IxJ normalised RMS over a group G.
# L should be a left translations semigroup
# gpfunc should be a function (represented as a list) from I to G
# t should be a transformation of I
InstallGlobalFunction(LeftTranslationOfNormalRMS,
function(L, gpfunc, t)
  local G, S;

  if not (IsLeftTranslationOfNormalRMSSemigroup(L)) then
    ErrorNoReturn("Semigroups: LeftTranslationOfNormalRMS: \n",
                  "the first argument must be a semigroup ",
                  "of left translations over a normalised ",
                  "Rees matrix semigroup over a group,");
  fi;
  S := UnderlyingSemigroup(L);
  G := UnderlyingSemigroup(S);

  if not (IsList(gpfunc) and
          ForAll(gpfunc, x -> x in G) and
          Size(gpfunc) = Size(Matrix(S)[1])) then
    ErrorNoReturn("Semigroups: LeftTranslationOfNormalRMS: \n",
                  "the second argument must be a list of group elements ",
                  "of length equal to the number of rows of the ",
                  "underlying semigroup of the first argument,");
  fi;

  if not (IsTransformation(t) and
          DegreeOfTransformation(t) <= Size(Matrix(S)[1])) then
    ErrorNoReturn("Semigroups: LeftTranslationOfNormalRMS: \n",
                  "the third argument must be a transformation on ",
                  "the number of rows of the ",
                  "underlying semigroup of the first argument,");
  fi;

  return LeftTranslationOfNormalRMSNC(L, gpfunc, t);
end);

InstallGlobalFunction(LeftTranslationOfNormalRMSNC,
function(L, gpfunc, t)
  return Objectify(TypeLeftTranslationsSemigroupElements(L), [gpfunc, t]);
end);

# Create a right translation of an IxJ normalised RMS over a group G.
# R should be a right translations semigroup
# gpfunc should be a function (represented as a list) from J to G
# t should be a transformation of J
InstallGlobalFunction(RightTranslationOfNormalRMS,
function(R, gpfunc, t)
  local G, S;

  if not (IsRightTranslationOfNormalRMSSemigroup(R)) then
    ErrorNoReturn("Semigroups: RightTranslationOfNormalRMS: \n",
                  "the first argument must be a semigroup ",
                  "of right translations of a normalised ",
                  "Rees matrix semigroup over a group,");
  fi;
  S := UnderlyingSemigroup(R);
  G := UnderlyingSemigroup(S);

  if not (IsList(gpfunc) and
          ForAll(gpfunc, x -> x in G) and
          Size(gpfunc) = Size(Matrix(S))) then
    ErrorNoReturn("Semigroups: RightTranslationOfNormalRMS: \n",
                  "the second argument must be a list of group elements ",
                  "of length equal to the number of columns of the ",
                  "underlying semigroup of the first argument,");
  fi;

  if not (IsTransformation(t) and
          DegreeOfTransformation(t) <= Size(Matrix(S))) then
    ErrorNoReturn("Semigroups: RightTranslationOfNormalRMS: \n",
                  "the third argument must be a transformation on ",
                  "the number of columns of the ",
                  "underlying semigroup of the first argument,");
  fi;

  return RightTranslationOfNormalRMSNC(R, gpfunc, t);
end);

InstallGlobalFunction(RightTranslationOfNormalRMSNC,
function(R, gpfunc, t)
  return Objectify(TypeRightTranslationsSemigroupElements(R), [gpfunc, t]);
end);

InstallGlobalFunction(BitranslationOfNormalRMS,
function(H, l, r)
  local i, I, j, J, lf, lt, P, rf, rt, S;

  if not IsBitranslationOfNormalRMSSemigroup(H) then
      ErrorNoReturn("Semigroups: BitranslationOfNormalRMS: \n",
                    "the first argument must be a semigroup of ",
                    "bitranslations over a normalised RMS,");
  fi;

  S := UnderlyingSemigroup(H);
  P := Matrix(S);
  I := Size(P[1]);
  J := Size(P);

  lf := l![1];
  lt := l![2];
  rf := r![1];
  rt := r![2];

  for i in [1 .. I] do
    for j in [1 .. J] do
      if not P[j][i ^ lt] * lf[i] = rf[j] * P[j ^ rt][i] then
        ErrorNoReturn("Semigroups: BitranslationOfNormalRMS: \n",
                      "the second and third arguments must be a ",
                      "linked left and right translation, respectively,");
      fi;
    od;
  od;

  return BitranslationOfNormalRMSNC(H, l, r);
end);

InstallGlobalFunction(BitranslationOfNormalRMSNC,
function(H, l, r)
  return Objectify(TypeBitranslations(H), [l, r]);
end);

############################################################################
# 3. Technical Methods
############################################################################

InstallMethod(Representative,
"for a semigroup of left or right translations over a normalised RMS",
[IsTranslationOfNormalRMSSemigroup and IsWholeFamily],
function(T)
  local e, G, S;

  S := UnderlyingSemigroup(T);
  G := UnderlyingSemigroup(S);
  e := MultiplicativeNeutralElement(G);

  if IsLeftTranslationOfNormalRMSSemigroup(T) then
    return LeftTranslationOfNormalRMS(T,
                                      List(Rows(S), x -> e),
                                      IdentityTransformation);
  else
    return RightTranslationOfNormalRMS(T,
                                       List(Columns(S), x -> e),
                                       IdentityTransformation);
  fi;
end);

InstallMethod(Representative, "for a translational hull over a normalised RMS",
[IsBitranslationOfNormalRMSSemigroup and IsWholeFamily],
function(H)
  local L, R, S;

  S := UnderlyingSemigroup(H);
  L := LeftTranslationsSemigroup(S);
  R := RightTranslationsSemigroup(S);

  return BitranslationOfNormalRMS(H, Representative(L), Representative(R));
end);

InstallMethod(\*, "for left translations of a normalised RMS",
IsIdenticalObj,
[IsLeftTranslationOfNormalRMS, IsLeftTranslationOfNormalRMS],
function(x, y)
  return Objectify(FamilyObj(x)!.type,
                   [List([1 .. Size(x![1])], i -> x![1][i ^ y![2]] * y![1][i]),
                    y![2] * x![2]]);
end);

InstallMethod(\=, "for left translations of a normalised RMS",
IsIdenticalObj,
[IsLeftTranslationOfNormalRMS, IsLeftTranslationOfNormalRMS],
function(x, y)
  return x![1] = y![1] and x![2] = y![2];
end);

InstallMethod(\<, "for left translations of a normalised RMS",
IsIdenticalObj,
[IsLeftTranslationOfNormalRMS, IsLeftTranslationOfNormalRMS],
function(x, y)
  return x![2] < y![2] or (x![2] = y![2] and x![1] < y![1]);
end);

InstallMethod(\*, "for right translations of a normalised RMS",
IsIdenticalObj,
[IsRightTranslationOfNormalRMS, IsRightTranslationOfNormalRMS],
function(x, y)
  return Objectify(FamilyObj(x)!.type,
                   [List([1 .. Size(x![1])], j -> x![1][j] * y![1][j ^ x![2]]),
                    x![2] * y![2]]);
end);

InstallMethod(\=, "for right translations of a normalised RMS",
IsIdenticalObj,
[IsRightTranslationOfNormalRMS, IsRightTranslationOfNormalRMS],
function(x, y)
  return x![1] = y![1] and x![2] = y![2];
end);

InstallMethod(\<, "for right translations of a normalised RMS",
IsIdenticalObj,
[IsRightTranslationOfNormalRMS, IsRightTranslationOfNormalRMS],
function(x, y)
  return x![2] < y![2] or (x![2] = y![2] and x![1] < y![1]);
end);

InstallMethod(\*, "for bitranslations of a normalised RMS",
IsIdenticalObj,
[IsBitranslationOfNormalRMS, IsBitranslationOfNormalRMS],
function(x, y)
    return Objectify(FamilyObj(x)!.type, [x![1] * y![1], x![2] * y![2]]);
end);

InstallMethod(\=, "for bitranslations of a normalised RMS",
IsIdenticalObj,
[IsBitranslationOfNormalRMS, IsBitranslationOfNormalRMS],
function(x, y)
  return x![1] = y![1] and x![2] = y![2];
end);

InstallMethod(\<, "for bitranslations of a normalised RMS",
IsIdenticalObj,
[IsBitranslationOfNormalRMS, IsBitranslationOfNormalRMS],
function(x, y)
  return x![1] < y![1] or (x![1] = y![1] and x![2] < y![2]);
end);

InstallMethod(\^, "for a semigroup element and a translation",
[IsReesMatrixSemigroupElement, IsTranslationOfNormalRMS],
function(x, t)
  if IsLeftTranslationOfNormalRMS(t) then
    return RMSElementNC(ReesMatrixSemigroupOfFamily(FamilyObj(x)),
                        x![1] ^ t![2],
                        t![1][x![1]] * x![2],
                        x![3]);
  else
    return RMSElementNC(ReesMatrixSemigroupOfFamily(FamilyObj(x)),
                        x![1],
                        x![2] * t![1][x![3]],
                        x![3] ^ t![2]);
  fi;
end);

InstallMethod(ChooseHashFunction, "for a left or right translation and int",
[IsTranslationOfNormalRMS, IsInt],
function(x, hashlen)
  return rec(func := SEMIGROUPS.HashFunctionForRMSTranslations,
             data := hashlen);
end);

InstallMethod(ChooseHashFunction, "for a translational hull element and int",
[IsBitranslationOfNormalRMS, IsInt],
function(x, hashlen)
  return rec(func := SEMIGROUPS.HashFunctionForRMSBitranslations,
             data := hashlen);
end);
