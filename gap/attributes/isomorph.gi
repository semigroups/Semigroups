#############################################################################
##
##  isomorph.gi
##  Copyright (C) 2014-17                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for finding isomorphisms between semigroups and
# some related methods.
#
# Isomorphism.* methods for transformation, partial perm, bipartition and Rees
# 0-matrix semigroups can be found in the files semitrans.gi, semipperm.gi,
# semibipart.gi , and reesmat.gi.

# returns the lex-least multiplication table of the semigroup <S>

InstallMethod(SmallestMultiplicationTable, "for a semigroup",
[IsSemigroup],
function(S)
  local LitNum, NumLit, diag2lits, tbl2lits, lits_to_tbl, onLiterals, n, mtS,
  diagS, phi, diaglitsS, minS, permS, tbllitsS, stabS;

  LitNum := function(ln, n)
    return [QuoInt(ln - 1, n ^ 2) + 1, QuoInt((ln - 1) mod n ^ 2, n) + 1,
            (ln - 1) mod n + 1];
  end;

  NumLit := function(lit, n)
    # lit = [ row, col, val ]
    return (lit[1] - 1) * n ^ 2 + (lit[2] - 1) * n + lit[3];
  end;

  diag2lits := function(diag, n)
    return List([1 .. n], i -> NumLit([i, i, diag[i]], n));
  end;

  tbl2lits := function(table, n)
    local literals, val, i, j;
    literals := [];
    for i in [1 .. n] do
      for j in [1 .. n] do
        val := table[i][j];
        Add(literals, NumLit([i, j, val], n));
      od;
    od;

    return literals;
  end;

  lits_to_tbl := function(lits, n)
    local table, i, j;
    table := [];
    for i in [0 .. n - 1] do
      table[i + 1] := [];
      for j in [1 .. n] do
        table[i + 1][j] := LitNum(lits[i * n + j], n)[3];
      od;
    od;
    return table;
  end;

  onLiterals := n -> function(ln, pi)
    local lit, imlit;
    lit := LitNum(ln, n);
    imlit := OnTuples(lit, pi);
    return NumLit(imlit, n);
  end;

  # for not too big semigroups...
  n := Size(S);
  mtS := MultiplicationTable(S);
  diagS := DiagonalOfMat(mtS);
  phi := ActionHomomorphism(SymmetricGroup(n), [1 .. n ^ 3], onLiterals(n));

  # get minimal representative of diagonal
  diaglitsS := diag2lits(diagS, n);
  minS := SmallestImageSet(Image(phi), diaglitsS);
  permS := RepresentativeAction(Image(phi), diaglitsS, minS, OnSets);
  diagS := List(minS, x -> LitNum(x, n)[3]);

  # work with stabiliser of new diagonal on changed table
  tbllitsS := OnSets(tbl2lits(mtS, n), permS);
  stabS := Stabilizer(Image(phi), minS, OnSets);

  return lits_to_tbl(SmallestImageSet(stabS, tbllitsS), n);
end);

InstallMethod(IsIsomorphicSemigroup, "for semigroups",
[IsSemigroup, IsSemigroup],
function(S, T)
  return IsomorphismSemigroups(S, T) <> fail;
end);

InstallMethod(IsomorphismSemigroups, "for finite simple semigroups",
[IsSimpleSemigroup and IsFinite, IsSimpleSemigroup and IsFinite],
function(S, T)
  local isoS, rmsS, invT, rmsT, iso;

  # Take an isomorphism of S to an RMS if appropriate
  if not (IsReesMatrixSemigroup(S) and IsWholeFamily(S)
      and IsPermGroup(UnderlyingSemigroup(S))) then
    isoS := IsomorphismReesMatrixSemigroupOverPermGroup(S);
    rmsS := Range(isoS);
  else
    rmsS := S;
  fi;
  # Take an isomorphism of T to an RMS if appropriate
  if not (IsReesMatrixSemigroup(T) and IsWholeFamily(T)
      and IsPermGroup(UnderlyingSemigroup(T))) then
    invT := IsomorphismReesMatrixSemigroupOverPermGroup(T);
    invT := InverseGeneralMapping(invT);
    rmsT := Source(invT);
  else
    rmsT := T;
  fi;

  # Uses more specific method to find isomorphism between RMS/RZMS
  iso := IsomorphismSemigroups(rmsS, rmsT);
  if iso = fail then
    return fail;
  elif IsBound(isoS) and IsBound(invT) then
    return CompositionMapping(invT, iso, isoS);
  elif IsBound(isoS) then
    return CompositionMapping(iso, isoS);
  elif IsBound(invT) then
    return CompositionMapping(invT, iso);
  fi;
  # If this method was selected, at least one of isoS and invT is bound
end);

InstallMethod(IsomorphismSemigroups, "for finite 0-simple semigroups",
[IsZeroSimpleSemigroup and IsFinite, IsZeroSimpleSemigroup and IsFinite],
function(S, T)
  local isoS, rmsS, invT, rmsT, iso;

  # Take an isomorphism of S to an RZMS if appropriate
  if not (IsReesZeroMatrixSemigroup(S) and IsWholeFamily(S)
      and IsPermGroup(UnderlyingSemigroup(S))) then
    isoS := IsomorphismReesZeroMatrixSemigroupOverPermGroup(S);
    rmsS := Range(isoS);
  else
    rmsS := S;
  fi;
  # Take an isomorphism of T to an RZMS if appropriate
  if not (IsReesZeroMatrixSemigroup(T) and IsWholeFamily(T)
      and IsPermGroup(UnderlyingSemigroup(T))) then
    invT := IsomorphismReesZeroMatrixSemigroupOverPermGroup(T);
    invT := InverseGeneralMapping(invT);
    rmsT := Source(invT);
  else
    rmsT := T;
  fi;

  # Uses more specific method to find isomorphism between RMS/RZMS
  iso := IsomorphismSemigroups(rmsS, rmsT);
  if iso = fail then
    return fail;
  elif IsBound(isoS) and IsBound(invT) then
    return CompositionMapping(invT, iso, isoS);
  elif IsBound(isoS) then
    return CompositionMapping(iso, isoS);
  elif IsBound(invT) then
    return CompositionMapping(invT, iso);
  fi;
  # If this method was selected, at least one of isoS and invT is bound
end);

InstallMethod(IsomorphismSemigroups, "for finite monogenic semigroups",
[IsMonogenicSemigroup and IsFinite, IsMonogenicSemigroup and IsFinite],
function(S, T)
  local genS, genT, SS, TT;

  # Monogenic semigroups are of the same size, are not groups, and have the
  # same number of D-classes by this point, and so they are isomorphism
  genS := Representative(MaximalDClasses(S)[1]);
  genT := Representative(MaximalDClasses(T)[1]);
  SS := Semigroup(genS);
  TT := Semigroup(genT);
  return MagmaIsomorphismByFunctionsNC(S, T,
           x -> genT ^ Length(Factorization(SS, x)),
           x -> genS ^ Length(Factorization(TT, x)));
end);

InstallMethod(IsomorphismSemigroups, "for semigroups",
[IsSemigroup, IsSemigroup],
function(S, T)
  local pS, pT;

  if S = T then
    return MagmaIsomorphismByFunctionsNC(S, T, IdFunc, IdFunc);
  elif IsFinite(S) <> IsFinite(T) then
    return fail;
  elif not IsFinite(S) then
    TryNextMethod();
  elif IsSimpleSemigroup(S) <> IsSimpleSemigroup(T) or
      IsZeroSimpleSemigroup(S) <> IsZeroSimpleSemigroup(T) or
      Size(S) <> Size(T) or NrRClasses(S) <> NrRClasses(T) or
      NrDClasses(S) <> NrDClasses(T) or NrLClasses(S) <> NrLClasses(T) or
      NrHClasses(S) <> NrHClasses(T) or NrIdempotents(S) <> NrIdempotents(T)
      then
    return fail;
  fi;

  if IsSimpleSemigroup(S) or IsZeroSimpleSemigroup(S)
      or (IsMonogenicSemigroup(S) and IsMonogenicSemigroup(T)) then
    return IsomorphismSemigroups(S, T);
  fi;

  # Compare the partial orders of the D-classes
  pS := DigraphReflexiveTransitiveClosure(Digraph(PartialOrderOfDClasses(S)));
  pT := DigraphReflexiveTransitiveClosure(Digraph(PartialOrderOfDClasses(T)));
  if not IsIsomorphicDigraph(pS, pT) then
    return fail;
  fi;

  TryNextMethod();
end);
