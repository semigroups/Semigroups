#############################################################################
##
# W  translat.gi
# Y  Copyright (C) 2015-18                     James D. Mitchell, Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
##
#############################################################################
## This file contains methods for dealing with left and right translation
## semigroups, as well as translational hulls.
## When one of these semigroups is created, the attribute AsList
## is calculated.
## To avoid this calculation at the time of creation, you can call
## XTranslationsSemigroup or TranslationalHull
##
## Left/Right translations are stored internally as transformations on the
## indices of the underlying semigroup (determined by AsListCanonical). Hence,
## only finite semigroups are supported.
##
## Much of the implementation in this file was based on the implementation of
## RMS in reesmatsemi.gi in the GAP library - in particular, the creation of
## the semigroups and their relation to their elements.
##
## The code specific to rectangular bands is based on
## Howie, J.M. (1995) 'Fundamentals of Semigroup theory'.
## United Kingdom: Oxford University Press. (p. 116)
##
## This file is organised as follows:
##    1. Internal functions
##    2. Functions for the creation of left/right translations semigroup
##        and translational hulls, and their elements
##    3. Methods for rectangular bands
##    4. Methods for monoids
##    5. Technical methods, eg. PrintObj, *, =, etc.
##
#############################################################################

#############################################################################
# 1. Internal Functions
#############################################################################

# Hash translations by their underlying transformations
  SEMIGROUPS.HashFunctionForTranslations := function(x, data)
    return ORB_HashFunctionForPlainFlatList(x![1], data);
  end;

# Hash linked pairs as sum of underlying transformation hashes
  SEMIGROUPS.HashFunctionForBitranslations := function(x, data)
      return (SEMIGROUPS.HashFunctionForTranslations(x![1], data)
        + SEMIGROUPS.HashFunctionForTranslations(x![2], data)) mod data + 1;
  end;

# Choose how to calculate the elements of a translations semigroup
# TODO: why am I returning a semigroup sometimes and a list other times?
SEMIGROUPS.TranslationsSemigroupElements := function(T)
  local S;
  S := UnderlyingSemigroup(T);
  if IsZeroSimpleSemigroup(S) or
      IsRectangularBand(S) or
      IsSimpleSemigroup(S) or
      SEMIGROUPS.IsNormalRMSOverGroup(S) or
      IsMonogenicSemigroup(S) or
      IsMonoid(S) then
    return Semigroup(GeneratorsOfSemigroup(T));
  elif HasGeneratorsOfSemigroup(S) then
    if IsLeftTranslationsSemigroup(T) then
      return SEMIGROUPS.LeftTranslationsBacktrack(T);
    else
      # TODO: dual or backtrack?
      return SEMIGROUPS.RightTranslationsByDual(T);
    fi;
  fi;
  Error("Semigroups: TranslationsSemigroupElements: \n",
        "no method of calculating this translations semigroup is known,");
end;

# TODO: attribute?
SEMIGROUPS.CanonicalMultTable := function(S)
  local n, slist, sortedlist, t, tinv, M;
  n           := Size(S);
  slist       := AsListCanonical(S);
  sortedlist  := AsSortedList(S);

  t    := Transformation(List([1 .. n],
                         i -> PositionCanonical(S, sortedlist[i])));
  tinv := InverseOfTransformation(t);
  M    := MultiplicationTable(S);

  return List([1 .. n], i -> List([1 .. n], j -> M[i ^ tinv][j ^ tinv] ^ t));
end;

SEMIGROUPS.LeftTranslationsBacktrackData := function(S, multtable)
  local n, slist, sortedlist, gens, m, genspos, possiblefgenvals, multsets,
  r_classes_below, max_R_intersects, intersect, maximals,
  right_intersect_multipliers, left_inverses_by_gen, right_inverses, seen, s, V,
  W, U, C, u, r, i, j, a, t, Lu;
  
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);

  multsets := List(multtable, Set);

  r_classes_below := List([1 .. m],
                          i -> Set(List(multsets[i],
                                        j -> RClass(S, 
                                                    EnumeratorCanonical(S)[j]))));

  max_R_intersects := List([1 .. m], x -> []);
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      intersect := Intersection(r_classes_below[i], r_classes_below[j]);
      maximals := Filtered(intersect, 
                           x -> not ForAny(intersect, 
                                           y -> x <> y and 
                                           IsGreensLessThanOrEqual(x, y)));

      max_R_intersects[i][j] := List(maximals, 
                                     x -> PositionCanonical(S, Representative(x)));
    od;
  od;

  # compute the right multipliers to the intersects from the gens
  right_intersect_multipliers := List([1 .. m], x -> []);
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for a in max_R_intersects[i][j] do
        if not IsBound(right_intersect_multipliers[i][a]) then
          right_intersect_multipliers[i][a] := Positions(multtable[genspos[i]], a);
        fi;
        if not IsBound(right_intersect_multipliers[j][a]) then
          right_intersect_multipliers[j][a] := Positions(multtable[genspos[j]], a);
        fi;
      od;
    od;
  od;

  # for all s in S, store the elements t such that gens[i] * t = s for each i
  left_inverses_by_gen := List([1 .. n], x -> List([1 .. m], y -> []));
  for i in [1 .. m] do
    for t in [1 .. n] do
      Add(left_inverses_by_gen[multtable[genspos[i]][t]][i], t);
    od;
  od;

  # for each t in the left inverses of some a in max_R_intersects[i][j] by 
  # gens[j], compute the right inverses of each s in S under t
  right_inverses := List([1 .. n], x -> List([1 .. n], y -> []));
  seen := List([1 .. n], x -> false);
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for a in max_R_intersects[i][j] do
        for t in left_inverses_by_gen[a][j] do
          # don't repeat the calculation if we've already done it for t!
          if not seen[t] then
            seen[t] := true;
            for u in [1 .. n] do
              s := multtable[u][t];
              Add(right_inverses[s][t], u);
            od;
          fi;
        od;
      od;
    od;
  od;

  # compute the sets V_{j, a, s} from paper
  # again leave the unused ones unbound for fast failure
  V := List([1 .. m], x -> List([1 .. n], y -> []));
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for a in max_R_intersects[i][j] do
        for s in [1 .. n] do
          intersect := [1 .. n];
          for t in left_inverses_by_gen[a][j] do
            intersect := Intersection(intersect, right_inverses[s][t]);
          od;
          V[j][a][s] := intersect;
        od;
      od;
    od;
  od;
 
  # compute the sets W_{i, j, s} from paper
  W := List([1 .. m], x -> List([1 .. m], y -> []));
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for s in [1 .. n] do
        intersect := [1 .. n];
        for a in max_R_intersects[i][j] do
          for r in right_intersect_multipliers[i][a] do
            intersect := Intersection(intersect, V[j][a][multtable[s][r]]);
          od;
        od;
        W[i][j][s] := intersect;
      od;
    od;
  od;

  # compute intersection over a of the sets U_{i, a} from the paper
  U := List([1 .. m], i -> [1 .. n]);
  for i in [1 .. m] do
    for a in multsets[genspos[i]] do
# TODO: optimise
      C := [];
      for Lu in LClasses(S) do
        u := PositionCanonical(S, Representative(Lu));
        if Size(Set(List(left_inverses_by_gen[a][i], 
                         t -> multtable[u][t]))) = 1 then
            UniteSet(C, List(Lu, y -> PositionCanonical(S, y)));
        fi;
      od;
      IntersectSet(U[i], C);
    od;
  od;

  r := rec();
  r.left_inverses_by_gen := left_inverses_by_gen;
  r.W := W;
  r.U := U;
  return r;
end;

SEMIGROUPS.RightTranslationsBacktrackData := function(S, multtable)
  local n, gens, m, genspos, possiblefgenvals, transpose_mult_table,
  transpose_mult_sets, L_classes_below, max_L_intersects, intersect, maximals,
  left_intersect_multipliers, right_inverses_by_gen, left_inverses, seen, s, F,
  G, T, C, u, r, i, j, a, t, l, Ru;
  
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);

  transpose_mult_table := TransposedMat(multtable);
  transpose_mult_sets := List(transpose_mult_table, Set);

  L_classes_below := List([1 .. m],
                          i -> Set(List(transpose_mult_sets[i],
                                        j -> LClass(S,
                                                    EnumeratorCanonical(S)[j]))));

  max_L_intersects := List([1 .. m], x -> []);
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      intersect := Intersection(L_classes_below[i], L_classes_below[j]);
      maximals := Filtered(intersect, 
                           x -> not ForAny(intersect, 
                                           y -> x <> y and 
                                           IsGreensLessThanOrEqual(x, y)));

      max_L_intersects[i][j] := List(maximals, 
                                     x -> PositionCanonical(S, Representative(x)));
    od;
  od;

  # compute the right multipliers to the intersects from the gens
  left_intersect_multipliers := List([1 .. m], x -> []);
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for a in max_L_intersects[i][j] do
        if not IsBound(left_intersect_multipliers[i][a]) then
          left_intersect_multipliers[i][a] := 
              Positions(transpose_mult_table[genspos[i]], a);
        fi;
        if not IsBound(left_intersect_multipliers[j][a]) then
          left_intersect_multipliers[j][a] := 
            Positions(transpose_mult_table[genspos[j]], a);
        fi;
      od;
    od;
  od;

  # for all s in S, store the elements t such that t * gens[i] = s for each i
  right_inverses_by_gen := List([1 .. n], x -> List([1 .. m], y -> []));
  for i in [1 .. m] do
    for t in [1 .. n] do
      Add(right_inverses_by_gen[transpose_mult_table[genspos[i]][t]][i], t);
    od;
  od;

  # for each t in the right inverses of some a in max_L_intersects[i][j] by 
  # gens[j], compute the left inverses of each s in S under t
  left_inverses := List([1 .. n], x -> List([1 .. n], y -> []));
  seen := List([1 .. n], x -> false);
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for a in max_L_intersects[i][j] do
        for t in right_inverses_by_gen[a][j] do
          # don't repeat the calculation if we've already done it for t!
          if not seen[t] then
            seen[t] := true;
            for u in [1 .. n] do
              s := transpose_mult_table[u][t];
              Add(left_inverses[s][t], u);
            od;
          fi;
        od;
      od;
    od;
  od;

  # compute the sets F_{j, a, s} from paper
  # again leave the unused ones unbound for fast failure
  F := List([1 .. m], x -> List([1 .. n], y -> []));
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for a in max_L_intersects[i][j] do
        for s in [1 .. n] do
          intersect := [1 .. n];
          for t in right_inverses_by_gen[a][j] do
            intersect := Intersection(intersect, left_inverses[s][t]);
          od;
          F[j][a][s] := intersect;
        od;
      od;
    od;
  od;
 
  # compute the sets G_{i, j, s} from paper
  G := List([1 .. m], x -> List([1 .. m], y -> []));
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for s in [1 .. n] do
        intersect := [1 .. n];
        for a in max_L_intersects[i][j] do
          for l in left_intersect_multipliers[i][a] do
            intersect := Intersection(intersect,
                                      F[j][a][transpose_mult_table[s][l]]);
          od;
        od;
        G[i][j][s] := intersect;
      od;
    od;
  od;

  # compute intersection over a of the sets T_{i, a} from the paper
  T := List([1 .. m], i -> [1 .. n]);
  for i in [1 .. m] do
    for a in transpose_mult_sets[genspos[i]] do
# TODO: optimise
      C := [];
      for Ru in RClasses(S) do
        u := PositionCanonical(S, Representative(Ru));
        if Size(Set(List(right_inverses_by_gen[a][i], 
                         t -> transpose_mult_table[u][t]))) = 1 then
            UniteSet(C, List(Ru, y -> PositionCanonical(S, y)));
        fi;
      od;
      IntersectSet(T[i], C);
    od;
  od;

  r := rec();
  r.right_inverses_by_gen := right_inverses_by_gen;
  r.T := T;
  r.G := G;
  return r;
end;

SEMIGROUPS.LeftTranslationsBacktrack := function(L)
  local S, n, gens, m, genspos, omega_stack, possiblefgenvals, multtable, data,
  U, W, bt, lambda, out, i, j, s;

  S           := UnderlyingSemigroup(L);
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);

  multtable := SEMIGROUPS.CanonicalMultTable(S);

  data := SEMIGROUPS.LeftTranslationsBacktrackData(S, multtable);
  U := data.U;
  W := data.W;

  # enforce arc consistency with the W_{ i, j, s }
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for s in [1 .. n] do
        if IsEmpty(W[i][j][s]) then
          RemoveSet(possiblefgenvals[i], s);
        fi;
      od;
    od;
  od;
  
  # restrict via the U_{i}
  for i in [1 .. m] do
    IntersectSet(possiblefgenvals[i], U[i]);
  od;

  bt := function(i) 
    local consistent, s, j;
    for s in omega_stack[i][i] do
      lambda[i] := s;
      if i = m then
        Add(out, ShallowCopy(lambda));
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          omega_stack[i + 1][j] := Intersection(omega_stack[i][j], W[i][j][s]);
          if IsEmpty(omega_stack[i + 1][j]) then
            consistent := false;
            break;
          fi;
        od;
        if consistent then
          bt(i + 1);
        fi;
      fi;
    od;
  end;

  omega_stack := [possiblefgenvals];
  lambda := [];
  out := [];
  bt(1);
  Apply(out, x -> LeftTranslationNC(L, x));
  return out;
end;

SEMIGROUPS.RightTranslationsBacktrack := function(L)
  local S, n, gens, m, genspos, omega_stack, multtable, data, G, T,
  possiblegenvals, bt, rho, out, i, j, s;

  S           := UnderlyingSemigroup(L);
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));

  multtable := SEMIGROUPS.CanonicalMultTable(S);

  data := SEMIGROUPS.RightTranslationsBacktrackData(S, multtable);
  G := data.G;
  T := data.T;
  
  possiblegenvals := List([1 .. m], i -> [1 .. n]);

  # enforce arc consistency with the G_{i, j, s }
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for s in [1 .. n] do
        if IsEmpty(G[i][j][s]) then
          RemoveSet(possiblegenvals[i], s);
        fi;
      od;
    od;
  od;
  
  # restrict via the T_{i}
  for i in [1 .. m] do
    IntersectSet(possiblegenvals[i], T[i]);
  od;

  bt := function(i) 
    local consistent, s, j;
    for s in omega_stack[i][i] do
      rho[i] := s;
      if i = m then
        Add(out, ShallowCopy(rho));
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          omega_stack[i + 1][j] := Intersection(omega_stack[i][j], G[i][j][s]);
          if IsEmpty(omega_stack[i + 1][j]) then
            consistent := false;
            break;
          fi;
        od;
        if consistent then
          bt(i + 1);
        fi;
      fi;
    od;
  end;

  omega_stack := [possiblegenvals];
  rho := [];
  out := [];
  bt(1);
  Apply(out, x -> LeftTranslationNC(L, x));
  return out;
end;

SEMIGROUPS.BitranslationsBacktrack := function(H)
  local S, n, gens, m, genspos, l_omega_stack, r_omega_stack, multtable,
  left_data, right_data, U, W, left_inverses_by_gen, G, T,
  right_inverses_by_gen, bt, lambda, rho, out, L, R, i, j, s;

  S             := UnderlyingSemigroup(H);
  n             := Size(S);
  gens          := GeneratorsOfSemigroup(S);
  m             := Size(gens);
  genspos       := List(gens, x -> PositionCanonical(S, x));
  l_omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  r_omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));

  multtable := SEMIGROUPS.CanonicalMultTable(S);

  left_data := SEMIGROUPS.LeftTranslationsBacktrackData(S, multtable);
  right_data := SEMIGROUPS.RightTranslationsBacktrackData(S, multtable);
  U := left_data.U;
  W := left_data.W;
  left_inverses_by_gen := left_data.left_inverses_by_gen;

  G := right_data.G;
  T := right_data.T;
  right_inverses_by_gen := right_data.right_inverses_by_gen;


  l_omega_stack[1] := List([1 .. m], i -> [1 .. n]);
  r_omega_stack[1] := List([1 .. m], i -> [1 .. n]);

  # enforce arc consistency with the G_{i, j, s} and W_{i, j, s}
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for s in [1 .. n] do
        if IsEmpty(W[i][j][s]) then
          RemoveSet(l_omega_stack[1][i], s);
        fi;
        if IsEmpty(G[i][j][s]) then
          RemoveSet(r_omega_stack[1][i], s);
        fi;
      od;
    od;
  od;
  
  # restrict via the T_{i} and U_{i}
  for i in [1 .. m] do
    IntersectSet(l_omega_stack[1][i], U[i]);
    IntersectSet(r_omega_stack[1][i], T[i]);
  od;

  bt := function(i) 
    local k, consistent, s, j;
    # i represents the combined depth in l and r, which alternate in the
    # backtrack
    # k represents the generator number we are defining the value of l or r for
    # now

    if (i mod 2 = 1) then 
      # we are dealing with the left translation
      k := (i + 1)/2;
      for s in l_omega_stack[i][k] do
        consistent := true;
        lambda[k] := s;
        l_omega_stack[i + 1] := [];
        r_omega_stack[i + 1] := [];
        # make sure to take care of linking condition
        # x_i * lambda(x_i) = (x_i)rho * x_i
        for j in [k .. m] do
          if (j > k) then
            l_omega_stack[i + 1][j] := Intersection(l_omega_stack[i][j], W[k][j][s]);
          fi;
          r_omega_stack[i + 1][j] := 
            Intersection(r_omega_stack[i][j],
                         right_inverses_by_gen[multtable[genspos[j]][s]][k]);

          if (j > k and IsEmpty(l_omega_stack[i + 1][j])) or 
             IsEmpty(r_omega_stack[i + 1][j]) then
            consistent := false;
            break;
          fi;
        od;
        if consistent then
          bt(i + 1);
        fi;
      od;
    else
      # we are dealing with the right translation
      k := i/2;
      for s in r_omega_stack[i][k] do
        rho[k] := s;
        if (k = m) then
          Add(out, [ShallowCopy(lambda), ShallowCopy(rho)]);
          continue;
        fi;
        consistent := true;
        l_omega_stack[i + 1] := [];
        r_omega_stack[i + 1] := [];
        for j in [k + 1 .. m] do
          r_omega_stack[i + 1][j] := Intersection(r_omega_stack[i][j], G[k][j][s]);
          l_omega_stack[i + 1][j] := 
            Intersection(l_omega_stack[i][j],
                         left_inverses_by_gen[multtable[s][genspos[j]]][k]);
          if IsEmpty(l_omega_stack[i + 1][j]) or 
             IsEmpty(r_omega_stack[i + 1][j]) then
            consistent := false;
            break;
          fi;
        od;
        if consistent then
          bt(i + 1);
        fi;
      od;
    fi;
  end;

  lambda := [];
  rho := [];
  out := [];
  bt(1);
  
  if ValueOption("SEMIGROUPS_bitranslat_nr_only") = true then
    return Length(out);
  fi;

  L := LeftTranslations(S);
  R := RightTranslations(S);
  Apply(out, x -> Bitranslation(H,
                                LeftTranslationNC(L, x[1]),
                                RightTranslationNC(R, x[2])));
  return out;
end;

SEMIGROUPS.RightTranslationsByDual := function(R)
  local S, Sl, D, Dl, map, inv, dual_trans, gens, translator;

  S           := UnderlyingSemigroup(R);
  Sl          := AsListCanonical(S);
  D           := DualSemigroup(S);
  Dl          := AsListCanonical(D);
  map         := AntiIsomorphismDualSemigroup(S);
  inv         := InverseGeneralMapping(map); 
  dual_trans  := LeftTranslations(D);
  gens        := UnderlyingGenerators(R);

  translator := function(x)
    local out, i;
    out := [];
    for i in [1 .. Length(gens)] do
      out[i] := PositionCanonical(S, ((gens[i] ^ map) ^ x) ^ inv);
    od;
    # TODO: NC
    return RightTranslation(R, out);
  end;

  return List(dual_trans, translator);
end;

# Choose how to calculate the elements of a translational hull
SEMIGROUPS.Bitranslations := function(H)
  local S;
  S := UnderlyingSemigroup(H);
  if IsRectangularBand(S) then
    return Semigroup(GeneratorsOfSemigroup(H));
  elif IsReesZeroMatrixSemigroup(S) then
    return SEMIGROUPS.BitranslationsOfZeroSimple(H);
  elif SEMIGROUPS.IsNormalRMSOverGroup(S) then
    return SEMIGROUPS.BitranslationsOfNormalRMS(H);
  else
    return SEMIGROUPS.BitranslationsBacktrack(H);
  fi;
end;

# Calculates bitranslations of an arbitrary (finite)
# semigroup with known generators.
# This is a backtrack search on functions from the semigroup to itself.
# Given a set X of generators, a linked pair (f, g) of
# translations is completely determined by the values on X. Having fixed x_i,
# we can restrict the values on x_k, k > i, by the linked pair conditions
# s * x_i f(x_k) = (s * x_i)g x_k and x_k f(x_i * s) = (x_k)g x_i * s,
# as well as restriction by the translation condition if Sx_i intersect Sx_k is
# non-empty or x_i S intersect x_k S is non-empty.
SEMIGROUPS.BitranslationsByGenerators := function(H)
  local S, n, isweaklyreductive, nronly, slist, sortedlist, L, R, multtable, t,
  tinv, M, reps, repspos, m, multtablepossets, transposepossets, pos, I, q,
  possibleidempotentfvals, possibleidempotentgvals, possiblefrepvals,
  possiblegrepvals, possiblefrepvalsfromidempotent,
  possiblegrepvalsfromidempotent, restrictbyweakreductivity, extendf,
  propagatef, propagateg, restrictfromf, restrictfromg, unrestrict, reject, bt,
  ftransrestrictionatstage, flinkedrestrictionatstage, gtransrestrictionatstage,
  glinkedrestrictionatstage, whenboundfvals, whenboundgvals, linkedpairs, f, g,
  count, k, i, j, e, s, y;

  S                 := UnderlyingSemigroup(H);
  n                 := Size(S);
  isweaklyreductive := Size(InnerTranslationalHull(S)) = n;
  nronly            := ValueOption("SEMIGROUPS_bitranslat_nr_only") = true;
  slist             := AsListCanonical(S);
  sortedlist        := AsSSortedList(S);
  L                 := LeftTranslations(S);
  R                 := RightTranslations(S);
  multtable         := MultiplicationTable(S);

  t := Transformation(List([1 .. n], i -> PositionCanonical(S, sortedlist[i])));

  tinv := InverseOfTransformation(t);
  M    := MultiplicationTable(S);

  multtable := List([1 .. n], i -> List([1 .. n],
                                        j -> M[i ^ tinv][j ^ tinv] ^ t));

  reps    := GeneratorsOfSemigroup(S);
  repspos := [];
  m       := Size(reps);
  for i in [1 .. m] do
    repspos[i] := Position(slist, reps[i]);
  od;

  # store which elements of the semigroups multiply each given element to form
  # another given element
  # eg., if a * b = a * c = d, with (a,b,c,d) having indices (i,j,k,l)
  # in the multiplication table, then we store [j,k] in the cell [i][l]
  multtablepossets := List([1 .. n], x -> List([1 .. n], y -> []));
  transposepossets := List([1 .. n], x -> List([1 .. n], y -> []));
  for i in [1 .. n] do
    for j in [1 .. n] do
      pos := multtable[i][j];
      Add(multtablepossets[i][pos], j);
      Add(transposepossets[j][pos], i);
    od;
  od;

  I                       := Idempotents(S);
  q                       := Size(I);
  possibleidempotentfvals := [1 .. q];
  possibleidempotentgvals := [1 .. q];
  for e in I do
    possibleidempotentfvals[Position(I, e)] := PositionsProperty(slist,
                                                   x -> x * e = x);
    possibleidempotentgvals[Position(I, e)] := PositionsProperty(slist,
                                                   x -> e * x = x);
  od;

  possiblefrepvals := List([1 .. m], x -> [1 .. n]);
  possiblegrepvals := List([1 .. m], x -> [1 .. n]);

  # restrict values for f, g based on idemopotents
  # if e is an idempotent with r_i * s = e
  # then f(r_i)*s = f(r_i)*s * r_i * s
  # and f(r_i) satisfies f(r_i) * s = x for some value x such that x * e = e
  for i in [1 .. m] do
    for s in S do
      if IsIdempotent(reps[i] * s) then
        possiblefrepvals[i] := Intersection(possiblefrepvals[i],
                                            PositionsProperty(slist,
                                             x -> x * s = x * s * reps[i] * s));
        possiblefrepvalsfromidempotent := [];
        for y in possibleidempotentfvals[Position(I, reps[i] * s)] do
          UniteSet(possiblefrepvalsfromidempotent,
                    transposepossets[Position(slist, s)][y]);
        od;
        possiblefrepvals[i] := Intersection(possiblefrepvals[i],
                                              possiblefrepvalsfromidempotent);
      fi;

      if IsIdempotent(s * reps[i]) then
        possiblegrepvals[i] := Intersection(possiblegrepvals[i],
                                            PositionsProperty(slist,
                                             x -> s * x = s * reps[i] * s * x));
        possiblegrepvalsfromidempotent := [];
        for y in possibleidempotentgvals[Position(I, s * reps[i])] do
          UniteSet(possiblegrepvalsfromidempotent,
                    multtablepossets[Position(slist, s)][y]);
        od;
        possiblegrepvals[i] := Intersection(possiblegrepvals[i],
                                              possiblegrepvalsfromidempotent);
      fi;
    od;
  od;

  # if S is weakly reductive then every pair of bitranslations permute
  # i.e. for (f, g) and (f', g') bitranslations, for all s in S,
  # f(sg') = (fs)g'
  # so if fs is a generator x_i, then (x_i)g lies in the range of f
  restrictbyweakreductivity := function(f, g)
    for i in [1 .. m] do
      if repspos[i] in f then
        # add the restriction...
        possiblegrepvals[i] := Intersection(possiblegrepvals[i], f);
        # stop the backtracking from undoing the restriction
        # by only letting it restore those things in the range of f
        for j in [1 .. m] do
          gtransrestrictionatstage[i][j]
            := Intersection(gtransrestrictionatstage[i][j], f);
          glinkedrestrictionatstage[i][j]
            := Intersection(glinkedrestrictionatstage[i][j], f);
        od;
      fi;
      if repspos[i] in g then
        # add the restriction...
        possiblefrepvals[i] := Intersection(possiblefrepvals[i], g);
        # stop the backtracking from undoing the restriction
        # by only letting it restore those things in the range of g
        for j in [1 .. m] do
          ftransrestrictionatstage[i][j]
            := Intersection(ftransrestrictionatstage[i][j], g);
          flinkedrestrictionatstage[i][j]
            := Intersection(flinkedrestrictionatstage[i][j], g);
        od;
      fi;
    od;
  end;

  extendf := function(k)
    # assign the first possible value of f for the next rep
    f[repspos[k + 1]] := possiblefrepvals[k + 1][1];
    return k + 1;
  end;

  propagatef := function(k)
    for i in [1 .. n] do
      pos := multtable[repspos[k]][i];
      if IsBound(f[pos]) then
        if not f[pos] = multtable[f[repspos[k]]][i] then
          UniteSet(glinkedrestrictionatstage[k][k], possiblegrepvals[k]);
          possiblegrepvals[k] := [];
          return fail;
        fi;
      else
        f[pos]              := multtable[f[repspos[k]]][i];
        whenboundfvals[pos] := k;
      fi;
    od;
    return k;
  end;

  propagateg := function(k)
    for i in [1 .. n] do
      pos := multtable[i][repspos[k]];
      if IsBound(g[pos]) then
        if not g[pos] = multtable[i][g[repspos[k]]] then
          return fail;
        fi;
      else
        g[pos]              := multtable[i][g[repspos[k]]];
        whenboundgvals[pos] := k;
      fi;
    od;
    return k;
  end;

  restrictfromf := function(k)
    local ipos, posrepsks, posfrepsks, fvalsi, gvalsi, p;
      for i in [k + 1 .. m] do
        ipos := repspos[i];
        for j in [1 .. n] do
          posrepsks   := multtable[repspos[k]][j];
          posfrepsks  := multtable[f[repspos[k]]][j];
          # restrict by the translation condition
          for p in multtablepossets[ipos][posrepsks] do
            fvalsi := transposepossets[p][posfrepsks];
            UniteSet(ftransrestrictionatstage[i][k],
                      Difference(possiblefrepvals[i], fvalsi));
            possiblefrepvals[i] := Intersection(possiblefrepvals[i], fvalsi);
            if Size(possiblefrepvals[i]) = 0 then
              return fail;
            fi;
          od;

          # deal with the cases reps[i] = reps[k] * slist[j]
          if ipos = multtable[repspos[k]][j] then
            fvalsi := [posfrepsks];
            UniteSet(ftransrestrictionatstage[i][k],
                      Difference(possiblefrepvals[i], fvalsi));
            possiblefrepvals[i] := Intersection(possiblefrepvals[i], fvalsi);
            if Size(possiblefrepvals[i]) = 0 then
              return fail;
            fi;
        fi;
      od;
      # deal with the cases reps[i] * slist[j] = reps[k]
      for p in multtablepossets[ipos][repspos[k]] do
        fvalsi := transposepossets[p][f[repspos[k]]];
        UniteSet(ftransrestrictionatstage[i][k],
                  Difference(possiblefrepvals[i], fvalsi));
        possiblefrepvals[i] := Intersection(possiblefrepvals[i], fvalsi);
        if Size(possiblefrepvals[i]) = 0 then
          return fail;
        fi;
      od;
    od;
    for i in [k .. m] do
      ipos := repspos[i];
      for j in [1 .. n] do
        # restrict by the linked pair condition
        posrepsks := multtable[repspos[k]][j];
        gvalsi    := transposepossets[posrepsks][multtable[ipos][f[posrepsks]]];
        UniteSet(glinkedrestrictionatstage[i][k],
                  Difference(possiblegrepvals[i], gvalsi));
        possiblegrepvals[i] := Intersection(possiblegrepvals[i], gvalsi);
      od;
      # deal with linked condition on reps[k]
      gvalsi := transposepossets[repspos[k]][multtable[ipos][f[repspos[k]]]];
      UniteSet(glinkedrestrictionatstage[i][k],
                Difference(possiblegrepvals[i], gvalsi));
      possiblegrepvals[i] := Intersection(possiblegrepvals[i], gvalsi);
      if Size(possiblegrepvals[i]) = 0 then
        return fail;
      fi;
    od;
    return k;
  end;

  restrictfromg := function(k)
    local ipos, possrepsk, possgrepsk, gvalsi, fvalsi, p;
    for i in [k + 1 .. m] do
      ipos := repspos[i];
      for j in [1 .. n] do
        possrepsk   := multtable[j][repspos[k]];
        possgrepsk  := multtable[j][g[repspos[k]]];
        for p in transposepossets[ipos][possrepsk] do
          gvalsi := multtablepossets[p][possgrepsk];
          UniteSet(gtransrestrictionatstage[i][k],
                    Difference(possiblegrepvals[i], gvalsi));
          possiblegrepvals[i] := Intersection(possiblegrepvals[i], gvalsi);
        od;

        # deal with the cases reps[i] = s * reps[k] and s * reps[i] = reps[k]
        if ipos = multtable[j][repspos[k]] then
          gvalsi := [possgrepsk];
          UniteSet(gtransrestrictionatstage[i][k],
                    Difference(possiblegrepvals[i], gvalsi));
          possiblegrepvals[i] := Intersection(possiblegrepvals[i], gvalsi);
        fi;

        for p in transposepossets[ipos][repspos[k]] do
          gvalsi := multtablepossets[p][g[repspos[k]]];
          UniteSet(gtransrestrictionatstage[i][k],
                    Difference(possiblegrepvals[i], gvalsi));
          possiblegrepvals[i] := Intersection(possiblegrepvals[i], gvalsi);
        od;

        fvalsi := multtablepossets[possrepsk][multtable[g[possrepsk]][ipos]];
        UniteSet(flinkedrestrictionatstage[i][k],
                  Difference(possiblefrepvals[i], fvalsi));
        possiblefrepvals[i] := Intersection(possiblefrepvals[i], fvalsi);
      od;
      if Size(possiblefrepvals[i]) = 0 or Size(possiblegrepvals[i]) = 0 then
        return fail;
      fi;
    od;
    return k;
  end;

  unrestrict := function(k, unrestrictf)
    for i in [1 .. n] do
      if whenboundgvals[i] = k then
        Unbind(g[i]);
        whenboundgvals[i] := 0;
      fi;
    od;
    for i in [k .. m] do
      UniteSet(possiblegrepvals[i], gtransrestrictionatstage[i][k]);
      UniteSet(possiblefrepvals[i], flinkedrestrictionatstage[i][k]);
      gtransrestrictionatstage[i][k]  := [];
      flinkedrestrictionatstage[i][k] := [];
    od;

    if unrestrictf then
      for i in [1 .. n] do
        if whenboundfvals[i] = k then
          Unbind(f[i]);
          whenboundfvals[i] := 0;
        fi;
      od;
      for i in [k .. m] do
        UniteSet(possiblefrepvals[i], ftransrestrictionatstage[i][k]);
        UniteSet(possiblegrepvals[i], glinkedrestrictionatstage[i][k]);
        ftransrestrictionatstage[i][k]  := [];
        glinkedrestrictionatstage[i][k] := [];
      od;
    fi;
  end;

  reject := function(k)
    local fposrepk, gposrepk;
    if k = 0 then
      return 0;
    fi;
    fposrepk := Position(possiblefrepvals[k], f[repspos[k]]);
    if IsBound(g[repspos[k]]) then
      gposrepk := Position(possiblegrepvals[k], g[repspos[k]]);
    else
      gposrepk := 0;
    fi;

    if gposrepk = 0 then
      if fposrepk < Size(possiblefrepvals[k]) then
        f[repspos[k]] := possiblefrepvals[k][fposrepk + 1];
        unrestrict(k, true);
        return k;
      else
        unrestrict(k, true);
        return reject(k - 1);
      fi;
    elif gposrepk < Size(possiblegrepvals[k]) then
      g[repspos[k]] := possiblegrepvals[k][gposrepk + 1];
      unrestrict(k, false);
      return k;
    elif fposrepk < Size(possiblefrepvals[k]) then
      f[repspos[k]] := possiblefrepvals[k][fposrepk + 1];
      if whenboundgvals[repspos[k]] = 0 then
        Unbind(g[repspos[k]]);
      fi;
      unrestrict(k, true);
      return k;
    else
      if whenboundfvals[repspos[k]] = 0 then
        # this occurs iff f[repspos[k]] was set at stage k
        # and not propagated from another rep
        Unbind(f[repspos[k]]);
      fi;
      if whenboundgvals[repspos[k]] = 0 then
        # this occurs iff g[repspos[k]] was set at stage k
        # and not propagated from another rep
        Unbind(g[repspos[k]]);
      fi;
      unrestrict(k, true);
      return reject(k - 1);
    fi;
  end;

  bt := function(k)
    if k = 0 or k = m + 1 then
      return k;
    elif k = m then
      if not (propagatef(k) = fail or restrictfromf(k) = fail) then
        if not IsBound(g[repspos[k]]) then
          g[repspos[k]] := possiblegrepvals[k][1];
        fi;
        if not propagateg(k) = fail then
          return m + 1;
        fi;
      fi;
      return bt(reject(k));
    elif not (propagatef(k) = fail or restrictfromf(k) = fail) then
      if not IsBound(g[repspos[k]]) then
        g[repspos[k]] := possiblegrepvals[k][1];
      fi;
      if not (propagateg(k) = fail or restrictfromg(k) = fail) then
        return bt(extendf(k));
      else
        return bt(reject(k));
      fi;
    else
      return bt(reject(k));
    fi;
  end;

  # The actual search
  ftransrestrictionatstage  := List([1 .. m], x -> List([1 .. m], y -> []));
  flinkedrestrictionatstage := List([1 .. m], x -> List([1 .. m], y -> []));
  gtransrestrictionatstage  := List([1 .. m], x -> List([1 .. m], y -> []));
  glinkedrestrictionatstage := List([1 .. m], x -> List([1 .. m], y -> []));
  whenboundfvals            := List([1 .. n], x -> 0);
  whenboundgvals            := ShallowCopy(whenboundfvals);
  linkedpairs               := [];

  f := [];
  g := [];

  count := 0;

  k := extendf(0);
  k := bt(k);
  while k = m + 1 do
    if isweaklyreductive then
      restrictbyweakreductivity(f, g);
    fi;
    if nronly then
      count := count + 1;
    else
      Add(linkedpairs, [ShallowCopy(f), ShallowCopy(g)]);
    fi;
    k := bt(reject(k - 1));
  od;

  if nronly then
    return count;
  fi;

  Apply(linkedpairs, x -> BitranslationNC(H,
                            LeftTranslationNC(L, Transformation(x[1])),
                            RightTranslationNC(R, Transformation(x[2]))));

  return linkedpairs;
end;

#############################################################################
# 2. Creation of translations semigroups, translational hull, and elements
#############################################################################

InstallMethod(LeftTranslations, "for a finite enumerable semigroup",
[IsEnumerableSemigroupRep and IsFinite],
function(S)
  local fam, L, type;

  if not IsEnumerableSemigroupRep(S) then
    ErrorNoReturn("Semigroups: LeftTranslations: \n",
                  "the semigroup must have representation ",
                  "IsEnumerableSemigroupRep,");
  fi;

  if HasLeftTranslations(S) then
    return LeftTranslations(S);
  fi;

  if SEMIGROUPS.IsNormalRMSOverGroup(S) then
    fam   := SEMIGROUPS.FamOfRMSLeftTranslationsByTriple();
    type  := fam!.type;
  else
    fam       := NewFamily("LeftTranslationsSemigroupElementsFamily",
                      IsLeftTranslationsSemigroupElement);
    type      := NewType(fam, IsLeftTranslationsSemigroupElement);
    fam!.type := type;
  fi;

  # create the semigroup of left translations
  L := Objectify(NewType(CollectionsFamily(fam), IsLeftTranslationsSemigroup
                         and IsWholeFamily and IsAttributeStoringRep), rec());

  # store the type of the elements in the semigroup
  SetTypeLeftTranslationsSemigroupElements(L, type);
  SetLeftTranslationsSemigroupOfFamily(fam, L);
  SetUnderlyingSemigroup(L, S);
  SetLeftTranslations(S, L);

  SetUnderlyingGenerators(L, GeneratorsOfSemigroup(S));

  return L;
end);

InstallMethod(RightTranslations, "for a finite enumerable semigroup",
[IsEnumerableSemigroupRep and IsFinite],
function(S)
  local fam, type, R;

  if not IsEnumerableSemigroupRep(S) then
    ErrorNoReturn("Semigroups: RightTranslations: \n",
                  "the semigroup must have representation ",
                  "IsEnumerableSemigroupRep,");
  fi;

  if HasRightTranslations(S) then
    return RightTranslations(S);
  fi;

  if SEMIGROUPS.IsNormalRMSOverGroup(S) then
    fam   := SEMIGROUPS.FamOfRMSRightTranslationsByTriple();
    type  := fam!.type;
  else
    fam       := NewFamily("RightTranslationsSemigroupElementsFamily",
                      IsRightTranslationsSemigroupElement);
    type      := NewType(fam, IsRightTranslationsSemigroupElement);
    fam!.type := type;
  fi;

  # create the semigroup of right translations
  R := Objectify(NewType(CollectionsFamily(fam), IsRightTranslationsSemigroup
    and IsWholeFamily and IsAttributeStoringRep), rec());

  # store the type of the elements in the semigroup
  SetTypeRightTranslationsSemigroupElements(R, type);
  SetRightTranslationsSemigroupOfFamily(fam, R);
  SetUnderlyingSemigroup(R, S);
  SetRightTranslations(S, R);

  SetUnderlyingGenerators(R, GeneratorsOfSemigroup(S));

  return R;
end);

InstallMethod(TranslationalHull, "for a finite enumerable semigroup",
[IsEnumerableSemigroupRep and IsFinite],
function(S)
  local fam, type, H;

  if HasTranslationalHull(S) then
    return TranslationalHull(S);
  fi;

  if SEMIGROUPS.IsNormalRMSOverGroup(S) then
    fam   := SEMIGROUPS.FamOfRMSBitranslationsByTriple();
    type  := fam!.type;
  else
    fam := NewFamily("BitranslationsFamily",
                      IsBitranslation);
    type      := NewType(fam, IsBitranslation);
    fam!.type := type;
  fi;

  # create the translational hull
  H := Objectify(NewType(CollectionsFamily(fam), IsBitranslationsSemigroup and
    IsWholeFamily and IsAttributeStoringRep), rec());

  # store the type of the elements in the semigroup
  SetTypeBitranslations(H, type);
  SetTranslationalHullOfFamily(fam, H);
  SetUnderlyingSemigroup(H, S);
  SetTranslationalHull(S, H);

  return H;
end);

# Create and calculate the semigroup of inner left translations
InstallMethod(InnerLeftTranslations, "for a semigroup",
[IsEnumerableSemigroupRep and IsFinite],
function(S)
  local A, I, L, l, s;

  I := [];
  L := LeftTranslations(S);

  if HasGeneratorsOfSemigroup(S) then
    A := GeneratorsOfSemigroup(S);
  else
    A := S;
  fi;
  for s in A do
    l := LeftTranslationNC(L, MappingByFunction(S, S, x -> s * x));
    Add(I, l);
  od;
  return Semigroup(I);
end);

# Create and calculate the semigroup of inner right translations
InstallMethod(InnerRightTranslations, "for a semigroup",
[IsEnumerableSemigroupRep and IsFinite],
function(S)
  local A, I, R, r, s;

  I := [];
  R := RightTranslations(S);

  if HasGeneratorsOfSemigroup(S) then
    A := GeneratorsOfSemigroup(S);
  else
    A := S;
  fi;
  for s in A do
    r := RightTranslationNC(R, MappingByFunction(S, S, x -> x * s));
    Add(I, r);
  od;
  return Semigroup(I);
end);

# Create a left translation as an element of a left translations semigroup.
# Second argument should be a mapping on the underlying semigroup or
# a transformation of its indices (as defined by AsListCanonical)
InstallGlobalFunction(LeftTranslation,
function(L, l)
  local S, reps, gens, semi_list, full_lambda, g, lg, x, y, i, s;
  S     := UnderlyingSemigroup(L);
  reps  := [];

  if not (IsLeftTranslationsSemigroup(L)) then
    ErrorNoReturn("Semigroups: LeftTranslation: \n",
          "the first argument must be a semigroup of left translations,");
  fi;

  gens := UnderlyingGenerators(L);

  # TODO allow general mapping from gens to S
  if IsGeneralMapping(l) then
    if not (S = Source(l) and Source(l) = Range(l)) then
      ErrorNoReturn("Semigroups: LeftTranslation (from Mapping): \n",
            "the domain and range of the second argument must be ",
            "the underlying semigroup of the first,");
    fi;
    if ForAny(gens, s -> ForAny(S, t -> (s ^ l) * t <> (s * t) ^ l)) then
      ErrorNoReturn("Semigroups: LeftTranslation: \n",
             "the mapping given must define a left translation,");
    fi;
  elif IsDenseList(l) then
    if not Size(l) = Size(gens) then
      ErrorNoReturn("Semigroups: LeftTranslation: \n",
                    "the second argument must map indices of generators to ",
                    "indices of elements of the semigroup of the first ",
                    "argument,");
    fi;
    if not ForAll(l, y -> IsInt(y) and y <= Size(S)) then
      ErrorNoReturn("Semigroups: LeftTranslation: \n",
                    "the second argument must map indices of generators to ",
                    "indices of elements of the semigroup of the first ",
                    "argument,");
    fi;
    # TODO store and use CanonicalMultiplicationTable and
    # LeftTranslationsBacktrackData
    semi_list := AsListCanonical(S);
    full_lambda := [];
    for i in [1 .. Size(gens)] do
      g := gens[i];
      lg := l[i];
      for s in S do
        x := PositionCanonical(S, g * s);
        y := PositionCanonical(S, semi_list[lg] * s);
        if not IsBound(full_lambda[x]) then
          full_lambda[x] := y;
        fi;
        if full_lambda[x] <> y then
          ErrorNoReturn("Semigroups: LeftTranslation: \n",
                        "the transformation given must define a left ",
                        "translation,");
        fi;
      od;
    od;
  else
    ErrorNoReturn("Semigroups: LeftTranslation: \n",
          "the first argument should be a left translations semigroup, and ",
          "the second argument should be a mapping on the underlying ",
          "semigroup of the first argument, or a list of indices of values ",
          "of the generators under the translation,");
  fi;
  return LeftTranslationNC(L, l);
end);

InstallGlobalFunction(LeftTranslationNC,
function(L, l)
  local S, tup, gens, map_as_list, i;
  S := UnderlyingSemigroup(L);
  if IsLeftTranslationOfNormalRMSSemigroup(L) then
    tup := SEMIGROUPS.LeftTransToNormalRMSTuple(S, l);
    return LeftTranslationOfNormalRMSNC(L, tup[1], tup[2]);
  fi;
  if IsDenseList(l) then
    return Objectify(TypeLeftTranslationsSemigroupElements(L), [l]);
  fi;
  # l is a mapping on UnderlyingSemigroup(S)
  gens := UnderlyingGenerators(L);
  map_as_list  := [];
  for i in [1 .. Length(gens)] do
    map_as_list[i] := PositionCanonical(S, gens[i] ^ l);
  od;

  return Objectify(TypeLeftTranslationsSemigroupElements(L), [map_as_list]);
end);

# Same for right translations.
InstallGlobalFunction(RightTranslation,
function(R, r)
  local S, reps, gens, semi_list, full_rho, g, rg, x, y, i, s;
  S     := UnderlyingSemigroup(R);
  reps  := [];

  if not (IsRightTranslationsSemigroup(R)) then
    ErrorNoReturn("Semigroups: RightTranslation: \n",
          "the first argument must be a semigroup of right translations,");
  fi;

  gens := UnderlyingGenerators(R);

  # TODO allow general mapping from gens to S
  if IsGeneralMapping(r) then
    if not (S = Source(r) and Source(r) = Range(r)) then
      ErrorNoReturn("Semigroups: RightTranslation (from Mapping): \n",
            "the domain and range of the second argument must be ",
            "the underlying semigroup of the first,");
    fi;
    if ForAny(gens, s -> ForAny(S, t -> s * (t ^ r) <> (s * t) ^ r)) then
      ErrorNoReturn("Semigroups: RightTranslation: \n",
             "the mapping given must define a right translation,");
    fi;
  elif IsDenseList(r) then
    if not Size(r) = Size(gens) then
      ErrorNoReturn("Semigroups: RightTranslation: \n",
                    "the second argument must map indices of generators to ",
                    "indices of elements of the semigroup of the first ",
                    "argument,");
    fi;
    if not ForAll(r, y -> IsInt(y) and y <= Size(S)) then
      ErrorNoReturn("Semigroups: RightTranslation: \n",
                    "the second argument must map indices of generators to ",
                    "indices of elements of the semigroup of the first ",
                    "argument,");
    fi;
    # TODO store and use CanonicalMultiplicationTable and
    # RightTranslationsBacktrackData
    semi_list := AsListCanonical(S);
    full_rho := [];
    for i in [1 .. Size(gens)] do
      g := gens[i];
      rg := r[i];
      for s in S do
        x := PositionCanonical(S, s * g);
        y := PositionCanonical(S, s * semi_list[rg]);
        if not IsBound(full_rho[x]) then
          full_rho[x] := y;
        fi;
        if full_rho[x] <> y then
          ErrorNoReturn("Semigroups: RightTranslation: \n",
                        "the transformation given must define a right ",
                        "translation,");
        fi;
      od;
    od;
  else
    ErrorNoReturn("Semigroups: RightTranslation: \n",
          "the first argument should be a right translations semigroup, and ",
          "the second argument should be a mapping on the underlying ",
          "semigroup of the first argument, or a list of indices of values ",
          "of the generators under the translation,");
  fi;
  return RightTranslationNC(R, r);
end);

InstallGlobalFunction(RightTranslationNC,
function(R, r)
  local S, tup, gens, map_as_list, i;
  S := UnderlyingSemigroup(R);
  if IsRightTranslationOfNormalRMSSemigroup(R) then
    tup := SEMIGROUPS.RightTransToNormalRMSTuple(S, r);
    return RightTranslationOfNormalRMSNC(R, tup[1], tup[2]);
  fi;
  if IsDenseList(r) then
    return Objectify(TypeRightTranslationsSemigroupElements(R), [r]);
  fi;
  # r is a mapping on UnderlyingSemigroup(S)
  gens := UnderlyingGenerators(R);
  map_as_list  := [];
  for i in [1 .. Length(gens)] do
    map_as_list[i] := PositionCanonical(S, gens[i] ^ r);
  od;

  return Objectify(TypeRightTranslationsSemigroupElements(R), [map_as_list]);
end);

# Creates the ideal of the translational hull consisting of
# all inner bitranslations
InstallMethod(InnerTranslationalHull, "for a semigroup",
[IsEnumerableSemigroupRep and IsFinite],
function(S)
  local A, I, H, L, R, l, r, s;

  I := [];
  H := TranslationalHull(S);
  L := LeftTranslations(S);
  R := RightTranslations(S);
  if HasGeneratorsOfSemigroup(S) then
    A := GeneratorsOfSemigroup(S);
  else
    A := S;
  fi;
  for s in A do
    l := LeftTranslationNC(L, MappingByFunction(S, S, x -> s * x));
    r := RightTranslationNC(R, MappingByFunction(S, S, x -> x * s));
    Add(I, BitranslationNC(H, l, r));
  od;
  return Semigroup(I);
end);

# Get the number of bitranslations without necessarily computing them all
InstallMethod(NrBitranslations, "for a semigroup",
[IsEnumerableSemigroupRep and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return SEMIGROUPS.BitranslationsBacktrack(TranslationalHull(S) :
                                            SEMIGROUPS_bitranslat_nr_only);
end);

# Creates a linked pair (l, r) from a left translation l and a right
# translation r, as an element of a translational hull H.
InstallGlobalFunction(Bitranslation,
function(H, l, r)
  local S, L, R, gens;

  if not IsBitranslationsSemigroup(H) then
    ErrorNoReturn("Semigroups: Bitranslation: \n",
          "the first argument must be a translational hull,");
  fi;

  if not (IsLeftTranslationsSemigroupElement(l) and
            IsRightTranslationsSemigroupElement(r)) then
    ErrorNoReturn("Semigroups: Bitranslation: \n",
          "the second argument must be a left translation ",
          "and the third argument must be a right translation,");
    return;
  fi;

  S := UnderlyingSemigroup(H);
  L := LeftTranslationsSemigroupOfFamily(FamilyObj(l));
  R := RightTranslationsSemigroupOfFamily(FamilyObj(r));

  gens := GeneratorsOfSemigroup(S);

  if not (UnderlyingSemigroup(L) = S and UnderlyingSemigroup(R) = S) then
      ErrorNoReturn("Semigroups: Bitranslation: \n",
            "each argument must have the same underlying semigroup,");
  fi;

  if ForAny(gens, t -> ForAny(gens, s -> s * (t ^ l) <> (s ^ r) * t)) then
     ErrorNoReturn("Semigroups: Bitranslation: \n",
           "the translations given must form a linked pair,");
  fi;

  return BitranslationNC(H, l, r);
end);

InstallGlobalFunction(BitranslationNC,
function(H, l, r)
  return Objectify(TypeBitranslations(H), [l, r]);
end);

#############################################################################
# 3. Methods for rectangular bands
#############################################################################

# Every transformation on the relevant index set corresponds to a translation.
# The R classes of an I x J rectangular band correspond to (i, J) for i in I.
# Dually for L classes.
InstallMethod(Size,
"for the semigroup of left or right translations of a rectangular band",
[IsTranslationsSemigroup and IsWholeFamily], 2,
function(T)
  local S, n;
  S := UnderlyingSemigroup(T);
  if not IsRectangularBand(S) then
    TryNextMethod();
  fi;
  if IsLeftTranslationsSemigroup(T) then
    n := NrRClasses(S);
  else
    n := NrLClasses(S);
  fi;

  return n ^ n;
end);

# The translational hull of a rectangular band is the direct product of the
# left translations and right translations
InstallMethod(Size, "for the translational hull of a rectangular band",
[IsBitranslationsSemigroup and IsWholeFamily],
1,
function(H)
  local S, L, R;
  S := UnderlyingSemigroup(H);
  if not IsRectangularBand(S) then
    TryNextMethod();
  fi;
  L := LeftTranslations(S);
  R := RightTranslations(S);
  return Size(L) * Size(R);
end);

# Generators of the left/right translations semigroup on the I x J rectangular
# band correspond to the generators of the full transformation monoid on I or J.
InstallMethod(GeneratorsOfSemigroup,
"for the semigroup of left or right translations of a rectangular band",
[IsTranslationsSemigroup and IsWholeFamily],
2,
function(T)
  local S, n, iso, inv, reesMatSemi, gens, t, f;
  S := UnderlyingSemigroup(T);
  if not IsRectangularBand(S) then
    TryNextMethod();
  fi;

  iso         := IsomorphismReesMatrixSemigroup(S);
  inv         := InverseGeneralMapping(iso);
  reesMatSemi := Range(iso);
  if IsLeftTranslationsSemigroup(T) then
    n := Length(Rows(reesMatSemi));
  else
    n := Length(Columns(reesMatSemi));
  fi;

  gens := [];
  for t in GeneratorsOfMonoid(FullTransformationMonoid(n)) do
    if IsLeftTranslationsSemigroup(T) then
      f := function(x)
        return ReesMatrixSemigroupElement(reesMatSemi, x[1] ^ t,
          (), x[3]);
      end;
      Add(gens, LeftTranslationNC(T, CompositionMapping(inv,
      MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    else
      f := function(x)
        return ReesMatrixSemigroupElement(reesMatSemi, x[1],
          (), x[3] ^ t);
      end;
      Add(gens, RightTranslationNC(T, CompositionMapping(inv,
        MappingByFunction(reesMatSemi, reesMatSemi, f), iso)));
    fi;
  od;
  return gens;
end);

# Generators of translational hull are the direct product of
# generators of left/right translations semigroup for rectangular bands
# since they are monoids
InstallMethod(GeneratorsOfSemigroup,
"for the translational hull of a rectangular band",
[IsBitranslationsSemigroup],
2,
function(H)
  local S, leftGens, rightGens, l, r, gens;

  S := UnderlyingSemigroup(H);
  if not IsRectangularBand(S) then
    TryNextMethod();
  fi;

  leftGens  := GeneratorsOfSemigroup(LeftTranslations(S));
  rightGens := GeneratorsOfSemigroup(RightTranslations(S));
  gens      := [];

  for l in leftGens do
    for r in rightGens do
      Add(gens, BitranslationNC(H, l, r));
    od;
  od;

  return gens;
end);

#############################################################################
# 4. Methods for monoids
#############################################################################

InstallMethod(GeneratorsOfSemigroup,
"for the left translations of a finite monogenic semigroup",
[IsLeftTranslationsSemigroup and IsWholeFamily],
function(L)
  if not IsMonoid(UnderlyingSemigroup(L)) then
    TryNextMethod();
  fi;
  return GeneratorsOfSemigroup(InnerLeftTranslations(UnderlyingSemigroup(L)));
end);

InstallMethod(GeneratorsOfSemigroup,
"for the right translations of a finite monogenic semigroup",
[IsRightTranslationsSemigroup and IsWholeFamily],
function(R)
  if not IsMonoid(UnderlyingSemigroup(R)) then
    TryNextMethod();
  fi;
  return GeneratorsOfSemigroup(InnerRightTranslations(UnderlyingSemigroup(R)));
end);

InstallMethod(GeneratorsOfSemigroup,
"for the right translations of a finite monogenic semigroup",
[IsBitranslationsSemigroup and IsWholeFamily],
function(H)
  if not IsMonoid(UnderlyingSemigroup(H)) then
    TryNextMethod();
  fi;
  return GeneratorsOfSemigroup(InnerTranslationalHull(UnderlyingSemigroup(H)));
end);

InstallMethod(Size,
"for a semigroup of left/right translations of a monogenic semigroup",
[IsTranslationsSemigroup and IsWholeFamily],
1,
function(T)
  if not IsMonoid(UnderlyingSemigroup(T)) then
    TryNextMethod();
  fi;
  return Size(UnderlyingSemigroup(T));
end);

InstallMethod(Size, "for a translational hull of a monogenic semigroup",
[IsBitranslationsSemigroup and IsWholeFamily],
1,
function(H)
  if not IsMonoid(UnderlyingSemigroup(H)) then
    TryNextMethod();
  fi;
  return Size(UnderlyingSemigroup(H));
end);

#############################################################################
# 5. Methods for monogenic semigroups
#############################################################################

InstallMethod(GeneratorsOfSemigroup,
"for the left translations of a finite monogenic semigroup",
[IsLeftTranslationsSemigroup and IsWholeFamily],
function(L)
  if not IsMonogenicSemigroup(UnderlyingSemigroup(L)) then
    TryNextMethod();
  fi;
  return Union([One(L)],
     GeneratorsOfSemigroup(InnerLeftTranslations(UnderlyingSemigroup(L))));
end);

InstallMethod(GeneratorsOfSemigroup,
"for the right translations of a finite monogenic semigroup",
[IsRightTranslationsSemigroup and IsWholeFamily],
function(R)
  if not IsMonogenicSemigroup(UnderlyingSemigroup(R)) then
    TryNextMethod();
  fi;
  return Union([One(R)],
     GeneratorsOfSemigroup(InnerRightTranslations(UnderlyingSemigroup(R))));
end);

InstallMethod(GeneratorsOfSemigroup,
"for the right translations of a finite monogenic semigroup",
[IsBitranslationsSemigroup and IsWholeFamily],
function(H)
  if not IsMonogenicSemigroup(UnderlyingSemigroup(H)) then
    TryNextMethod();
  fi;
  return Union([One(H)],
     GeneratorsOfSemigroup(InnerTranslationalHull(UnderlyingSemigroup(H))));
end);

InstallMethod(Size,
"for a semigroup of left/right translations of a monogenic semigroup",
[IsTranslationsSemigroup and IsWholeFamily],
1,
function(T)
  if not IsMonogenicSemigroup(UnderlyingSemigroup(T)) then
    TryNextMethod();
  fi;
  return Size(UnderlyingSemigroup(T));
end);

InstallMethod(Size, "for a translational hull of a monogenic semigroup",
[IsBitranslationsSemigroup and IsWholeFamily],
1,
function(H)
  if not IsMonogenicSemigroup(UnderlyingSemigroup(H)) then
    TryNextMethod();
  fi;
  return Size(UnderlyingSemigroup(H));
end);

#############################################################################
# 6. Technical methods
#############################################################################

InstallMethod(AsList, "for a semigroup of left or right translations",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  if HasGeneratorsOfSemigroup(T) then
    return Immutable(AsList(Semigroup(GeneratorsOfSemigroup(T))));
  fi;
  return Immutable(AsList(SEMIGROUPS.TranslationsSemigroupElements(T)));
end);

InstallMethod(AsList, "for a translational hull",
[IsBitranslationsSemigroup and IsWholeFamily],
function(H)
  return Immutable(AsList(SEMIGROUPS.Bitranslations(H)));
end);

InstallMethod(Size, "for a semigroups of left or right translations",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  return Size(AsList(T));
end);

InstallMethod(Size, "for a translational hull",
[IsBitranslationsSemigroup and IsWholeFamily],
function(H)
  return Size(AsList(H));
end);

InstallMethod(Representative, "for a semigroup of left or right translations",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  local S;
  S := UnderlyingSemigroup(T);
  if IsLeftTranslationsSemigroup(T) then
    return LeftTranslation(T, MappingByFunction(S, S, x -> x));
  else
    return RightTranslation(T, MappingByFunction(S, S, x -> x));
  fi;
end);

InstallMethod(Representative, "for a translational hull",
[IsBitranslationsSemigroup and IsWholeFamily],
function(H)
  local L, R, S;
  S := UnderlyingSemigroup(H);
  L := LeftTranslations(S);
  R := RightTranslations(S);
  return Bitranslation(H, Representative(L), Representative(R));
end);

InstallMethod(ViewObj, "for a semigroup of left or right translations",
[IsTranslationsSemigroup and IsWholeFamily],
function(T)
  Print("<the semigroup of");
  if IsLeftTranslationsSemigroup(T) then Print(" left");
    else Print(" right");
  fi;
  Print(" translations of ", ViewString(UnderlyingSemigroup(T)), ">");
end);

InstallMethod(ViewObj, "for a semigroup of translations",
[IsTranslationsSemigroup], PrintObj);

InstallMethod(PrintObj, "for a semigroup of translations",
[IsTranslationsSemigroup and HasGeneratorsOfSemigroup],
function(T)
  Print("<semigroup of ");
  if IsLeftTranslationsSemigroup(T) then Print("left ");
  else Print("right ");
  fi;
  Print("translations of ", ViewString(UnderlyingSemigroup(T)), " with ",
    Length(GeneratorsOfSemigroup(T)),
    " generator");
  if Length(GeneratorsOfSemigroup(T)) > 1 then
    Print("s");
  fi;
  Print(">");
  return;
end);

InstallMethod(ViewObj, "for a translation",
[IsTranslationsSemigroupElement], PrintObj);

InstallMethod(PrintObj, "for a translation",
[IsTranslationsSemigroupElement],
function(t)
  local L, S;
  L := IsLeftTranslationsSemigroupElement(t);
  if L then
    S := UnderlyingSemigroup(LeftTranslationsSemigroupOfFamily(FamilyObj(t)));
    Print("<left ");
  else
    S := UnderlyingSemigroup(RightTranslationsSemigroupOfFamily(FamilyObj(t)));
    Print("<right ");
  fi;

  Print("translation on ", ViewString(S), ">");
end);

InstallMethod(ViewObj, "for a translational hull",
[IsBitranslationsSemigroup], PrintObj);

InstallMethod(PrintObj, "for a translational hull",
[IsBitranslationsSemigroup and IsWholeFamily],
function(H)
  Print("<translational hull over ", ViewString(UnderlyingSemigroup(H)), ">");
end);

InstallMethod(PrintObj, "for a subsemigroup of a translational hull",
[IsBitranslationsSemigroup],
function(H)
  Print("<semigroups of translational hull elements over ",
        ViewString(UnderlyingSemigroup(H)), ">");
end);

InstallMethod(ViewObj, "for a translational hull element",
[IsBitranslation], PrintObj);

InstallMethod(PrintObj, "for a translational hull element",
[IsBitranslation],
function(t)
  local H;
  H := TranslationalHullOfFamily(FamilyObj(t));
  Print("<linked pair of translations on ",
        ViewString(UnderlyingSemigroup(H)), ">");
end);

# Note the order of multiplication
InstallMethod(\*, "for left translations of a semigroup",
IsIdenticalObj,
[IsLeftTranslationsSemigroupElement, IsLeftTranslationsSemigroupElement],
function(x, y)
  local L, S, prod, i;
  L := LeftTranslationsSemigroupOfFamily(FamilyObj(x));
  S := UnderlyingSemigroup(L);
  prod := [];
  for i in [1 .. Size(UnderlyingGenerators(L))] do
    prod[i] := PositionCanonical(S, EnumeratorCanonical(S)[y![1][i]] ^ x);
  od;
  return Objectify(FamilyObj(x)!.type, [prod]);
end);

InstallMethod(\=, "for left translations of a semigroup",
IsIdenticalObj,
[IsLeftTranslationsSemigroupElement, IsLeftTranslationsSemigroupElement],
function(x, y)
  return x![1] = y![1];
end);

InstallMethod(\<, "for left translations of a semigroup",
IsIdenticalObj,
[IsLeftTranslationsSemigroupElement, IsLeftTranslationsSemigroupElement],
function(x, y)
  return x![1] < y![1];
end);

# Different order of multiplication
InstallMethod(\*, "for right translations of a semigroup",
IsIdenticalObj,
[IsRightTranslationsSemigroupElement, IsRightTranslationsSemigroupElement],
function(x, y)
  local R, S, prod, i;
  R := RightTranslationsSemigroupOfFamily(FamilyObj(x));
  S := UnderlyingSemigroup(R);
  prod := [];
  for i in [1 .. Size(UnderlyingGenerators(R))] do
    prod[i] := PositionCanonical(S, EnumeratorCanonical(S)[x![1][i]] ^ y);
  od;
  return Objectify(FamilyObj(x)!.type, [prod]);
end);

InstallMethod(\=, "for right translations of a semigroup",
IsIdenticalObj,
[IsRightTranslationsSemigroupElement, IsRightTranslationsSemigroupElement],
function(x, y)
  return x![1] = y![1];
end);

InstallMethod(\<, "for right translations of a semigroup",
IsIdenticalObj,
[IsRightTranslationsSemigroupElement, IsRightTranslationsSemigroupElement],
function(x, y)
  return x![1] < y![1];
end);

InstallMethod(\^, "for a semigroup element and a left translation",
[IsAssociativeElement, IsLeftTranslationsSemigroupElement],
function(x, t)
  local L, S, gens, enum, g, s;
  L := LeftTranslationsSemigroupOfFamily(FamilyObj(t));
  S := UnderlyingSemigroup(L);
  if not x in S then
    ErrorNoReturn("Semigroups: ^ for a semigroup element and left translation:",
                  "\n the first argument must be an element of the domain of ",
                  "the second,");
  fi;
  gens := UnderlyingGenerators(L);
  enum := EnumeratorCanonical(S);
  x := PositionCanonical(S, x);
  g := Position(gens, enum[FirstLetter(S, x)]);
  s := Suffix(S, x);
  if s = 0 then
    return enum[t![1][g]];
  else
    return enum[t![1][g]] * enum[s];
  fi;
end);

InstallMethod(\^, "for a semigroup element and a right translation",
[IsAssociativeElement, IsRightTranslationsSemigroupElement],
function(x, t)
  local R, S, gens, enum, g, s;
  R := RightTranslationsSemigroupOfFamily(FamilyObj(t));
  S := UnderlyingSemigroup(R);
  if not x in S then
    ErrorNoReturn("Semigroups: ^ for a semigroup element and right translation:",
                  "\n the first argument must be an element of the domain of ",
                  "the second,");
  fi;
  gens := UnderlyingGenerators(R);
  enum := EnumeratorCanonical(S);
  x := PositionCanonical(S, x);
  g := Position(gens, enum[FinalLetter(S, x)]);
  s := Prefix(S, x);
  if s = 0 then
    return enum[t![1][g]];
  else
    return enum[s] * enum[t![1][g]];
  fi;
end);

InstallMethod(\*, "for bitranslations",
IsIdenticalObj,
[IsBitranslation, IsBitranslation],
function(x, y)
  return Objectify(FamilyObj(x)!.type, [x![1] * y![1], x![2] * y![2]]);
end);

InstallMethod(\=, "for translational hull elements (linked pairs)",
IsIdenticalObj,
[IsBitranslation, IsBitranslation],
function(x, y)
  return x![1] = y![1] and x![2] = y![2];
end);

InstallMethod(\<, "for translational hull elements (linked pairs)",
IsIdenticalObj,
[IsBitranslation, IsBitranslation],
function(x, y)
  return x![1] < y![1] or (x![1] = y![1] and x![2] < y![2]);
end);

InstallMethod(UnderlyingSemigroup,
"for a semigroup of left or right translations",
[IsTranslationsSemigroup],
function(T)
  if IsLeftTranslationsSemigroup(T) then
    return UnderlyingSemigroup(LeftTranslationsSemigroupOfFamily(
                                                                ElementsFamily(
                                                                FamilyObj(T))));
  else
    return UnderlyingSemigroup(RightTranslationsSemigroupOfFamily(
                                                                ElementsFamily(
                                                                FamilyObj(T))));
  fi;
end);

InstallMethod(UnderlyingSemigroup,
"for a subsemigroup of the translational hull",
[IsBitranslationsSemigroup],
function(H)
    return UnderlyingSemigroup(TranslationalHullOfFamily(ElementsFamily(
                                                        FamilyObj(H))));
end);

InstallMethod(ChooseHashFunction, "for a left or right translation and int",
[IsTranslationsSemigroupElement, IsInt],
function(x, hashlen)
  return rec(func := SEMIGROUPS.HashFunctionForTranslations,
             data := hashlen);
end);

InstallMethod(ChooseHashFunction, "for a translational hull element and int",
[IsBitranslation, IsInt],
function(x, hashlen)
  return rec(func := SEMIGROUPS.HashFunctionForBitranslations,
             data := hashlen);
end);

InstallMethod(OneOp, "for a translational hull",
[IsBitranslation],
function(h)
  local H, L, R, S, l, r;
  H := TranslationalHullOfFamily(FamilyObj(h));
  S := UnderlyingSemigroup(H);
  L := LeftTranslations(S);
  R := RightTranslations(S);
  l := LeftTranslation(L, MappingByFunction(S, S, x -> x));
  r := RightTranslation(R, MappingByFunction(S, S, x -> x));
  return Bitranslation(H, l, r);
end);

InstallMethod(OneOp, "for a semigroup of translations",
[IsTranslationsSemigroupElement],
function(t)
  local S, T;
  if IsLeftTranslationsSemigroupElement(t) then
    T := LeftTranslationsSemigroupOfFamily(FamilyObj(t));
    S := UnderlyingSemigroup(T);
    return LeftTranslation(T, MappingByFunction(S, S, x -> x));
  else
    T := RightTranslationsSemigroupOfFamily(FamilyObj(t));
    S := UnderlyingSemigroup(T);
    return RightTranslation(T, MappingByFunction(S, S, x -> x));
  fi;
end);

# TODO: why is this a global function?
InstallGlobalFunction(LeftPartOfBitranslation, "for a bitranslation",
function(h)
  if not IsBitranslation(h) then
     ErrorNoReturn("Semigroups: LeftPartOfBitranslation: \n",
                    "the argument must be a bitranslation,");
  fi;
  return h![1];
end);

InstallGlobalFunction(RightPartOfBitranslation, "for a bitranslation",
function(h)
  if not IsBitranslation(h) then
     ErrorNoReturn("Semigroups: RightPartOfBitranslation: \n",
                    "the argument must be a bitranslation,");
  fi;
  return h![2];
end);

InstallMethod(IsWholeFamily, "for a collection of translations",
[IsTranslationsSemigroupElementCollection],
function(C)
  local L, S, T, t;

  t := Representative(C);
  L := IsLeftTranslationsSemigroupElement(t);

  if L then
    T := LeftTranslationsSemigroupOfFamily(FamilyObj(t));
  else
    T := RightTranslationsSemigroupOfFamily(FamilyObj(t));
  fi;

  S := UnderlyingSemigroup(T);

  if not HasSize(T) or
          IsRectangularBand(S) or
          IsSimpleSemigroup(S) or
          IsZeroSimpleSemigroup(S) or
          IsMonoidAsSemigroup(S) then
    TryNextMethod();
  fi;

  return Size(T) = Size(C);
end);

InstallMethod(IsWholeFamily, "for a collection of bitranslations",
[IsBitranslationCollection],
function(C)
  local b, H, S;
  b := Representative(C);
  H := TranslationalHullOfFamily(FamilyObj(b));
  S := UnderlyingSemigroup(H);

  if not HasSize(H) or
          IsRectangularBand(S) or
          IsMonoidAsSemigroup(S) then
    TryNextMethod();
  fi;

  return Size(H) = Size(C);
end);
