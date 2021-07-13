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


if IsPackageMarkedForLoading("io", "4.5.4") then 
  __JDMS_GLOBAL_TIMINGS_RECORD := rec(running := false);
  StartTimer := function()
    if not __JDMS_GLOBAL_TIMINGS_RECORD.running then 
      __JDMS_GLOBAL_TIMINGS_RECORD.timeofday := IO_gettimeofday();
    fi;
  end;
  
  # Time in microseconds!
  ElapsedTimer := function()
    local  timeofday, elapsed;
    if IsBound(__JDMS_GLOBAL_TIMINGS_RECORD) and
        IsBound(__JDMS_GLOBAL_TIMINGS_RECORD.timeofday) then 
      timeofday := IO_gettimeofday();
      elapsed := (timeofday.tv_sec - __JDMS_GLOBAL_TIMINGS_RECORD.timeofday.tv_sec)
                  * 10 ^ 6 + Int((timeofday.tv_usec -
                   __JDMS_GLOBAL_TIMINGS_RECORD.timeofday.tv_usec));
      return elapsed;
    else 
      return 0;
    fi;
  end;

  StopTimer := function()
    local t;
    t := ElapsedTimer();
    __JDMS_GLOBAL_TIMINGS_RECORD.running := false;
    Unbind(__JDMS_GLOBAL_TIMINGS_RECORD.timeofday);
    return t;
  end;

  RunFunc := function(func, arg)
    local t;
    StartTimer();
    CallFuncList(func, arg);
    t := StopTimer();
    GASMAN("collect");
    return t;
  end;

  Benchmark := function(func, arg)
    local t, max_time, nr_runs, extra_runs, times, i;
    t := RunFunc(func, arg);
    max_time := 100000000;
    nr_runs := Int(max_time / t);
    extra_runs := Minimum(nr_runs, 5);
    times := [t];
    for i in [1 .. extra_runs] do
      Add(times, RunFunc(func, arg));
    od;
    return [Int(Sum(times)/Length(times)), Length(times)];
  end;
  
  BenchmarkLeftTranslationsWithGensStabilisedDown := function(arg)
    local res;
    res := Benchmark(SEMIGROUPS.LeftTranslationsStabilisedBacktrackWithGens, arg);
    Print("Left translations, StabilisedBacktrack: ", res[1], " in ", res[2], " runs.\n");
  end;
  
  BenchmarkLeftTranslationsWithGensStandardDown := function(arg)
    local res;
    res := Benchmark(SEMIGROUPS.LeftTranslationsBacktrackWithGens, arg);
    Print("Left translations, StandardBacktrack: ", res[1], " in ", res[2], " runs.\n");
    BenchmarkLeftTranslationsWithGensStabilisedDown(arg[1], arg[2]);
  end;

  BenchmarkLeftTranslationsWithGensNoCacheDown := function(arg)
    local res;
    res := Benchmark(SEMIGROUPS.LeftTranslationsBacktrackNoCacheWithGens, arg);
    Print("Left translations, CachelessBacktrack: ", res[1], " in ", res[2], " runs.\n");
    BenchmarkLeftTranslationsWithGensStandardDown(arg[1], arg[2]);
  end;

  BenchmarkLeftTranslationsAll := function(arg)
    local res;
    res := Benchmark(SEMIGROUPS.LeftTranslationsNaiveBacktrackWithGens, arg);
    Print("Left translations, NaiveBacktrack: ", res[1], " in ", res[2], " runs.\n");
    BenchmarkLeftTranslationsWithGensNoCacheDown(arg[1], arg[2]);
  end;

  BenchmarkFPLeftTranslations := function(S)
    local iso, T_gens, res;

    Print("Benchmarking:");
    ViewObj(S);
    Print("\n");
    SEMIGROUPS.LeftTranslationsFPBacktrackData(S);
    iso := IsomorphismTransformationSemigroup(S);
    T_gens := List(GeneratorsOfSemigroup(S), x -> x ^ iso);

    SEMIGROUPS.LeftTranslationsBacktrackDataWithGens(Range(iso), T_gens);
    res := Benchmark(SEMIGROUPS.LeftTranslationsFPBacktrack, [S]);
    Print("Left translations, FPBacktrack: ", res[1], " in ", res[2], " runs.\n");
    res := Benchmark(SEMIGROUPS.LeftTranslationsBacktrackWithGens, [Range(iso), T_gens]);
    Print("Left translations, StandardBacktrack: ", res[1], " in ", res[2], " runs.\n");
  end;
  
  BenchmarkLeftTranslations := function(arg)
    BenchmarkLeftTranslationsAll(arg[1], GeneratorsOfSemigroup(arg[1]));  
  end;

  BenchmarkTranslationalHull := function(arg)
    local funcs, strs, gens, res, i;
    funcs := [SEMIGROUPS.BitranslationsAlternatingBacktrack,
             SEMIGROUPS.BitranslationsRLSequentialBacktrack,
             SEMIGROUPS.BitranslationsAlternatingStabilisedBacktrack];
    
    strs := ["AlternatingBacktrack",
             "RLSequentialBacktrack",
             "StabilisedAlternatingBacktrack"];

    Print("Benchmarking:");
    ViewObj(arg[1]);
    Print("\n");
    gens := GeneratorsOfSemigroup(arg[1]);
    SEMIGROUPS.LeftTranslationsBacktrackDataWithGens(arg[1], gens);
    SEMIGROUPS.RightTranslationsBacktrackDataWithGens(arg[1], gens);
    for i in [1 .. Length(funcs)] do
      res := Benchmark(funcs[i], arg);
      Print("Bitranslations, ", strs[i] , ": ", res[1], " in ", res[2], " runs.\n");
    od;

  end;
fi;

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
      return SEMIGROUPS.LeftTranslationsBacktrack(UnderlyingSemigroup(T));
    else
      # TODO: dual or backtrack?
      return SEMIGROUPS.RightTranslationsBacktrack(UnderlyingSemigroup(T));
    fi;
  fi;
  Error("Semigroups: TranslationsSemigroupElements: \n",
        "no method of calculating this translations semigroup is known,");
end;

SEMIGROUPS.LeftAutoTranslations := function(mult_table, gens_pos)
  local n, m, D, x, vertex_cols, edge_cols, auts, g, s;
  n := Size(mult_table);
  m := Size(gens_pos);
  D := Digraph(IsMutableDigraph, []);
  DigraphAddVertices(D, n);
  for g in gens_pos do
    for s in [1 .. Size(mult_table)] do
      x := mult_table[s][g];
      DigraphAddEdge(D, [s, x]);
    od;
  od;
  vertex_cols := fail;
  edge_cols := List([1 .. n], x -> [1 .. m]);
  MakeImmutable(D);
  if IsMultiDigraph(D) then
    return Range(Projection(AutomorphismGroup(D, vertex_cols, edge_cols), 1));
  else
    return AutomorphismGroup(D, vertex_cols, edge_cols);
  fi;
end;

SEMIGROUPS.RightAutoTranslations := function(mult_table, gens_pos)
  local n, m, D, x, vertex_cols, edge_cols, auts, g, s;
  n := Size(mult_table);
  m := Size(gens_pos);
  D := Digraph(IsMutableDigraph, []);
  DigraphAddVertices(D, n);
  for g in gens_pos do
    for s in [1 .. Size(mult_table)] do
      x := mult_table[g][s];
      DigraphAddEdge(D, [s, x]);
    od;
  od;
  vertex_cols := fail;
  edge_cols := List([1 .. n], x -> [1 .. m]);
  MakeImmutable(D);
  if IsMultiDigraph(D) then
    return Range(Projection(AutomorphismGroup(D, vertex_cols, edge_cols), 1));
  else
    return AutomorphismGroup(D, vertex_cols, edge_cols);
  fi;
end;

SEMIGROUPS.AutoBitranslations := function(mult_table, gens_pos)
  local G, left_proj, right_proj, foo;
  G := DirectProduct(SEMIGROUPS.LeftAutoTranslations(mult_table, gens_pos),
                     SEMIGROUPS.RightAutoTranslations(mult_table, gens_pos));
  left_proj := Projection(G, 1);
  right_proj := Projection(G, 2);
  foo := x -> ForAll(gens_pos,
                     i -> ForAll(gens_pos,
                                 j -> mult_table[i][j ^ (x ^ left_proj)] =
                                      mult_table[i ^ (x ^ right_proj)][j]));
  return SubgroupByProperty(G, foo);
end;

SEMIGROUPS.LeftTranslationsBacktrackData := function(S)
  return SEMIGROUPS.LeftTranslationsBacktrackDataWithGens(S, GeneratorsOfSemigroup(S));
end;

SEMIGROUPS.LeftTranslationsBacktrackDataWithGens := function(S, gens)
  local n, m, id, genspos, multtable, multsets, r_classes, r_class_map,
  r_class_inv_map, r_classes_below, max_R_intersects, intersect, reps,
  left_canon_inverse_by_gen, left_inverses_by_gen, x, right_inverses, seen, t,
  s, transposed_multtable, transposed_multsets, U, Ui, keep, B, sb, r, i, j, a,
  u;

  n           := Size(S);
  m           := Size(gens);
  id          := n + 1;
  genspos     := List(gens, x -> PositionCanonical(S, x));

  multtable := List(MultiplicationTableWithCanonicalPositions(S), ShallowCopy);
  for i in [1 .. n] do
    Add(multtable[i], i);
  od;
  Add(multtable, [1 .. id]);

  multsets := List(multtable, Set);

  r_classes := RClasses(S);
  r_class_map := [];
 
  for i in [1 .. Length(r_classes)] do
    for s in r_classes[i] do
      r_class_map[PositionCanonical(S, s)] := i;
    od;
  od;
  
  r_class_inv_map := List(r_classes,
                          x -> PositionCanonical(S, Representative(x)));
  r_classes_below := List([1 .. m], i -> Set(r_class_map{multsets[genspos[i]]}));
  max_R_intersects := List([1 .. m], x -> []);
  
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      intersect := Intersection(r_classes_below[i], r_classes_below[j]);
      reps := r_class_inv_map{intersect};

      max_R_intersects[i][j] := Filtered(reps,
                                         x -> not ForAny(reps, 
                                                         y -> x <> y and
                                                           x in multsets[y]));

      max_R_intersects[j][i] := max_R_intersects[i][j];
    od;
  od;

  left_canon_inverse_by_gen := List([1 .. n], x -> []);
  
  # for all s in S, store the elements t such that gens[i] * t = s for each i
  left_inverses_by_gen := List([1 .. n], x -> List([1 .. m], y -> []));
  for i in [1 .. m] do
    for t in [1 .. n + 1] do
      x := multtable[genspos[i]][t];
      Add(left_inverses_by_gen[x][i], t);
      if not IsBound(left_canon_inverse_by_gen[x][i]) then
        left_canon_inverse_by_gen[x][i] := t;
      fi;
    od;
  od;


  # for each t in the left inverses of some a in max_R_intersects[i][j] by 
  # gens[j], compute the right inverses of each s in S under t
  right_inverses := List([1 .. n], x -> ListWithIdenticalEntries(n + 1, fail));
  seen := List([1 .. n + 1], x -> false);
  for i in [1 .. m] do
    for j in [1 .. m] do
      if i = j then
        continue;
      fi;
      for a in max_R_intersects[i][j] do
        t := left_canon_inverse_by_gen[a][j];
        # don't repeat the calculation if we've already done it for t!
        if not seen[t] then
          seen[t] := true;
          for u in [1 .. n] do
            s := multtable[u][t];
            if right_inverses[s][t] = fail then
              right_inverses[s][t] := [];
            fi;
            Add(right_inverses[s][t], u);
          od;
        fi;
      od;
    od;
  od;

  transposed_multtable := TransposedMat(multtable);
  transposed_multsets  := List(transposed_multtable, Set);

  # compute intersection over a of the sets U_{i, a} from the paper
  U := [];
  for i in [1 .. m] do
    Ui := BlistList([1 .. n], []);
    for s in [1 .. n] do
      if Ui[s] then 
        continue;
      fi;
      keep := true;
      for a in multsets[genspos[i]] do
        B := left_inverses_by_gen[a][i];
        sb := multtable[s][B[1]];
        if multtable[s]{B} <> ListWithIdenticalEntries(Size(B), sb) then
          keep := false;
          break;
        fi;
      od;
      if keep then
        UniteBlistList([1 .. n], Ui, transposed_multsets[s]);
        Ui[s] := true;
      fi;
    od;
    U[i] := ListBlist([1 .. n], Ui);
  od;

  r := rec();
  r.left_canon_inverse_by_gen := left_canon_inverse_by_gen;
  r.left_inverses_by_gen := left_inverses_by_gen;
  r.max_R_intersects := max_R_intersects;
  r.multtable := multtable;
  r.n := n;
  r.right_inverses := right_inverses;
  r.U := U;
  r.V := List([1 .. m], j -> List([1 .. n], a -> []));
  r.W := List([1 .. m], i -> List([1 .. m], j -> []));
  return r;
end;

SEMIGROUPS.RightTranslationsBacktrackData := function(S)
  return SEMIGROUPS.RightTranslationsBacktrackDataWithGens(S, GeneratorsOfSemigroup(S));
end;

# TODO: remove F like V was removed for left translations
SEMIGROUPS.RightTranslationsBacktrackDataWithGens := function(S, gens)
  local n, m, id, genspos, transpose_multtable, transpose_multsets, l_classes,
  l_class_map, l_class_inv_map, l_classes_below, max_L_intersects, intersect,
  reps, right_inverses_by_gen, left_inverses, seen, s, multsets, T, Ti, keep, B,
  sb, r, i, j, t, a, u;
  
  n           := Size(S);
  m           := Size(gens);
  id          := n + 1;
  genspos     := List(gens, x -> PositionCanonical(S, x));

  transpose_multtable :=
    List(TransposedMultiplicationTableWithCanonicalPositions(S),
         ShallowCopy);
  for i in [1 .. n] do
    Add(transpose_multtable[i], i);
  od;
  Add(transpose_multtable, [1 .. id]);
  transpose_multsets := List(transpose_multtable, Set);

  l_classes := LClasses(S);
  l_class_map := [];
 
  for i in [1 .. Length(l_classes)] do
    for s in l_classes[i] do
      l_class_map[PositionCanonical(S, s)] := i;
    od;
  od;
  
  l_class_inv_map := List(l_classes,
                          x -> PositionCanonical(S, Representative(x)));
  l_classes_below := List([1 .. m],
                          i -> Set(l_class_map{transpose_multsets[genspos[i]]}));
  max_L_intersects := List([1 .. m], x -> []);
  
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      intersect := Intersection(l_classes_below[i], l_classes_below[j]);
      reps := l_class_inv_map{intersect};

      max_L_intersects[i][j] := Filtered(reps,
                                         x -> not ForAny(reps, 
                                                         y -> x <> y and
                                                           x in transpose_multsets[y]));

      max_L_intersects[j][i] := max_L_intersects[i][j];
    od;
  od;

  # for all s in S, store the elements t such that t * gens[i] = s for each i
  right_inverses_by_gen := List([1 .. n], x -> List([1 .. m], y -> []));
  for i in [1 .. m] do
    for t in [1 .. n] do
      Add(right_inverses_by_gen[transpose_multtable[genspos[i]][t]][i], t);
    od;
  od;

  # for each t in the right inverses of some a in max_L_intersects[i][j] by 
  # gens[j], compute the left inverses of each s in S under t
  left_inverses := List([1 .. n], x -> ListWithIdenticalEntries(n + 1, fail));
  seen := List([1 .. n], x -> false);
  for i in [1 .. m] do
    for j in [1 .. m] do
      if i = j then
        continue;
      fi;
      for a in max_L_intersects[i][j] do
        for t in right_inverses_by_gen[a][j] do
          # don't repeat the calculation if we've already done it for t!
          if not seen[t] then
            seen[t] := true;
            for u in [1 .. n] do
              s := transpose_multtable[u][t];
              if left_inverses[s][t] = fail then
                left_inverses[s][t] := [];
              fi;
              Add(left_inverses[s][t], u);
            od;
          fi;
        od;
      od;
    od;
  od;

  multsets := List(MultiplicationTableWithCanonicalPositions(S), Set);

  T := [];
  for i in [1 .. m] do
    Ti := BlistList([1 .. n], []);
    for s in [1 .. n] do
      if Ti[s] then 
        continue;
      fi;
      keep := true;
      for a in transpose_multsets[genspos[i]] do
        B := right_inverses_by_gen[a][i];
        sb := transpose_multtable[s][B[1]];
        if transpose_multtable[s]{B} <> ListWithIdenticalEntries(Size(B), sb) then
          keep := false;
          break;
        fi;
      od;
      if keep then
        UniteBlistList([1 .. n], Ti, multsets[s]);
        Ti[s] := true;
      fi;
    od;
    T[i] := ListBlist([1 .. n], Ti);
  od;

  r := rec();
  r.left_inverses := left_inverses;
  r.max_L_intersects := max_L_intersects;
  r.n := n;
  r.right_inverses_by_gen := right_inverses_by_gen;
  r.transpose_multtable := transpose_multtable;
  r.T := T;
  r.F := List([1 .. m], j -> List([1 .. n], a -> []));
  r.G := List([1 .. m], i -> List([1 .. m], j -> []));
  return r;
end;

# Caution: this assumes that:
# S does not contain the empty word
# S is finite!
SEMIGROUPS.LeftTranslationsFPBacktrackData := function(S)
  local gens, rels, n, m, iso, T, suff_rels, suff_rels_by_first_letter,
  suff_rel, i, j, word, suffix, T_gens, iso_with_one,
  T_suff_rels_by_first_letter, U, keep, T_suff_rels, T_suffs,
  right_inverses_by_suffix, Tlist, r, rel, x, suffs, t, T_rel, suff;

  gens := GeneratorsOfSemigroup(S);
  rels := RelationsOfFpSemigroup(S);

  n := Size(S);
  m := Size(gens);

  iso := IsomorphismTransformationSemigroup(S);
  T := Range(iso);

  suff_rels := [];
  suff_rels_by_first_letter := List(gens, x -> List(gens, y -> []));
  for rel in rels do
    suff_rel := [];
    i := ExtRepOfObj(rel[1])[1];
    j := ExtRepOfObj(rel[2])[1];
    for x in [1, 2] do
      word := SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(rel[x]));
      if Length(word) > 1 then
        suffix := Product(List(word{[2 .. Length(word)]}, k -> gens[k]));
      else
        suffix := SEMIGROUPS.UniversalFakeOne;
      fi;
      Add(suff_rel, suffix);
    od;
    AddSet(suff_rels, suff_rel);
    if i <= j then 
      AddSet(suff_rels_by_first_letter[i][j], suff_rel);
    else
      AddSet(suff_rels_by_first_letter[j][i], Reversed(suff_rel));
    fi;
  od;

  T_gens := List(gens, x -> x^iso);

  iso_with_one := function(x)
    if x = SEMIGROUPS.UniversalFakeOne then
      return x;
    else
      return x ^ iso;
    fi;
  end;

  T_suff_rels_by_first_letter := List(gens, x -> List(gens, y -> []));
  for i in [1 .. m] do
    for j in [i .. m] do
      for suffs in suff_rels_by_first_letter[i][j] do
        Add(T_suff_rels_by_first_letter[i][j], List(suffs, iso_with_one));
      od;
    od;
  od;


  U := List([1 .. m], x -> []);
  for i in [1 .. m] do
    for t in T do
      keep := true;
      for T_rel in T_suff_rels_by_first_letter[i][i] do
        if t * rel[1] <> t * rel[2] then
          keep := false;
          break;
        fi;
      od;
    if keep then
      Add(U[i], PositionCanonical(T, t));
    fi;
    od;
  od;

  T_suff_rels := List(suff_rels, suff_rel -> List(suff_rel, iso_with_one));
  T_suffs := Set(Flat(T_suff_rels));
  right_inverses_by_suffix := List([1 .. n], x -> List([1 .. n + 1], y -> []));

  Tlist := AsListCanonical(T);;

  for suff in T_suffs do
    if suff = SEMIGROUPS.UniversalFakeOne then
      i := n + 1;
    else
      i := PositionCanonical(T, suff);
    fi;
    for t in [1 .. n] do
      Add(right_inverses_by_suffix[PositionCanonical(T, Tlist[t] * suff)][i], t);
    od;
  od;

  r := rec();
  r.n := n;
  r.right_inverses_by_suffix := right_inverses_by_suffix;
  r.S := S;
  r.T := T;
  r.T_suff_rels_by_first_letter := T_suff_rels_by_first_letter;
  r.T_suffixes := T_suffs;
  r.U := U;
  r.W := List([1 .. m], i -> List([1 .. m], j -> []));
  return r;
end;

SEMIGROUPS.LeftTranslationsBacktrackDataV := function(data, j, a, s)
  local right_inverses, V, C, t;

  if IsBound(data.V[j][a][s]) then
    return data.V[j][a][s];
  fi;

  right_inverses := data.right_inverses;
  t :=  data.left_canon_inverse_by_gen[a][j];
  C := right_inverses[s][t];
  if C = fail then
    V := [];
  else
  fi;
  data.V[j][a][s] := V;
  return V;
end;

SEMIGROUPS.LeftTranslationsBacktrackDataWNoCache := function(data, i, j, s)
  local left_canon_inverse_by_gen, multtable, right_inverses, W, r, x, a;

  left_canon_inverse_by_gen := data.left_canon_inverse_by_gen;
  multtable := data.multtable;
  right_inverses := data.right_inverses;
  W := [1 .. data.n];
  for a in data.max_R_intersects[i][j] do
    r := left_canon_inverse_by_gen[a][i];
    x := multtable[s][r];
    if right_inverses[x][left_canon_inverse_by_gen[a][j]] = fail then
      W := [];
      break;
    else 
      W := Intersection(W, 
           right_inverses[x][left_canon_inverse_by_gen[a][j]]);
    fi;
  od;
  return W;
end;

SEMIGROUPS.LeftTranslationsBacktrackDataW := function(data, i, j, s)
  local left_canon_inverse_by_gen, multtable, right_inverses, W, r, x, a;

  if IsBound(data.W[i][j][s]) then
    return data.W[i][j][s];
  fi;

  left_canon_inverse_by_gen := data.left_canon_inverse_by_gen;
  multtable := data.multtable;
  right_inverses := data.right_inverses;
  W := [1 .. data.n];
  for a in data.max_R_intersects[i][j] do
    r := left_canon_inverse_by_gen[a][i];
    x := multtable[s][r];
    if right_inverses[x][left_canon_inverse_by_gen[a][j]] = fail then
      W := [];
      break;
    else 
      W := Intersection(W, 
           right_inverses[x][left_canon_inverse_by_gen[a][j]]);
    fi;
  od;
  data.W[i][j][s] := W;
  return W;
end;

SEMIGROUPS.LeftTranslationsFPBacktrackDataW := function(data, i, j, s)
  local S, W, enum, u, suff_rel;

  if IsBound(data.W[i][j][s]) then
    return data.W[i][j][s];
  fi;

  S := data.S;

  W := [1 .. Size(S)];
  enum := EnumeratorCanonical(data.T);
  for suff_rel in data.T_suff_rels_by_first_letter[i][j] do
    if suff_rel[2] = SEMIGROUPS.UniversalFakeOne then
      u := data.n + 1;
    else
      u := PositionCanonical(data.T, suff_rel[2]);
    fi;
    W := Intersection(W, 
          data.right_inverses_by_suffix[PositionCanonical(data.T, enum[s] * suff_rel[1])][u]);
  od;

  data.W[i][j][s] := W;
  return W;
end;

SEMIGROUPS.RightTranslationsBacktrackDataF := function(data, j, a, s)
  local left_inverses, F, C, t;

  if IsBound(data.F[j][a][s]) then
    return data.F[j][a][s];
  fi;

  left_inverses := data.left_inverses;
  F := [1 .. data.n];
  for t in data.right_inverses_by_gen[a][j] do
    C := left_inverses[s][t];
    if C = fail then
      F := [];
      break;
    else
      F := Intersection(F, C);
    fi;
  od;
  data.F[j][a][s] := F;
  return F;
end;

SEMIGROUPS.RightTranslationsBacktrackDataG := function(data, i, j, s)
  local right_inverses_by_gen, transpose_multtable, G, x, a, l;

  if IsBound(data.G[i][j][s]) then
    return data.G[i][j][s];
  fi;

  right_inverses_by_gen := data.right_inverses_by_gen;
  transpose_multtable := data.transpose_multtable;
  G := [1 .. data.n];
  for a in data.max_L_intersects[i][j] do
    for l in right_inverses_by_gen[a][i] do
      x := transpose_multtable[s][l];
      G := Intersection(G, 
                        SEMIGROUPS.RightTranslationsBacktrackDataF(data,
                                                                   j,
                                                                   a,
                                                                   x));
    od;
  od;
  data.G[i][j][s] := G;
  return G;
end;

SEMIGROUPS.LeftTranslationsNaiveBacktrackWithGens := function(S, gens)
  local n, m, genspos, multtable, bt, lambda, out;

  n           := Size(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));

  multtable := MultiplicationTableWithCanonicalPositions(S);

  bt := function(i) 
    local bound, consistent, xit, st, s, t, x;
    for s in [1 .. n] do
      bound := [];
      consistent := true;
      if IsBound(lambda[genspos[i]]) then
        if lambda[genspos[i]] <> s then
          consistent := false;
        fi;
      else
        lambda[genspos[i]] := s;
        Add(bound, genspos[i]);
      fi;
      if consistent then
        for t in [1 .. n] do
          xit := multtable[genspos[i]][t];
          st := multtable[s][t];
          if IsBound(lambda[xit]) then
            if lambda[xit] <> multtable[s][t] then
              consistent := false;
              break;
            fi;
          else
            lambda[xit] := multtable[s][t];
            Add(bound, xit);
          fi;
        od;
      fi;
      if consistent then
        if i = m then
          Add(out, ShallowCopy(lambda));
        else
          bt(i + 1);
        fi;
      fi;
      for x in bound do
        Unbind(lambda[x]);
      od;
    od;
  end;

  lambda := [];
  out := [];
  bt(1);
#  Apply(out, x -> LeftTranslationNC(L, x));
  return out;
end;


SEMIGROUPS.LeftTranslationsBacktrack := function(L)
  return SEMIGROUPS.LeftTranslationsBacktrackWithGens(UnderlyingSemigroup(L),
          GeneratorsOfSemigroup(UnderlyingSemigroup(L)));
end;

SEMIGROUPS.LeftTranslationsBacktrackWithGens := function(S, gens)
  local n, m, genspos, omega_stack, possiblefgenvals, multtable, data, U, bt,
  lambda, out, i;

  n           := Size(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.LeftTranslationsBacktrackDataWithGens(S, gens);
  U := data.U;

  # restrict via the U_{i}
  for i in [1 .. m] do
    IntersectSet(possiblefgenvals[i], U[i]);
  od;

  bt := function(i) 
    local consistent, W, s, j;
    for s in omega_stack[i][i] do
      lambda[i] := s;
      if i = m then
        Add(out, ShallowCopy(lambda));
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          W := SEMIGROUPS.LeftTranslationsBacktrackDataW(data, i, j, s);
          omega_stack[i + 1][j] := Intersection(omega_stack[i][j], W);
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
  Apply(out, x -> LeftTranslationNC(LeftTranslations(S), x));
  return out;
end;

SEMIGROUPS.LeftTranslationsFPBacktrack := function(S)
  local n, m, omega_stack, possiblefgenvals, data, bt, lambda, out;

  n           := Size(S);
  m           := Size(GeneratorsOfSemigroup(S));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);

  data := SEMIGROUPS.LeftTranslationsFPBacktrackData(S);
  
  bt := function(i) 
    local consistent, W, t, j;
    for t in omega_stack[i][i] do
      lambda[i] := t;
      if i = m then
        Add(out, ShallowCopy(lambda));
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          W := SEMIGROUPS.LeftTranslationsFPBacktrackDataW(data, i, j, t);
          omega_stack[i + 1][j] := Intersection(omega_stack[i][j], W);
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

  omega_stack := [data.U];
  lambda := [];
  out := [];
  bt(1);
  #Apply(out, x -> LeftTranslationNC(S, x));
  return out;
end;

SEMIGROUPS.LeftTranslationsBacktrackNoCacheWithGens := function(S, gens)
  local n, m, genspos, omega_stack, possiblefgenvals, multtable, data, U, bt,
  lambda, out, i;

  n           := Size(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.LeftTranslationsBacktrackData(S);
  U := data.U;
  
  # restrict via the U_{i}
  for i in [1 .. m] do
    IntersectSet(possiblefgenvals[i], U[i]);
  od;

  bt := function(i) 
    local consistent, W, s, j;
    for s in omega_stack[i][i] do
      lambda[i] := s;
      if i = m then
        Add(out, ShallowCopy(lambda));
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          W := SEMIGROUPS.LeftTranslationsBacktrackDataWNoCache(data, i, j, s);
          omega_stack[i + 1][j] := Intersection(omega_stack[i][j], W);
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
#  Apply(out, x -> LeftTranslationNC(L, x));
  return out;
end;

SEMIGROUPS.LeftTranslationsStabilisedBacktrack := function(L)
  return SEMIGROUPS.LeftTranslationsStabilisedBacktrackWithGens(UnderlyingSemigroup(L),
          GeneratorsOfSemigroup(UnderlyingSemigroup(L)));
end;

# TODO: remove optional gens argument on all of these
SEMIGROUPS.LeftTranslationsStabilisedBacktrackWithGens := function(S, gens)
  local n, m, genspos, omega_stack, possiblefgenvals, stabs, stab_thresh,
  coset_reps, multtable, data, U, aut, add_stabilised_lambda, bt, lambda, out,
  nr, i;

  n           := Size(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);
  stabs := [];
  stabs[m + 1] := [];
  stab_thresh := 20;
  coset_reps := [];

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.LeftTranslationsBacktrackDataWithGens(S, gens);
  U := data.U;

  aut := SEMIGROUPS.LeftAutoTranslations(multtable,
                                         List(GeneratorsOfSemigroup(S), 
                                              x -> PositionCanonical(S, x)));

  # restrict via the U_{i}
  for i in [1 .. m] do
    IntersectSet(possiblefgenvals[i], U[i]);
  od;

  add_stabilised_lambda := function()
    local stab_depth, it, mult;
    stab_depth := PositionProperty(stabs, x -> Size(x) = 0) - 1;
    nr := nr + Product(List(coset_reps{[1 .. stab_depth]}, Length));
    AddSet(out, ShallowCopy(lambda));
    it := IteratorOfCartesianProduct(coset_reps{[1 .. stab_depth]});
    while not IsDoneIterator(it) do
      mult := Product(NextIterator(it));
      AddSet(out, OnTuples(lambda, mult));
    od;
  end;

  bt := function(i) 
    local factors, stab, big_stab, orbs, reps, consistent, W, cart, s, j,
    intersect, t, k, u;
    if i > 1 then
      stab := stabs[i - 1];
    else
      stab := aut;
    fi;
    big_stab := Size(stab) > stab_thresh;
    if big_stab then
      orbs := Orbits(stab, omega_stack[i][i]);
      reps := List(orbs, x -> x[1]);
    else
      reps := omega_stack[i][i];
    fi;
    for s in reps do
      lambda[i] := s;
      if i = m then
        if big_stab then
          # this is necessary in theory
          stabs[i] := Stabiliser(stab, s);
          coset_reps[i] := RightTransversal(stab, Stabiliser(stab, s));
        else
          stabs[i] := [];
        fi;
        add_stabilised_lambda();
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          W := SEMIGROUPS.LeftTranslationsBacktrackDataW(data, i, j, s);
          omega_stack[i + 1][j] := Intersection(omega_stack[i][j], W);
          if IsEmpty(omega_stack[i + 1][j]) then
            consistent := false;
            break;
          fi;
        od;
        if consistent then
          if big_stab then
            if Size(reps) = 1 then
              stabs[i] := stab;
              coset_reps[i] := [()];
            else
              stabs[i] := Stabiliser(stab, s);
              coset_reps[i] := RightTransversal(stab, stabs[i]);
            fi;
          else
            stabs[i] := [];
            coset_reps[i] := [];
          fi;
          bt(i + 1);
        fi;
      fi;
    od;
  end;

  omega_stack := [possiblefgenvals];
  lambda := [];
  out := [];
  nr := 0;
  bt(1);
#  Apply(out, x -> LeftTranslationNC(S, x));
  return out;
end;

SEMIGROUPS.RightTranslationsBacktrack := function(S)
  local n, gens, m, genspos, omega_stack, multtable, data, G, T,
  possiblegenvals, bt, rho, out, i, j, s;

  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.RightTranslationsBacktrackData(S);
  T := data.T;
  
  possiblegenvals := List([1 .. m], i -> [1 .. n]);
  
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
          G := SEMIGROUPS.RightTranslationsBacktrackDataG(data, i, j, s);
          omega_stack[i + 1][j] := Intersection(omega_stack[i][j], G);
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
  
  if ValueOption("SEMIGROUPS_translat_number_only") = true then
    return Length(out);
  fi;

#  Apply(out, x -> RightTranslationNC(S, x));
  return out;
end;

SEMIGROUPS.RightTranslationsStabilisedBacktrack := function(R)
  return SEMIGROUPS.RightTranslationsStabilisedBacktrackWithGens(UnderlyingSemigroup(R),
                      GeneratorsOfSemigroup(UnderlyingSemigroup(R)));
end;

SEMIGROUPS.RightTranslationsStabilisedBacktrackWithGens := function(S, gens)
  local n, m, genspos, omega_stack, possiblegenvals, stabs, coset_reps,
  stab_thresh, multtable, data, T, aut, add_stabilised_rho, bt, rho, out, nr, i;

  n           := Size(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblegenvals := List([1 .. m], i -> [1 .. n]);
  stabs := [];
  stabs[m + 1] := [];
  coset_reps := [];
  stab_thresh := 20;

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.RightTranslationsBacktrackData(S);
  T := data.T;
  
  aut := SEMIGROUPS.RightAutoTranslations(multtable,
                                         List(GeneratorsOfSemigroup(S), 
                                              x -> PositionCanonical(S, x)));
  
  # restrict via the T_{i}
  for i in [1 .. m] do
    IntersectSet(possiblegenvals[i], T[i]);
  od;

  add_stabilised_rho := function()
    local stab_depth, it, mult;
    stab_depth := PositionProperty(stabs, x -> Size(x) = 0) - 1;
    nr := nr + Product(List(coset_reps{[1 .. stab_depth]}, Length));
    Add(out, ShallowCopy(rho));
    it := IteratorOfCartesianProduct(coset_reps{[1 .. stab_depth]});
    while not IsDoneIterator(it) do
      mult := Product(NextIterator(it));
      Add(out, OnTuples(rho, mult));
    od;
  end;

  bt := function(i) 
    local stab, big_stab, orbs, reps, consistent, G, s, j;
    if i > 1 then
      stab := stabs[i - 1];
    else
      stab := aut;
    fi;
    big_stab := Size(stab) > stab_thresh;
    if big_stab then
      orbs := Orbits(stab, omega_stack[i][i]);
      reps := List(orbs, x -> x[1]);
    else
      reps := omega_stack[i][i];
    fi;
    for s in reps do
      rho[i] := s;
      if i = m then
        if big_stab then
          # this is necessary in theory
          stabs[i] := Stabiliser(stab, s);
          coset_reps[i] := RightTransversal(stab, Stabiliser(stab, s));
        else
          stabs[i] := [];
        fi;
        add_stabilised_rho();
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          G := SEMIGROUPS.RightTranslationsBacktrackDataG(data, i, j, s);
          omega_stack[i + 1][j] := Intersection(omega_stack[i][j], G);
          if IsEmpty(omega_stack[i + 1][j]) then
            consistent := false;
            break;
          fi;
        od;
        if consistent then
          if big_stab then
            if Size(reps) = 1 then
              stabs[i] := stab;
              coset_reps[i] := [()];
            else
              stabs[i] := Stabiliser(stab, s);
              coset_reps[i] := RightTransversal(stab, stabs[i]);
            fi;
          else
            stabs[i] := [];
            coset_reps[i] := [];
          fi;
          bt(i + 1);
        fi;
      fi;
    od;
  end;

  omega_stack := [possiblegenvals];
  rho := [];
  out := [];
  nr := 0;
  bt(1);

  if ValueOption("SEMIGROUPS_translat_number_only") = true then
    return Length(out);
  fi;

#  Apply(out, x -> RightTranslationNC(S, x));
  return out;
end;

SEMIGROUPS.BitranslationsAlternatingBacktrack := function(S)
  local n, gens, m, genspos, l_omega_stack, r_omega_stack, multtable,
  left_data, right_data, U, W, left_inverses_by_gen, G, T,
  right_inverses_by_gen, bt, lambda, rho, out, L, R, i, j, s;

  n             := Size(S);
  gens          := GeneratorsOfSemigroup(S);
  m             := Size(gens);
  genspos       := List(gens, x -> PositionCanonical(S, x));
  l_omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  r_omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));

  multtable := MultiplicationTableWithCanonicalPositions(S);

  left_data := SEMIGROUPS.LeftTranslationsBacktrackData(S);
  right_data := SEMIGROUPS.RightTranslationsBacktrackData(S);
  left_inverses_by_gen := left_data.left_inverses_by_gen;
  right_inverses_by_gen := right_data.right_inverses_by_gen;
  U := left_data.U;
  T := right_data.T;

  l_omega_stack[1] := List([1 .. m], i -> [1 .. n]);
  r_omega_stack[1] := List([1 .. m], i -> [1 .. n]);
  
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
            W := SEMIGROUPS.LeftTranslationsBacktrackDataW(left_data, k, j, s);
            l_omega_stack[i + 1][j] := Intersection(l_omega_stack[i][j], W);
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
          G := SEMIGROUPS.RightTranslationsBacktrackDataG(right_data, k, j, s);
          r_omega_stack[i + 1][j] := Intersection(r_omega_stack[i][j], G);
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
  
  if ValueOption("SEMIGROUPS_bitranslat_number_only") = true then
    return Length(out);
  fi;

  L := LeftTranslations(S);
  R := RightTranslations(S);
#  Apply(out, x -> BitranslationNC(TranslationalHull(S),
#                                  LeftTranslationNC(L, x[1]),
#                                  RightTranslationNC(R, x[2])));
  return out;
end;

SEMIGROUPS.BitranslationsRLSequentialBacktrack := function(S)
  local n, gens, m, genspos, multtable, data, left_inverses_by_gen, U, bt,
  out, R, omega_stack, lambda, L, rho, i, j;

  n             := Size(S);
  gens          := GeneratorsOfSemigroup(S);
  m             := Size(gens);
  genspos       := List(gens, x -> PositionCanonical(S, x));

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.LeftTranslationsBacktrackData(S);
  left_inverses_by_gen := data.left_inverses_by_gen;
  U := data.U;

  bt := function(i) 
    local consistent, W, s, j;
    for s in omega_stack[i][i] do
      lambda[i] := s;
      if i = m then
        Add(out, ShallowCopy(lambda));
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          W := SEMIGROUPS.LeftTranslationsBacktrackDataW(data, i, j, s);
          omega_stack[i + 1][j] := Intersection(omega_stack[i][j], W);
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

  out := [];
  
  for rho in SEMIGROUPS.RightTranslationsBacktrack(S) do
    omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
    omega_stack[1] := List([1 .. m], i -> U[i]);
    for i in [1 .. m] do
      for j in [1 .. m] do
          omega_stack[1][j] :=
            Intersection(omega_stack[1][j],
                         left_inverses_by_gen[multtable[rho[i]][genspos[j]]][i]);
      od;
    od;

    lambda := [];
    bt(1);
  od;
  
  if ValueOption("SEMIGROUPS_bitranslat_nr_only") = true then
    return Length(out);
  fi;

#  L := LeftTranslations(S);
#  Apply(out, x -> Bitranslation(H,
#                                LeftTranslationNC(L, x[1]),
#                                RightTranslationNC(R, x[2])));
  return out;
end;



SEMIGROUPS.BitranslationsAlternatingStabilisedBacktrack := function(S)
  local n, gens, m, genspos, l_omega_stack, r_omega_stack, l_stabs,
  l_coset_reps, r_stabs, r_coset_reps, stab_thresh, multtable, left_data,
  right_data, left_inverses_by_gen, right_inverses_by_gen, U, T, aut,
  add_stabilised_pair, bt, lambda, rho, out, L, R, i;

  n             := Size(S);
  gens          := GeneratorsOfSemigroup(S);
  m             := Size(gens);
  genspos       := List(gens, x -> PositionCanonical(S, x));
  l_omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  r_omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));

  l_stabs := [];
  l_stabs[m] := [];
  l_coset_reps := [];
  r_stabs := [];
  r_stabs[m] := [];
  r_coset_reps := [];
  stab_thresh := 20;

  multtable := MultiplicationTableWithCanonicalPositions(S);

  left_data := SEMIGROUPS.LeftTranslationsBacktrackData(S);
  right_data := SEMIGROUPS.RightTranslationsBacktrackData(S);
  left_inverses_by_gen := left_data.left_inverses_by_gen;
  right_inverses_by_gen := right_data.right_inverses_by_gen;
  U := left_data.U;
  T := right_data.T;

  l_omega_stack[1] := List([1 .. m], i -> [1 .. n]);
  r_omega_stack[1] := List([1 .. m], i -> [1 .. n]);
  
  # restrict via the T_{i} and U_{i}
  for i in [1 .. m] do
    IntersectSet(l_omega_stack[1][i], U[i]);
    IntersectSet(r_omega_stack[1][i], T[i]);
  od;

  aut := SEMIGROUPS.AutoBitranslations(multtable, genspos);

  add_stabilised_pair := function()
    local l_stab_depth, r_stab_depth, it, next_it, l_mult, r_mult;
    l_stab_depth := PositionProperty(l_stabs, x -> Size(x) = 0) - 1;
    r_stab_depth := PositionProperty(r_stabs, x -> Size(x) = 0) - 1;
    it := IteratorOfCartesianProduct(Concatenation(l_coset_reps{[1 ..
                                                      l_stab_depth]},
                                                   r_coset_reps{[1 ..
                                                      r_stab_depth]}));
    while not IsDoneIterator(it) do
      next_it := NextIterator(it);
      l_mult := Product(next_it{[1 .. l_stab_depth]});
      r_mult := Product(next_it{[l_stab_depth + 1 ..
                                 l_stab_depth + r_stab_depth]});
      Add(out, [OnTuples(lambda, l_mult), OnTuples(rho, r_mult)]);
    od;
  end;

  bt := function(i) 
    local k, stab, use_stab, orbs, reps, consistent, W, G, s, j;
    # i represents the combined depth in l and r, which alternate in the
    # backtrack
    # k represents the generator number we are defining the value of l or r for
    # now

    if (i mod 2 = 1) then 
      # we are dealing with the left translation
      k := (i + 1)/2;

      if k > 1 then
        stab := l_stabs[k - 1];
      else
        stab := aut;
      fi;
      use_stab := Size(stab) > stab_thresh;
      if use_stab then
        orbs := Orbits(stab, l_omega_stack[i][k]);
        reps := List(orbs, x -> x[1]);
      else
        reps := l_omega_stack[i][k];
      fi;

      for s in reps do
        consistent := true;
        lambda[k] := s;
        l_omega_stack[i + 1] := [];
        r_omega_stack[i + 1] := [];
        # make sure to take care of linking condition
        # x_i * lambda(x_i) = (x_i)rho * x_i
        for j in [k .. m] do
          if (j > k) then
            W := SEMIGROUPS.LeftTranslationsBacktrackDataW(left_data, k, j, s);
            l_omega_stack[i + 1][j] := Intersection(l_omega_stack[i][j], W);
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
          if use_stab then
            l_stabs[k] := Stabiliser(stab, s);
            l_coset_reps[k] := List(RightCosets(stab, l_stabs[k]), Representative);
          else
            l_stabs[k] := [];
            l_coset_reps[k] := [];
          fi;
          bt(i + 1);
        fi;
      od;
    else
      # we are dealing with the right translation
      k := i/2;
      if k > 1 then
        stab := r_stabs[k - 1];
      else
        stab := aut;
      fi;
      use_stab := Size(stab) > stab_thresh;
      if use_stab then
        orbs := Orbits(stab, r_omega_stack[i][k]);
        reps := List(orbs, x -> x[1]);
      else
        reps := r_omega_stack[i][k];
      fi;
      for s in reps do
        rho[k] := s;
        if (k = m) then
          Add(out, [ShallowCopy(lambda), ShallowCopy(rho)]);
          continue;
        fi;
        consistent := true;
        l_omega_stack[i + 1] := [];
        r_omega_stack[i + 1] := [];
        for j in [k + 1 .. m] do
          G := SEMIGROUPS.RightTranslationsBacktrackDataG(right_data, k, j, s);
          r_omega_stack[i + 1][j] := Intersection(r_omega_stack[i][j], G);
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
          if use_stab then
            r_stabs[k] := Stabiliser(stab, s);
            r_coset_reps[k] := List(RightCosets(stab, r_stabs[k]), Representative);
          else
            r_stabs[k] := [];
            r_coset_reps[k] := [];
          fi;
          bt(i + 1);
        fi;
      od;
    fi;
  end;

  lambda := [];
  rho := [];
  out := [];
  bt(1);

#  L := LeftTranslations(S);
#  R := RightTranslations(S);
#  Apply(out, x -> Bitranslation(H,
#                                LeftTranslationNC(L, x[1]),
#                                RightTranslationNC(R, x[2])));
  return out;
end;

# Choose how to calculate the elements of a translational hull
SEMIGROUPS.Bitranslations := function(S)
  if IsRectangularBand(S) then
    return Semigroup(GeneratorsOfSemigroup(TranslationalHull(S)));
  elif IsReesZeroMatrixSemigroup(S) then
    return SEMIGROUPS.BitranslationsOfRZMS(S);
  elif SEMIGROUPS.IsNormalRMSOverGroup(S) then
    return SEMIGROUPS.BitranslationsOfNormalRMS(S);
  else
    return SEMIGROUPS.BitranslationsBacktrack(S);
  fi;
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
    # TODO store and use MultiplicationTableWithCanonicalPositions and
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
    # TODO store and use MultiplicationTableWithCanonicalPositions and
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
                                            SEMIGROUPS_bitranslat_number_only);
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

SEMIGROUPS.ImagePositionsOfTranslation := function(x)
  local T, S, tab, enum, images, g;
  if IsLeftTranslationsSemigroupElement(x) then
    T := LeftTranslationsSemigroupOfFamily(FamilyObj(x));
    S := UnderlyingSemigroup(T);
    tab := MultiplicationTableWithCanonicalPositions(S);
  else
    T := RightTranslationsSemigroupOfFamily(FamilyObj(x));
    S := UnderlyingSemigroup(T);
    tab := TransposedMultiplicationTableWithCanonicalPositions(S);
  fi;
  enum := EnumeratorCanonical(S);
  images := [];
  for g in UnderlyingGenerators(T) do
    UniteSet(images, tab[PositionCanonical(S, g ^ x)]);
  od;
  return images;
end;

InstallMethod(ImageOfTranslation, "for a left or right translation",
[IsTranslationsSemigroupElement],
function(x)
  local T, S, enum;
  if IsLeftTranslationsSemigroupElement(x) then
    T := LeftTranslationsSemigroupOfFamily(FamilyObj(x));
    S := UnderlyingSemigroup(T);
  else
    T := RightTranslationsSemigroupOfFamily(FamilyObj(x));
    S := UnderlyingSemigroup(T);
  fi;
  enum := EnumeratorCanonical(S);
  return Set(List(SEMIGROUPS.ImagePositionsOfTranslation(x), i -> enum[i]));
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
InstallGlobalFunction(LeftPartOfBitranslation,
function(h)
  if not IsBitranslation(h) then
     ErrorNoReturn("Semigroups: LeftPartOfBitranslation: \n",
                    "the argument must be a bitranslation,");
  fi;
  return h![1];
end);

InstallGlobalFunction(RightPartOfBitranslation,
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
