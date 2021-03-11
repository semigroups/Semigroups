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


if IsPackageMarkedForLoading("io", "4.4.4") then 
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

  BenchmarkLeftTranslations := function(arg)
    local funcs, strs, res, i;
    funcs := [SEMIGROUPS.LeftTranslationsNaiveBacktrack,
             SEMIGROUPS.LeftTranslationsBacktrackNoCache,
             SEMIGROUPS.LeftTranslationsBacktrack,
             SEMIGROUPS.LeftTranslationsStabilisedBacktrack,
             SEMIGROUPS.LeftTranslationsStabilisedOrderedBacktrack,
             SEMIGROUPS.LeftTranslationsStabilisedReverseOrderedBacktrack];
    
    strs := ["NaiveBacktrack",
             "BacktrackNoCache",
             "BacktrackWithCache",
             "StabilisedBacktrack",
             "StabilisedOrderedBacktrack",
             "StabilisedReverseOrderedBacktrack"];

    Print("Benchmarking:");
    ViewObj(arg[1]);
    Print("\n");
    SEMIGROUPS.LeftTranslationsBacktrackData(UnderlyingSemigroup(arg[1]));
    for i in [1 .. Length(funcs)] do
      res := Benchmark(funcs[i], arg);
      Print(strs[i] , ": ", res[1], " in ", res[2], " runs.\n");
    od;
  end;
  
  BenchmarkLeftIdealTranslations := function(arg)
    local funcs, strs, res, i;
    funcs := [SEMIGROUPS.LeftTranslationsIdealBacktrack,
             SEMIGROUPS.LeftTranslationsIdealStabilisedBacktrack];
    strs := ["IdealBacktrack", "StabilisedIdealBacktrack"];
    Print("Benchmarking ideal left translations of ",
          arg[1],
          " in ",
          arg[2],
          "\n");
    SEMIGROUPS.LeftTranslationsIdealBacktrackData(arg[1], arg[2], arg[3]);
    for i in [1 .. Length(funcs)] do
      res := Benchmark(funcs[i], arg);
      Print(strs[i] , ": ", res[1], " in ", res[2], " runs.\n");
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
      return SEMIGROUPS.LeftTranslationsBacktrack(T);
    else
      # TODO: dual or backtrack?
      return SEMIGROUPS.RightTranslationsBacktrack(T);
    fi;
  fi;
  Error("Semigroups: TranslationsSemigroupElements: \n",
        "no method of calculating this translations semigroup is known,");
end;

SEMIGROUPS.LeftTranslationsBacktrackData := function(S)
  local n, gens, m, genspos, multtable, multsets, r_classes, r_class_map,
  r_class_inv_map, r_classes_below, max_R_intersects, intersect, reps,
  right_intersect_multipliers, left_inverses_by_gen, right_inverses, seen, s,
  transposed_multtable, transposed_multsets, U, Ui, bad, keep, B, sb, r, i, j,
  a, t, u;

  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));

  multtable := MultiplicationTableWithCanonicalPositions(S);
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
  right_inverses := List([1 .. n], x -> ListWithIdenticalEntries(n, fail));
  seen := List([1 .. n], x -> false);
  for i in [1 .. m] do
    for j in [1 .. m] do
      if i = j then
        continue;
      fi;
      for a in max_R_intersects[i][j] do
        for t in left_inverses_by_gen[a][j] do
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
  od;

  transposed_multtable := TransposedMat(multtable);
  transposed_multsets  := List(transposed_multtable, Set);

  # compute intersection over a of the sets U_{i, a} from the paper
  U := [];
  for i in [1 .. m] do
    Ui := BlistList([1 .. n], []);
    bad := [];
    for s in [1 .. n] do
      #if Ui[s] or ForAny(transposed_multsets[s], x -> x in bad) then
      #  continue;
      #fi;
      if Ui[s] then 
        continue;
      fi;
      keep := true;
      for a in multsets[genspos[i]] do
        B := left_inverses_by_gen[a][i];
        sb := multtable[s][B[1]];
        if multtable[s]{B} <> ListWithIdenticalEntries(Size(B), sb) then
          keep := false;
#          AddSet(bad, s);
          break;
        fi;
      od;
      if keep then
        UniteBlistList([1 .. n], Ui, transposed_multsets[s]);
      fi;
#      Print("Computing U_", i, ", has size: ", SizeBlist(Ui), "\n");
    od;
    U[i] := ListBlist([1 .. n], Ui);
  od;

  r := rec();
  r.left_inverses_by_gen := left_inverses_by_gen;
  r.max_R_intersects := max_R_intersects;
  r.multtable := multtable;
  r.right_intersect_multipliers := right_intersect_multipliers;
  r.right_inverses := right_inverses;
  r.U := U;
  r.V := List([1 .. m], j -> List([1 .. n], a -> []));
  r.W := List([1 .. m], i -> List([1 .. m], j -> []));
  return r;
end;

SEMIGROUPS.LeftTranslationsBacktrackDataNoU := function(S)
  local n, gens, m, genspos, multtable, multsets, r_classes, r_class_map,
  r_class_inv_map, r_classes_below, max_R_intersects, intersect, reps,
  right_intersect_multipliers, left_inverses_by_gen, right_inverses, seen, s,
  transposed_multtable, transposed_multsets, U, Ui, bad, keep, B, sb, r, i, j,
  a, t, u;

  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));

  multtable := MultiplicationTableWithCanonicalPositions(S);
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
  right_inverses := List([1 .. n], x -> ListWithIdenticalEntries(n, fail));
  seen := List([1 .. n], x -> false);
  for i in [1 .. m] do
    for j in [1 .. m] do
      if i = j then
        continue;
      fi;
      for a in max_R_intersects[i][j] do
        for t in left_inverses_by_gen[a][j] do
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
  od;

  transposed_multtable := TransposedMat(multtable);
  transposed_multsets  := List(transposed_multtable, Set);

  r := rec();
  r.genspos := genspos;
  r.left_inverses_by_gen := left_inverses_by_gen;
  r.max_R_intersects := max_R_intersects;
  r.multsets := multsets;
  r.multtable := multtable;
  r.right_intersect_multipliers := right_intersect_multipliers;
  r.right_inverses := right_inverses;
  r.transposed_multsets := transposed_multsets;
  r.U := List([1 .. m], i -> BlistList([1 .. n], []));
  r.U_considered := List([1 .. m], i -> BlistList([1 .. n], []));
  r.V := List([1 .. m], j -> List([1 .. n], a -> []));
  r.W := List([1 .. m], i -> List([1 .. m], j -> []));
  return r;
end;

# I     - Right ideal of 
# S     - Semigroup
# gens  - subset of I s.t.
#         ({decomposable in I gens} * S^1) U ({other gens} * I^1) = I
SEMIGROUPS.LeftTranslationsIdealBacktrackData := function(I, S, gens)
  local n, m, genspos, gensposS, decomp, multtableI, multtableS, multsetsI,
  multsetsS, enumI, enumS, id_map, ItoS, StoI, rI_classes, rS_classes,
  rI_class_map, rS_class_map, rI_class_inv_map, rS_class_inv_map,
  rI_classes_below, rS_classes_below, max_R_intersects, intersect, reps,
  left_inverses_by_gen_in_I, left_inverses_by_gen_in_S, right_inverses, seen, C,
  k, map, s, transposed_multtableI, transposed_multsetsI, U, A, Ui, keep, B,
  tab, sb, r, i, j, t, a, u;

  n         := Size(I);
  m         := Size(gens);
  genspos   := List(gens, x -> PositionCanonical(I, x));
  gensposS  := List(gens, x -> PositionCanonical(S, x));
  decomp    := List([1 .. m], i -> not gens[i] in IndecomposableElements(I));

  multtableI := MultiplicationTableWithCanonicalPositions(I);
  multtableS := MultiplicationTableWithCanonicalPositions(S);
  multsetsI   := List(multtableI, Set);
  multsetsS   := List(multtableS, Set);

  enumI := EnumeratorCanonical(I);
  enumS := EnumeratorCanonical(S);

  id_map := [1 .. n];
  ItoS := List([1 .. n], i -> PositionCanonical(S, enumI[i]));
  StoI := [1 .. Size(S)];
  for i in [1 .. n] do
    StoI[ItoS[i]] := i;
  od;

  rI_classes := RClasses(I);
  rS_classes := RClasses(S);
  
  rI_class_map := [];
  rS_class_map := [];
 
  for i in [1 .. Length(rI_classes)] do
    for s in rI_classes[i] do
      rI_class_map[PositionCanonical(I, s)] := i;
    od;
  od;
  
  for i in [1 .. Length(rS_classes)] do
    for s in rS_classes[i] do
      rS_class_map[PositionCanonical(S, s)] := i;
    od;
  od;

  rI_class_inv_map := List(rI_classes,
                           x -> PositionCanonical(I, Representative(x)));
  rS_class_inv_map := List(rS_classes,
                           x -> PositionCanonical(I, Representative(x)));

  rI_classes_below := List([1 .. m], i -> Set(rI_class_map{multsetsI[genspos[i]]}));
  rS_classes_below := List([1 .. m], i -> Set(rS_class_map{multsetsS[gensposS[i]]}));

  max_R_intersects := List([1 .. m], x -> []);
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      if decomp[i] and decomp[j] then
        intersect := Intersection(rS_classes_below[i], rS_classes_below[j]);
        reps := rS_class_inv_map{intersect};
      else
        intersect := Intersection(rI_classes_below[i], rI_classes_below[j]);
        reps := rI_class_inv_map{intersect};
      fi;

      max_R_intersects[i][j] := Filtered(reps,
                                         x -> not ForAny(reps, 
                                                         y -> x <> y and
                                                           x in multsetsS[y]));

      max_R_intersects[j][i] := max_R_intersects[i][j];
    od;
  od;

  # for all s in I, store the elements t such that gens[i] * t = s for each i
  left_inverses_by_gen_in_I := List([1 .. n], x -> List([1 .. m], y -> []));
  for i in [1 .. m] do
    if decomp[i] then
      continue;
    fi;
    for t in [1 .. n] do
      Add(left_inverses_by_gen_in_I[multtableI[genspos[i]][t]][i], t);
    od;
  od;

  left_inverses_by_gen_in_S := List([1 .. Size(S)], x -> List([1 .. m],
                                                              y -> []));
  for i in [1 .. m] do
    if not decomp[i] then
      continue;
    fi;
    for t in [1 .. Size(S)] do
      Add(left_inverses_by_gen_in_S[multtableS[gensposS[i]][t]][i], t);
    od;
  od;

  # for each t in the left inverses of some a in max_R_intersects[i][j] by 
  # gens[j], compute the right inverses of each s in S under t
  right_inverses := List([1 .. n],
                         x -> ListWithIdenticalEntries(n + Size(S), fail));
  seen := List([1 .. n + Size(S)], x -> false);
  for i in [1 .. m - 1] do
    for j in [i + 1 .. m] do
      for a in max_R_intersects[i][j] do
        if decomp[j] then
          C := List(left_inverses_by_gen_in_S[ItoS[a]][j]);
        else
          C := left_inverses_by_gen_in_I[a][j];
        fi;
        for t in C do
          # don't repeat the calculation if we've already done it for t!
          if decomp[j] then
            k := n + t;
            map := StoI;
          else
            k := t;
            map := id_map;
          fi;
          if not seen[k] then
            seen[k] := true;
            for u in [1 .. n] do
              s := multtableI[u][map[t]];
              if right_inverses[s][k] = fail then
                right_inverses[s][k] := [];
              fi;
              Add(right_inverses[s][k], u);
            od;
          fi;
        od;
      od;
    od;
  od;

  transposed_multtableI := TransposedMat(multtableI);
  transposed_multsetsI  := List(transposed_multtableI, Set);

  # compute intersection over a of the sets U_{i, a} from the paper
  U := [];
  for i in [1 .. m] do
    if decomp[i] then
      A := multsetsS[gensposS[i]];
    else
      A := multsetsI[genspos[i]];
    fi;
    Ui := ListWithIdenticalEntries(n, true);
    for a in A do
      keep := ListWithIdenticalEntries(n, false);
      # TODO: optimise
      for s in [1 .. n] do
        if keep[s] then
          continue;
        fi;
        if decomp[i] then
          s := ItoS[s];
          B := left_inverses_by_gen_in_S[a][i];
          tab := multtableS;
        else
          B := left_inverses_by_gen_in_I[a][i];
          tab := multtableI;
        fi;
        sb := tab[s][B[1]];
        if ForAll(B, b -> tab[s][b] = sb) then
            UniteBlistList([1 .. n], keep, transposed_multsetsI[s]);
        fi;
      od;
      IntersectBlist(Ui, keep);
    od;
    U[i] := ListBlist([1 .. n], Ui);
  od;

  r := rec();
  r.left_inverses_by_gen_in_I := left_inverses_by_gen_in_I;
  r.left_inverses_by_gen_in_S := left_inverses_by_gen_in_S;
  r.StoI := StoI;
  r.ItoS := ItoS;
  r.decomp := decomp;
  r.max_R_intersects := max_R_intersects;
  r.multtableI := multtableI;
  r.multtableS := multtableS;
  r.right_inverses := right_inverses;
  r.U := U;
  r.U_considered := List([1 .. m], i -> BlistList([1 .. n], []));
  r.V := List([1 .. m], j -> List([1 .. n], a -> []));
  r.W := List([1 .. m], i -> List([1 .. m], j -> []));
  return r;
end;

SEMIGROUPS.LeftTranslationsBacktrackUContains := function(data, i, s)
  local Ui, Ui_considered, n, multtable, keep, B, sb, a;
  Ui := data.U[i];
  Ui_considered := data.U_considered[i];
  n := Length(Ui);

  if Ui_considered[s] then
    return Ui[s];
  fi;

  multtable := data.multtable;

  keep := true;
  for a in data.multsets[data.genspos[i]] do
    B := data.left_inverses_by_gen[a][i];
    sb := multtable[s][B[1]];
    if multtable[s]{B} <> ListWithIdenticalEntries(Size(B), sb) then
      keep := false;
      break;
    fi;
  od;
  if keep then
    UniteBlistList([1 .. n], Ui, data.transposed_multsets[s]);
    UniteBlistList([1 .. n], Ui_considered, data.transposed_multsets[s]);
  fi;
  Ui_considered[s] := true;
  return keep;
end;

SEMIGROUPS.LeftTranslationsBacktrackDataV := function(data, j, a, s)
  local right_inverses, V, C, t;

  if IsBound(data.V[j][a][s]) then
    return data.V[j][a][s];
  fi;

  right_inverses := data.right_inverses;
  V := [1 .. Size(data.multtable)];
  for t in data.left_inverses_by_gen[a][j] do
    C := right_inverses[s][t];
    if C = fail then
      V := [];
      break;
    else
      V := Intersection(V, C);
    fi;
  od;
  data.V[j][a][s] := V;
  return V;
end;

SEMIGROUPS.LeftTranslationsBacktrackDataW := function(data, i, j, s)
  local right_intersect_multipliers, multtable, W, x, a, r;

  if IsBound(data.W[i][j][s]) then
    return data.W[i][j][s];
  fi;

  right_intersect_multipliers := data.right_intersect_multipliers;
  multtable := data.multtable;
  W := [1 .. Size(data.multtable)];
  for a in data.max_R_intersects[i][j] do
    for r in right_intersect_multipliers[i][a] do
      x := multtable[s][r];
      W := Intersection(W, 
                        SEMIGROUPS.LeftTranslationsBacktrackDataV(data,
                                                                  j,
                                                                  a,
                                                                  x));
    od;
  od;
  data.W[i][j][s] := W;
  return W;
end;

SEMIGROUPS.LeftTranslationsBacktrackDataVNoCache := function(data, j, a, s)
  local right_inverses, V, C, t;

  right_inverses := data.right_inverses;
  V := [1 .. Size(data.multtable)];
  for t in data.left_inverses_by_gen[a][j] do
    C := right_inverses[s][t];
    if C = fail then
      V := [];
      break;
    else
      V := Intersection(V, C);
    fi;
  od;
  return V;
end;

SEMIGROUPS.LeftTranslationsBacktrackDataWNoCache := function(data, i, j, s)
  local right_intersect_multipliers, multtable, W, x, a, r;

  right_intersect_multipliers := data.right_intersect_multipliers;
  multtable := data.multtable;
  W := [1 .. Size(data.multtable)];
  for a in data.max_R_intersects[i][j] do
    for r in right_intersect_multipliers[i][a] do
      x := multtable[s][r];
      W := Intersection(W, 
                        SEMIGROUPS.LeftTranslationsBacktrackDataVNoCache(data,
                                                                         j,
                                                                         a,
                                                                         x));
    od;
  od;
  return W;
end;

SEMIGROUPS.LeftTranslationsIdealBacktrackDataV := function(data, j, a, s)
  local right_inverses, V, T, add_n, C, t;

  if IsBound(data.V[j][a][s]) then
    return data.V[j][a][s];
  fi;

  right_inverses := data.right_inverses;
  V := [1 .. Size(data.multtableI)];
  if data.decomp[j] then
    T := data.left_inverses_by_gen_in_S[data.ItoS[a]][j];
    add_n := 1;
  else
    T := data.left_inverses_by_gen_in_I[a][j];
    add_n := 0;
  fi;
  for t in T do
    C := right_inverses[s][t + Size(data.multtableI) * add_n];
    if C = fail then
      V := [];
      break;
    else
      V := Intersection(V, C);
    fi;
  od;
  data.V[j][a][s] := V;
  return V;
end;

SEMIGROUPS.LeftTranslationsIdealBacktrackDataW := function(data, i, j, s)
  local toI, fromI, mults, tab, multtableI, W, x, a, r;

  if IsBound(data.W[i][j][s]) then
    return data.W[i][j][s];
  fi;

  if data.decomp[i] then
    toI := data.StoI;
    fromI := data.ItoS;
    mults := data.left_inverses_by_gen_in_S;
    tab := data.multtableS;
  else
    toI := [1 .. Size(data.multtableI)];
    fromI := toI;
    mults := data.left_inverses_by_gen_in_I;
    tab := data.multtableI;
  fi;

  W := [1 .. Size(data.multtableI)];
  for a in data.max_R_intersects[i][j] do
    for r in mults[fromI[a]][i] do
      x := toI[tab[fromI[s]][r]];
      W := Intersection(W,
                        SEMIGROUPS.LeftTranslationsIdealBacktrackDataV(data,
                                                                       j,
                                                                       a,
                                                                       x));
    od;
  od;
  data.W[i][j][s] := W;
  return W;
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
  local n, m, D, x, y, vertex_cols, edge_cols, g, s;
  n := Size(mult_table);
  m := Size(gens_pos);
  D := Digraph(IsMutableDigraph, []);
  DigraphAddVertices(D, 2 * n);
  for g in gens_pos do
    for s in [1 .. Size(mult_table)] do
      x := mult_table[s][g];
      y := mult_table[g][s];
      DigraphAddEdge(D, [s, x]);
      DigraphAddEdge(D, [s + n, y + n]);
    od;
  od;
  DigraphAddEdges(D, List([1 .. n], i -> [i, i + n]));
  DigraphAddEdges(D, List([1 .. n], i -> [i + n, i]));
  vertex_cols := Concatenation(ListWithIdenticalEntries(n, 1),
                               ListWithIdenticalEntries(n, 2));;
  edge_cols := List([1 .. 2 * n], x -> [1 .. m + 1]);
  MakeImmutable(D);
  if IsMultiDigraph(D) then
    return Range(Projection(AutomorphismGroup(D, vertex_cols, edge_cols), 1));
  else
    return AutomorphismGroup(D, vertex_cols, edge_cols);
  fi;
end;

SEMIGROUPS.RightTranslationsBacktrackData := function(S)
  local n, gens, m, genspos, possiblefgenvals, transpose_mult_table,
  transpose_mult_sets, L_classes_below, max_L_intersects, intersect, maximals,
  left_intersect_multipliers, right_inverses_by_gen, left_inverses, seen, s, F,
  G, T, C, u, r, i, j, a, t, l, Ru;
  
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);

  transpose_mult_table := TransposedMultiplicationTableWithCanonicalPositions(S);
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

  # compute the left multipliers to the intersects from the gens
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
  r.max_L_intersects := max_L_intersects;
  r.transpose_mult_table := transpose_mult_table;
  r.left_intersect_multipliers := left_intersect_multipliers;
  r.left_inverses := left_inverses;
  r.T := T;
  r.F := List([1 .. m], j -> List([1 .. n], a -> []));
  r.G := List([1 .. m], i -> List([1 .. m], j -> []));
  return r;
end;

SEMIGROUPS.RightTranslationsBacktrackDataF := function(data, j, a, s)
  local left_inverses, F, t;

  if IsBound(data.F[j][a][s]) then
    return data.F[j][a][s];
  fi;

  left_inverses := data.left_inverses;
  F := [1 .. Size(data.transpose_mult_table)];
  for t in data.right_inverses_by_gen[a][j] do
    F := Intersection(F, left_inverses[s][t]);
  od;
  data.F[j][a][s] := F;
  return F;
end;

SEMIGROUPS.RightTranslationsBacktrackDataG := function(data, i, j, s)
  local left_intersect_multipliers, transpose_mult_table, G, x, a, l;

  if IsBound(data.G[i][j][s]) then
    return data.G[i][j][s];
  fi;

  left_intersect_multipliers := data.left_intersect_multipliers;
  transpose_mult_table := data.transpose_mult_table;
  G := [1 .. Size(data.transpose_mult_table)];
  for a in data.max_L_intersects[i][j] do
    for l in left_intersect_multipliers[i][a] do
      x := transpose_mult_table[s][l];
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

SEMIGROUPS.IsWeaklyReductive := function(S)
  return Size(InnerTranslationalHull(S)) = Size(S);
end;

# TODO: doesn't this already have a name somewhere?
SEMIGROUPS.IsGloballyIdempotent := function(S)
  return Size(IndecomposableElements(S)) = 0;
end;

SEMIGROUPS.PermutabilityStrategyRightTranslations := function(S)
  return RightTranslations(S);
end;

SEMIGROUPS.LeftTranslationsPermutabilityRestrictions := function(L, data)
  local S, gens, m, n, genspos, multtable, permitted_vals, right_inverses,
  rho_img, invs, inv_img, rho, i, j, r, k;
  S               := UnderlyingSemigroup(L);
  gens            := UnderlyingGenerators(L);
  m               := Size(gens);
  n               := Size(S);
  genspos         := List(gens, x -> PositionCanonical(S, x));
  multtable       := MultiplicationTableWithCanonicalPositions(S);
  permitted_vals  := List([1 .. m], i -> [1 .. n]);
  right_inverses  := List([1 .. n], i -> []);
  for rho in SEMIGROUPS.PermutabilityStrategyRightTranslations(S) do
    rho_img := SEMIGROUPS.ImagePositionsOfTranslation(rho);
    # TODO: experiment here
    if Size(rho_img) > n/3 then
      continue;
    fi;
    for i in [1 .. m] do
      for j in [1 .. m] do
        if gens[j] in IndecomposableElements(S) then
          continue;
        fi;
        invs := data.left_inverses_by_gen[PositionCanonical(S, 
                                                            gens[j] ^ rho)][i];
        for r in invs do
          inv_img := [];
          for k in rho_img do
            if not IsBound(right_inverses[k][r]) then
              right_inverses[k][r] := Filtered([1 .. n],
                                               x -> multtable[x][r] = k);
            fi;
            UniteSet(inv_img, right_inverses[k][r]);
            if Size(inv_img) = n then
              break;
            fi;
          od;
          IntersectSet(permitted_vals[i], inv_img);
        od;
      od;
    od;
    Print(List(permitted_vals, Size));
  od;
  return permitted_vals;
end;

SEMIGROUPS.LeftTranslationsBacktrack := function(L)
  local S, n, gens, m, genspos, omega_stack, possiblefgenvals, multtable, data,
  U, bt, lambda, out, i, j, s;

  S           := UnderlyingSemigroup(L);
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
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
#  Apply(out, x -> LeftTranslationNC(L, x));
  return out;
end;

SEMIGROUPS.LeftTranslationsIdealBacktrack := function(I, S, gens)
  local n, m, genspos, omega_stack, possiblefgenvals, multtable, data, U, bt,
  lambda, out, i;

  n           := Size(I);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(I, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);

  multtable := MultiplicationTableWithCanonicalPositions(I);

  data := SEMIGROUPS.LeftTranslationsIdealBacktrackData(I, S, gens);
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
          W := SEMIGROUPS.LeftTranslationsIdealBacktrackDataW(data, i, j, s);
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
  local S, n, gens, m, genspos, omega_stack, possiblefgenvals, stabs,
  stab_thresh, reps_thresh, coset_reps, multtable, data, U, aut,
  add_stabilised_lambda, bt, lambda, out, considered, i;

  S           := UnderlyingSemigroup(L);
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);
  stabs := [];
  stabs[m] := [];
  stab_thresh := 10;
  reps_thresh := 10;
  coset_reps := [];

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.LeftTranslationsBacktrackData(S);
  U := data.U;

  aut := SEMIGROUPS.LeftAutoTranslations(multtable, genspos);

  # restrict via the U_{i}
  for i in [1 .. m] do
    IntersectSet(possiblefgenvals[i], U[i]);
  od;

  add_stabilised_lambda := function()
    local stab_depth, it, mult;
    stab_depth := PositionProperty(stabs, x -> Size(x) = 0) - 1;
    it := IteratorOfCartesianProduct(coset_reps{[1 .. stab_depth]});
    while not IsDoneIterator(it) do
      mult := Product(NextIterator(it));
      Add(out, OnTuples(lambda, mult));
    od;
  end;

  bt := function(i) 
    local stab, big_stab, big_reps, orbs, reps, consistent, W, s, j;
    if i > 1 then
      stab := stabs[i - 1];
    else
      stab := aut;
    fi;
    big_stab := Size(stab) > stab_thresh;
    big_reps := Size(omega_stack[i][i]) > reps_thresh;
    if big_stab and big_reps then
      orbs := Orbits(stab, omega_stack[i][i]);
      reps := List(orbs, x -> x[1]);
#      Print("omega_stack[", i, "][", i, "] has size: ", Size(omega_stack[i][i]),
#            "\n");
#      Print("orbits have size: ", List(orbs, Size));
    else
      reps := omega_stack[i][i];
    fi;
    for s in reps do
#      considered[i][s] := true;
      lambda[i] := s;
      if i = m then
#        add_stabilised_lambda();
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
          if big_stab then
            if Size(reps) = 1 then
              stabs[i] := stab;
              coset_reps[i] := [()];
            else
              stabs[i] := Stabiliser(stab, s);
              coset_reps[i] := RightTransversal(stab, stabs[i]);
            fi;
#             Print("Stab size: ", Size(stabs[i]), "\n");
          else
            stabs[i] := [];
            coset_reps[i] := [];
          fi;
          bt(i + 1);
        fi;
      fi;
    od;
#    Print("backtracking \n");
  end;

  omega_stack := [possiblefgenvals];
  lambda := [];
  out := [];
  considered := List([1 .. m], x -> BlistList([1 .. n], []));
  bt(1);
#  Apply(out, x -> LeftTranslationNC(L, x));
#  for i in [1 .. m] do
#    Print("Considered ",
#          Number(omega_stack[1][i], j -> considered[i][j]),
#          "/",
#          Size(omega_stack[1][i]),
#          " values in U_",
#          i,
#          "\n");
#  od;
  return out;
end;

SEMIGROUPS.LeftTranslationsStabilisedLazyBacktrack := function(L)
  local S, n, gens, m, genspos, omega_stack, possiblefgenvals, stabs,
  stab_thresh, reps_thresh, coset_reps, multtable, data, U, aut,
  add_stabilised_lambda, bt, lambda, out, considered, i;

  S           := UnderlyingSemigroup(L);
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);
  stabs := [];
  stabs[m] := [];
  stab_thresh := 10;
  reps_thresh := 10;
  coset_reps := [];

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.LeftTranslationsBacktrackDataNoU(S);

  aut := SEMIGROUPS.LeftAutoTranslations(multtable, genspos);

  add_stabilised_lambda := function()
    local stab_depth, it, mult;
    stab_depth := PositionProperty(stabs, x -> Size(x) = 0) - 1;
    it := IteratorOfCartesianProduct(coset_reps{[1 .. stab_depth]});
    while not IsDoneIterator(it) do
      mult := Product(NextIterator(it));
      Add(out, OnTuples(lambda, mult));
    od;
  end;

  bt := function(i) 
    local stab, use_stab, orbs, reps, consistent, W, s, j;
    if i > 1 then
      stab := stabs[i - 1];
    else
      stab := aut;
    fi;
    use_stab := Size(stab) > stab_thresh and
                Size(omega_stack[i][i]) > reps_thresh;
    if use_stab then
      orbs := Orbits(stab, omega_stack[i][i]);
      reps := List(orbs, x -> x[1]);
#      Print("omega_stack[", i, "][", i, "] has size: ", Size(omega_stack[i][i]),
#            "\n");
#      Print("orbits have size: ", List(orbs, Size));
    else
      reps := omega_stack[i][i];
    fi;
    for s in reps do
      if not SEMIGROUPS.LeftTranslationsBacktrackUContains(data, i, s) then
        for j in [1 .. i] do
          RemoveSet(omega_stack[j][i], s);
        od;
        continue;
      fi;
#      considered[i][s] := true;
      lambda[i] := s;
      if i = m then
#        add_stabilised_lambda();
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
          if use_stab then
            stabs[i] := Stabiliser(stab, s);
#            Print("Stab size: ", Size(stabs[i]), "\n");
            coset_reps[i] := RightTransversal(stab, stabs[i]);
          else
            stabs[i] := [];
            coset_reps[i] := [];
          fi;
          bt(i + 1);
        fi;
      fi;
    od;
#    Print("backtracking \n");
  end;

  omega_stack := [possiblefgenvals];
  lambda := [];
  out := [];
#  considered := List([1 .. m], x -> BlistList([1 .. n], []));
  bt(1);
#  Apply(out, x -> LeftTranslationNC(L, x));
#  for i in [1 .. m] do
#    Print("Considered ",
#          Number(omega_stack[1][i], j -> considered[i][j]),
#          "/",
#          Size(omega_stack[1][i]),
#          " values in U_",
#          i,
#          "\n");
#  od;
  return out;
end;

SEMIGROUPS.LeftTranslationsStabilisedOrderedBacktrack := function(L)
  local S, n, gens, m, genspos, omega_stack, possiblefgenvals, stabs,
  stab_thresh, coset_reps, multtable, data, U, gens_map, aut,
  add_stabilised_lambda, bt, lambda, out, i;

  S           := UnderlyingSemigroup(L);
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);
  stabs := [];
  stabs[m] := [];
  stab_thresh := 20;
  coset_reps := [];

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.LeftTranslationsBacktrackData(S);
  U := data.U;
  
  # restrict via the U_{i}
  for i in [1 .. m] do
    IntersectSet(possiblefgenvals[i], U[i]);
  od;

  gens_map := [1 .. m];
  Sort(gens_map, {x, y} -> Size(possiblefgenvals[x]) <
                            Size(possiblefgenvals[y]));

  Print(List(possiblefgenvals, Size), "\n");

  aut := SEMIGROUPS.LeftAutoTranslations(multtable, genspos);

  add_stabilised_lambda := function()
    local stab_depth, it, mult;
    stab_depth := PositionProperty(stabs, x -> Size(x) = 0) - 1;
    it := IteratorOfCartesianProduct(coset_reps{[1 .. stab_depth]});
    while not IsDoneIterator(it) do
      mult := Product(NextIterator(it));
      Add(out, OnTuples(lambda, mult));
    od;
  end;

  bt := function(i) 
    local stab, use_stab, orbs, reps, consistent, W, s, j;
    if i > 1 then
      stab := stabs[i - 1];
    else
      stab := aut;
    fi;
    use_stab := Size(stab) > stab_thresh;
    if use_stab then
      orbs := Orbits(stab, omega_stack[i][gens_map[i]]);
      reps := List(orbs, x -> x[1]);
    else
      reps := omega_stack[i][gens_map[i]];
    fi;
    for s in reps do
      lambda[gens_map[i]] := s;
      if i = m then
        add_stabilised_lambda();
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          W := SEMIGROUPS.LeftTranslationsBacktrackDataW(data,
                                                         gens_map[i],
                                                         gens_map[j], s);
#          Error();
          omega_stack[i + 1][gens_map[j]] :=
            Intersection(omega_stack[i][gens_map[j]], W);
          if IsEmpty(omega_stack[i + 1][gens_map[j]]) then
            consistent := false;
            break;
          fi;
        od;
        if consistent then
          if use_stab then
            stabs[i] := Stabiliser(stab, s);
            coset_reps[i] := List(RightCosets(stab, stabs[i]), Representative);
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
  bt(1);
#  Apply(out, x -> LeftTranslationNC(L, x));
  return out;
end;

SEMIGROUPS.LeftTranslationsStabilisedReverseOrderedBacktrack := function(L)
  local S, n, gens, m, genspos, omega_stack, possiblefgenvals, stabs,
  stab_thresh, coset_reps, multtable, data, U, gens_map, aut,
  add_stabilised_lambda, bt, lambda, out, i;

  S           := UnderlyingSemigroup(L);
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);
  stabs := [];
  stabs[m] := [];
  stab_thresh := 20;
  coset_reps := [];

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.LeftTranslationsBacktrackData(S);
  U := data.U;
  
  # restrict via the U_{i}
  for i in [1 .. m] do
    IntersectSet(possiblefgenvals[i], U[i]);
  od;

  gens_map := [1 .. m];
  Sort(gens_map, {x, y} -> Size(possiblefgenvals[x]) >
                            Size(possiblefgenvals[y]));

  Print(List(possiblefgenvals, Size), "\n");

  aut := SEMIGROUPS.LeftAutoTranslations(multtable, genspos);

  add_stabilised_lambda := function()
    local stab_depth, it, mult;
    stab_depth := PositionProperty(stabs, x -> Size(x) = 0) - 1;
    it := IteratorOfCartesianProduct(coset_reps{[1 .. stab_depth]});
    while not IsDoneIterator(it) do
      mult := Product(NextIterator(it));
      Add(out, OnTuples(lambda, mult));
    od;
  end;

  bt := function(i) 
    local stab, use_stab, orbs, reps, consistent, W, s, j;
    if i > 1 then
      stab := stabs[i - 1];
    else
      stab := aut;
    fi;
    use_stab := Size(stab) > stab_thresh;
    if use_stab then
      orbs := Orbits(stab, omega_stack[i][gens_map[i]]);
      reps := List(orbs, x -> x[1]);
    else
      reps := omega_stack[i][gens_map[i]];
    fi;
    for s in reps do
      lambda[gens_map[i]] := s;
      if i = m then
        add_stabilised_lambda();
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          W := SEMIGROUPS.LeftTranslationsBacktrackDataW(data,
                                                         gens_map[i],
                                                         gens_map[j], s);
#          Error();
          omega_stack[i + 1][gens_map[j]] :=
            Intersection(omega_stack[i][gens_map[j]], W);
          if IsEmpty(omega_stack[i + 1][gens_map[j]]) then
            consistent := false;
            break;
          fi;
        od;
        if consistent then
          if use_stab then
            stabs[i] := Stabiliser(stab, s);
            coset_reps[i] := List(RightCosets(stab, stabs[i]), Representative);
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
  bt(1);
#  Apply(out, x -> LeftTranslationNC(L, x));
  return out;
end;

SEMIGROUPS.LeftTranslationsIdealStabilisedBacktrack := function(I, S, gens)
  local n, m, genspos, omega_stack, possiblefgenvals, stabs, orbs, stab_thresh,
  multtable, data, U, aut, bt, lambda, out, i;

  n           := Size(I);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(I, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  possiblefgenvals := List([1 .. m], i -> [1 .. n]);
  stabs := [];
  orbs := List([1 .. m], i -> []);
  stab_thresh := 20;

  multtable := MultiplicationTableWithCanonicalPositions(I);

  data := SEMIGROUPS.LeftTranslationsIdealBacktrackData(I, S, gens);
  U := data.U;

  aut := SEMIGROUPS.LeftAutoTranslations(multtable, genspos);
  
  # restrict via the U_{i}
  for i in [1 .. m] do
    IntersectSet(possiblefgenvals[i], U[i]);
  od;

  bt := function(i) 
    local stab, use_stab, seen, reps, orb, consistent, W, s, j;
    if i > 1 then
      stab := stabs[i - 1];
    else
      stab := aut;
    fi;
    use_stab := Size(stab) > stab_thresh;
    if use_stab then
      seen := BlistList([1 .. n], []);
      reps := [];
      for s in omega_stack[i][i] do
        if not seen[s] then
          Add(reps, s);
          orb := Orbit(stab, s);
          UniteBlistList([1 .. n], seen, orb);
        fi;
      od;
    else
      reps := omega_stack[i][i];
    fi;
    for s in reps do
      lambda[i] := s;
      if i = m then
        Add(out, ShallowCopy(lambda));
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          W := SEMIGROUPS.LeftTranslationsIdealBacktrackDataW(data, i, j, s);
          omega_stack[i + 1][j] := Intersection(omega_stack[i][j], W);
          if IsEmpty(omega_stack[i + 1][j]) then
            consistent := false;
            break;
          fi;
        od;
        if consistent then
          if Size(stab)/2 > stab_thresh then
            stabs[i] := Stabiliser(stab, s);
          else
            stabs[i] := [];
          fi;
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

SEMIGROUPS.LeftTranslationsNaiveBacktrack := function(L)
  local S, n, gens, m, genspos, multtable, bt, omega_stack, lambda, out;

  S           := UnderlyingSemigroup(L);
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
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
        Add(bound, i);
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

SEMIGROUPS.LeftTranslationsBacktrackNoCache := function(L)
  local S, n, gens, m, genspos, omega_stack, possiblefgenvals, multtable, data,
  U, bt, lambda, out, i, j, s;

  S           := UnderlyingSemigroup(L);
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
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
  Apply(out, x -> RightTranslationNC(L, x));
  return out;
end;

SEMIGROUPS.RightTranslationsStabilisedBacktrack := function(R)
  local S, n, gens, m, genspos, omega_stack, stabs, coset_reps, stab_thresh,
  multtable, data, T, possiblegenvals, aut, add_stabilised_rho, bt, rho, out, i;

  S           := UnderlyingSemigroup(R);
  n           := Size(S);
  gens        := GeneratorsOfSemigroup(S);
  m           := Size(gens);
  genspos     := List(gens, x -> PositionCanonical(S, x));
  omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
  stabs := [];
  stabs[m] := [];
  coset_reps := [];
  stab_thresh := 20;

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.RightTranslationsBacktrackData(S);
  T := data.T;
  
  possiblegenvals := List([1 .. m], i -> [1 .. n]);
  
  aut := SEMIGROUPS.RightAutoTranslations(multtable, genspos);
  
  # restrict via the T_{i}
  for i in [1 .. m] do
    IntersectSet(possiblegenvals[i], T[i]);
  od;

  add_stabilised_rho := function()
    local stab_depth, it, mult;
    stab_depth := PositionProperty(stabs, x -> Size(x) = 0) - 1;
    it := IteratorOfCartesianProduct(coset_reps{[1 .. stab_depth]});
    while not IsDoneIterator(it) do
      mult := Product(NextIterator(it));
      Add(out, OnTuples(rho, mult));
    od;
  end;

  bt := function(i) 
    local stab, use_stab, orbs, reps, consistent, G, s, j;
    if i > 1 then
      stab := stabs[i - 1];
    else
      stab := aut;
    fi;
    use_stab := Size(stab) > stab_thresh;
    if use_stab then
      orbs := Orbits(stab, omega_stack[i][i]);
      reps := List(orbs, x -> x[1]);
    else
      reps := omega_stack[i][i];
    fi;
    for s in reps do
      rho[i] := s;
      if i = m then
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
          if use_stab then
            stabs[i] := Stabiliser(stab, s);
            coset_reps[i] := List(RightCosets(stab, stabs[i]), Representative);
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
  bt(1);
#  Apply(out, x -> RightTranslationNC(R, x));
  return out;
end;

SEMIGROUPS.BitranslationsAlternatingBacktrack := function(H)
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
  
  if ValueOption("SEMIGROUPS_bitranslat_nr_only") = true then
    return Length(out);
  fi;

  L := LeftTranslations(S);
  R := RightTranslations(S);
#  Apply(out, x -> Bitranslation(H,
#                                LeftTranslationNC(L, x[1]),
#                                RightTranslationNC(R, x[2])));
  return out;
end;

SEMIGROUPS.BitranslationsAlternatingStabilisedBacktrack := function(H)
  local S, n, gens, m, genspos, l_omega_stack, r_omega_stack, l_stabs,
  l_coset_reps, r_stabs, r_coset_reps, stab_thresh, multtable, left_data,
  right_data, left_inverses_by_gen, right_inverses_by_gen, U, T, aut,
  add_stabilised_pair, bt, lambda, rho, out, L, R, i;

  S             := UnderlyingSemigroup(H);
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
            l_coset_reps[k] := List(RightCosets(stab, r_stabs[k]), Representative);
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
  
  if ValueOption("SEMIGROUPS_bitranslat_nr_only") = true then
    return Length(out);
  fi;

  L := LeftTranslations(S);
  R := RightTranslations(S);
#  Apply(out, x -> Bitranslation(H,
#                                LeftTranslationNC(L, x[1]),
#                                RightTranslationNC(R, x[2])));
  return out;
end;

SEMIGROUPS.BitranslationsRLSequentialBacktrack := function(H)
  local S, n, gens, m, genspos, multtable, data, left_inverses_by_gen, U, bt,
  out, R, omega_stack, lambda, L, rho, i, j;

  S             := UnderlyingSemigroup(H);
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
  
  R := RightTranslations(S);
  for rho in SEMIGROUPS.RightTranslationsStabilisedBacktrack(R) do
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

SEMIGROUPS.BitranslationsLRSequentialBacktrack := function(H)
  local S, n, gens, m, genspos, multtable, data, right_inverses_by_gen, T, bt,
  out, R, omega_stack, rho, lambda, i, j;

  S             := UnderlyingSemigroup(H);
  n             := Size(S);
  gens          := GeneratorsOfSemigroup(S);
  m             := Size(gens);
  genspos       := List(gens, x -> PositionCanonical(S, x));

  multtable := MultiplicationTableWithCanonicalPositions(S);

  data := SEMIGROUPS.RightTranslationsBacktrackData(S);
  right_inverses_by_gen := data.right_inverses_by_gen;
  T := data.T;

  bt := function(i) 
    local consistent, W, s, j;
    for s in omega_stack[i][i] do
      rho[i] := s;
      if i = m then
        Add(out, ShallowCopy(rho));
      else
        consistent := true;
        omega_stack[i + 1] := [];
        for j in [i + 1 .. m] do
          W := SEMIGROUPS.RightTranslationsBacktrackDataG(data, i, j, s);
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
  
  R := LeftTranslations(S);
  for lambda in SEMIGROUPS.LeftTranslationsStabilisedBacktrack(R) do
    omega_stack := List([1 .. m], i -> List([1 .. m], j -> []));
    omega_stack[1] := List([1 .. m], i -> T[i]);
    for i in [1 .. m] do
      for j in [1 .. m] do
          omega_stack[1][j] :=
            Intersection(omega_stack[1][j],
                         right_inverses_by_gen[multtable[genspos[j]][lambda[i]]][i]);
      od;
    od;

    rho := [];
    bt(1);
  od;
  
  if ValueOption("SEMIGROUPS_bitranslat_nr_only") = true then
    return Length(out);
  fi;

#  L := RightTranslations(S);
#  Apply(out, x -> Bitranslation(H,
#                                RightTranslationNC(L, x[1]),
#                                LeftTranslationNC(R, x[2])));
  return out;
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
