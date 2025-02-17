#############################################################################
##
##  attributes/maximal.gi
##  Copyright (C) 2013-2022                              James D. Mitchell
##                                                          Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# WAW A more complicated version incorporating some maximal subsemigroup theory
#     did not seem to perform significantly better, and so was removed.

InstallMethod(IsMaximalSubsemigroup, "for a semigroup and a semigroup",
[IsSemigroup, IsSemigroup],
function(S, T)
  local gens;
  if IsSubsemigroup(S, T) and S <> T then
    if not IsFinite(S) then
      TryNextMethod();
    fi;
    gens := GeneratorsOfSemigroup(T);
    return ForAll(S, x -> x in T or Semigroup(gens, x) = S);
  fi;
  return false;
end);

# NrMaximalSubsemigroups:
# uses MaximalSubsemigroups algorithm without creating the semigroups themselves

InstallMethod(NrMaximalSubsemigroups,
"for a semigroup with known maximal subsemigroups",
[IsSemigroup and HasMaximalSubsemigroups],
S -> Length(MaximalSubsemigroups(S)));

InstallMethod(NrMaximalSubsemigroups, "for a semigroup", [IsSemigroup],
S -> MaximalSubsemigroupsNC(S, rec(number := true)));

InstallMethod(MaximalSubsemigroups, "for a semigroup", [IsSemigroup],
S -> MaximalSubsemigroupsNC(S, rec(number := false)));

# MaximalSubsemigroups(S, r):
#
# - <r.number>:  (true) count the maximal subsemigroups;
#                (false) create the maximal subsemigroups (default).
# - <r.gens>:    relevant only if <r.number> is false:
#                  (true) return the results as generating sets;
#                  (false) return the results as semigroup objects (default).
# - <r.contain>: a duplicate-free subset of S:
#                  find only those maximal subsemigps which contain <r.contain>.
# - <r.D>:       a D-class of S:
#                  a maximal subsemigroup of a finite semigroup lacks part of
#                  only one D-class. With this option, the function will find
#                  only maximal subsemigroups which lack part of <r.D>.
# - <r.types>:   relevant only if S is a Rees (0-)matrix semigroup:
#                  a subset of [1 .. 6], enumerating the types to find.
#                  (see paper in preparation for a description of each type)
# - <r.zero>:    not user-facing! relevant when S is an RZMS & <r.gens> is true:
#                  (true) include all generators, which may include 0 (default).
#                  (false) remove 0 from each generating set, if present.

InstallMethod(MaximalSubsemigroups, "for a semigroup and a record",
[IsSemigroup, IsRecord],
function(S, r)
  local opts, x;

  if not IsFinite(S) then
    Info(InfoSemigroups, 1, "This method only works for finite semigroups");
    TryNextMethod();
  fi;

  opts := rec();

  # <rec.number> should be a boolean
  if IsBound(r.number) then
    if not IsBool(r.number) then
      ErrorNoReturn("the record component <number> of the optional 2nd ",
                    "argument <r> should be true or false");
    fi;
    opts.number := r.number;
  else
    opts.number := false;
  fi;

  # <rec.contain> should be a duplicate-free subset of <S>
  if IsBound(r.contain) then
    if not IsHomogeneousList(r.contain)
        or not IsDuplicateFreeList(r.contain)
        or not ForAll(r.contain, x -> x in S) then
      ErrorNoReturn("the record component <contain> of the optional 2nd ",
                    "argument <r> should be a duplicate-free list of ",
                    "elements of the semigroup in the 1st argument, <S>");
    fi;
    opts.contain := r.contain;
  else
    opts.contain := [];
  fi;

  # <rec.D> should be a D-class of <S>
  if IsBound(r.D) then
    if not IsGreensDClass(r.D) or not Parent(r.D) = S then
      ErrorNoReturn("the record component <D> of the optional 2nd ",
                    "argument <r> should be a D-class of the semigroup in ",
                    "the 1st argument, <S>");
    fi;
    opts.D := r.D;
  fi;

  # <rec.types> should be a duplicate-free subset of [1 .. 6]
  if IsBound(r.types) then
    if not (IsReesMatrixSemigroup(S) or IsReesZeroMatrixSemigroup(S))
        or not IsGroupAsSemigroup(UnderlyingSemigroup(S))
        or not IsRegularSemigroup(S) then
      Info(InfoSemigroups, 2, "the option 'types' is relevant only if <S> is ",
           "a regular Rees (0-)matrix semigroup over a group");
    else
      if not IsHomogeneousList(r.types)
          or not IsDuplicateFreeList(r.types)
          or not IsSubset([1 .. 6], r.types) then
        ErrorNoReturn("the record component <types> of the optional 2nd ",
                      "argument <r> should be a subset of [ 1 .. 6 ]");
      fi;

      if IsReesMatrixSemigroup(S) then
        # types 1, 2, and 5 are irrelevant to a RMS
        x := Intersection([1, 2, 5], r.types);
        if not IsEmpty(x) then
          Info(InfoSemigroups, 2, "a Rees matrix semigroup has no maximal ",
                                  "subsemigroups of types ", x);
        fi;
      fi;
      opts.types := r.types;
    fi;
  else
    opts.types := [1 .. 6];
  fi;

  # <rec.gens> should be <true> or <false>
  if IsBound(r.gens) then
    if not IsBool(r.gens) then
      ErrorNoReturn("the record component <gens> of the optional 2nd ",
                    "argument <r> should be true or false");
    fi;
    opts.gens := r.gens;
  else
    opts.gens := false;
  fi;

  return MaximalSubsemigroupsNC(S, opts);
end);

# MaximalSubsemigroupsNC for an RMS and a record:
#   The following method comes from Remark 1 of Graham, Graham, and Rhodes '68.
#   It only works for a finite Rees matrix semigroup over a group.
# The possible options record components are specified above.

InstallMethod(MaximalSubsemigroupsNC,
"for a Rees matrix subsemigroup and a record",
[IsReesMatrixSubsemigroup, IsRecord],
function(R, opts)
  local type, contain, mat, rows, cols, I, L, G, out, tot, lookup_rows, remove,
  i, x, n, lookup_cols, subgroups, iso, inv, R_n, G_k, iso_p, inv_p, invert, t,
  trans, gens, H, l;

  if not IsFinite(R)
      or not IsReesMatrixSemigroup(R)
      or not IsGroupAsSemigroup(UnderlyingSemigroup(R)) then
    TryNextMethod();
  fi;

  opts := ShallowCopy(opts);  # in case <opts> is immutable

  # Bind default options
  if not IsBound(opts.number) then
    opts.number := false;
  fi;
  if not IsBound(opts.gens) then
    opts.gens := false;
  fi;
  if not IsBound(opts.types) then
    opts.types := [3, 4, 6];
  fi;
  type := BlistList([1 .. 6], opts.types);
  # A RMS over a group only has one D-class, so <opts.D> is irrelevant
  if not IsBound(opts.contain) then
    contain := [];
  else
    contain := opts.contain;
  fi;

  mat := Matrix(R);
  rows := Rows(R);
  cols := Columns(R);
  I := Length(rows);
  L := Length(cols);
  G := UnderlyingSemigroup(R);

  out := [];
  tot := 0;

  # Maximal subsemigroups equal to I x G x L', where L' = L \ {l} for some l
  # These are the maximal subsemigroups of R of type (iv)
  if type[3] then
    Info(InfoSemigroups, 2, "Type 3: looking for maximal subsemigroups ",
                            "formed by discarding a column...");
    if L > 1 then
      lookup_cols := EmptyPlist(Maximum(cols));
      for i in [1 .. L] do
        lookup_cols[cols[i]] := i;
      od;
      remove := BlistList(Columns(R), Columns(R));
      i := 0;
      while i < Length(contain) and SizeBlist(remove) > 0 do
        i := i + 1;
        x := contain[i];
        remove[lookup_cols[x![3]]] := false;
      od;
      n := SizeBlist(remove);
    fi;
    if L > 1 and n > 0 then
      Info(InfoSemigroups, 2, "...found ", n, " result(s).");
      tot := tot + n;
      if not opts.number then
        Info(InfoSemigroups, 2, "...creating these maximal subsemigroups.");
        for l in ListBlist(cols, remove) do
          x := Difference(cols, [l]);
          x := ReesMatrixSubsemigroupNC(R, rows, G, x);
          if opts.gens then
            x := GeneratorsOfSemigroup(x);
          fi;
          Add(out, x);
        od;
      fi;
    else
      Info(InfoSemigroups, 2, "...found none.");
    fi;
  fi;

  # Maximal subsemigroups equal to I' x G x L, where I' = I \ {i} for some i
  # These are the maximal subsemigroups of R of type (iii)
  if type[4] then
    Info(InfoSemigroups, 2, "Type 4: looking for maximal subsemigroups ",
                            "formed by discarding a row...");
    if I > 1 then
      # A row can be removed iff it doesn't intersect <contain>.
      lookup_rows := EmptyPlist(Maximum(rows));
      for i in [1 .. I] do
        lookup_rows[rows[i]] := i;
      od;
      remove := BlistList(rows, rows);
      i := 0;
      while i < Length(contain) and SizeBlist(remove) > 0 do
        i := i + 1;
        x := contain[i];
        remove[lookup_rows[x![1]]] := false;
      od;
      n := SizeBlist(remove);
    fi;
    if I > 1 and n > 0 then
      Info(InfoSemigroups, 2, "...found ", n, " result(s).");
      tot := tot + n;
      if not opts.number then
        Info(InfoSemigroups, 2, "...creating these maximal subsemigroups.");
        for i in ListBlist(rows, remove) do
          x := Difference(rows, [i]);
          x := ReesMatrixSubsemigroupNC(R, x, G, cols);
          if opts.gens then
            x := GeneratorsOfSemigroup(x);
          fi;
          Add(out, x);
        od;
      fi;
    else
      Info(InfoSemigroups, 2, "...found none.");
    fi;
  fi;

  # Maximal subsemigroups isomorphic to I x H x L, where H < G is maximal.
  # These are the maximal subsemigroups of R of type (vi)
  if type[6] then
    Info(InfoSemigroups, 2, "Type 6: looking for maximal subsemigroups ",
                            "arising from maximal subgroups...");
    subgroups := [];
    if not IsTrivial(G) then
      iso := RMSNormalization(R);              # The normalization of R
      inv := InverseGeneralMapping(iso);       # The normalization inverse
      R_n := Range(iso);                       # R (normalized)
      G_k := ShallowCopy(MatrixEntries(R_n));  # Gens of the idempotent group

      if IsGroup(G) then
        iso_p := IdentityMapping(G);
        inv_p := iso_p;
        invert := InverseOp;
      else  # We need to use methods that apply only to IsGroup, e.g. Normalizer
        iso_p := IsomorphismPermGroup(G);
        inv_p := InverseGeneralMapping(iso_p);
        invert := x -> ((x ^ iso_p) ^ -1) ^ inv_p;
        G := Range(iso_p);
        G_k := List(G_k, x -> x ^ iso_p);
      fi;

      i := 0;
      x := Group(G_k);
      while i < Length(contain) and Size(x) < Size(G) do
        i := i + 1;
        t := UnderlyingElementOfReesMatrixSemigroupElement(contain[i] ^ iso);
        t := t ^ iso_p;
        x := ClosureGroup(x, t);
        AddSet(G_k, t);
      od;

      if Size(x) < Size(G) then  # Otherwise there are no results
        for H in MaximalSubgroupClassReps(G) do
          trans := RightTransversal(G, Normalizer(G, H));
          for t in trans do
            if ForAll(G_k, x -> x ^ (t ^ -1) in H) then
              # A maximal subsemigroup has been found arising from H ^ t
              Add(subgroups, [H, t]);
            fi;
          od;
        od;
      fi;
    fi;
    if Length(subgroups) > 0 then
      Info(InfoSemigroups, 2, "...found ", Length(subgroups), " result(s).");
      tot := tot + Length(subgroups);
      if not opts.number then
        Info(InfoSemigroups, 2, "...creating these maximal subsemigroups.");
        gens := [];
        # These 3 loops ensure a small generating set
        for i in [2 .. Minimum(I, L)] do
          Add(gens, RMSElement(R, rows[i], invert(mat[i][i]), cols[i]));
        od;
        for i in [L + 1 .. I] do
          Add(gens, RMSElement(R, rows[i], invert(mat[1][i]), cols[1]));
        od;
        for l in [I + 1 .. L] do
          Add(gens, RMSElement(R, rows[1], invert(mat[l][1]), cols[l]));
        od;
        for x in subgroups do
          H := x[1];
          t := x[2];
          x := List(GeneratorsOfSemigroup(H), x ->
                    RMSElement(R_n, 1, (x ^ t) ^ inv_p, 1) ^ inv);
          Append(x, gens);
          if not opts.gens then
            x := Semigroup(x);
          fi;
          Add(out, x);
        od;
      fi;
    else
      Info(InfoSemigroups, 2, "...found none.");
    fi;
  fi;

  if opts.number then
    return tot;
  fi;
  return out;
end);

# MaximalSubsemigroupsNC for an RZMS and a record:
#   The following method comes from Remark 1 in Graham, Graham, and Rhodes.
#   It only works for a regular Rees 0-matrix semigroup over a group
# The required record components are given above.

InstallMethod(MaximalSubsemigroupsNC,
"for a Rees 0-matrix subsemigroup and a record",
[IsReesZeroMatrixSubsemigroup, IsRecord],
function(R, opts)
  local zero, x, type, contain, mat, rows, cols, I, L, G, lookup_cols,
  lookup_rows, out, tot, dig, pos, remove, i, n, nbs, deg, l, r, dig_contain,
  count, rectangles, bicomp_I, bicomp_L, b, invert, gens, one, reps, i1, l1, a,
  H1, l2, i2, H2, rows_in, rows_out, cols_in, cols_out, failed, iso, inv, R_n,
  iso_p, inv_p, ccs, comp_row, comp_col, comp, con, sup, m, lim, P, max, q,
  same_coset, results, V, normal, trans, len, T, success, visited, conjugator,
  queue, u, candidate, idems, recursion, genset, k, j, t, v, choice, gen;

  if not IsFinite(R)
      or not IsReesZeroMatrixSemigroup(R)
      or not IsGroupAsSemigroup(UnderlyingSemigroup(R))
      or not IsRegularSemigroup(R) then
    TryNextMethod();
  fi;

  zero := MultiplicativeZero(R);

  opts := ShallowCopy(opts);  # in case <opts> is immutable

  # Bind default options
  if not IsBound(opts.number) then
    opts.number := false;
  fi;
  if not IsBound(opts.gens) then
    opts.gens := false;
  fi;
  if not IsBound(opts.types) then
    opts.types := [1 .. 6];
  fi;
  if not IsBound(opts.zero) then
    opts.zero := true;
  fi;
  if IsBound(opts.D) then
    # A regular RZMS over a group has two D-classes: {0} and R\{0}.
    x := Representative(opts.D);
    if x = zero then
      opts.types := Intersection(opts.types, [2]);
    else
      opts.types := Intersection(opts.types, [1, 3, 4, 5, 6]);
    fi;
  fi;
  type := BlistList([1 .. 6], opts.types);
  if not IsBound(opts.contain) then
    contain := [];
  else
    contain := opts.contain;
  fi;

  mat := Matrix(R);
  rows := Rows(R);
  cols := Columns(R);
  I := Length(rows);
  L := Length(cols);
  G := UnderlyingSemigroup(R);

  lookup_cols := EmptyPlist(Maximum(cols));
  for i in [1 .. L] do
    lookup_cols[cols[i]] := i;
  od;
  lookup_rows := EmptyPlist(Maximum(rows));
  for i in [1 .. I] do
    lookup_rows[rows[i]] := i;
  od;

  out := [];
  tot := 0;

  # Type 1: The maximal subsemigroup {0}.

  # {0} is a maximal subsemigroup if and only if R is the 2-element semilattice
  if type[1] then
    Info(InfoSemigroups, 2, "Type 1: looking for a maximal subsemigroup {0}",
                            "...");
    if I = 1 and L = 1 and IsTrivial(G) and IsSubset([zero], contain) then
      tot := tot + 1;
      if not opts.number then
        if opts.gens then
          if opts.zero then
            Add(out, [zero]);
          else
            Add(out, []);
          fi;
        else
          Add(out, Semigroup(zero));
        fi;
      fi;
      Info(InfoSemigroups, 2, "...found one result.");
    else
      Info(InfoSemigroups, 2, "...found none.");
    fi;
  fi;

  # Type 2: The maximal subsemigroup R \ {0}.

  # R \ {0} is a maximal subsemigroup...
  #         if and only if <mat> contains no 0 entry
  #         if and only if the Graham-Houghton bipartite graph is complete
  if type[2] then
    Info(InfoSemigroups, 2, "Type 2: looking for a maximal subsemigroup ",
                            "formed by discarding 0...");
    dig := RZMSDigraph(R);
    if IsCompleteBipartiteDigraph(dig) and not zero in contain then
      tot := tot + 1;
      if not opts.number then
        if opts.gens then
          x := ShallowCopy(GeneratorsOfSemigroup(R));
          # The 0 of <R> is necessarily contained in x
          pos := Position(x, zero);
          Remove(x, pos);
        else
          x := ReesZeroMatrixSubsemigroupNC(R, rows, G, cols);
        fi;
        Add(out, x);
      fi;
      Info(InfoSemigroups, 2, "...found one result.");
    else
      Info(InfoSemigroups, 2, "...found none.");
    fi;
  fi;

  # All other maximal subsemigroups contain 0, so remove it from <contain>.
  pos := Position(contain, zero);
  if pos <> fail then
    Remove(contain, pos);
  fi;

  # Type 3: Maximal subsemigroups I x G x L' + {0} where L' = L \ {l} for some l

  # In the Graham-Houghton bipartite graph, we can remove any vertex <l> in <L>
  # which is not adjacent to a vertex <i> in <I> which is only adjacent to <l>.
  # So, we run through the vertices <i> of <I> and find the ones of degree 1,
  # and we remember the vertices <l> adjacent to such <i>.
  if type[3] then
    Info(InfoSemigroups, 2, "Type 3: looking for ",
         "maximal subsemigroups formed by discarding a column...");

    if L > 1 then
      remove := BlistList(cols, cols);
      i := 0;
      while i < Length(contain) and SizeBlist(remove) > 0 do
        i := i + 1;
        x := contain[i];
        remove[lookup_cols[x![3]]] := false;
      od;
      n := SizeBlist(remove);
    fi;

    if L > 1 and SizeBlist(remove) > 0 then
      dig := RZMSDigraph(R);
      nbs := OutNeighbours(dig);
      deg := OutDegrees(dig);
      i := 0;
      while i < I and SizeBlist(remove) > 0 do
        i := i + 1;
        if deg[i] = 1 then
          remove[nbs[i][1] - I] := false;
        fi;
      od;
      n := SizeBlist(remove);
    fi;

    if L > 1 and n > 0 then
      Info(InfoSemigroups, 2, "...found ", n, " result(s).");
      tot := tot + n;
      if not opts.number then
        Info(InfoSemigroups, 2, "creating these maximal subsemigroups.");
        # Code to produce smaller generating set:
        #   Check whether removing any particular col leaves a matrix without 0.
        #   In the case that this happens, 0 must sometimes be included as a gen
        x := 0;
        l := 0;
        r := 0;
        while x < 2 and l < L do
          l := l + 1;
          if deg[l + I] < I then
            x := x + 1;
            r := cols[l];  # Col <l> corresponds to a row of <mat> with 0's
          fi;
        od;
        if x >= 2 then
          r := infinity;  # At least 2 cols correspond to rows of <mat> with 0s
        fi;

        # Remove each possible col in turn...
        for l in ListBlist(cols, remove) do
          x := Difference(cols, [l]);
          x := ReesZeroMatrixSubsemigroupNC(R, rows, G, x);
          if opts.gens then
            x := ShallowCopy(GeneratorsOfSemigroup(x));
            if opts.zero and (r = 0 or r = l) then  # 0 is necessarily a gen.
              Add(x, zero);
            fi;
          else
            if r = 0 or r = l then
              x := Semigroup(x, zero);
            fi;
            SetIsReesZeroMatrixSemigroup(x, true);
          fi;
          Add(out, x);
        od;
      fi;
    else
      Info(InfoSemigroups, 2, "...found none.");
    fi;
  fi;

  # Type 4: Maximal subsemigroups I' x G x L + {0} where I' = I \ {i} for some i

  # In the Graham-Houghton bipartite graph, we can remove any vertex <i> in <I>
  # which is not adjacent to a vertex <l> in <L> which is only adjacent to <i>.
  # So, we run through the vertices <l> of <L> and find the ones of degree 1,
  # and we remember the vertices <i> adjacent to such <l>.
  if type[4] then
    Info(InfoSemigroups, 2, "Type 4: looking for maximal subsemigroups ",
                             "formed by discarding a row...");
    if I > 1 then
      remove := BlistList(rows, rows);
      i := 0;
      while i < Length(contain) and SizeBlist(remove) > 0 do
        i := i + 1;
        x := contain[i];
        remove[lookup_rows[x![1]]] := false;
      od;
      n := SizeBlist(remove);
    fi;

    if I > 1 and SizeBlist(remove) > 0 then
      dig := RZMSDigraph(R);
      nbs := OutNeighbours(dig);
      deg := OutDegrees(dig);
      l := I;
      while l < L + I and SizeBlist(remove) > 0 do
        l := l + 1;
        if deg[l] = 1 then
          remove[nbs[l][1]] := false;
        fi;
      od;
      n := SizeBlist(remove);
    fi;

    if I > 1 and n > 0 then
      Info(InfoSemigroups, 2, "...found ", n, " result(s).");
      tot := tot + n;
      if not opts.number then
        Info(InfoSemigroups, 2, "creating these maximal subsemigroups.");
        # Code to produce smaller generating set:
        #   Check whether removing any particular row leaves a matrix without 0.
        #   In the case that this happens, 0 must sometimes be included as a gen
        x := 0;
        i := 0;
        r := 0;
        while x < 2 and i < I do
          i := i + 1;
          if deg[i] < L then
            x := x + 1;
            r := rows[i];  # Row <i> corresponds to a column of <mat> with 0's
          fi;
        od;
        if x >= 2 then  # At least 2 rows correspond to cols of <mat> with 0's
          r := infinity;
        fi;

        # Remove each possible row in turn...
        for i in ListBlist(rows, remove) do
          x := Difference(rows, [i]);
          x := ReesZeroMatrixSubsemigroupNC(R, x, G, cols);
          if opts.gens then
            x := ShallowCopy(GeneratorsOfSemigroup(x));
            if opts.zero and (r = 0 or r = i) then  # 0 must be a gen.
              Add(x, zero);
            fi;
          else
            if r = 0 or r = i then
              x := Semigroup(x, zero);
            fi;
            SetIsReesZeroMatrixSemigroup(x, true);
          fi;
          Add(out, x);
        od;
      fi;
    else
      Info(InfoSemigroups, 2, "...found none.");
    fi;
  fi;

  # Type 5: Maximal subsemigroups arising from maximal rectangles of zeroes
  if type[5] then
    Info(InfoSemigroups, 2, "Type 5: looking for maximal subsemigroups ",
                            "arising from maximal rectangles...");
    dig := RZMSDigraph(R);

    # Create digraph <dig_contain> to remember which H-classes meet <contain>
    # Every such H-class must be contained in maximal subsemigroup of type 5.
    # Hence either the R- or L-class of every such H-class must be contained.
    dig_contain := List([1 .. I + L], x -> BlistList([1 .. I + L], []));
    for x in contain do
      dig_contain[lookup_rows[x![1]]][lookup_cols[x![3]] + I] := true;
      dig_contain[lookup_cols[x![3]] + I][lookup_rows[x![1]]] := true;
    od;
    dig_contain := DigraphByAdjacencyMatrix(dig_contain);
    SetDigraphBicomponents(dig_contain, [[1 .. I], [I + 1 .. I + L]]);

    count := 0;
    if not IsCompleteBipartiteDigraph(dig)
        and not IsCompleteBipartiteDigraph(dig_contain) then
      Info(InfoSemigroups, 2, "...calculating maximal independent sets of the ",
                              "Graham-Houghton graph...");
      rectangles := DigraphMaximalIndependentSets(dig);
      n := Length(rectangles) - 2;
      Info(InfoSemigroups, 2, "...found ", n, " maximal independent set(s).");
      if n > 0 then
        bicomp_I := BlistList([1 .. I + L], [1 .. I]);
        bicomp_L := BlistList([1 .. I + L], [I + 1 .. I + L]);
        rectangles := [];
        for r in DigraphMaximalIndependentSets(dig) do
          b := BlistList([1 .. I + L], r);

          # Check that <r> is not one of the bicomponents of <dig>
          if b = bicomp_I or b = bicomp_L then
            continue;
          fi;

          # Check that maximal subsemigroup defined by <r> contains <contain>
          if not ForAll(DigraphEdges(dig_contain), y -> b[y[1]] or b[y[2]]) then
            continue;
          fi;

          # <r> defines a maximal subsemigroup of <R>
          count := count + 1;
          if not opts.number then
            Add(rectangles, [r, b]);
          fi;
        od;
      fi;
    fi;

    if count > 0 then
      Info(InfoSemigroups, 2, "...found ", count, " result(s).");
      tot := tot + count;
      if not opts.number then
        Info(InfoSemigroups, 2, "...creating these maximal subsemigroups.");

        # We need to be able to invert elements of <G> easily
        if IsGroup(G) then
          invert := InverseOp;
        else
          invert := x -> InversesOfSemigroupElementNC(G, x)[1];
        fi;

        gens := GeneratorsOfSemigroup(G);
        one := MultiplicativeNeutralElement(G);
        nbs := BooleanAdjacencyMatrix(dig);

        # Pre-process the H-class representatives of R
        reps := List(rows, i -> List(cols,
                                        l -> RMSElement(R, i, one, l)));

        for x in rectangles do
          r := x[1];  # the maximal independent set
          b := x[2];  # a blist representing the maximal independent set

          # Add generators for two group H-classes (in appropriate locations)
          i1 := First(r, x -> x <= I);
          l1 := First([1 .. L],
                      x -> not b[x + I] and not mat[cols[x]][rows[i1]] = 0);

          a := invert(mat[cols[l1]][rows[i1]]);
          H1 := List(gens, x -> RMSElement(R, rows[i1], x * a, cols[l1]));

          l2 := First(r, x -> x > I) - I;
          i2 := First([1 .. I],
                      x -> not b[x] and not mat[cols[l2]][rows[x]] = 0);

          a := invert(mat[cols[l2]][rows[i2]]);
          H2 := List(gens, x -> RMSElement(R, rows[i2], x * a, cols[l2]));

          x := Concatenation(H1, H2);

          # Add a generator to every row
          for i in Difference([1 .. I], [i1, i2]) do
            if b[i] then  # <b> needs to be indexed differently
              Add(x, reps[i][l1]);
            else
              Add(x, reps[i][l2]);
            fi;
          od;

          # Add a generator for every column
          for l in Difference([1 .. L], [l1, l2]) do
            if b[l + I] then
              Add(x, reps[i2][l]);
            else
              Add(x, reps[i1][l]);
            fi;
          od;

          # If necessary, add generators for the maximal rectangle itself.
          rows_in := [];
          rows_out := [];
          cols_in := [];
          cols_out := [];
          for i in [1 .. I] do
            if b[i] then
              Add(rows_in, i);
            else
              Add(rows_out, i);
            fi;
          od;
          for i in [I + 1 .. I + L] do
            if b[i] then
              Add(cols_in, i - I);
            else
              Add(cols_out, i);
            fi;
          od;

          if not ForAny(rows_out, i -> ForAny(cols_out, l -> nbs[i][l])) then
            n := Minimum(Length(rows_in), Length(cols_in));
            for i in [1 .. n] do
              Add(x, reps[rows_in[i]][cols_in[i]]);
            od;
            for i in [n + 1 .. Length(rows_in)] do
              Add(x, reps[rows_in[i]][cols_in[1]]);
            od;
            for i in [n + 1 .. Length(cols_in)] do
              Add(x, reps[rows_in[1]][cols_in[i]]);
            od;
          fi;

          if not opts.gens then
            x := Semigroup(x);  # <new> is guaranteed to generate the <0>.
          fi;
          Add(out, x);
        od;
      fi;
    else
      Info(InfoSemigroups, 2, "...found no maximal subsemigroups of type 5.");
    fi;
  fi;

  # Type 6: Maximal subsemigps isomorphic to I x H x L + {0}, H < G maximal
  if type[6] then
    Info(InfoSemigroups, 2, "Type 6: looking for maximal subsemigroups ",
                            "arising from maximal subgroups...");
    count := 0;
    failed := false;
    if IsTrivial(G) then
      failed := true;
    fi;

    if not failed then
      iso := RZMSNormalization(R);        # The normalization of R
      inv := InverseGeneralMapping(iso);  # The normalization inverse
      R_n := Range(iso);                  # R (normalized)
      mat := Matrix(R_n);                 # Normalized matrix

      if IsGroup(G) then
        iso_p := IdentityMapping(G);
        inv_p := iso_p;
      else  # We need to use methods that apply only to IsGroup, e.g. Normalizer
        iso_p := IsomorphismPermGroup(G);
        inv_p := InverseGeneralMapping(iso_p);
        G := Range(iso_p);
      fi;

      one := Identity(G);

      # Get the connected components of R_n
      ccs := RZMSConnectedComponents(R_n);
      n := Length(ccs);
      Info(InfoSemigroups, 2, "...the Graham-Houghton graph has ", n, " ",
                              "connected component(s).");
      I := EmptyPlist(n);  # I[k] = list of rows in the k^th connected component
      L := EmptyPlist(n);  # L[k] = list of cols in the k^th connected component

      # row -> connected component
      comp_row := EmptyPlist(Length(Rows(R_n)));
      # col -> connected component
      comp_col := EmptyPlist(Length(Columns(R_n)));
      for k in [1 .. n] do
        comp := ccs[k][1];
        I[k] := comp[1];
        for j in comp do
          comp_row[j] := k;
        od;
        comp := ccs[k][2];
        L[k] := comp[1];
        for j in comp do
          comp_col[j] := k;
        od;
      od;

      # Sort the elements of <contain> into their 'blocks'.
      # By doing so, we find the necessary relationships between components.
      # We call a class of related connected components 'super components'

      # <con[k][l]> lists those group elts which must be contained in I_k x L_l
      con := List([1 .. n], x -> List([1 .. n], y -> []));

      # Adjacency mat for a <dig> which defines relationship between components.
      # {k,l} is an edge of <dig> iff <con[k][l]> or <con[l][k]> is non-empty.
      nbs := List([1 .. n], x -> BlistList([1 .. n], []));

      contain := List(contain, x -> x ^ iso);
      for x in contain do
        a := comp_row[x![1]];
        b := comp_col[x![3]];
        if a <= b then
          AddSet(con[a][b], x![2] ^ iso_p);
        else
          AddSet(con[b][a], (x![2] ^ iso_p) ^ -1);
        fi;
        nbs[a][b] := true;
        nbs[b][a] := true;
      od;
      # There is roughly 1 'degree of freedom' per connected component of <dig>
      dig := DigraphByAdjacencyMatrix(nbs);  # <dig> is graph defined by <nbs>
      sup := DigraphConnectedComponents(dig).comps;
      m := Length(sup);

      # <lim> = size of the largest set <con[k][l]>.
      lim := Maximum(List(con, x -> Maximum(List(x, Length))));

      # Prepare the gens of the idempotent generated group, P[k], for each k.
      P := EmptyPlist(n);
      for k in [1 .. n] do
        # <P[k]> consists on non-zero matrix entries in cc <k>; and <con[k][k]>
        P[k] := Concatenation(mat{ccs[k][2]}{ccs[k][1]});
        P[k] := Unique(Concatenation(P[k], con[k][k]));
        pos := Position(P[k], 0);
        if pos <> fail then
          Remove(P[k], pos);
        fi;
        P[k] := List(P[k], x -> x ^ iso_p);
        if Size(Group(P[k])) = Size(G) then  # If <P[k]> generates <G>
          failed := true;
          break;
        fi;
        pos := Position(P[k], one);
        if pos <> fail then
          Remove(P[k], pos);  # remove the identity from the generating set
        fi;
      od;
    fi;

    if not failed then
      max := MaximalSubgroupClassReps(G);
      q := Length(max);
      Info(InfoSemigroups, 2, "...there are ", q, " maximal subgroup(s) of ",
                              "the underlying group, up to conjugacy.");

      # Technical function to respect the restrictions imposed by <con>:
      # For each block {k,l} (with l > pos), do the elements of <con[k][l]>
      # all define the same right coset of V ^ t?
      same_coset := function(t, k, pos)
        local block, l, x;
        for l in [pos + 1 .. Length(sup[k])] do
          block := con[sup[k][pos]][sup[k][l]];
          for x in [1 .. Length(block) - 1] do
            if not (block[x] * (block[x + 1] ^ -1)) ^ (t ^ -1) in V then
              return false;
            fi;
          od;
        od;
        return true;
      end;

      # Max subsemigroup arising from <max[i]> <--> Transversal of <results[i]>
      results := List([1 .. q], x -> List([1 .. m], x -> []));

      for i in [1 .. q] do
        V := max[i];

        if Size(V) < lim then
          continue;  # <V> is not big enough to contain every set <con[k][l]>
        fi;

        normal := IsNormal(G, V);
        trans := Elements(RightTransversal(G, V));

        # Check that the 1st cc of each super-comp <j> contains group <P[j]>
        for j in [1 .. m] do
          comp := sup[j];
          r := comp[1];  # <r> is the least cc of the super-component
          len := Length(comp);

          Assert(0, j <> 1 or r = 1, "1st elt of 1st super-comp must be 1");

          T := [];
          if normal and IsSubset(V, P[r]) then
            # if <V> is normal then P[r] < V ^ g iff P[r] < V so only check once
            if r = 1 then
              # Special case for the 1st connected component if V is normal
              # For 1st cc we take a transversal of the normalizer of <V> in <G>
              # This transversal is [one] if <V> is normal, else it is <trans>
              T := [one];
            else
              T := trans;
            fi;
          elif not normal then
            # if <V> is not normal we must check P[r] < V ^ g separately
            T := Filtered(trans, g -> ForAll(P[r], x -> g * x * g ^ -1 in V));
          fi;

          # Only allow V ^ g if that choice satisfies <con>.
          T := Filtered(T, g -> same_coset(g, j, 1));

          if IsEmpty(T) then
            break;
          elif len = 1 then
            results[i][j] := List(T, x -> [x]);
            continue;
          fi;

          # Given a conjugator V ^ t for one cc in the super-component,
          # there is no freedom in choosing the conjugators for the other cc's
          # in the super-component. Given t, determine these conjugators.
          for t in T do
            success := true;
            visited := BlistList([1 .. n], []);
            conjugator := EmptyPlist(n);
            conjugator[r] := t;
            queue := [r];

            while success and not IsEmpty(queue) do
              u := Remove(queue, 1);
              visited[u] := true;
              for v in ListBlist([1 .. n], DifferenceBlist(nbs[u], visited)) do
                Add(queue, v);
                # the coset defined in block [u][v] determines what g_v must be
                if u < v then
                  candidate := conjugator[u] * con[u][v][1];
                else
                  candidate := conjugator[u] * con[v][u][1] ^ -1;
                fi;

                # define the conjugator for cc <v> if it is not yet defined
                if not IsBound(conjugator[v]) then
                  conjugator[v] := candidate;

                  # 1. check that g_v is such that P[v] < V ^ g_v
                  # 2. check that g_v satisfies <con>
                  if not ForAll(P[v], g -> g ^ (conjugator[v] ^ -1) in V)
                      or not same_coset(candidate, j, Position(comp, v)) then
                    success := false;
                    break;
                  fi;
                elif not candidate * conjugator[v] ^ -1 in V then
                  success := false;
                  break;
                fi;
              od;
            od;

            if success then
              Add(results[i][j], List(comp, x -> conjugator[x]));
            fi;
          od;

          if IsEmpty(results[i][j]) then
            break;  # No results arise from <V[i]> because of super-comp <j>.
          fi;
        od;
        count := count + Product(List(results[i], Length));
      od;
      failed := count = 0;
    fi;

    if failed then
      Info(InfoSemigroups, 2, "...found none.");
    else
      Info(InfoSemigroups, 2, "...found ", count, " result(s).");
      tot := tot + count;
    fi;

    if not failed and not opts.number then
      Info(InfoSemigroups, 2, "...creating these maximal subsemigroups.");

      idems := GeneratorsOfSemigroup(IdempotentGeneratedSubsemigroup(R));
      idems := ShallowCopy(idems);
      if not opts.zero or not IsCompleteBipartiteDigraph(RZMSDigraph(R_n)) then
        x := Position(idems, zero);
        if x <> fail then
          Remove(idems, x);
        fi;
      fi;

      # Max subsemigroup arising from <max[i]> <--> Transversal of <results[i]>

      # Recursive function to return a transversal/extend a partial transversal
      recursion := function(x, depth)
        local new, choice, j;

        if depth = m then
          if not opts.gens then
            x := Semigroup(x);
          fi;
          Add(out, x);
          return;
        fi;

        depth := depth + 1;
        for choice in results[i][depth] do
          new := ShallowCopy(x);
          for j in [1 .. Length(choice)] do
            Add(new, RMSElement(R_n, I[1],
                                     (conjugator ^ -1 * choice[j]) ^ inv_p,
                                     L[sup[depth][j]]) ^ inv);
            Add(new, RMSElement(R_n, I[sup[depth][j]],
                                     (choice[j] ^ -1 * conjugator) ^ inv_p,
                                     L[1]) ^ inv);
          od;
          recursion(new, depth);
        od;
      end;

      # Function to create the initial partial transversals
      for i in [1 .. q] do
        if Product(List(results[i], Length)) = 0 then
          continue;
        fi;
        V := max[i];
        for choice in results[i][1] do
          genset := ShallowCopy(idems);
          conjugator := choice[1];
          for gen in GeneratorsOfGroup(V) do
            Add(genset,
                RMSElement(R_n, I[1], (gen ^ conjugator) ^ inv_p, L[1]) ^ inv);
          od;
          for j in [2 .. Length(choice)] do
            Add(genset, RMSElement(R_n, I[1],
                                        (conjugator ^ -1 * choice[j]) ^ inv_p,
                                        L[sup[1][j]]) ^ inv);
            Add(genset, RMSElement(R_n, I[sup[1][j]],
                                        (choice[j] ^ -1 * conjugator) ^ inv_p,
                                        L[1]) ^ inv);
          od;
          recursion(genset, 1);
        od;
      od;
    fi;
  fi;

  if opts.number then
    return tot;
  fi;
  return out;
end);

InstallMethod(MaximalSubsemigroupsNC,
"for a semigroup and a record",
[IsSemigroup, IsRecord],
function(S, opts)
  local tot, out, try, gen, D, class_gen, reps, po, below, above, vertex_class,
  x, create_ideal, contain, ideal, inj, inv, R, M, num, num_start, gen_class,
  above_gen, above_semigroup, outside_gens, L, LL, RR, m, n, gamma_L,
  gamma_R, y, comp_L, comp_R, red_L, red_R, k, gamma, comp, delta,
  label, nredges, delta_prime, l, r, forbidden_L, forbidden_R, rectangles, min,
  bicomp_L, bicomp_R, b, source_L, source_R, L_in, L_out, R_in, R_out,
  L_out_source, R_out_source, L_in_source, R_in_source, found, v, u, H, remove,
  sources, must, e, z, i, j;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  opts := ShallowCopy(opts);  # in case <opts> is immutable

  # Bind default options
  if not IsBound(opts.number) then
    opts.number := false;
  fi;
  if not IsBound(opts.gens) then
    opts.gens := false;
  fi;
  if IsBound(opts.contain) then
    opts.contain := ShallowCopy(opts.contain);  # assume duplicate-free
  else
    opts.contain := [];
  fi;

  tot := 0;
  out := [];
  try := [];

  if not IsTrivial(S) then
    Info(InfoSemigroups, 1, "enumerating the D-classes...");
    gen := GeneratorsOfSemigroup(S);
    D := GreensDClasses(S);
    # <gen[i]> is in the D-class index by <class_gen[i]>
    class_gen := List(gen, x -> PositionProperty(D, d -> x in d));
    try := Unique(class_gen);
    reps := DClassReps(S);

    # Prepare the digraphs which relate D-classes, from the partial order
    # <below>: i->j is an edge if and only if <D[j]> is immediately below <D[i]>
    #          This digraph captures the D-class partial order.
    # <above>: i->j is an edge if <D[label[j]]> is above <D[label[i]]> in p-o.
    #          captures which generators are above which other generators
    Info(InfoSemigroups, 1, "computing the partial order of D-classes...");
    po := PartialOrderOfDClasses(S);
    Info(InfoSemigroups, 1, "processing the partial order of D-classes...");

    po := DigraphMutableCopy(po);
    DigraphRemoveLoops(po);
    MakeImmutable(po);
    SetIsAcyclicDigraph(po, true);

    below := DigraphReflexiveTransitiveReduction(po);

    above := DigraphReverse(DigraphMutableCopy(below));
    DigraphRemoveLoops(DigraphTransitiveClosure(above));
    ClearDigraphVertexLabels(above);
    InducedSubdigraph(above, try);
    MakeImmutable(above);
    SetIsAcyclicDigraph(above, true);

    # <vertex_class[i]> is the vertex number of <above> corresponding to <D[i]>
    # <DigraphVertexLabel(above, vertex_class[i])> is the list of gens in <D[i]>
    vertex_class := EmptyPlist(Maximum(try));
    for i in DigraphVertices(above) do
      vertex_class[DigraphVertexLabel(above, i)] := i;
    od;
    SetDigraphVertexLabels(above, List(DigraphVertices(above), y -> []));
    for i in [1 .. Length(gen)] do
      Add(above!.vertexlabels[vertex_class[class_gen[i]]], gen[i]);
    od;

    if IsBound(opts.D) then
      x := try;
      try := [];
      for i in x do
        if reps[i] in opts.D then
          try := [i];
          break;
        fi;
      od;
    fi;
  fi;

  # <create_ideal()> finds a generating set for the ideal consisting of
  # those elements which are strictly less than <D[i]> in the usual D-order.
  create_ideal := function()
    if not IsBound(ideal) then
      ideal := reps{OutNeighboursOfVertexNC(below, i)};
      if not IsEmpty(ideal) then
        Info(InfoSemigroups, 1,
             "...computing a generating set for the ideal below D[", i, "]...");
        ideal := GeneratorsOfSemigroup(SemigroupIdeal(S, ideal));
      fi;
    fi;
  end;

  # TODO(later) sort <try> optimally such that any D-classes which have the
  # same ideal directly below occur together, so that that ideal needs to be
  # computed only once. And then check to actually see if it is necessary to
  # re-compute ideal Can possibly use <below> to help do this?

  for i in try do  # The D-classes to contain are those indexed by <try>
    Unbind(ideal);

    ############################################################################
    ############################################################################
    # Code for maximal D-classes
    if OutDegreeOfVertex(above, vertex_class[i]) = 0 then
      Info(InfoSemigroups, 1, "## considering maximal D-class D[", i, "]...");

      if not IsEmpty(opts.contain) then
        contain := Intersection(D[i], opts.contain);
        opts.contain := Difference(opts.contain, contain);
        if Length(contain) = Size(D[i]) then
          Info(InfoSemigroups, 1, "found no maximal subsemigroups.");
          continue;
        fi;
      else
        contain := [];
      fi;

      if not opts.number then
        ideal := gen{Filtered([1 .. Length(gen)], x -> class_gen[x] <> i)};
        ideal := Concatenation(reps{OutNeighboursOfVertexNC(below, i)}, ideal);
        if not IsEmpty(ideal) then
          Info(InfoSemigroups, 1, "...computing the ideal below D[", i, "]...");
          ideal := SemigroupIdeal(S, ideal);
          if not IsTrivial(D[i]) or opts.gens then
            Info(InfoSemigroups, 1, "...computing a generating set...");
            ideal := GeneratorsOfSemigroup(ideal);
          fi;
        fi;
      fi;

      if IsTrivial(D[i]) then  # trivial D-classes include all non-regular ones.
        tot := tot + 1;
        if not opts.number then
          Add(out, ideal);  # <ideal> is already in the correct form
        fi;
        Info(InfoSemigroups, 1, "* found 1 maximal subsemigroup arising from ",
                                "D[", i, "], formed by removing it.");
        continue;
      fi;

      inj := InjectionPrincipalFactor(D[i]);
      inv := InverseGeneralMapping(inj);
      R := Range(inj);
      Info(InfoSemigroups, 1, "...calculating maximal subsemigroups of the ",
                              "principal factor...");
      M := MaximalSubsemigroupsNC(R, rec(types := [3, 4, 5, 6],
                                         number := opts.number,
                                         zero := false,
                                         gens := true,
                                         contain := List(contain,
                                                         x -> x ^ inj)));
      if opts.number then
        num := M;
      else
        num := Length(M);
      fi;
      tot := tot + num;

      Info(InfoSemigroups, 1, "* found ", num, " maximal subsemigroup(s) ",
                              "arising from D[", i, "].");
      if not opts.number then
        Info(InfoSemigroups, 1, "...creating these maximal subsemigroups...");
        for R in M do
          x := Concatenation(ideal, OnTuples(R, inv));
          if not opts.gens then
            x := Semigroup(x);
          fi;
          Add(out, x);
        od;
      fi;
      continue;
    fi;

    ############################################################################
    ############################################################################
    # Code for non-maximal D-classes
    Info(InfoSemigroups, 1, "## considering non-maximal D-class D[", i, "]...");
    num_start := tot;
    gen_class := DigraphVertexLabel(above, vertex_class[i]);
    above_gen := OutNeighbours(above)[vertex_class[i]];
    above_gen := List(above_gen, x -> DigraphVertexLabel(above, x));
    above_gen := Concatenation(above_gen);
    above_semigroup := Semigroup(above_gen);

    # Work out whether <D[i]> is generated by the generators above
    # - For a reg D-class, check if every generator in <D[i]> is so generated.
    # - For a non-reg D-class, if any element is so generated, then all are.
    if (IsRegularDClass(D[i]) and ForAll(gen_class, x -> x in above_semigroup))
        or (not IsRegularDClass(D[i]) and gen_class[1] in above_semigroup) then
      Info(InfoSemigroups, 1, "found no maximal subsemigroups.");
      continue;
    fi;

    # Work out which elements of <D[i]> are required to be in any result.
    if not IsEmpty(opts.contain) then
      contain := Intersection(D[i], opts.contain);
      opts.contain := Difference(opts.contain, contain);
      if Length(contain) = Size(D[i]) then
        Info(InfoSemigroups, 1, "found no maximal subsemigroups.");
        continue;
      fi;
    else
      contain := [];
    fi;

    if not opts.number then
      outside_gens := gen{Filtered([1 .. Length(gen)], x -> class_gen[x] <> i)};
    fi;

    ############################################################################
    # Non-regular (or trivial) non-maximal D-classes
    if not IsRegularDClass(D[i]) or IsTrivial(D[i]) then
      if not IsEmpty(contain) then
        Info(InfoSemigroups, 1, "found no maximal subsemigroups.");
        continue;
      fi;
      tot := tot + 1;
      Info(InfoSemigroups, 1, "* found 1 maximal subsemigroup by removing ",
                              "the non-regular/trivial D[", i, "].");
      if not opts.number then
        create_ideal();
        x := Concatenation(ideal, outside_gens);
        if not opts.gens then
          x := Semigroup(x);
        fi;
        Add(out, x);
      fi;
      continue;
    fi;

    ############################################################################
    # Regular and non-trivial non-maximal D-class
    Info(InfoSemigroups, 1, "...D[", i, "] is regular and non-trivial...");

    x := Representative(GroupHClass(D[i]));
    # L/R-class reps
    L := HClassReps(GreensRClassOfElement(D[i], x));
    R := HClassReps(GreensLClassOfElement(D[i], x));
    # L/R-class objects
    LL := List(L, x -> GreensLClassOfElement(D[i], x));
    RR := List(R, x -> GreensRClassOfElement(D[i], x));

    Info(InfoSemigroups, 1, "...computing digraphs of the D-class...");

    # DIGRAPHS GAMMA_L AND GAMMA_R
    m := Length(L);
    n := Length(R);

    gamma_L := List([1 .. m], x -> []);
    gamma_R := List([1 .. n], x -> []);

    # Act on the L/R-classes of <D[i]> by the generators above <D[i]>.
    for x in above_gen do
      for k in [1 .. m] do
        y := L[k] * x;
        y := First([1 .. m], z -> y in LL[z]);
        if y <> fail then
          Add(gamma_L[k], y);
        fi;
      od;
    od;
    for x in above_gen do
      for k in [1 .. n] do
        y := x * R[k];
        y := First([1 .. n], z -> y in RR[z]);
        if y <> fail then
          Add(gamma_R[k], y);
        fi;
      od;
    od;

    gamma_L := DigraphNC(gamma_L);
    gamma_R := DigraphNC(gamma_R);
    comp_L := DigraphStronglyConnectedComponents(gamma_L);
    comp_R := DigraphStronglyConnectedComponents(gamma_R);
    gamma_L := QuotientDigraph(gamma_L, comp_L.comps);
    gamma_L := DigraphRemoveLoops(DigraphRemoveAllMultipleEdges(gamma_L));
    ClearDigraphVertexLabels(gamma_L);
    gamma_R := QuotientDigraph(gamma_R, comp_R.comps);
    gamma_R := DigraphRemoveLoops(DigraphRemoveAllMultipleEdges(gamma_R));
    ClearDigraphVertexLabels(gamma_R);

    m := DigraphNrVertices(gamma_L);
    n := DigraphNrVertices(gamma_R);

    gamma := DigraphDisjointUnion(gamma_L, gamma_R);
    red_L := BlistList(DigraphVertices(gamma_L), []);
    red_R := BlistList(DigraphVertices(gamma_R), []);
    comp := rec(id := Concatenation(comp_L.id, comp_R.id + m),
                comps := Concatenation(comp_L.comps, comp_R.comps + m));

    # GRAPH DELTA
    delta := List([1 .. m + n], y -> BlistList([1 .. m + n], []));
    label := List([1 .. m], x -> List([1 .. n], y -> []));
    nredges := 0;
    for k in [1 .. m] do
      for j in [1 .. n] do
        for x in IteratorOfCartesianProduct(comp_L.comps[k],
                                            comp_R.comps[j]) do
          if L[x[1]] * R[x[2]] in D[i] then
            nredges := nredges + 1;
            delta[k][j + m] := true;
            delta[j + m][k] := true;
            label[k][j] := [x[1], x[2]];
            break;
          fi;
        od;
      od;
    od;
    delta := DigraphByAdjacencyMatrix(delta);
    SetIsBipartiteDigraph(delta, true);
    SetIsSymmetricDigraph(delta, true);
    SetDigraphNrEdges(delta, nredges * 2);
    SetDigraphBicomponents(delta, [[1 .. m], [m + 1 .. m + n]]);

    # GRAPH DELTA PRIME
    delta_prime := List([1 .. m + n], y -> BlistList([1 .. m + n], []));
    nredges := 0;
    k := 0;
    while nredges < m * n and k < Length(contain) do
      k := k + 1;
      # work out the vertices of gamma which contain <contain[k]>
      l := comp_L.id[First([1 .. Length(L)], y -> contain[k] in LL[y])];
      r := comp_R.id[First([1 .. Length(R)], y -> contain[k] in RR[y])] + m;
      if not delta_prime[l][r] then
        nredges := nredges + 1;
        delta_prime[l][r] := true;
        delta_prime[r][l] := true;
        red_L[l] := true;
        red_R[r] := true;
      fi;
    od;

    # Find those elements of <D[i]> which are produced by the generators above
    # In fact, can I do better than this by only considering source comps?
    Info(InfoSemigroups, 1, "...computing which elements of D[", i, "] are ",
                            "products of the generators above...");
    for l in [1 .. Length(comp_L.comps)] do
      for r in [1 .. Length(comp_R.comps)] do
        k := comp_L.comps[l][1];
        j := comp_R.comps[r][1];
        if not delta_prime[l][r + m] then
          if ForAny(HClass(S, R[j] * L[k]), h -> h in above_semigroup) then
            nredges := nredges + 1;
            delta_prime[l][r + m] := true;
            delta_prime[r + m][l] := true;
            red_L[l] := true;
            red_R[r] := true;
          fi;
        fi;
      od;
    od;

    delta_prime := DigraphByAdjacencyMatrix(delta_prime);
    SetIsBipartiteDigraph(delta_prime, true);
    SetIsSymmetricDigraph(delta_prime, true);
    SetDigraphNrEdges(delta_prime, nredges * 2);
    SetDigraphBicomponents(delta_prime, [[1 .. m], [m + 1 .. m + n]]);

    ############################################################################
    # Find maximal subsemigroups from maximal rectangles
    num := 0;
    forbidden_L := BlistList([1 .. m], []);
    forbidden_R := BlistList([1 .. n], []);
    if m * n > 1 and not IsCompleteBipartiteDigraph(delta) and
        not IsCompleteBipartiteDigraph(delta_prime) then
      Info(InfoSemigroups, 1, "...calculating maximal independent sets...");
      rectangles := DigraphMaximalIndependentSets(delta);
      num := Length(rectangles) - 2;
      Info(InfoSemigroups, 1, "...found ", num, " maximal independent set(s).");
      if num > 0 then
        num := 0;
        min := Minimum(m, n) - 1;
        bicomp_L := BlistList([1 .. m + n], [1 .. m]);
        bicomp_R := BlistList([1 .. m + n], [m + 1 .. m + n]);
        rectangles := [];
        for r in DigraphMaximalIndependentSets(delta) do
          # Check that <r> is not one of the bicomponents of <delta>.
          b := BlistList([1 .. m + n], r);
          if b = bicomp_L or b = bicomp_R then
            continue;
          fi;

          # Check that the subset of <S> defined by <r> contains <contain>.
          # To do this, check that <r> is a vertex cover for <delta_prime>.
          if not ForAll(DigraphEdges(delta_prime), y -> b[y[1]] or b[y[2]]) then
            continue;
          fi;

          # Check that the subset of <g> defined by <r> has no out-neighbours.
          if ForAny(r, y -> ForAny(OutNeighbours(gamma)[y], z -> not b[z])) then
            continue;
          fi;

          # <r> defines a maximal subsemigroup of <S>
          num := num + 1;

          # Check whether <r> lacks only one s.c.c. of rows/columns:
          # * if it does, then the subsemigroup formed by removing that row or
          #   column is not maximal.
          if Length(r) >= min then
            if SizeBlist(IntersectionBlist(bicomp_L, b)) + 1 = m then
              forbidden_L[First([1 .. m], y -> not b[y])] := true;
            fi;
            if SizeBlist(IntersectionBlist(bicomp_R, b)) + 1 = n then
              forbidden_R[First([m + 1 .. m + n], y -> not b[y]) - m] := true;
            fi;
          fi;
          if not opts.number then
            Add(rectangles, r);
          fi;
        od;
      fi;
    fi;
    if num > 0 then
      Info(InfoSemigroups, 1, "* found ", num, " maximal subsemigroup(s) ",
                              "from  maximal rectangles in D[", i, "].");
      tot := tot + num;
      if not opts.number then
        create_ideal();
        Info(InfoSemigroups, 1, "...creating these maximal subsemigroups...");
        # Find a generating set for the maximal subsemigroup defined by <r>
        source_L := DigraphSources(gamma_L);
        source_R := DigraphSources(gamma_R) + m;
        for r in rectangles do
          b := BlistList([1 .. m + n], r);

          L_in := [];
          L_out := [];
          R_in := [];
          R_out := [];
          for k in [1 .. m] do
            if b[k] then
              Add(L_in, k);
            else
              Add(L_out, k);
            fi;
          od;
          for k in [m + 1 .. m + n] do
            if b[k] then
              Add(R_in, k);
            else
              Add(R_out, k);
            fi;
          od;

          L_out_source := Filtered(source_L, y -> not b[y]);
          R_out_source := Filtered(source_R, y -> not b[y]);
          x := InducedSubdigraph(gamma, L_in);
          L_in_source := List(DigraphSources(x), y -> x!.vertexlabels[y]);
          x := InducedSubdigraph(gamma, R_in);
          R_in_source := List(DigraphSources(x), y -> x!.vertexlabels[y]);

          # Locate a group H-class in the L-section of the maximal subsemigroup
          found := false;
          for u in L_in_source do
            v := First(R_out_source, v -> IsDigraphEdge(delta, u, v));
            if v <> fail then
              found := true;
              break;
            fi;
          od;
          if not found then
            # choose <u> arbitrarily, and find a <v> that works
            u := L_in_source[1];
            v := OutNeighbours(delta)[u][1];
          fi;
          # s.c.c. L_u intersect s.c.c. R_v contains a group H-class
          # Get the index of an actual L/R-class pair such that H_{l,r} is group
          l := label[u][v - m][1];
          r := label[u][v - m][2];
          H := HClass(D[i], R[r] * L[l]);
          inv := InverseGeneralMapping(IsomorphismPermGroup(H));
          x := List(GeneratorsOfSemigroup(Source(inv)), x -> x ^ inv);

          for k in R_out_source do
            if k <> v then
              # Add a rep in an R-class in s.c.c. <k>, in L-class <l>
              Add(x, R[comp.comps[k][1] - m] * L[l]);
            fi;
          od;
          for k in L_in_source do
            if k <> u then
              # Add a rep in an L-class in s.c.c. <k>, in R-class <r>
              Add(x, R[r] * L[comp.comps[k][1]]);
            fi;
          od;

          # Locate a group H-class in the R-section of the maximal subsemigroup
          found := false;
          for v in R_in_source do
            u := First(L_out_source, u -> IsDigraphEdge(delta, u, v));
            if u <> fail then
              found := true;
              break;
            fi;
          od;
          if not found then
            # choose <v> arbitrarily, and find a <u> that works
            v := R_in_source[1];
            u := OutNeighbours(delta)[v][1];
          fi;
          # s.c.c. L_u intersect s.c.c. R_v contains a group H-class
          # Get the index of an actual L/R-class pair such that H_{l,r} is group
          l := label[u][v - m][1];
          r := label[u][v - m][2];
          H := HClass(D[i], R[r] * L[l]);
          inv := InverseGeneralMapping(IsomorphismPermGroup(H));
          Append(x, Images(inv, GeneratorsOfSemigroup(Source(inv))));

          for k in L_out_source do
            if k <> u then
              Add(x, R[r] * L[comp.comps[k][1]]);
            fi;
          od;
          for k in R_in_source do
            if k <> v then
              Add(x, R[comp.comps[k][1] - m] * L[l]);
            fi;
          od;

          # Add generators for the max rectangle if necessary
          if not ForAny(L_out,
                        x -> ForAny(R_out, y -> IsDigraphEdge(delta, x, y)))
              then
            min := Minimum(Length(R_in), Length(L_in));
            for k in [1 .. min] do
              Add(x, R[comp.comps[R_in[k]][1] - m] * L[comp.comps[L_in[k]][1]]);
            od;
            for k in [min + 1 .. Length(L_in)] do
              Add(x, R[comp.comps[R_in[1]][1] - m] * L[comp.comps[L_in[k]][1]]);
            od;
            for k in [min + 1 .. Length(R_in)] do
              Add(x, R[comp.comps[R_in[k]][1] - m] * L[comp.comps[L_in[1]][1]]);
            od;
          fi;
          Append(x, outside_gens);
          Append(x, ideal);
          if not opts.gens then
            x := Semigroup(x);
          fi;
          Add(out, x);
        od;
      fi;
    fi;

    ############################################################################
    # Find maximal subsemigroups by removing L-classes
    num := 0;
    if m > 1 and not IsCompleteBipartiteDigraph(delta_prime) then
      # Look for "non-red" sources of Gamma_L
      remove := Filtered(DigraphSources(gamma_L),
                         x -> not red_L[x] and not forbidden_L[x]);
      num := Length(remove);
    fi;

    # Each s.c.c. of Gamma_L in <remove> can be removed to form a max subsemigp
    if num > 0 then
      tot := tot + num;
      Info(InfoSemigroups, 1, "* found ", num, " maximal subsemigroup(s) by ",
                              "removing L-classes from D[", i, "].");
      if not opts.number then
        create_ideal();
        Info(InfoSemigroups, 1, "...creating these maximal subsemigroups...");
        source_R := DigraphSources(gamma_R) + m;
        for v in remove do
          x := InducedSubdigraph(gamma_L, Difference([1 .. m], [v]));
          sources := List(DigraphSources(x), y -> DigraphVertexLabel(x, y));
          u := Remove(sources, 1);
          v := First(source_R, y -> y in OutNeighbours(delta)[u]);
          if v = fail then
            v := OutNeighbours(delta)[u][1];
          fi;

          l := label[u][v - m][1];
          r := label[u][v - m][2];

          # Generate a group H-class
          H := HClass(D[i], R[r] * L[l]);
          inv := InverseGeneralMapping(IsomorphismPermGroup(H));
          x := Images(inv, GeneratorsOfSemigroup(Source(inv)));

          for k in source_R do
            if k <> v then
              # Add a rep in an R-class in s.c.c. <k>, in L-class <l>
              Add(x, R[comp.comps[k][1] - m] * L[l]);
            fi;
          od;
          for k in sources do
            # Add a rep in an L-class in s.c.c. <k>, in R-class <r>
            Add(x, R[r] * L[comp.comps[k][1]]);
          od;

          x := Concatenation(ideal, outside_gens, x);
          if not opts.gens then
            x := Semigroup(x);
          fi;
          Add(out, x);
        od;
      fi;
    fi;

    ############################################################################
    # Find maximal subsemigroups by removing R-classes
    num := 0;
    if n > 1 and not IsCompleteBipartiteDigraph(delta_prime) then
      # Look for "non-red" sources of Gamma_R
      remove := Filtered(DigraphSources(gamma_R),
                         x -> not red_R[x] and not forbidden_R[x]);
      num := Length(remove);
    fi;

    # Each s.c.c. of Gamma_R in <remove> can be removed to form a max subsemigp
    if num > 0 then
      tot := tot + num;
      Info(InfoSemigroups, 1, "* found ", num, " maximal subsemigroup(s) by ",
                              "removing R-classes from D[", i, "].");
      if not opts.number then
        create_ideal();
        Info(InfoSemigroups, 1, "...creating these maximal subsemigroups...");
        source_L := DigraphSources(gamma_L);
        for v in remove do
          x := InducedSubdigraph(gamma_R, Difference([1 .. n], [v]));
          sources := List(DigraphSources(x), y -> DigraphVertexLabel(x, y));
          v := Remove(sources, 1) + m;
          u := First(source_L, y -> y in OutNeighbours(delta)[v]);
          if u = fail then
            u := OutNeighbours(delta)[v][1];
          fi;

          l := label[u][v - m][1];
          r := label[u][v - m][2];

          # Generate a group H-class
          H := HClass(D[i], R[r] * L[l]);
          inv := InverseGeneralMapping(IsomorphismPermGroup(H));
          x := Images(inv, GeneratorsOfSemigroup(Source(inv)));

          for k in source_L do
            if k <> u then
              Add(x, R[r] * L[comp_L.comps[k][1]]);
            fi;
          od;
          for k in sources do
            Add(x, R[comp_R.comps[k][1]] * L[l]);
          od;
          x := Concatenation(ideal, outside_gens, x);
          if not opts.gens then
            x := Semigroup(x);
          fi;
          Add(out, x);
        od;
      fi;
    fi;

    ############################################################################
    # Find maximal subsemigroups which intersect every H-class of D[i]
    must := ShallowCopy(contain);
    if NrLClasses(D[i]) <= NrRClasses(D[i]) then
      e := List(LClasses(D[i]), x -> Idempotents(x)[1]);
      for x in e do
        for y in above_gen do
          z := y * x;
          if z in D[i] then
            AddSet(must, z);
          fi;
        od;
      od;
    else
      e := List(RClasses(D[i]), x -> Idempotents(x)[1]);
      for x in e do
        for y in above_gen do
          z := x * y;
          if z in D[i] then
            AddSet(must, z);
          fi;
        od;
      od;
    fi;

    inj := InjectionPrincipalFactor(D[i]);
    inv := InverseGeneralMapping(inj);
    Info(InfoSemigroups, 1, "...calculating maximal subsemigroups of the ",
                            "principal factor...");
    must := List(must, z -> z ^ inj);
    M := MaximalSubsemigroupsNC(Range(inj), rec(types := [6],
                                                number := opts.number,
                                                contain := must,
                                                gens := true,
                                                zero := false));
    if opts.number then
      num := M;
    else
      num := Length(M);
    fi;

    if num = 0 then
      Info(InfoSemigroups, 1, "found no maximal subsemigroups which ",
                              "intersect every H-class.");
    else
      tot := tot + num;
      Info(InfoSemigroups, 1, "* found ", num, " maximal subsemigroup(s) ",
                              "which intersect(s) every H-class.");

      if not opts.number then
        create_ideal();
        Info(InfoSemigroups, 1, "...creating these maximal subsemigroups...");
        for R in M do
          x := List(R, x -> x ^ inv);
          x := Concatenation(ideal, outside_gens, x);
          if not opts.gens then
            x := Semigroup(x);
          fi;
          Add(out, x);
        od;
      fi;
    fi;

    # TODO(later) check for maximal subsemigroups formed by removing D[i]
    # if tot is what it started as, then if we have reached here,
    # that means there ARE maximal subsemigroups arising from D[i]
    # but none of the types so far have been found. Therefore S\D[i]
    # is a maximal subsemigroup of S
    if tot = num_start and IsEmptyDigraph(delta_prime) then
      Info(InfoSemigroups, 1, "* found a maximal subsemigroup by removing ",
                              "D[", i, "].");
      tot := tot + 1;
      if not opts.number then
        create_ideal();
        x := Concatenation(ideal, outside_gens);
        if not opts.gens then
          x := Semigroup(x);
        fi;
        Add(out, x);
      fi;
    fi;
  od;
  Info(InfoSemigroups, 1, "found ", tot, " maximal subsemigroups in total.");
  if opts.number then
    return tot;
  fi;
  return out;
end);
