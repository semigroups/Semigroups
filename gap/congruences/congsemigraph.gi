############################################################################
##
##  congsemigraph.gi
##  Copyright (C) 2022-2026                Marina Anagnostopoulou-Merkouri
##                                                          James Mitchell
##                                                             Joseph Ward
##
##  Licensing information can be found in the README file of this package.
##
############################################################################

BindGlobal("SEMIGROUPS_IsHereditarySubset",
function(S, H)
  local out, h, v, D, BlistH;
  D := GraphOfGraphInverseSemigroup(S);
  out := OutNeighbours(D);
  if IsEmpty(H) or H = DigraphVertices(D) then
    return true;
  fi;
  BlistH := BlistList(DigraphVertices(D), H);
  for h in H do
    for v in out[h] do
      if not BlistH[v] then
        return false;
      fi;
    od;
  od;
  return IsDuplicateFreeList(H) and IsSortedList(H);
end);

BindGlobal("SEMIGROUPS_IsValidWSet",
function(S, H, W)
  local w, out, D;
  D := GraphOfGraphInverseSemigroup(S);
  out := OutNeighbours(D);
  for w in W do
    if Size(Intersection(out[w], H)) <> Size(out[w]) - 1 then
      return false;
    fi;
  od;
  return IsDuplicateFreeList(H) and IsSortedList(H);
end);

BindGlobal("SEMIGROUPS_ValidateWangPair",
function(S, H, W)
  local D;
  D := GraphOfGraphInverseSemigroup(S);
  if not IsSubset(DigraphVertices(D), Union(H, W)) then
    ErrorNoReturn("the items in the 2nd and 3rd arguments",
                  " (lists) are not all vertices of the 1st argument",
                  "(a digraph)");
  elif not SEMIGROUPS_IsHereditarySubset(S, H) then
    ErrorNoReturn("the 2nd argument (a list) is not a valid hereditary set");
  elif not SEMIGROUPS_IsValidWSet(S, H, W) then
    ErrorNoReturn("the 3rd argument (a list) is not a valid W-set");
  fi;
  return true;
end);

InstallMethod(CongruenceByWangPair,
"for a graph inverse semigroup, homogeneous list, and homogeneous list",
[IsGraphInverseSemigroup, IsHomogeneousList, IsHomogeneousList],
function(S, H, W)
  local fam, cong;
  if not IsFinite(S) then
    ErrorNoReturn("the 1st argument (a graph inverse semigroup)",
                  " must be a finite");
  fi;
  SEMIGROUPS_ValidateWangPair(S, H, W);
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam, IsCongruenceByWangPair), rec(H := H, W := W));
  SetSource(cong, S);
  SetRange(cong, S);
  GeneratingPairsOfSemigroupCongruence(cong);
  return cong;
end);

InstallMethod(ViewObj, "for a congruence by Wang pair",
[IsCongruenceByWangPair],
function(C)
  Print(ViewString(C));
end);

InstallMethod(ViewString, "for a congruence by Wang pair",
[IsCongruenceByWangPair],
function(C)
  return StringFormatted(
    "<graph inverse semigroup congruence with H = {} and W = {}>",
    ViewString(C!.H),
    ViewString(C!.W));
end);

InstallMethod(GeneratingPairsOfSemigroupCongruence,
"for a congruence by Wang pair",
[IsCongruenceByWangPair],
function(cong)
  local pairs, gens, verts, v, u, e, D, out, S, H, W, BlistH;
  S := Source(cong);
  H := cong!.H;
  W := cong!.W;
  pairs := [];
  gens := GeneratorsOfSemigroup(S);
  verts := VerticesOfGraphInverseSemigroup(S);
  D := GraphOfGraphInverseSemigroup(S);
  out := OutNeighbours(D);
  BlistH := BlistList(DigraphVertices(D), H);
  for v in H do
    Add(pairs, [verts[v], MultiplicativeZero(S)]);
  od;
  for v in W do
    for u in out[v] do
      if not BlistH[u] then
        for e in gens do
          if Source(e) = verts[v] and Range(e) = verts[u] then
            Add(pairs, [verts[v], e * e ^ -1]);
          fi;
        od;
      fi;
    od;
  od;
  return pairs;
end);

BindGlobal("SEMIGROUPS_MinimalHereditarySubsetsVertex",
function(D, v)
  local subsets, hereditary, u, out, s, a;
  if IsMultiDigraph(D) then
    ErrorNoReturn("the 1st argument (a digraph) must not have multiple edges");
  elif not (v in DigraphVertices(D)) then
    ErrorNoReturn("the 2nd argument (a pos. int.) is not a vertex of ",
                  "the 1st argument (a digraph)");
  fi;
  out := Set(OutNeighboursMutableCopy(D)[v]);
  subsets := [];
  for u in [1 .. Length(out)] do
    a := out[u];
    RemoveSet(out, a);
    hereditary := ShallowCopy(out);
    for s in out do
      UniteSet(hereditary, VerticesReachableFrom(D, s));
    od;
    if not (a in hereditary) and not (hereditary in subsets) then
      AddSet(subsets, hereditary);
    fi;
    AddSet(out, a);
  od;
  return AsSortedList(subsets);
end);

InstallMethod(GeneratingCongruencesOfLattice,
"for a graph inverse semigroup",
[IsGraphInverseSemigroup],
function(S)
  local congs, out, v, h, D;
  if not IsFinite(S) then
    ErrorNoReturn("the 1st argument (a graph inverse semigroup) must ",
                  "be finite");
  fi;
  D := GraphOfGraphInverseSemigroup(S);
  congs := [];
  out := OutNeighbours(D);
  for v in DigraphVertices(D) do
    if IsEmpty(out[v]) then
      Add(congs, CongruenceByWangPair(S, [v], []));
    elif Length(out[v]) = 1 then
      Add(congs, CongruenceByWangPair(S, [], [v]));
    else
      for h in SEMIGROUPS_MinimalHereditarySubsetsVertex(D, v) do
        Add(congs, CongruenceByWangPair(S, h, [v]));
      od;
    fi;
  od;
  Add(congs, CongruenceByWangPair(S, [], []));
  return congs;
end);

InstallMethod(AsCongruenceByWangPair, "for a semigroup congruence",
[IsSemigroupCongruence],
function(C)
  local H, W, eq, result, pairs, j;

  if not IsGraphInverseSemigroup(Source(C)) then
    ErrorNoReturn("the source of the 1st argument (a congruence)",
                  " is not a graph inverse semigroup");
  fi;
  H := [];
  W := [];
  eq := EquivalenceRelationPartition(C);
  eq := Filtered(eq, x -> ForAny(x, IsVertex));
  for j in eq do
    if MultiplicativeZero(Source(C)) in j then
      H := Union(H, List(Filtered(j, IsVertex),
      IndexOfVertexOfGraphInverseSemigroup));
    else
      W := Union(W, List(Filtered(j, IsVertex),
      IndexOfVertexOfGraphInverseSemigroup));
    fi;
  od;
  result := CongruenceByWangPair(Source(C), H, W);
  if HasGeneratingPairsOfMagmaCongruence(C) then
    pairs := GeneratingPairsOfMagmaCongruence(C);
    SetGeneratingPairsOfMagmaCongruence(result, pairs);
  fi;
  return result;
end);

InstallMethod(JoinSemigroupCongruences,
"for two congruences by Wang pair",
[IsCongruenceByWangPair, IsCongruenceByWangPair],
function(cong1, cong2)
  local out, H, W, v, u, X, W_zero, S, D, w, k;
  S := Source(cong1);
  D := GraphOfGraphInverseSemigroup(S);
  out := OutNeighbours(D);
  X := [];
  H := Union(cong1!.H, cong2!.H);
  W := Difference(Union(cong1!.W, cong2!.W), H);
  W_zero := [];
  for v in W do
    if ForAll(out[v], w -> w in H) then
      Add(W_zero, v);
      Add(X, v);
    fi;
  od;
  for v in W do
    for u in W_zero do
      for k in IteratorOfPaths(D, v, u) do
        if ForAll(k[1], x -> x in W) then
          Add(X, v);
        fi;
      od;
    od;
  od;
  return CongruenceByWangPair(S, Union(H, X), Difference(W, Union(H, X)));
end);

InstallMethod(MeetSemigroupCongruences,
"for two congruences by Wang pair",
[IsCongruenceByWangPair, IsCongruenceByWangPair],
function(cong1, cong2)
  local out, H1, H2, W1, W2, H, V0, v;
  out := OutNeighbours(GraphOfGraphInverseSemigroup(Source(cong1)));
  H1 := cong1!.H;
  H2 := cong2!.H;
  W1 := cong1!.W;
  W2 := cong2!.W;
  H := Union(H1, H2);
  V0 := [];
  for v in Difference(Union(W1, W2), H) do
    if ForAll(out[v], w -> w in H) then
      Add(V0, v);
    fi;
  od;
  return CongruenceByWangPair(Source(cong1),
                              Intersection(H1, H2),
                              Union(Intersection(W1, H2),
                                    Intersection(W2, H1),
                                    Difference(Intersection(W1, W2), V0)));
end);

InstallMethod(IsSubrelation,
"for two congruences by Wang pair",
[IsCongruenceByWangPair, IsCongruenceByWangPair],
{cong1, cong2}
-> IsSubset(Union(cong1!.H, cong1!.W), Union(cong2!.H, cong2!.W)));

InstallMethod(IsSuperrelation,
"for two congruences by Wang pair",
[IsCongruenceByWangPair, IsCongruenceByWangPair],
{cong1, cong2}
-> IsSubset(Union(cong2!.H, cong2!.W), Union(cong1!.H, cong1!.W)));

InstallMethod(\=, "for two congruences by Wang pair",
[IsCongruenceByWangPair, IsCongruenceByWangPair],
{cong1, cong2} -> cong1!.H = cong2!.H and cong1!.W = cong2!.W);

InstallMethod(CayleyDigraphOfCongruences,
"for a graph inverse semigroup",
[IsGraphInverseSemigroup],
function(S)
  return _ClosureLattice(S,
                         GeneratingCongruencesOfLattice(S),
                         WrappedTwoSidedCongruence);
end);

InstallMethod(TrivialCongruence,
"for a graph inverse semigroup",
[IsGraphInverseSemigroup],
S -> AsCongruenceByWangPair(SemigroupCongruence(S, [])));

InstallMethod(TraceOfCongruenceByWangPair,
"for a congruence by Wang pair",
[IsCongruenceByWangPair],
function(cong)
  local S, fam, tr;

  S := IdempotentGeneratedSubsemigroup(Source(cong));

  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  tr := Objectify(NewType(fam, IsTraceOfCongruenceByWangPair),
                  rec(cong := cong));
  SetSource(tr, S);
  SetRange(tr, S);
  return tr;
end);

InstallMethod(JoinSemigroupCongruences,
"for two traces of congruences by Wang pair",
[IsTraceOfCongruenceByWangPair, IsTraceOfCongruenceByWangPair],
{tr1, tr2} -> TraceOfCongruenceByWangPair(
JoinSemigroupCongruences(tr1!.cong, tr2!.cong)));

InstallMethod(MeetSemigroupCongruences,
"for two traces of congruences by Wang pair",
[IsTraceOfCongruenceByWangPair, IsTraceOfCongruenceByWangPair],
{tr1, tr2} -> TraceOfCongruenceByWangPair(
    MeetSemigroupCongruences(tr1!.cong, tr2!.cong)));

InstallMethod(IsSubrelation,
"for two traces of congruences by Wang pair",
[IsTraceOfCongruenceByWangPair, IsTraceOfCongruenceByWangPair],
{tr1, tr2} -> IsSubrelation(tr1!.cong, tr2!.cong));

InstallMethod(IsSuperrelation,
"for two traces of congruences by Wang pair",
[IsTraceOfCongruenceByWangPair, IsTraceOfCongruenceByWangPair],
{tr1, tr2} -> IsSuperrelation(tr1!.cong, tr2!.cong));

InstallMethod(ViewObj, "for trace of a congruence by Wang pair",
[IsTraceOfCongruenceByWangPair],
function(tr)
  Print(ViewString(tr));
end);

InstallMethod(ViewString, "for a congruence by Wang pair",
[IsTraceOfCongruenceByWangPair],
function(tr)
  return StringFormatted(
    "<trace of graph inverse semigroup congruence with H = {} and W = {}>",
    ViewString(tr!.cong!.H),
    ViewString(tr!.cong!.W));
end);

InstallMethod(CongruenceTestMembershipNC,
"for the trace of a congruence by Wang pair",
[IsTraceOfCongruenceByWangPair,
 IsGraphInverseSemigroupElement,
 IsGraphInverseSemigroupElement],
function(tr, elm1, elm2)
  local p1, p2, range_elm1_in_H, range_elm2_in_H, tmp, S, e, i;

  if elm1 = elm2 then
    return true;
  fi;

  p1 := PositivePath(elm1);
  p2 := PositivePath(elm2);

  range_elm1_in_H := IsMultiplicativeZero(Source(tr), elm1)
    or IndexOfVertexOfGraphInverseSemigroup(Range(p1)) in tr!.cong!.H;
  range_elm2_in_H := IsMultiplicativeZero(Source(tr), elm2)
    or IndexOfVertexOfGraphInverseSemigroup(Range(p2)) in tr!.cong!.H;

  if range_elm1_in_H or range_elm2_in_H then
    return range_elm1_in_H and range_elm2_in_H;
    # If either is in the zero class, then the other must be

  elif Source(elm1) <> Source(elm2) or Range(elm1) <> Range(elm2) then
    return false;  # Since the elements are outside the zero class,
    # they cannot be related if they have different source and range
  fi;
  if Length(p1![1]) > Length(p2![1]) then
    tmp := p1;
    p1 := p2;
    p2 := tmp;  # let p1 be the longer path
  fi;

  for i in [1 .. Length(p1![1])] do
    if p1![1][i] <> p2![1][i] then
      return false;  # check if the shorter path is a prefix of the longer one
    fi;
  od;

  S := Source(tr!.cong);
  for i in [Length(p1![1]) .. Length(p2![1])] do
    e := EdgesOfGraphInverseSemigroup(S)[p2![1][i]];
    if not IndexOfVertexOfGraphInverseSemigroup(Source(e)) in tr!.cong!.W then
      return false;
      # if any of the edges in the longer path are not W edges,
      # then the elements are not related
    fi;
  od;

  return true;
  # p1 is a prefix of p2 and every edge in p2 outside p1 is a W-edge,
  # so the pair is related
end);

InstallMethod(EdgesWithRange,
"for a graph inverse semigroup element",
[IsGraphInverseSemigroupElement],
function(x)
  local G;
  G := FamilyObj(x)!.semigroup;
  if not IsVertex(x) then
    return EdgesWithRange(Source(x));
  fi;
  return Filtered(EdgesOfGraphInverseSemigroup(G), e -> Range(e) = x);
end);

InstallMethod(PathsWithRange,
"for a graph inverse semigroup element",
[IsGraphInverseSemigroupElement],
function(x)
  local inPaths, e, inPath, G;
  G := FamilyObj(x)!.semigroup;
  inPaths := [x];
  for e in EdgesWithRange(Source(x)) do
    Append(inPaths, List(PathsWithRange(Source(e)), p -> p * e * x));
  od;
  return inPaths;
end);

InstallMethod(EdgesWithSource,
"for a graph inverse semigroup element",
[IsGraphInverseSemigroupElement],
function(x)
  local G;
  if not IsVertex(x) then
    return EdgesWithSource(Range(x));
  else
    G := FamilyObj(x)!.semigroup;
    return Filtered(EdgesOfGraphInverseSemigroup(G), e -> Source(e) = x);
  fi;
end);

InstallMethod(PathsWithSource,
"for a graph inverse semigroup element",
[IsGraphInverseSemigroupElement],
function(x)
  local outPaths, e, outPath;
  outPaths := [x];
  for e in EdgesWithSource(Range(x)) do
    Append(outPaths, List(PathsWithSource(Range(e)), p -> x * e * p));
  od;
  return outPaths;
end);

BindGlobal("SEMIGROUPS_LastEdgeOfPathGIS",
function(p)
  local edgeIndex;
  edgeIndex := Last(p![1]);
  return EdgesOfGraphInverseSemigroup(FamilyObj(p)!.semigroup)[edgeIndex];
end);

InstallMethod(ImagesElm,
"for the trace of a congruence by Wang pair and an element",
[IsTraceOfCongruenceByWangPair, IsGraphInverseSemigroupElement],
function(tr, x)
  local class, p, h, h_elts, h_paths, w_elts;
  if not IsIdempotent(x) then
    Error("x is not an idempotent!");
  elif not FamilyRange(FamilyObj(tr!.cong))!.semigroup =
      FamilyObj(x)!.semigroup then
    # TODO use family checker?
    Error("x is not in the semigroup that cong is defined on!");
  fi;
  p := PositivePath(x);
  h_elts := List(tr!.cong!.H, h -> VerticesOfGraphInverseSemigroup(Source(
  tr!.cong))[h]);
  # check if x is in the zero class
  if IsMultiplicativeZero(Source(tr!.cong), x) or (
      IndexOfVertexOfGraphInverseSemigroup(Range(p)) in tr!.cong!.H) then
    class := [];
    for h in h_elts do
      h_paths := PathsWithRange(h);
      for p in h_paths do
        Add(class, p * p ^ -1);
      od;
    od;
    Add(class, MultiplicativeZero(Source(tr!.cong)));
  else
    class := [x];
    w_elts := List(tr!.cong!.W, w -> VerticesOfGraphInverseSemigroup(Source(
    tr!.cong))[w]);
    while Range(p) in w_elts do
      p := p * First(EdgesWithSource(Range(p)), e -> not
           Range(e) in h_elts);
      Add(class, p * p ^ -1);
    od;
    if not IsVertex(x) then
      p := PositivePath(x);
      while (not (IsVertex(p)) and Source(SEMIGROUPS_LastEdgeOfPathGIS(p)) in w_elts) do
        if Length(p![1]) > 1 then
          p := EvaluateWord(GeneratorsOfSemigroup(Source(tr!.cong)),
               p![1]{[1 .. Length(p![1]) - 1]});
        else
          p := Source(p);
        fi;
        Add(class, p * p ^ -1);
      od;
    fi;
  fi;
  return class;
end);

InstallMethod(EquivalenceRelationPartition,
"for the trace of a congruence by Wang Pair",
[IsTraceOfCongruenceByWangPair],
function(tr)
  local classes, w, p, zero_class, w_elt;
  zero_class := ImagesElm(tr, MultiplicativeZero(Source(tr!.cong)));
  if Length(zero_class) > 1 then
    classes := [zero_class];
  else
    classes := [];
  fi;
  for w in tr!.cong!.W do
    w_elt := VerticesOfGraphInverseSemigroup(Source(tr!.cong))[w];
    if IsEmpty(Intersection(InNeighbours(GraphOfGraphInverseSemigroup(Source(
        tr!.cong)))[w], tr!.cong!.W)) then
      for p in PathsWithRange(w_elt) do
        Add(classes, p * ImagesElm(tr, w_elt) * p ^ -1);
      od;
    else
      Add(classes, ImagesElm(tr, w_elt));
    fi;
  od;
  return classes;
end);

InstallMethod(EquivalenceRelationPartition,
"for a congruence by Wang Pair",
[IsCongruenceByWangPair],
function(cong)
  local classes, w, p, q, ps, pws, zero_class, w_elt, w_class, w_cond;
  zero_class := ImagesElm(cong, MultiplicativeZero(Source(cong)));
  if Length(zero_class) > 1 then
    classes := [zero_class];
  else
    classes := [];
  fi;
  for w in cong!.W do
    if Intersection(InNeighbours(GraphOfGraphInverseSemigroup(
        Source(cong)))[w], cong!.W) = [] then
      for p in PathsWithRange(VerticesOfGraphInverseSemigroup(
          Source(cong))[w]) do
        for q in PathsWithRange(VerticesOfGraphInverseSemigroup(
            Source(cong))[w]) do
          Add(classes, p * ImagesElm(TraceOfCongruenceByWangPair(cong),
            VerticesOfGraphInverseSemigroup(Source(cong))[w]) * q ^ -1);
        od;
      od;
    else
      w_elt := VerticesOfGraphInverseSemigroup(Source(cong))[w];
      w_class := ImagesElm(TraceOfCongruenceByWangPair(cong), w_elt);
      ps := PathsWithRange(w_elt);
      # w_cond is true iff the final edge of p is a w-edge
      w_cond := BlistList(ps, Filtered(ps, p -> (not IsVertex(p)) and
             IndexOfVertexOfGraphInverseSemigroup(Source(
             SEMIGROUPS_LastEdgeOfPathGIS(p))) in cong!.W));
      for p in [1..Length(ps)] do
        for q in [1..Length(ps)]  do
          if w_cond[p] and w_cond[q] then
            if Last(ps[p]![1]) <> Last(ps[q]![1]) then
              Add(classes, ps[p] * w_class * ps[q] ^ -1);
            fi;
          else
            Add(classes, ps[p] * w_class * ps[q] ^ -1);
          fi;
        od;
      od;
    fi;
  od;
  return classes;
end);

InstallMethod(ImagesElm,
"for a congruence by Wang Pair and an element",
[IsCongruenceByWangPair, IsGraphInverseSemigroupElement],
function(cong, x)
  local p, q, class, h_elms, h, ps, w_elts;
  if not x in Source(cong) then
    Error(
      "Element must be in the semigroup that the congruence is defined on!");
  elif IsMultiplicativeZero(Source(cong), x) or (
      IndexOfVertexOfGraphInverseSemigroup(Range(PositivePath(x))) in cong!.H)
  then
    class := [MultiplicativeZero(Source(cong))];
    h_elms := List(cong!.H, h -> VerticesOfGraphInverseSemigroup(Source(
      cong))[h]);
    for h in h_elms do
      ps := PathsWithRange(h);
      for p in ps do
        for q in ps do
          Add(class, p * q ^ -1);
        od;
      od;
    od;
    return class;
  elif IsIdempotent(x) then
    return ImagesElm(TraceOfCongruenceByWangPair(cong), x);
  fi;
  p := PositivePath(x);
  q := NegativePath(x);
  w_elts := List(cong!.W, w -> VerticesOfGraphInverseSemigroup(Source(cong))
    [w]);
  if Last(p![1]) = - First(q![1]) and (not IsVertex(p))
      and (Source(EdgesOfGraphInverseSemigroup(Source(cong))[-First(q![1])])
        in w_elts) then
    if Length(p![1]) = 1 then
      p := Source(p);
      q := EvaluateWord(GeneratorsOfSemigroup(Source(cong)),
           q![1]{[2 .. Length(q![1])]});
    elif Length(q![1]) = 1 then
      p := EvaluateWord(GeneratorsOfSemigroup(Source(cong)),
           p![1]{[1 .. Length(p![1]) - 1]});
      q := Range(q);
    else
      p := EvaluateWord(GeneratorsOfSemigroup(Source(cong)),
           p![1]{[1 .. Length(p![1]) - 1]});
      q := EvaluateWord(GeneratorsOfSemigroup(Source(cong)),
           q![1]{[2 .. Length(q![1])]});
    fi;
    return ImagesElm(cong, p * q);
  else
    return p * ImagesElm(TraceOfCongruenceByWangPair(cong), Range(p)) * q;
  fi;
end);

InstallMethod(CongruenceTestMembershipNC,
"for a congruence by Wang pair and two graph inverse semigroup elements",
[IsCongruenceByWangPair, IsGraphInverseSemigroupElement,
  IsGraphInverseSemigroupElement],
138,
function(cong, x, y)
  # (x, y) in cong if and only if xy^-1 in Ker and (x^-1x, y^-1y) in Tr
  # and Ker consists only of zero class and idempotents
  return CongruenceTestMembershipNC(TraceOfCongruenceByWangPair(cong),
      x ^ -1 * x, y ^ -1 * y)
    and (IsIdempotent(x * y ^ -1)
    or IndexOfVertexOfGraphInverseSemigroup(Range
      (PositivePath(x * y ^ -1))) in cong!.H);
end);
