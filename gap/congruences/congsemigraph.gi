############################################################################
##
##  congsemigraph.gi
##  Copyright (C) 2021                       Marina Anagnostopoulou-Merkouri
##                                                            James Mitchell
##
##  Licensing information can be found in the README file of this package.
##
############################################################################

BindGlobal("SEMIGROUPS_IsHereditarySubset",
function(S, H)
  local out, h, v, D;
  D := GraphOfGraphInverseSemigroup(S);
  if IsMultiDigraph(D) then
    ErrorNoReturn(D, " must not have multiple edges");
  fi;
  out := OutNeighbours(D);
  for h in H do
    for v in out[h] do
      if not (v in H) then
        return false;
      fi;
    od;
  od;
  return true;
end);

BindGlobal("SEMIGROUPS_IsValidWSet",
function(S, H, W)
  local w, c, v, out, D;
  if not SEMIGROUPS_IsHereditarySubset(S, H) then
    ErrorNoReturn(H, "must be a valid hereditary subset");
  fi;
  D := GraphOfGraphInverseSemigroup(S);
  if IsMultiDigraph(D) then
    ErrorNoReturn(D, "must not have multiple edges");
  fi;
  out := OutNeighbours(D);
  for w in W do
    c := 0;
    for v in out[w] do
      if v in H then
        c := c + 1;
      fi;
    od;
    if c <> Length(out[w]) - 1 then
      return false;
    fi;
  od;
  return true;
end);

BindGlobal("SEMIGROUPS_ValidateWangPair",
function(S, H, W)
  local v, D;
  D := GraphOfGraphInverseSemigroup(S);
  for v in Union(H, W) do
    if not (v in DigraphVertices(D)) then
      ErrorNoReturn("the elements of ", H, " and ",
                    W, " are not all vertices of ", D);
    fi;
  od;
  if not SEMIGROUPS_IsHereditarySubset(S, H) then
    ErrorNoReturn(H, " is not a valid hereditary subset");
  fi;
  if not SEMIGROUPS_IsValidWSet(S, H, W) then
    ErrorNoReturn(W, " is not a valid W-set");
  fi;
  return true;
end);

InstallMethod(CongruenceByWangPair,
"for a graph inverse semigroup, homogeneous list, and homogeneous list",
[IsGraphInverseSemigroup, IsHomogeneousList, IsHomogeneousList],
function(S, H, W)
  local fam, cong;
  SEMIGROUPS_ValidateWangPair(S, H, W);
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam, IsCongruenceByWangPair),
                    rec(H := H, W := W));
  SetSource(cong, S);
  SetRange(cong, S);
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
  return Concatenation("<gis-congruence with H=",
                       ViewString(C!.H),
                       ", W=",
                       ViewString(C!.W),
                       ">");
end);

InstallMethod(AsSemigroupCongruenceByGeneratingPairs,
"for a congruence by Wang pair",
[IsCongruenceByWangPair],
function(cong)
  local pairs, gens, verts, v, u, e, D, out, S, H, W;
  S := Source(cong);
  H := cong!.H;
  W := cong!.W;
  pairs := [];
  gens := GeneratorsOfSemigroup(S);
  verts := Filtered(gens, IsVertex);
  D := GraphOfGraphInverseSemigroup(S);
  out := OutNeighbours(D);
  for v in H do
    Add(pairs, [verts[v], MultiplicativeZero(S)]);
  od;
  for v in W do
    for u in out[v] do
      if not (u in H) then
        for e in gens do
          if Source(e) = verts[v] and Range(e) = verts[u] then
            Add(pairs, [verts[v], e * e ^ -1]);
          fi;
        od;
      fi;
    od;
  od;
  return SemigroupCongruence(S, pairs);
end);

BindGlobal("SEMIGROUPS_MinimalHereditarySubsetsVertex",
function(D, v)
  local subsets, hereditary, u, out, s, a;
  if IsMultiDigraph(D) then
    ErrorNoReturn(D, " must not have multiple edges");
  fi;
  if not (v in DigraphVertices(D)) then
    ErrorNoReturn(v, " must be a vertex of ", D);
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

InstallMethod(GISGeneratingCongruences,
"for a graph inverse semigroup",
[IsGraphInverseSemigroup],
function(S)
  local congs, out, v, h, D;
  D := GraphOfGraphInverseSemigroup(S);
  congs := [];
  out := OutNeighbours(D);
  for v in DigraphVertices(D) do
    if Length(out[v]) = 0 then
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
  local H, W, eq, j, Hgraph, Wgraph, v;
  Hgraph := [];
  Wgraph := [];
  if not IsGraphInverseSemigroup(Source(C)) then
    ErrorNoReturn(Source(C), " is not a graph inverse semigroup");
  fi;

  H := [];
  W := [];
  eq := EquivalenceRelationPartition(C);
  eq := Filtered(eq, x -> ForAny(x, IsVertex));
  for j in eq do
    if MultiplicativeZero(Source(C)) in j then
      Append(H, Filtered(j, IsVertex));
    else
      Append(W, Filtered(j, IsVertex));
    fi;
  od;
  Sort(H);
  Sort(W);
  for v in H do
    Add(Hgraph, IndexOfVertexOfGraphInverseSemigroup(v));
  od;
  for v in W do
    Add(Wgraph, IndexOfVertexOfGraphInverseSemigroup(v));
  od;
  return CongruenceByWangPair(Source(C), Hgraph, Wgraph);
end);

InstallMethod(JoinSemigroupCongruences,
"for two congruences by Wang pair",
[IsCongruenceByWangPair, IsCongruenceByWangPair],
function(cong1, cong2)
  local out, H, W, v, u, X, W_zero, S, D, w, e, k, d, H1, H2;
  S := Source(cong1);
  D := GraphOfGraphInverseSemigroup(S);
  out := OutNeighbours(D);
  X := [];
  H1 := cong1!.H;
  H2 := cong2!.H;
  H := Union(H1, H2);
  W := Difference(Union(cong1!.W, cong2!.W), H);
  W_zero := [];
  for v in W do
    c := 0;
    if ForAll(out[v], w -> w in H) then
      Add(W_zero, v);
      Add(X, v);
    fi;
  od;
  for v in W do
    for u in W_zero do
      for k in IteratorOfPaths(D, v, u) do
        d := 0;
        for e in [1 .. Length(k[1]) - 1] do
          if k[1][e] in W then
            d := d + 1;
          fi;
        od;
        if d = Length(k[1]) - 1 then
          Add(X, v);
        fi;
      od;
    od;
  od;
  return CongruenceByWangPair(S, Union(H, X), Difference(W, Union(H, X)));
end);

InstallMethod(IsSubrelation,
"for two congruences by Wang pair",
[IsCongruenceByWangPair, IsCongruenceByWangPair],
function(cong1, cong2)
  return IsSubset(Union(cong1!.H, cong1!.W), Union(cong2!.H, cong2!.W));
end);

InstallMethod(IsSuperrelation,
"for two congruences by Wang pair",
[IsCongruenceByWangPair, IsCongruenceByWangPair],
function(cong1, cong2)
  return IsSubset(Union(cong2!.H, cong2!.W), Union(cong1!.H, cong1!.W));
end);

InstallMethod(LatticeOfCongruences,
"for a graph inverse semigroup",
[IsGraphInverseSemigroup],
S -> JoinSemilatticeOfCongruences(GISGeneratingCongruences(S),
                                  JoinSemigroupCongruences));
