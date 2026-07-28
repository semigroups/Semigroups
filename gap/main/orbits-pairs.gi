PartitionRightCosets := function(S, SS, G)
  local RC, uf, i, C, c;
  RC := RightCosets(S, G);
  uf := PartitionDS(IsPartitionDS, Size(S));
  for C in RC do
    i := Position(SS, Representative(C));
    for c in C do
      Unite(uf, i, Position(SS, c));
    od;
  od;
  return uf;
end;

PartitionLeftCosets := function(S, SS, G)
  local LC, uf, i, C, c;
  LC := LeftCosets(S, G);
  uf := PartitionDS(IsPartitionDS, Size(S));
  for C in LC do
    i := Position(SS, Representative(C));
    for c in C do
      Unite(uf, i, Position(SS, c));
    od;
  od;
  return uf;
end;

PartitionInverses := function(G)
  local elts, parts, g, h;
  elts := Elements(G);
  parts := [];
  for g in elts do
    h := g ^ -1;
    if g = h then
      Add(parts, [g]);
    else
      Add(parts, [g, h]);
    fi;
  od;
  return parts;
end;

# RootsOfPartitionDS := function(uf)
#   local pt, gp, data, n, result;
# 
#     pt := 0;
#     gp := UF.getParent;
#     data := uf!.data;
#     n := SizeUnderlyingSetDS(uf);
#     result := [];
#     while pt <= n do
#       pt := pt + 1;
#       while pt <= n and gp(data[pt]) <> pt do
#         pt := pt + 1;
#       od;
#       if pt <= n then
#         Add(result, pt);
#       fi;
#     od;
#     return result;
# end;
# 
UfJoin := function(uf1parts, uf2)
    local join, rep, part, x;
    join := ShallowCopy(uf2);
    for part in uf1parts do
      rep := Representative(uf2, part[1]);
      for x in part do
        Unite(join, rep, x);
      od;
    od;
    return join;
end;

UfFromPartition := function(domain, partition)
  local uf, part, k, p;
  uf := PartitionDS(IsPartitionDS, Size(domain));
  for part in partition do
    k := Position(domain, part[1]);
    for p in part do
      Unite(uf, k, Position(domain, p));
    od;
  od;
  return uf;
end;

OrbitsOfPairs := function(G, S, act)
  local T, tLength, A, GS, J, D, R, L, invPartition, invPartUF, invParts, i,
    lParts, j, pairs, g, x, y;
  T := List(Orbits(G, S, act), l -> l[1]);
  tLength := Length(T);
  A := List(T, t -> Stabiliser(G, t, act));
  GS := AsSet(G);
  J := [];
  D := [];
  R := [];
  L := [];
  invPartition := PartitionInverses(G);
  invPartUF := UfFromPartition(GS, invPartition);
  invParts := PartsOfPartitionDS(invPartUF);
  for i in [1 .. tLength] do
    Add(R, PartitionRightCosets(G, GS, A[i]));
    Add(L, PartitionLeftCosets(G, GS, A[i]));
    Print(Concatenation(["\rPartitioned Cosets ", String(i), " of ",
      String(tLength)]));
  od;
  Print("\033[2K\rPartitioned All Cosets\n");
  lParts := List(L, PartsOfPartitionDS);

  for i in [1 .. tLength] do
    Add(J, []);
    Add(D, []);
    for j in [1 .. i - 1] do
      Add(J[i], UfJoin(lParts[j], R[i]));
      Add(D[i], List(RootsOfPartitionDS(J[i][j]), k -> GS[k]));
    od;
    Add(J[i], UfJoin(lParts[i], (UfJoin(invParts, R[i]))));
    Add(D[i], List(RootsOfPartitionDS(J[i][i]), k -> GS[k]));
    Print(Concatenation(["\rFound Join for Element ", String(i), " of ",
      String(tLength)]));
  od;
  Print("\033[2K\rFound All Joins\n");
  pairs := [];

  for i in [tLength, tLength - 1 .. 1] do
    for j in [i - 1, i - 2 .. 1] do
      for g in D[i][j] do
        x := act(T[i], g);
        y := T[j];
        Add(pairs, [x, y]);
      od;
    od;
    for g in D[i][i] do
      y := T[i];
      x := act(y, g);
      if x <> y then
        Add(pairs, [x, y]);
      fi;
    od;
    Print(Concatenation(["\rFound Pairs for Element ", String(tLength - i + 1),
      " of ", String(tLength)]));
  od;
  Print("\033[2K\rFound All Pairs\n");
  return pairs;
end;

OrbitsOfPairsWithCount := function(G, S, act)
  local T, tLength, A, GS, J, D, R, L, invPartition, invPartUF, invParts, i,
    lParts, j, pairs, g, x, y;
  T := List(Orbits(G, S, act), l -> l[1]);
  tLength := Length(T);
  A := List(T, t -> Stabiliser(G, t, act));
  GS := AsSet(G);
  J := [];
  D := [];
  R := [];
  L := [];
  invPartition := PartitionInverses(G);
  invPartUF := UfFromPartition(GS, invPartition);
  invParts := PartsOfPartitionDS(invPartUF);
  for i in [1 .. tLength] do
    Add(R, PartitionRightCosets(G, GS, A[i]));
    Add(L, PartitionLeftCosets(G, GS, A[i]));
    Print(Concatenation(["\rPartitioned Cosets ", String(i), " of ",
      String(tLength)]));
  od;
  Print("\033[2K\rPartitioned All Cosets\n");
  lParts := List(L, PartsOfPartitionDS);

  for i in [1 .. tLength] do
    Add(J, []);
    Add(D, []);
    for j in [1 .. i - 1] do
      Add(J[i], UfJoin(lParts[j], R[i]));
      Add(D[i], List(RootsOfPartitionDS(J[i][j]), k -> GS[k]));
    od;
    Add(J[i], UfJoin(lParts[i], (UfJoin(invParts, R[i]))));
    Add(D[i], List(RootsOfPartitionDS(J[i][i]), k -> GS[k]));
    Print(Concatenation(["\rFound Join for Element ", String(i), " of ",
      String(tLength)]));
  od;
  Print("\033[2K\rFound All Joins\n");
  pairs := [];

  for i in [tLength, tLength - 1 .. 1] do
    for j in [i - 1, i - 2 .. 1] do
      for g in D[i][j] do
        x := act(T[i], g);
        y := T[j];
        Add(pairs, [[x, y], Size(Intersection(A[i] ^ g, A[j]))]);
      od;
    od;
    for g in D[i][i] do
      y := T[i];
      x := act(y, g);
      if x <> y then
        Add(pairs, [[x, y], Size]);
      fi;
    od;
    Print(Concatenation(["\rFound Pairs for Element ", String(tLength - i + 1),
      " of ", String(tLength)]));
  od;
  Print("\033[2K\rFound All Pairs\n");
  return pairs;
end;

# OrbitsOfPairsIterator := undefined;
# 
# OrbitsOfPairsWithCountIterator := undefined;
