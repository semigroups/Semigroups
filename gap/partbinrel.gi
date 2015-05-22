############################################################################
##
#W  partbinrel.gi
#Y  Copyright (C) 2015                                   Attila Egri-Nagy
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains an intitial implementation of partitioned binary
# relations (PBRs) as defined in:
# 
# MARTIN, Paul; MAZORCHUK, Volodymyr.
# Partitioned Binary Relations. MATHEMATICA SCANDINAVICA, v113, n1, p. 30-52, 
# http://arxiv.org/abs/1102.0862

# Internally a PBR is stored as the adjacency list of digraph with 
# vertices [1 .. 2 * n] for some n. More precisely if <x> is a PBR, then:
# 
#   * <x![1]> is equal to <n>
#   
#   * <x![i + 1]> is the vertices adjacent to <i> 
# 
# The number <n> is the *degree* of <x>.

# TODO IsUniversalPBR, IsEmptyPBR, the embeddings from the paper, 
# IsBipartitionPBR, IsTransformationPBR, IsPartialPermPBR,
# IsDualTransformationPBR, etc

InstallMethod(AsBooleanMat, "for a partitioned binary relation",
[IsPartitionedBinaryRelation],
function(x)
  local n, y, i;
  n := x![1];
  y := EmptyPlist(2 * n);

  for i in [2 ..  2 * n + 1] do 
    Add(y, BlistList([1 .. 2 * n], x![i]));
  od;
  return BooleanMat(y);
end);

InstallMethod(AsPartitionedBinaryRelation, "for an associative element", 
[IsAssociativeElement], x -> AsPartitionedBinaryRelation(AsBipartition(x)));

InstallMethod(AsPartitionedBinaryRelation, "for a bipartition", 
[IsBipartition],
function(x)
  local n, blocks, out, i, block;

  n := DegreeOfBipartition(x); 
  blocks := ExtRepOfBipartition(x);
  out := [[], []];
  
  for block in blocks do 
    for i in block do 
      if i < 0 then 
        i := -i;
        out[2][i] := ShallowCopy(block);
      else 
        out[1][i] := ShallowCopy(block);
      fi;
    od;
  od;

  return CallFuncList(PartitionedBinaryRelation, out);
end);

# TODO use RandomDigraph here! 
# TODO make a method that takes a float between 0 and 1 as the probability of
# an edge existing.

InstallMethod(RandomPartitionedBinaryRelation, "for a pos int", [IsPosInt],
function(n)
  local p, adj, k, i, j;

  # probability of an edge  
  p := Random([0..9999]);
  
  adj := [n];
  for i in [2 .. 2 * n + 1] do
    Add(adj, []);
  od;

  for i in [2 .. 2 * n + 1] do
    for j in [1 .. 2 * n] do
      k := Random(Integers) mod 10000;
      if k < p then
        Add(adj[i], j);
      fi;
    od;
  od;
  return Objectify(PartitionedBinaryRelationType, adj);
end);

InstallGlobalFunction(PartitionedBinaryRelation,
function(arg)
  local left_adj, right_adj, n, i, j;

  arg := ShallowCopy(arg);
  left_adj := arg[1];  # things adjacent to positives
  right_adj := arg[2]; # things adjacent to negatives
  
  n := Length(left_adj);

  for i in [1 .. n] do
    for j in [1 .. Length(left_adj[i])] do
      if left_adj[i][j] < 0 then
        left_adj[i][j] := -left_adj[i][j] + n;
      fi;
    od;
    for j in [1 .. Length(right_adj[i])] do
      if right_adj[i][j] < 0 then
        right_adj[i][j] := -right_adj[i][j] + n;
      fi;
    od;
  od;
  arg := Concatenation([Length(arg[1])], Concatenation(arg));
  return Objectify(PartitionedBinaryRelationType, arg);
end);

InstallMethod(DegreeOfPartitionedBinaryRelation, "for a partitioned binary relation", 
[IsPartitionedBinaryRelation], pbr -> pbr![1]);

# can't we use some sort of Floyd-Warshall Algorithm here, the current method
# involves searching in the same part of the graph repeatedly??

InstallMethod(\*, "for partitioned binary relations",
[IsPartitionedBinaryRelation, IsPartitionedBinaryRelation],
function(x, y)
  local n, out, x_seen, y_seen, empty, x_dfs, y_dfs, i;

  n := x![1];
  
  out := Concatenation([n], List([1 ..  2 * n], x -> []));

  x_seen := BlistList([1 .. 2 * n], []);
  y_seen := BlistList([1 .. 2 * n], []);
  empty  := BlistList([1 .. 2 * n], []);

  x_dfs := function(i, adj) # starting in x
    local j;
    if x_seen[i] then 
      return;
    fi;
    x_seen[i] := true;
    for j in x![i + 1] do 
      if j <= n then 
        AddSet(adj, j);
      else # j > n
        y_dfs(j - n, adj);
      fi;
    od;
    return;
  end;
  
  y_dfs := function(i, adj) # starting in y
    local j;
    if y_seen[i] then 
      return;
    fi;
    y_seen[i] := true;
    for j in y![i + 1] do 
      if j > n then 
        AddSet(adj, j);
      else # j <= n
        x_dfs(j + n, adj);
      fi;
    od;
    return;
  end;

  for i in [1 .. n] do # find everything connected to vertex i
    IntersectBlist(x_seen, empty);
    IntersectBlist(y_seen, empty);
    x_dfs(i, out[i + 1]);
  od;

  for i in [n + 1 .. 2 * n] do # find everything connected to vertex i
    IntersectBlist(x_seen, empty);
    IntersectBlist(y_seen, empty);
    y_dfs(i, out[i + 1]);
  od;

  return Objectify(PartitionedBinaryRelationType, out);
end);

InstallGlobalFunction(ExtRepOfPBR, 
function(x)
  local n, out, i, j, k;
  
  if not IsPartitionedBinaryRelation(x) then 
    Error();
    return;
  fi;
  
  n := x![1];
  out := [[], []];
  for i in [0, 1] do 
    for j in [1 + n * i .. n + n * i] do 
      Add(out[i + 1], []);
      for k in x![j + 1] do 
        if k > n then 
          Add(out[i + 1][j - n * i], - (k - n));
        else
          Add(out[i + 1][j- n * i], k);
        fi;
      od;
    od;
  od;

  return out;
end);

InstallMethod(ViewString, "for a partitioned binary relation",
[IsPartitionedBinaryRelation], 
function(x)
  local str, n, ext, i;

  str := "\>\>\>\>\><pbr:";

  n := DegreeOfPartitionedBinaryRelation(x);
  ext := ExtRepOfPBR(x);
  
  Append(str, "\>");
  Append(str, PRINT_STRINGIFY("[\>", ext[1][1]));
  for i in [2 .. n] do 
    Append(str, ",\< \>");
    Append(str, PrintString(ext[1][i]));
  od;
  Append(str, "\<],\<");
  Append(str, " \>");
  Append(str, PRINT_STRINGIFY("[\>", ext[2][1]));
  for i in [2 .. n] do 
    Append(str, ",\< \>");
    Append(str, PrintString(ext[2][i]));
  od;
  Append(str, "\<]\<");
  Append(str, ">\<\<\<\<\<");
  return str;
end);

InstallMethod(PrintString, "for a partitioned binary relation",
[IsPartitionedBinaryRelation], 
function(x)
  local str, ext;

  str := "\>\>PBR(\>\>";

  ext := ExtRepOfPBR(x);
  Append(str, "\>");
  Append(str, PrintString(ext[1]));
  Append(str, "\<");
  Append(str, ", \>");
  Append(str, PrintString(ext[2]));
  Append(str, "\<");
  Append(str, "\<\<)\<\<");
  return str;
end);

InstallMethod(\=, "for partitioned binary relations",
[IsPartitionedBinaryRelation, IsPartitionedBinaryRelation],
function(x, y)
  local n, i;

  n := x![1];
  for i in [1 .. 2 * n] do 
    if x![i] <> y![i] then 
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(\<, "for partitioned binary relations",
[IsPartitionedBinaryRelation, IsPartitionedBinaryRelation],
function(x, y)
  local n, i;

  n := x![1];
  for i in [1 .. 2 * n] do 
    if x![i] > y![i] then 
      return false;
    fi;
  od;
  return true;
end);

InstallMethod(One, "for a partitioned binary relation",
[IsPartitionedBinaryRelation],
function(x)
  local n, out, i;

  n := x![1];
  out := [n];
  for i in [1 .. n] do 
    out[i + 1] := [i + n];
    out[i + n + 1] := [i];
  od;
  return Objectify(PartitionedBinaryRelationType, out);
end);

#
#
##fully calculated elements
#PartitionedBinaryRelationMonoid := function(n)
#  local binrels;
#  binrels := List(Tuples(Combinations([1..n]),n),
#                  x -> BinaryRelationOnPoints(x));
#  return Semigroup(List(Tuples(binrels,4),
#                 x-> PartitionedBinaryRelation(x[1],x[2],x[3],x[4])));
#end;
