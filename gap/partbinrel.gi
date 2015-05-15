############################################################################
##
#W  partbinrel.gi
#Y  Copyright (C) 2015                                   Attila Egri-Nagy
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## Partitioned Binary Relations
## Binary relations on 2n points with a different multiplication
## as described in: MARTIN, Paul; MAZORCHUK, Volodymyr.
## Partitioned Binary Relations. MATHEMATICA SCANDINAVICA,v113, n1, p. 30-52, 
## http://arxiv.org/abs/1102.0862
##  

# input: a binary relation on 2n points
# the function decomposes the relation into 4 relations on n points
# Dom: 1..n
# Codom: n+1..2n

# JDM: same as bipartitions pos and neg ints, 

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

InstallMethod(\*, "for partitioned binary relations",
[IsPartitionedBinaryRelation, IsPartitionedBinaryRelation],
function(x, y)
  local n, out, x_dfs, y_dfs, i;

  n := x![1];
  
  out := Concatenation([n], List([1 ..  2 * n], x -> []));

  x_dfs := function(i, adj, depth) # starting in x
    local j;

    for j in x![i + 1] do 
      if i <= n and j <= n then 
        if depth = 1 then 
          Add(adj, j);
        fi;
      elif (i > n and j <= n) then 
        Add(adj, j);
      else # (i <= n and j > n) or (i > n and j > n)
        y_dfs(j, adj, depth + 1);
      fi;
    od;
    return;
  end;
  
  y_dfs := function(i, adj, depth) # starting in y
    local j;

    for j in y![i + 1] do 
      if i > n and j > n then 
        if depth = 1 then 
          Add(adj, j);
        fi;
      elif (i > n and j <= n) or (i <= n and j <= n) then 
        x_dfs(j, adj, depth + 1);
      else # i <= n and j > n
        Add(adj, j);
      fi;
    od;
    return;
  end;

  for i in [1 .. n] do # find everything connected to vertex i
    x_dfs(i, out[i + 1], 1);
  od;

  for i in [n .. 2 * n] do # find everything connected to vertex i
    y_dfs(i, out[i + 1], 1);
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
  local str, ext, i;

  str := "\>\>\>\><pbr: \>\>";

  ext := ExtRepOfPBR(x);
  Append(str, "\>\>");
  Append(str, String(ext[1]));
  Append(str, "\<\<");
  Append(str, ", \>\>");
  Append(str, String(ext[2]));
  Append(str, "\<\<");
  Append(str, "\<\<>\<\<\<\<");
  return str;
end);



#InstallMethod(\=, "for partitioned binary relations",
#        [IsPartitionedBinaryRelation, IsPartitionedBinaryRelation],
#function(p1, p2)
#  return p1!.a11 = p2!.a11 and 
#         p1!.a12 = p2!.a12 and 
#         p1!.a21 = p2!.a21 and 
#         p1!.a22 = p2!.a22;
#  end);
#  
#  FlatPBR := function(pbr)
#    return Concatenation([Successors(pbr!.a11),
#                   Successors(pbr!.a12),
#                   Successors(pbr!.a21),
#                   Successors(pbr!.a22)]);
#  end;
#  
#  InstallMethod(\<, "for partitioned binary relations",
#        [IsPartitionedBinaryRelation, IsPartitionedBinaryRelation],
#function(p1, p2)
#    return FlatPBR(p1) < FlatPBR(p2);
#  end);
#
#
#InstallMethod(One, "for a partitioned binary relation",
#[IsPartitionedBinaryRelation],
#function(pbr)
#  local half, tmp;
#  half := DegreeOfBinaryRelation(pbr!.a11);
#  tmp := [];
#  Perform([1..half], function(x) tmp[x] := [half+x]; tmp[half+x]:=[x]; end);
#  return PartitionedBinaryRelation(
#                 BinaryRelationOnPoints(tmp));
#end);
#
#
#InstallMethod( ViewObj,"for partitioned binary relation",
#        [IsPartitionedBinaryRelation and IsPartitionedBinaryRelationRep],
#function(pbr)
#  Print("a11: ", Successors(pbr!.a11), " ");
#  Print("a12: ", Successors(pbr!.a12), " ");
#  Print("a21: ", Successors(pbr!.a21), " ");
#  Print("a22: ", Successors(pbr!.a22), " ");
#end);
#
#InstallMethod(Display,"for partitioned binary relation",
#        [IsPartitionedBinaryRelation and IsPartitionedBinaryRelationRep],
#function( pbr ) ViewObj(pbr); Print("\n"); end);
#
##fully calculated elements
#PartitionedBinaryRelationMonoid := function(n)
#  local binrels;
#  binrels := List(Tuples(Combinations([1..n]),n),
#                  x -> BinaryRelationOnPoints(x));
#  return Semigroup(List(Tuples(binrels,4),
#                 x-> PartitionedBinaryRelation(x[1],x[2],x[3],x[4])));
#end;
