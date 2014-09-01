#############################################################################
##
#W  digraph.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(DigraphRelabel, "for a digraph and perm",
[IsDigraph, IsPerm], 
function(digraph, perm)
  local nr, out;
  nr := Length(digraph);
  out := Permuted(digraph, perm);
  Apply(out, x-> OnTuples(x, perm));

  return out;
end);

InstallMethod(DigraphRemoveLoops, "for a digraph", 
[IsDigraph], 
function(digraph)
  local nr, out, i, v;
  
  nr := Length(digraph);
  out := EmptyPlist(nr);

  for i in [1..nr] do 
    out[i]:=[];
    for v in digraph[i] do 
      if v<>i then 
        Add(out[i], v);
      fi;
    od;
  od;
  
  return out;
end);

if IsBound(IS_ACYCLIC_DIGRAPH) then 
  InstallMethod(IsAcyclicDigraph, "for a digraph", 
  [IsDigraph], IS_ACYCLIC_DIGRAPH);
else 
  InstallMethod(IsAcyclicDigraph, "for a digraph", 
  [IsDigraph], 
  function(digraph)  
    local nr, marked1, marked2, dfs, i;

    nr := Length(digraph);
    marked1 := BlistList([1..nr], []);
    marked2 := BlistList([1..nr], []);

    dfs := function(i)
      local j;
      if marked2[i] then 
        return false; # not an acyclic graph!
      fi;
      if not marked1[i] then 
        marked2[i]:=true;
        for j in digraph[i] do 
          if not dfs(j) then 
            return false;
          fi;
        od;
        marked1[i]:=true;
        marked2[i]:=false;
      fi;
      return true;
    end;

    for i in [1..nr] do 
      if not marked1[i] then
        if not dfs(i) then 
          return false;
        fi;
      fi;
    od;
    return true;
  end);
fi;

# returns the vertices (i.e. numbers) of <digraph> ordered so that there are no
# edges from <out[j]> to <out[i]> for all <i> greater than <j>.



if not IsBound(DIGRAPH_TOPO_SORT) then 
  BindGlobal("DIGRAPH_TOPO_SORT", function(digraph, ignoreloops)
    local nr, out, marked1, marked2, dfs, i;

    nr := Length(digraph);
    out := EmptyPlist(nr);
    marked1 := BlistList([1..nr], []);
    marked2 := BlistList([1..nr], []);

    dfs := function(i)
      local j;
      if marked2[i] then 
        if not ignoreloops then 
          Error("the digraph is not acyclic,");
        fi;
        return; # not an acyclic graph!
      fi;
      if not marked1[i] then 
        marked2[i]:=true;
        for j in digraph[i] do 
          dfs(j);
        od;
        marked1[i]:=true;
        marked2[i]:=false;
        Add(out, i);
      fi;
    end;

    for i in [1..nr] do 
      if not marked1[i] then 
        dfs(i);
      fi;
    od;
    return out;
  end);
fi;

InstallMethod(DigraphTopologicalSort, "for a digraph", 
[IsDigraph], x-> DIGRAPH_TOPO_SORT(x, false));

InstallMethod(DigraphTopologicalSort, "for a digraph and boolean", 
[IsDigraph, IsBool], DIGRAPH_TOPO_SORT);

# only works for acyclic digraphs

InstallMethod(DigraphReflexiveTransitiveClosure, 
"for a digraph", [IsDigraph],
function(digraph)
  local sorted, vertices, out, trans, v, u;
  
  sorted := DigraphTopologicalSort(digraph, true); # ignore loops
  vertices := [1..Length(digraph)];
  out := EmptyPlist(Length(digraph));
  trans := EmptyPlist(Length(digraph));

  for v in sorted do 
    trans[v] := BlistList(vertices, [v]);
    for u in digraph[v] do 
      trans[v] := UnionBlist(trans[v], trans[u]);
    od;
    out[v] := ListBlist(vertices, trans[v]);
  od;
  return out;
end);

# only works for acyclic digraphs

InstallMethod(DigraphTransitiveClosure, "for a digraph", 
[IsDigraph], 
function(digraph)
  local sorted, vertices, out, trans, reflex, v, u;

  sorted := DigraphTopologicalSort(digraph, true); # ignore loops
  vertices := [1..Length(digraph)];
  out := EmptyPlist(Length(digraph));
  trans := EmptyPlist(Length(digraph));

  for v in sorted do 
    trans[v] := BlistList(vertices, [v]);
    reflex := false;
    for u in digraph[v] do
      trans[v] := UnionBlist(trans[v], trans[u]);
      if u = v then 
        reflex := true;
      fi;
    od;
    if not reflex then 
      trans[v][v] := false;
    fi; 
    out[v] := ListBlist(vertices, trans[v]);
    trans[v][v] := true;
  od;
  
  return out;
end);

if IsBound(IS_STRONGLY_CONNECTED_DIGRAPH) then 
  InstallMethod(IsStronglyConnectedDigraph, "for a digraph", 
  [IsDigraph], IS_STRONGLY_CONNECTED_DIGRAPH);
else 
  InstallMethod(IsStronglyConnectedDigraph, "for a digraph", 
  [IsDigraph], 
  function(digraph)   
    local n, stack1, len1, stack2, len2, id, count, fptr, level, l, w, v;
    
    n:=Length(digraph);
    
    if n=0 then 
      return true;
    fi;

    stack1:=EmptyPlist(n); len1:=0;
    stack2:=EmptyPlist(n); len2:=0;
    id:=[1..n]*0;
    count:=Length(digraph);
    fptr:=[];

    for v in [1..Length(digraph)] do
      if id[v]=0 then 
        level:=1;
        fptr[1] := v; #fptr[0], vertex
        fptr[2] := 1; #fptr[2], index
        len1:=len1+1;
        stack1[len1]:=v;   
        len2:=len2+1;
        stack2[len2]:=len1; 
        id[v]:=len1;

        while level>0 do
          if fptr[2*level] > Length(digraph[fptr[2*level-1]]) then 
            if stack2[len2]=id[fptr[2*level-1]] then
              if count=Length(digraph)+1 then 
                return false;
              fi;
              len2:=len2-1;
              count:=count+1;
              l:=0;
              repeat
                w:=stack1[len1];
                id[w]:=count;
                len1:=len1-1; #pop from stack1
                l:=l+1;
              until w=fptr[2*level-1];
            fi;
            level:=level-1;
          else
            w:=digraph[fptr[2*level-1]][fptr[2*level]];
            fptr[2*level]:=fptr[2*level]+1;

            if id[w]=0 then 
              level:=level+1;
              fptr[2*level-1]:=w; #fptr[0], vertex
              fptr[2*level]:=1;   #fptr[2], index
              len1:=len1+1;
              stack1[len1]:=w;   
              len2:=len2+1;
              stack2[len2]:=len1; 
              id[w]:=len1;

            else # we saw <w> earlier in this run
              while stack2[len2] > id[w] do
                len2:=len2-1; # pop from stack2
              od;
            fi;
          fi;
        od;
      fi;
    od;

    return true;
  end);
fi;

#EOF
