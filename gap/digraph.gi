#############################################################################
##
#W  digraph.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# constructors

InstallMethod(Graph, "for a directed graph", 
[IsDirectedGraph], 
function(graph)
  local adj;

  adj:=function(i, j)
    return j in Adjacencies(graph)[i];
  end;

  return Graph(Group(()), Vertices(graph), OnPoints, adj, true); 
end);

InstallMethod(RandomSimpleDirectedGraph, "for a pos int",
[IsPosInt], 
function(n)
  local verts, adj, nr, i, j;

  verts := [1..n];
  adj := [];
  
  for i in verts do 
    nr := Random(verts);
    adj[i] := [];
    for j in [1..nr] do 
      AddSet(adj[i], Random(verts));
    od;
  od;

  return DirectedGraph(adj);
end);

# <func> must be a function that accepts a record (defining a digraph) and
# returns another such record. 

InstallMethod(DirectedGraph, "for a list and a function", [IsList, IsFunction], 
function(vertices, func)
  local record;
  record:=rec(vertices:=vertices, source:=[], range:=[]);
  return DirectedGraph(func(record));
end);

#

InstallMethod(DirectedGraph, "for a record", [IsRecord], 
function(record)
  local i;
 
  if IsGraph(record) then 
    return DirectedGraph(List(Vertices(record), x-> Adjacency(record, x)));
  fi;

  if not (IsBound(record.source) and IsBound(record.range) and 
    IsBound(record.vertices)) then 
    Error("usage: the argument must be a record with components `source',", 
    " `range', and `vertices'");
    return;
  fi;

  if not (IsList(record.vertices) and IsList(record.source) and
    IsList(record.range)) then 
    Error("usage: the record components 'vertices', 'source'", 
    " and 'range' should be lists,");
    return;
  fi;

  if Length(record.source)<>Length(record.range) then 
    Error("usage: the record components 'source'", 
    " and 'range' should be of equal length,");
    return;
  fi;

  if not ForAll(record.source, x-> x in record.vertices) then 
    Error("usage: every element of the record components 'source'", 
    " must belong to the component 'vertices',");
    return;
  fi;

  if not ForAll(record.range, x-> x in record.vertices) then 
    Error("usage: every element of the record components 'range'", 
    " must belong to the component 'vertices',");
    return;
  fi;
   
  # rewrite the vertices to numbers
  if record.vertices<>[1..Length(record.vertices)] then 
    record.names:=record.vertices;
    record.vertices:=[1..Length(record.names)];
    for i in [1..Length(record.range)] do 
      record.range[i]:=Position(record.names, record.range[i]);
      record.source[i]:=Position(record.names, record.source[i]);
    od;
  else 
    record.names := record.vertices;
  fi;
    
  # make sure that the record.source is sorted
  record.range:=Permuted(record.range, Sortex(record.source));
  
  MakeImmutable(record.range);
  MakeImmutable(record.source);
  MakeImmutable(record.vertices);
  MakeImmutable(record.names);

  return Objectify(DirectedGraphType, record);
end);

InstallMethod(DirectedGraph, "for a list of lists of pos ints", 
[IsList], 
function(adjacencies)
  local record, names, i, j;

  if not ForAll(adjacencies, x -> ForAll(x, i -> IsPosInt(i) 
    and i <= Length(adjacencies)) and IsDuplicateFreeList(x)) then
    Error("usage: the argument must be a list of duplicate free lists of positive", 
    " integers not exceeding the length of the argument,");
    return;
  fi;

  record:=rec( source:=[], range:=[], 
               vertices:=[1..Length(adjacencies)], 
               names:=[1..Length(adjacencies)]);

  for i in [1..Length(adjacencies)] do 
    for j in [1..Length(adjacencies[i])] do 
      Add(record.source, i);
      Add(record.range, adjacencies[i][j]);
    od;
  od;
  
  MakeImmutable(record.range);
  MakeImmutable(record.source);
  MakeImmutable(record.vertices);
  MakeImmutable(record.names);

  ObjectifyWithAttributes(record, DirectedGraphType, Adjacencies, adjacencies);
  return record;
end);

# basic attributes

InstallMethod(Vertices, "for a directed graph", 
[IsDirectedGraph], 
function(graph)
  return graph!.vertices;
end);

InstallMethod(Range, "for a directed graph", 
[IsDirectedGraph], 
function(graph)
  return graph!.range;
end);

InstallMethod(Source, "for a directed graph", 
[IsDirectedGraph], 
function(graph)
  return graph!.source;
end);

InstallMethod(Edges, "for a directed graph",
[IsDirectedGraph], 
function(graph)
  local out, range, source, i;

  out:=EmptyPlist(Length(Range(graph)));
  range:=Range(graph);
  source:=Source(graph);
  for i in [1..Length(source)] do 
    Add(out, [source[i], range[i]]);
  od;
  return out;
end);

InstallMethod(Adjacencies, "for a directed graph",
[IsDirectedGraph], 
function(graph)
  local range, source, out, i;
  
  range:=Range(graph);
  source:=Source(graph);
  out:=List(Vertices(graph), x-> []);

  for i in [1..Length(source)] do 
    AddSet(out[source[i]], range[i]);
  od;

  return out;
end);

# operators

InstallMethod(\=, "for directed graphs",
[IsDirectedGraph, IsDirectedGraph], 
function(graph1, graph2)
  return Vertices(graph1)=Vertices(graph2) and Range(graph1)=Range(graph2) 
   and Source(graph1)=Source(graph2);
end);

# simple means no multiple edges (loops are allowed)

InstallMethod(IsSimpleDirectedGraph, "for a directed graph",
[IsDirectedGraph],
function(graph)
  return IsDuplicateFreeList(Edges(graph));
end);

InstallMethod(ViewString, "for a directed graph",
[IsDirectedGraph], 
function(graph)
  local str;

  str:="<directed graph with ";
  Append(str, String(Length(Vertices(graph))));
  Append(str, " vertices, ");
  Append(str, String(Length(Range(graph))));
  Append(str, " edges>");
  return str;
end);

InstallMethod(PrintString, "for a directed graph",
[IsDirectedGraph], 
function(graph)
  local str, com, i, nam;
  
  str:="DirectedGraph( ";
  
  if IsSimpleDirectedGraph(graph) then 
    return Concatenation(str, PrintString(Adjacencies(graph)), " )");
  fi;
  
  Append(str, "\>\>rec(\n\>\>");
  com := false;
  i := 1;
  for nam in ["range", "source", "vertices"] do
    if com then
      Append(str, "\<\<,\n\>\>");
    else
      com := true;
    fi;
    SET_PRINT_OBJ_INDEX(i);
    i := i+1;
    Append(str, nam);
    Append(str, "\< := \>");
    Append(str, PrintString(graph!.(nam)));
  od;
  Append(str, " \<\<\<\<)");
  return str;
end);

InstallMethod(String, "for a directed graph",
[IsDirectedGraph], 
function(graph)
  local str, com, i, nam;
  
  str:="DirectedGraph( ";
  
  if IsSimpleDirectedGraph(graph) then 
    return Concatenation(str, PrintString(Adjacencies(graph)), " )");
  fi;
  
  Append(str, "rec( ");
  com := false;
  i := 1;
  for nam in ["range", "source", "vertices"] do
    if com then
      Append(str, ", ");
    else
      com := true;
    fi;
    SET_PRINT_OBJ_INDEX(i);
    i := i+1;
    Append(str, nam);
    Append(str, " := ");
    Append(str, PrintString(graph!.(nam)));
  od;
  Append(str, " )");
  return str;
end);

# graph algorithms

InstallMethod(DirectedGraphRemoveLoops, "for a digraph", 
[IsDirectedGraph], 
function(graph)
  local source, range, newsource, newrange, i;
  
  source := Source(graph);
  range := Range(graph);

  newsource := [];
  newrange := [];
  
  for i in [ 1 .. Length(source) ] do 
    if range[i] <> source[i] then 
      Add(newrange, range[i]);
      Add(newsource, source[i]);
    fi;
  od;

  return DirectedGraph(rec( source:=newsource, range:=newrange, 
                            vertices:=ShallowCopy(Vertices(graph))));
end);

InstallMethod(DirectedGraphRemoveEdges, "for a digraph and a list", 
[IsDirectedGraph, IsList], 
function(graph, edges)
  local source, range, newsource, newrange, i;
  
  source := Source(graph);
  range := Range(graph);

  newsource := [ ];
  newrange := [ ];
  
  for i in [ 1 .. Length(source) ] do 
    if not [ source[i], range[i] ] in edges then 
      Add(newrange, range[i]);
      Add(newsource, source[i]);
    fi;
  od;

  return DirectedGraph(rec( source:=newsource, range:=newrange, 
                            vertices:=ShallowCopy(Vertices(graph))));
end);

InstallMethod(DirectedGraphRelabel, "for a digraph and perm",
[IsDirectedGraph, IsPerm], 
function(graph, perm)
  
  if OnSets(Vertices(graph), perm) <> Vertices(graph) then 
    Error("usage: the 2nd argument <perm> must permute ", 
    "the vertices of the 1st argument <graph>,");
    return;
  fi;

  return DirectedGraph(rec( 
    source := ShallowCopy(OnTuples(Source(graph), perm)), 
    range:= ShallowCopy(OnTuples(Range(graph), perm)), 
    vertices:=ShallowCopy(Vertices(graph))));
end);

if IsBound(IS_ACYCLIC_DIGRAPH) then 
  InstallMethod(IsAcyclicDirectedGraph, "for a digraph", 
  [IsDirectedGraph], IS_ACYCLIC_DIGRAPH);
else
  InstallMethod(IsAcyclicDirectedGraph, "for a digraph", 
  [IsDirectedGraph], 
  function(graph)
    local adj, nr, marked1, marked2, stack, level, ii, k, i;

    adj := Adjacencies(graph);

    nr:=Length(adj);
    marked1 := BlistList([1..nr], []);
    marked2 := BlistList([1..nr], []);
    stack:=EmptyPlist(2*nr);

    for i in [1..nr] do
      if Length(adj[i]) = 0 then
        marked1[i]:=true;
      elif not marked1[i] then

        level:=1;
        stack[1]:=i;
        stack[2]:=1;

        while level > 0 do
          ii:=stack[level*2-1];
          k:=stack[level*2];
          if marked2[ii] then
            return false;  # We have just travelled around a cycle
          fi;
                
          # Check whether we've already checked this vertex OR
          # whether we've now chosen all possible branches descending from it
          if marked1[ii] or k > Length(adj[ii]) then
            marked1[ii]:=true;
            level:=level-1;
            if level>0 then
              stack[level*2]:=stack[level*2]+1;
              marked2[stack[level*2-1]]:=false;
            fi;
          else # Otherwise we move onto the next available branch
            marked2[ii]:=true;
            level:=level+1;
            stack[level*2-1]:=adj[ii][k];
            stack[level*2]:=1;
          fi;
        od;
      fi;
    od;
    return true;
  end);
fi;

InstallMethod(Floyd, "for a digraph", 
[IsDirectedGraph],
function(graph)
  local dist, i, j, k, n, m;
  
  # Firstly assuming no multiple edges or loops. Will be easy to include.
  # Also not yet dealing with graph weightings.
  # Algorithm used from Wikipedia.
  # Need discussions on suitability of data structures, etc

  n:=Length(Vertices(graph));
  m:=Length(Edges(graph));
  dist:=List([1..n],x->List([1..n],x->infinity));

  for i in [1..n] do
    dist[i][i]:=0;
  od;
 
  for i in [1..m] do
    dist[Source(graph)[i]][Range(graph)[i]]:=1;
  od;

  for k in [1..n] do
    for i in [1..n] do
      for j in [1..n] do
        if dist[i][k] <> infinity and dist[k][j] <> infinity and dist[i][j] > dist[i][k] + dist[k][j] then
          dist[i][j]:= dist[i][k] + dist[k][j];
        fi;
      od;
    od;
  od;

  return dist;

end);

# returns the vertices (i.e. numbers) of <digraph> ordered so that there are no
# edges from <out[j]> to <out[i]> for all <i> greater than <j>.

if not IsBound(DIGRAPH_TOPO_SORT) then 
  BindGlobal("DIGRAPH_TOPO_SORT", function(graph)
    local adj, nr, marked1, marked2, stack, level, ii, k, i, out;
    
    adj := Adjacencies(graph);
    
    nr:=Length(adj);
    marked1 := BlistList([1..nr], []);
    marked2 := BlistList([1..nr], []);
    stack:=EmptyPlist(2*nr);
    out := EmptyPlist(nr);
    
    for i in [1..nr] do
      if Length(adj[i]) = 0 then
        marked1[i]:=true;
      elif not marked1[i] then

        level:=1;
        stack[1]:=i;
        stack[2]:=1;

        while level > 0 do
          ii:=stack[level*2-1];
          k:=stack[level*2];
          if marked2[ii] then
            SetIsAcyclicDirectedGraph(graph, false);
            Error("the digraph is not acyclic,");
            return; # not an acyclic graph!
          fi;
                
          # Check whether we've already checked this vertex OR
          # whether we've now chosen all possible branches descending from it
          if marked1[ii] or k > Length(adj[ii]) then
            if not marked1[ii] then
              Add(out, ii);
            fi;

            marked1[ii]:=true;
            level:=level-1;
            if level>0 then
              stack[level*2]:=stack[level*2]+1;
              marked2[stack[level*2-1]]:=false;
            fi;
          else # Otherwise we move onto the next available branch
            marked2[ii]:=true;
            level:=level+1;
            stack[level*2-1]:=adj[ii][k];
            stack[level*2]:=1;
          fi;
        od;
      fi;
    od;
    return out;
  end);
fi;
#function(graph, ignoreloops)
#    local adj, nr, out, marked1, marked2, dfs, i;
#    
#    adj := Adjacencies(graph);
#    nr := Length(adj);
#    out := EmptyPlist(nr);
#    marked1 := BlistList([1..nr], []);
#    marked2 := BlistList([1..nr], []);
#
#    dfs := function(i)
#      local j;
#      if marked2[i] then 
#        if not ignoreloops then 
#          Error("the digraph is not acyclic,");
#        fi;
#        return; # not an acyclic graph!
#      fi;
#      if not marked1[i] then 
#        marked2[i]:=true;
#        for j in adj[i] do 
#          dfs(j);
#        od;
#        marked1[i]:=true;
#        marked2[i]:=false;
#        Add(out, i);
#      fi;
#    end;
#
#    for i in [1..nr] do 
#      if not marked1[i] then 
#        dfs(i);
#      fi;
#    od;
#    return out;
#  end);
#fi;

InstallMethod(DirectedGraphTopologicalSort, "for a digraph", 
[IsDirectedGraph], x-> DIGRAPH_TOPO_SORT(x));

# JDM: requires a method for non-acyclic graphs

InstallMethod(DirectedGraphReflexiveTransitiveClosure, 
"for a digraph", [IsDirectedGraph],
function(graph)
  local sorted, vertices, out, trans, adj, v, u;
    
  if not IsSimpleDirectedGraph(graph) then 
    Error("usage: the argument should be a simple directed graph,");
    return;
  fi;

  sorted := DirectedGraphTopologicalSort(graph, true); # ignore loops
  
  vertices := Vertices(graph);
  out := EmptyPlist(Length(vertices));
  trans := EmptyPlist(Length(vertices));
  adj := Adjacencies(graph);

  for v in sorted do 
    trans[v] := BlistList(vertices, [v]);
    for u in adj[v] do 
      trans[v] := UnionBlist(trans[v], trans[u]);
    od;
    out[v] := ListBlist(vertices, trans[v]);
  od;

  return DirectedGraph(out);
end);

# JDM: requires a method for non-acyclic graphs

InstallMethod(DirectedGraphTransitiveClosure, "for a digraph", 
[IsDirectedGraph], 
function(graph)
  local sorted, vertices, out, trans, adj, reflex, v, u;
  
  if not IsSimpleDirectedGraph(graph) then 
    Error("usage: the argument should be a simple directed graph,");
    return;
  fi;

  sorted := DirectedGraphTopologicalSort(graph, true); # ignore loops
  
  vertices := Vertices(graph);
  out := EmptyPlist(Length(vertices));
  trans := EmptyPlist(Length(vertices));
  adj := Adjacencies(graph);

  for v in sorted do 
    trans[v] := BlistList(vertices, [v]);
    reflex := false;
    for u in adj[v] do
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
  
  return DirectedGraph(out);
end);

if IsBound(IS_STRONGLY_CONNECTED_DIGRAPH) then 
  InstallMethod(IsStronglyConnectedDirectedGraph, "for a digraph", 
  [IsDirectedGraph], IS_STRONGLY_CONNECTED_DIGRAPH);
else 
  InstallMethod(IsStronglyConnectedDirectedGraph, "for a digraph", 
  [IsDirectedGraph], 
  function(graph)   
    local adj, n, stack1, len1, stack2, len2, id, count, fptr, level, l, w, v;
    
    adj := Adjacencies(graph);
    n:=Length(adj);
    
    if n=0 then 
      return true;
    fi;

    stack1:=EmptyPlist(n); len1:=0;
    stack2:=EmptyPlist(n); len2:=0;
    id:=[1..n]*0;
    count:=Length(adj);
    fptr:=[];

    for v in [1..Length(adj)] do
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
          if fptr[2*level] > Length(adj[fptr[2*level-1]]) then 
            if stack2[len2]=id[fptr[2*level-1]] then
              if count=Length(adj)+1 then 
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
            w:=adj[fptr[2*level-1]][fptr[2*level]];
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

# the scc index 1 corresponds to the "deepest" scc, i.e. the minimal ideal in
# our case...

if not IsBound(GABOW_SCC) then 
  BindGlobal("GABOW_SCC", 
  function(digraph)   
    local n, stack1, len1, stack2, len2, id, count, comps, fptr, level, l, comp, w, v;
    
    n:=Length(digraph);
    
    if n=0 then 
      return rec( comps:=[], id:=[]);
    fi;

    stack1:=EmptyPlist(n); len1:=0;
    stack2:=EmptyPlist(n); len2:=0;
    id:=[1..n]*0;
    count:=Length(digraph);
    comps:=[];
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
              len2:=len2-1;
              count:=count+1;
              l:=0;
              comp:=[];
              repeat
                w:=stack1[len1];
                id[w]:=count;
                len1:=len1-1; #pop from stack1
                l:=l+1;
                comp[l]:=w;
              until w=fptr[2*level-1];
              ShrinkAllocationPlist(comp);
              MakeImmutable(comp);
              Add(comps, comp);
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

    MakeImmutable(id);
    ShrinkAllocationPlist(comps);
    MakeImmutable(comps);
    return rec(id:=id-Length(digraph), comps:=comps);
  end);
fi;

#

# A few methods which call GRAPE functions
InstallMethod(IsIsomorphicDirectedGraph, "for two digraphs", 
[IsDirectedGraph, IsDirectedGraph],
function(g1, g2)
  return IsIsomorphicGraph( Graph(g1), Graph(g2) );
end);

#

InstallMethod(AutomorphismGroup, "for a digraph",
[IsDirectedGraph],
function(graph)
  return AutomorphismGroup(Graph(graph));
end);

#

InstallMethod(DirectedGraphIsomorphism, "for two digraphs", 
[IsDirectedGraph, IsDirectedGraph],
function(g1, g2)
  # TODO: convert back to directed graphs
  return GraphIsomorphism( Graph(g1), Graph(g2) );
end);

#

#InstallMethod(Girth, "for a digraph",
#[IsDirectedGraph],
#function(graph)
#  return Girth( Graph(graph) );
#end);

#

InstallMethod(Diameter, "for a digraph",
[IsDirectedGraph],
function(graph)
  return Diameter( Graph(graph) );
end);

#

InstallMethod(IsConnectedDigraph, "for a digraph",
[IsDirectedGraph],
function(graph)
  return IsConnectedGraph( Graph(graph) );
end);

#EOF
