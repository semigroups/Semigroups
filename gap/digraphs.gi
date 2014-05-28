

# to get the R-class of a semigroup you do GabowSCC(S!.semigroup.right)
# to get the L-class of a semigroup you do GabowDClasses(S!.semigroup.left, GabowSCC(S!.semigroup.right))

# or I suppose we could do vice versa if the L-classes are requested first. 

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

# returns the strongly connected components of union of two graphs <digraph1>
# and <digraph2> with strongly connected components <scc1> and <scc2> (as output
# by GABOW_SCC). 

if not IsBound(SCC_UNION_DIGRAPHS) then
  BindGlobal("SCC_UNION_DIGRAPHS", 
  function(scc1, scc2)  
    local id1, comps1, id2, comps2, id, comps, nr, seen, tst, comp, i, j;

    id1:=scc1.id;
    comps1:=scc1.comps;
    id2:=scc2.id;
    comps2:=scc2.comps;

    id:=[1..Length(id1)]*0;
    comps:=[];
    nr:=0;
    
    seen:=BlistList([1..Length(comps2)], []);
    tst:=0;

    for comp in comps1 do 
      if id[comp[1]]=0 then 
        nr:=nr+1;
        comps[nr]:=[];
        for i in comp do
          if not seen[id2[i]] then 
            seen[id2[i]]:=true;
            for j in comps2[id2[i]] do 
              tst:=tst+1;
              id[j]:=nr;
              Add(comps[nr], j);
            od;
          fi;
        od;
        MakeImmutable(comps[nr]);
        ShrinkAllocationPlist(comps[nr]);
      fi;
    od;
    ShrinkAllocationPlist(comps);
    MakeImmutable(comps);
    ShrinkAllocationPlist(id);
    MakeImmutable(id);

    return rec(comps:=comps, id:=id, tst:=tst);
  end);
fi;

# non-recursive versions of the above...

#graph:=[[2,6], [3,4,5], [1], [3,5], [1], [7,10,11], [5,8,9], [5], [8], [11], [], [10,11], 
#[9,11,15], [13], [14]];

#GABOW_SCC_2:=function(digraph)   
#  local stack1, len1, stack2, len2, id, count, dfs, v;
#
#  stack1:=[]; len1:=0;
#  stack2:=[]; len2:=0;
#  id:=[1..Length(digraph)]*0;
#  count:=Length(digraph);
#  
#  #
#  dfs:=function(graph, v)
#    local w;
#    len1:=len1+1;
#    len2:=len2+1;
#    stack1[len1]:=v;    # all vertices visited in one run 
#    stack2[len2]:=len1; # strictly weakly visited vertices (i.e. 1-way only)
#    
#    id[v]:=len1;
#
#    #Print("v=", v, "\n");
#    #Print("stack1=", stack1{[1..len1]}, "\n");
#    #Print("stack2=", stack2{[1..len2]}, "\n");
#    #Print("id=", id, "\n");
#
#    for w in digraph[v] do 
#      if id[w]=0 then 
#        #Print("dfs on ", w, " from ", v, "\n\n");
#        dfs(digraph, w);
#      else # we saw <w> earlier in this run
#        while stack2[len2] > id[w] do
#          len2:=len2-1; # pop from stack2
#        od;
#        #Print("v=", v, "\n");
#        #Print("id[", w, "]<>0\n");
#        #Print("stack1=", stack1{[1..len1]}, "\n");
#        #Print("stack2=", stack2{[1..len2]}, "\n");
#      fi;
#    od;
#
#    if stack2[len2]=id[v] then
#      len2:=len2-1;
#      count:=count+1;
#      repeat
#        w:=stack1[len1];
#        id[w]:=count;
#        len1:=len1-1; #pop from stack1
#      until w=v;
#      #Print("v=", v, "\n");
#      #Print("stack2[len2]=", v, "\n");
#      #Print("stack1=", stack1{[1..len1]}, "\n");
#      #Print("stack2=", stack2{[1..len2]}, "\n");
#      #Print("id=", id);
#
#    fi;
#    #Print("\n");
#  end;
#  #
#
#  for v in [1..Length(digraph)] do 
#    if id[v]=0 then 
#      dfs(digraph, v);
#    fi;
#  od;
#
#  return rec(id:=id-Length(digraph), count:=count-Length(digraph));
#end;

