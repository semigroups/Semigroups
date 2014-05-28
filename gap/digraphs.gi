

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

# <scc> is the output of GABOW_SCC applied to right

if not IsBound(GABOW_SCC_DCLASSES) then 
  BindGlobal("GABOW_SCC_DCLASSES",
  function(left, scc)   
    local stack1, len1, stack2, len2, marked, preorder, id, pre, count, nr,
    sccid, did, level, branch, wstack, v, deeper, w;

    stack1:=[]; len1:=0;
    stack2:=[]; len2:=0;
    marked:=BlistList([1..Length(left)], []);
    preorder:=[];
    id:=[1..Length(left)]*0;
    pre:=1;
    count:=1;

    nr:=1; # nr of D-classes
    sccid:=scc.id;
    did:=[1..scc.count]*0; # lookup for scc of R-classes
    
    for v in [1..Length(left)] do 
      if not marked[v] then
        level:=1;
        branch:=[v];
        wstack:=[1];
        marked[branch[level]]:=true;
        preorder[branch[level]]:=pre;
        pre:=pre+1;
        len1:=len1+1;
        len2:=len2+1;
        stack1[len1]:=branch[level];
        stack2[len2]:=branch[level];
        
        while level>0 do 
          v:=branch[level];
          deeper:=false;
          for w in [wstack[level]..Length(left[v])] do 
            if not marked[left[v][w]] then 
              wstack[level]:=w+1; # where we restart when we get back here...
              level:=level+1;
              branch[level]:=left[v][w];
              wstack[level]:=1;
              marked[branch[level]]:=true;
              preorder[branch[level]]:=pre;
              pre:=pre+1;
              len1:=len1+1;
              len2:=len2+1;
              stack1[len1]:=branch[level];
              stack2[len2]:=branch[level];
              deeper:=true;
              break;
              #dfs(left, w);
            elif id[left[branch[level]][w]]=0 then 
              while preorder[stack2[len2]] > preorder[left[branch[level]][w]] do
                len2:=len2-1; # pop from stack2
              od;
            fi;
          od;
          
          if not deeper then 
            if stack2[len2]=branch[level] then
              len2:=len2-1;
              if did[sccid[stack1[len1]]]=0 then 
                # i.e. didn't see the scc of the R-class of <w>
                repeat
                  w:=stack1[len1];
                  id[w]:=count;
                  len1:=len1-1; #pop from stack1
                  did[sccid[w]]:=nr; 
                until w=v;
                nr:=nr+1;
              else
                repeat
                  w:=stack1[len1];
                  id[w]:=count;
                  len1:=len1-1; #pop from stack1
                until w=v;
              fi;
              count:=count+1;
            fi;
            level:=level-1;
          fi;
        od;
      fi;
    od;

    return rec(id:=id, preorder:=preorder, count:=count-1, dclassesid:=did, 
    nrdclasses:=nr-1);
  end);
fi;

# non-recursive versions of the above...

graph:=[[2,6], [3,4,5], [1], [3,5], [1], [7,10,11], [5,8,9], [5], [8], [11], [], [10,11], 
[9,11,15], [13], [14]];

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

#GabowDClasses:=function(left, scc)   
#  local stack1, len1, stack2, len2, marked, preorder, id, pre, count, nr, sccid, did, dfs, v;
#
#  # for the scc of <left>
#  stack1:=[]; len1:=0;
#  stack2:=[]; len2:=0;
#  marked:=BlistList([1..Length(left)], []);
#  preorder:=[];
#  id:=[1..Length(left)]*0;
#  pre:=1;
#  count:=1;
#
#  nr:=1; # nr of D-classes
#  sccid:=scc.id;
#  did:=[1..scc.count-1]*0; # lookup for scc of R-classes
#  
#  #
#  dfs:=function(left, v)
#    local w;
#
#    marked[v]:=true;
#    preorder[v]:=pre;
#    pre:=pre+1;
#    len1:=len1+1;
#    len2:=len2+1;
#    stack1[len1]:=v;
#    stack2[len2]:=v;
#    
#    for w in left[v] do 
#      if not marked[w] then 
#        dfs(left, w);
#      elif id[w]=0 then 
#        while preorder[stack2[len2]] > preorder[w] do
#          len2:=len2-1; # pop from stack2
#        od;
#      fi;
#    od;
#
#    if stack2[len2]=v then
#      len2:=len2-1;
#      
#      if did[sccid[stack1[len1]]]=0 then # i.e. didn't see the scc of the R-class of <w>
#        repeat
#          w:=stack1[len1];
#          id[w]:=count;
#          len1:=len1-1; #pop from stack1
#          did[sccid[w]]:=nr; 
#        until w=v;
#        nr:=nr+1;
#      else
#        repeat
#          w:=stack1[len1];
#          id[w]:=count;
#          len1:=len1-1; #pop from stack1
#        until w=v;
#      fi;
#      count:=count+1;
#    fi;
#  end;
#  #
#
#  for v in [1..Length(left)] do 
#    if not marked[v] then 
#      dfs(left, v);
#    fi;
#  od;
#
#  return [rec(id:=id, preorder:=preorder, count:=count), did];
#end;
