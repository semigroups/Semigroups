

# to get the R-class of a semigroup you do GabowSCC(S!.semigroup.right)
# to get the L-class of a semigroup you do GabowDClasses(S!.semigroup.left, GabowSCC(S!.semigroup.right))

# or I suppose we could do vice versa if the L-classes are requested first. 

# the scc index 1 corresponds to the "deepest" scc, i.e. the minimal ideal in
# our case...
GabowSCC:=function(digraph)   
  local stack1, len1, stack2, len2, marked, preorder, id, pre, count, dfs, v;

  stack1:=[]; len1:=0;
  stack2:=[]; len2:=0;
  marked:=BlistList([1..Length(digraph)], []);
  preorder:=[];
  id:=[1..Length(digraph)]*0;
  pre:=1;
  count:=1;
  
  #
  dfs:=function(graph, v)
    local w;

    marked[v]:=true;
    preorder[v]:=pre;
    pre:=pre+1;
    len1:=len1+1;
    len2:=len2+1;
    stack1[len1]:=v;
    stack2[len2]:=v;
    
    for w in digraph[v] do 
      if not marked[w] then 
        dfs(digraph, w);
      elif id[w]=0 then 
        while preorder[stack2[len2]] > preorder[w] do
          len2:=len2-1; # pop from stack2
        od;
      fi;
    od;

    if stack2[len2]=v then
      len2:=len2-1;
      
      repeat
        w:=stack1[len1];
        id[w]:=count;
        len1:=len1-1; #pop from stack1
      until w=v;
      count:=count+1;
    fi;
    #Print(id, "\n");
  end;
  #

  for v in [1..Length(digraph)] do 
    if not marked[v] then 
      dfs(digraph, v);
    fi;
  od;

  return rec(id:=id, preorder:=preorder, count:=count);
end;

NonRecursiveGabowSCC:=function(digraph)   
  local stack1, len1, stack2, len2, marked, preorder, id, pre, count, level, branch, wstack, deeper, w, v;

  stack1:=[]; len1:=0;
  stack2:=[]; len2:=0;
  marked:=BlistList([1..Length(digraph)], []);
  preorder:=[];
  id:=[1..Length(digraph)]*0;
  pre:=1;
  count:=1;
  
  for v in [1..Length(digraph)] do 
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
        #Print("here0\n");
        #Print("branch[level]=", branch[level], "\n");
        #Print("wstack[level]=", wstack[level], "\n");
        #Print("Length(digraph[v])=", Length(digraph[v]), "\n");
        v:=branch[level];
        deeper:=false;
        for w in [wstack[level]..Length(digraph[v])] do 
          if not marked[digraph[v][w]] then 
            #Print("here1\n");
            wstack[level]:=w+1; # where we restart when we get back here...
            level:=level+1;
            branch[level]:=digraph[v][w];
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
            #dfs(digraph, w);
          elif id[digraph[v][w]]=0 then 
            #Print("here2\n");
            while preorder[stack2[len2]] > preorder[digraph[v][w]] do
              #Print("len2=", len2, "\n");
              #Print("preorder[stack2[len2]]=", preorder[stack2[len2]], "\n");
              #Print("preorder[graph[v][w]]=", preorder[digraph[v][w]], "\n");

              len2:=len2-1; # pop from stack2
            od;
          fi;
        od;
        
        if not deeper then 
          if stack2[len2]=branch[level] then
            len2:=len2-1;
            #Print("here3"); 
            repeat
              w:=stack1[len1];
              id[w]:=count;
              len1:=len1-1; #pop from stack1
            until w=branch[level];
            count:=count+1;
          fi;
          level:=level-1;
        fi;
      od;
    fi;
  od;

  return rec(id:=id, preorder:=preorder, count:=count);
end;

# <scc> is the output of GabowSCC applied to right

GabowDClasses:=function(left, scc)   
  local stack1, len1, stack2, len2, marked, preorder, id, pre, count, nr, sccid, did, dfs, v;

  # for the scc of <left>
  stack1:=[]; len1:=0;
  stack2:=[]; len2:=0;
  marked:=BlistList([1..Length(left)], []);
  preorder:=[];
  id:=[1..Length(left)]*0;
  pre:=1;
  count:=1;

  nr:=1; # nr of D-classes
  sccid:=scc.id;
  did:=[1..scc.count-1]*0; # lookup for scc of R-classes
  
  #
  dfs:=function(left, v)
    local w;

    marked[v]:=true;
    preorder[v]:=pre;
    pre:=pre+1;
    len1:=len1+1;
    len2:=len2+1;
    stack1[len1]:=v;
    stack2[len2]:=v;
    
    for w in left[v] do 
      if not marked[w] then 
        dfs(left, w);
      elif id[w]=0 then 
        while preorder[stack2[len2]] > preorder[w] do
          len2:=len2-1; # pop from stack2
        od;
      fi;
    od;

    if stack2[len2]=v then
      len2:=len2-1;
      
      if did[sccid[stack1[len1]]]=0 then # i.e. didn't see the scc of the R-class of <w>
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
  end;
  #

  for v in [1..Length(left)] do 
    if not marked[v] then 
      dfs(left, v);
    fi;
  od;

  return [rec(id:=id, preorder:=preorder, count:=count), did];
end;

NonRecursiveGabowDClasses:=function(left, scc)   
  local stack1, len1, stack2, len2, marked, preorder, id, pre, count, nr, sccid, did, level, branch, wstack, v, deeper, w;

  stack1:=[]; len1:=0;
  stack2:=[]; len2:=0;
  marked:=BlistList([1..Length(left)], []);
  preorder:=[];
  id:=[1..Length(left)]*0;
  pre:=1;
  count:=1;

  nr:=1; # nr of D-classes
  sccid:=scc.id;
  did:=[1..scc.count-1]*0; # lookup for scc of R-classes
  
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

  return [rec(id:=id, preorder:=preorder, count:=count), did];
end;

