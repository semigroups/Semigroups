###########################################################################
##
#W  semigroupe.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# SEEData = Semigroup Exhaustive Enumeration Data structure

#  for details see:
#
#  V. Froidure, and J.-E. Pin, Algorithms for computing finite semigroups.
#  Foundations of computational mathematics (Rio de Janeiro, 1997), 112-126,
#  Springer, Berlin,  1997.

# the following are GAP versions of some kernel functions used below, these are
# where most of the work in finding Green's relations/classes is done. 

# the scc index 1 corresponds to the "deepest" scc, i.e. the minimal ideal in
# our case...

if not IsBound(GABOW_SCC) then # non-recursive version below...
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

if not IsBound(SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS) then
  BindGlobal("SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS", 
  function(scc1, scc2)  
    local id1, comps1, id2, comps2, id, comps, nr, seen, comp, i, j;

    comps1:=scc1.comps;
    id2:=scc2.id;
    comps2:=scc2.comps;

    id:=[1..Length(scc1.id)]*0;
    comps:=[];
    nr:=0;
    
    seen:=BlistList([1..Length(comps2)], []);

    for comp in comps1 do 
      if id[comp[1]]=0 then 
        nr:=nr+1;
        comps[nr]:=[];
        for i in comp do
          if not seen[id2[i]] then 
            seen[id2[i]]:=true;
            for j in comps2[id2[i]] do 
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

    return rec(comps:=comps, id:=id);
  end);
fi;

if not IsBound(FIND_HCLASSES) then
  BindGlobal("FIND_HCLASSES", 
  function(left, right)
    local rightid, leftid, comps, nextpos, len, sorted, hindex, rindex, id,
     lookup, j, init, i;
    
    rightid:=right.id;
    leftid:=left.id;

    comps:=right.comps;
    nextpos:=EmptyPlist(Length(comps));
    nextpos[1]:=1;
    for i in [2..Length(comps)] do 
      nextpos[i]:=nextpos[i-1]+Length(comps[i-1]);
    od;

    # List(sorted, i-> right.id[i])= right.id sorted
    len:=Length(rightid);
    sorted:=EmptyPlist(len);
    for i in [1..len] do 
      sorted[nextpos[rightid[i]]]:=i;
      nextpos[rightid[i]]:=nextpos[rightid[i]]+1;
    od;

    hindex:=0;            # current H-class index
    rindex:=0;            # current R-class index
    id:=EmptyPlist(len);  # id component for H-class data structure
    comps:=[];
    lookup:=[1..Length(left.comps)]*0; 
    # H-class corresponding to L-class in the current R-class <now>

    # browse the L-class table...
    for i in [1..len] do
      j:=sorted[i];
      if rightid[j]>rindex then # new R-class
        rindex:=rightid[j]; 
        init:=hindex; 
        # H-class indices for elements of R-class <rindex> start at <init+1>
      fi;
      if lookup[leftid[j]]<=init then 
        # we have a new H-class, otherwise, this is an existing H-class in the 
        # current R-class.
        hindex:=hindex+1;           
        lookup[leftid[j]]:=hindex;
        comps[hindex]:=[];
      fi;
      id[j]:=lookup[leftid[j]];
      Add(comps[lookup[leftid[j]]], j);
    od;

    return rec(id:=id, comps:=comps);
  end);
fi;

# the following functions can be used to access/create the scc data structure
# for R-, L-, D-, and H-classes.

InstallMethod(GreensHClasses, "for SEE data", [IsSEEData], 
function(data)
  return FIND_HCLASSES(GreensRClasses(data), GreensLClasses(data));
end);

InstallMethod(GreensLClasses, "for SEE data", [IsSEEData], 
function(data)
  return GABOW_SCC(Enumerate(data)!.left);
end);

InstallMethod(GreensRClasses, "for SEE data", [IsSEEData], 
function(data)
  return GABOW_SCC(Enumerate(data)!.right);
end);

InstallMethod(GreensDClasses, "for SEE data", [IsSEEData], 
function(data)
  return SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS(GreensRClasses(data),
    GreensLClasses(data));
end);

#

InstallMethod(SEEData, "for a finite semigroup with generators",
[IsFinite and IsSemigroup and HasGeneratorsOfSemigroup], 
function(S)
  local data, nr, val, i;

  data:=rec( elts := [  ], final := [  ], first := [  ], found := false, 
    genslookup := [  ], ind := 1, left := [  ], len := 1, nrrules := 0, 
    nrwordsoflen := [  ], prefix := [  ], reduced := [ [  ] ], right := [  ], 
    rules := [  ], stopper := false, suffix := [  ], words := [  ], 
    wordsoflen := [  ] );

  if IsMonoid(S) then 
    data.gens:=ShallowCopy(GeneratorsOfMonoid(S));
    data.ht:=HTCreate(One(S), rec(treehashsize:=SemigroupsOptionsRec.hashlen.L));
    data.nr:=1; 
    HTAdd(data.ht, One(S), 1); 
    data.elts[1]:=One(S);
    data.words[1]:=[];   
    data.first[1]:=0;    
    data.final[1]:=0;
    data.prefix[1]:=0;   
    data.suffix[1]:=0;
    data.reduced[1]:=BlistList([1..Length(data.gens)], [1..Length(data.gens)]);
    data.one:=1;
    data.pos:=1; # we don't apply generators to the One(S)
    data.left[1]:=data.genslookup;    
    data.right[1]:=data.genslookup;
  else
    data.gens:=ShallowCopy(GeneratorsOfSemigroup(S));
    data.ht:=HTCreate(data.gens[1], rec(treehashsize:=SemigroupsOptionsRec.hashlen.L));
    data.nr:=0;
    data.one:=false;
    data.pos:=0;
  fi;

  data.genstoapply:=[1..Length(data.gens)];

  if Length(data.gens)<>0 then 
    data.maxwordlen:=1;
    data.wordsoflen[1]:=[];
    data.nrwordsoflen[1]:=0;
  else 
    data.maxwordlen:=0;
  fi;
  
  nr:=data.nr;

  # add the generators 
  for i in data.genstoapply do 
    val:=HTValue(data.ht, data.gens[i]);
    if val=fail then # new generator
      nr:=nr+1; 
      HTAdd(data.ht, data.gens[i], nr); 
      data.elts[nr]:=data.gens[i];
      data.words[nr]:=[i];  
      data.first[nr]:=i;    
      data.final[nr]:=i;
      data.prefix[nr]:=0;   
      data.suffix[nr]:=0;
      data.left[nr]:=[];    
      data.right[nr]:=[];
      data.genslookup[i]:=nr;
      data.reduced[nr]:=BlistList(data.genstoapply, []);
      data.nrwordsoflen[1]:=data.nrwordsoflen[1]+1;
      data.wordsoflen[1][data.nrwordsoflen[1]]:=nr;
      
      if data.one=false and ForAll(data.gens, y-> data.gens[i]*y=y and y*data.gens[i]=y) then 
        data.one:=nr;
      fi;
    else # duplicate generator
      data.genslookup[i]:=val;
      data.nrrules:=data.nrrules+1;
      data.rules[data.nrrules]:=[[i], [val]];
    fi;
  od;
  
  data.nr:=nr;

  return Objectify(NewType(FamilyObj(S), IsSEEData and IsMutable and
   IsAttributeStoringRep), data);
end);

# the main algorithm

InstallMethod(Enumerate, "for SEE data", [IsSEEData], 
function(data)
  return Enumerate(data, infinity, ReturnFalse);
end);

#

InstallMethod(Enumerate, "for SEE data and cyclotomic", [IsSEEData, IsCyclotomic], 
function(data, limit)
  return Enumerate(data, limit, ReturnFalse);
end);

# <lookfunc> has arguments <data=S!.semigroupe> and an index <j> in
# <[1..Length(data!.elts)]>.

InstallMethod(Enumerate, "for SEE data, cyclotomic, function",
[IsSEEData, IsCyclotomic, IsFunction], 
function(data, limit, lookfunc)
  local looking, found, len, maxwordlen, nr, elts, gens, genstoapply, genslookup, one, right, left, first, final, prefix, suffix, reduced, words, stopper, ht, rules, nrrules, wordsoflen, nrwordsoflen, pos, i, htadd, htvalue, lentoapply, b, s, r, new, newword, val, p, ind, j, k;
  
  if lookfunc<>ReturnFalse then 
    looking:=true;              # only applied to new elements, not old ones!!!
    data!.found:=false;         # in case we previously looked for something and found it
  else
    looking:=false;
  fi;
  
  found:=false; 
  
  len:=data!.len;                   # current word length
  maxwordlen:=data!.maxwordlen;     # the maximum length of a word so far
  pos:=data!.pos;                   # number of points to which generators have been   
                                    # applied, this is needed in ClosureSemigroup
  nr:=data!.nr;                     # nr=Length(elts);
 
  if pos=nr then
    SetFilterObj(data, IsClosedSEEData);
    if looking then 
      data!.found:=false;
    fi;
    return data;
  fi;
  
  elts:=data!.elts;                 # the so far enumerated elements
  gens:=data!.gens;                 # the generators
  genstoapply:=data!.genstoapply;   # list of indices of generators to apply in inner loop
  genslookup:=data!.genslookup;     # genslookup[i]=Position(elts, gens[i])
                                    # this is not always <i+1>!
  one:=data!.one;                   # <elts[one]> is the mult. neutral element
  right:=data!.right;               # elts[right[i][j]]=elts[i]*gens[j], right Cayley graph
  left:=data!.left;                 # elts[left[i][j]]=gens[j]*elts[i], left Cayley graph
  first:=data!.first;               # elts[i]=gens[first[i]]*elts[suffix[i]], first letter 
  final:=data!.final;               # elts[i]=elts[prefix[i]]*gens[final[i]]
  prefix:=data!.prefix;             # see final, 0 if prefix is empty i.e. elts[i] is a gen
  suffix:=data!.suffix;             # see first, 0 if suffix is empty i.e. elts[i] is a gen
  reduced:=data!.reduced;           # words[right[i][j]] is reduced if reduced[i][j]=true
  words:=data!.words;               # words[i] is a word in the gens equal to elts[i]
  nr:=data!.nr;                     # nr=Length(elts);
  stopper:=data!.stopper;           # stop when we have applied generators to elts[stopper] 
  ht:=data!.ht;                     # HTValue(ht, x)=Position(elts, x)
  rules:=data!.rules;               # the relations
  nrrules:=data!.nrrules;           # Length(rules)
  wordsoflen:=data!.wordsoflen;     # wordsoflen[len]=list of positions in <words>
                                    # of length <len>
  nrwordsoflen:=data!.nrwordsoflen; # nrwordsoflen[len]=Length(wordsoflen[len]);

  ind:=data!.ind;                   # index in wordsoflen[len]
  i:=wordsoflen[len][ind];          # the position in the orbit we are about to
                                    # apply generators to 
  
  if IsBoundGlobal("ORBC") then 
    htadd:=HTAdd_TreeHash_C;
    htvalue:=HTValue_TreeHash_C;
  else
    htadd:=HTAdd;
    htvalue:=HTValue;
  fi;
  
  while nr<=limit and len<=maxwordlen do 
    lentoapply:=[1..len];
    while nr<=limit and len<=maxwordlen and i<>stopper and not (looking and found) then 
    for k in [ind..nrwordsoflen[len]] do 
      pos:=pos+1;
      i:=wordsoflen[len][k];
      b:=first[i];  s:=suffix[i];  # elts[i]=gens[b]*elts[s]

      for j in genstoapply do # consider <elts[i]*gens[j]>
        if s<>0 and not reduced[s][j] then     # <elts[s]*gens[j]> is not reduced
          r:=right[s][j];                      # elts[r]=elts[s]*gens[j]
          if prefix[r]<>0 then 
            right[i][j]:=right[left[prefix[r]][b]][final[r]];
            # elts[i]*gens[j]=gens[b]*elts[prefix[r]]*gens[final[r]];
            # reduced[i][j]=([words[i],j]=words[right[i][j]])
            reduced[i][j]:=false;
          elif r=one then               # <elts[r]> is the identity
            right[i][j]:=genslookup[b]; 
            reduced[i][j]:=true;        # <elts[i]*gens[j]=b> and it is reduced
          else # prefix[r]=0, i.e. elts[r] is one of the generators
            right[i][j]:=right[genslookup[b]][final[r]];
            # elts[i]*gens[j]=gens[b]*gens[final[r]];
            # reduced[i][j]=([words[i],j]=words[right[i][j]])
            reduced[i][j]:=false;
          fi;
        else # <elts[s]*gens[j]> is reduced
          new:=elts[i]*gens[j];
          # <newword>=<elts[i]*gens[j]>
          newword:=words[i]{lentoapply}; # better than ShallowCopy
          newword[len+1]:=j;             # using Concatenate here is very bad!
          val:=htvalue(ht, new);
          
          if val<>fail then 
            nrrules:=nrrules+1;
            rules[nrrules]:=[newword, words[val]];
            right[i][j]:=val;
            # <newword> and <words[val]> represent the same element (but are not
            # equal) and so <newword> is not reduced

          else #<new> is a new element!
            nr:=nr+1;
           
            if one=false and ForAll(gens, y-> new*y=y and y*new=y) then
              one:=nr;
            fi;
            if s<>0 then 
              suffix[nr]:=right[s][j];
            else 
              suffix[nr]:=genslookup[j];
            fi;
            
            elts[nr]:=new;        htadd(ht, new, nr);
            words[nr]:=newword;   
            first[nr]:=b;         final[nr]:=j;
            prefix[nr]:=i;        right[nr]:=[];        
            left[nr]:=[];         right[i][j]:=nr;      
            reduced[i][j]:=true;  reduced[nr]:=BlistList(genstoapply, []);
            
            if not IsBound(wordsoflen[len+1]) then 
              maxwordlen:=len+1;
              wordsoflen[len+1]:=[];
              nrwordsoflen[len+1]:=0;
            fi;
            nrwordsoflen[len+1]:=nrwordsoflen[len+1]+1;
            wordsoflen[len+1][nrwordsoflen[len+1]]:=nr;
            
            if looking and not found then
              if lookfunc(data, nr) then
                found:=true;
                data!.found := nr;
              fi;
            fi;
          fi;
        fi;
      od; # finished applying gens to <elts[i]>

    od; # finished words of length <len> or <looking and found>
    if nr>=limit or i=stopper or (looking and found) then 
      break;
    fi;
    # process words of length <len> into <left>
    if len>1 then 
      for j in wordsoflen[len] do # loop over all words of length <len-1>
        p:=prefix[j]; b:=final[j];
        for k in genstoapply do 
          left[j][k]:=right[left[p][k]][b];
          # gens[k]*elts[j]=(gens[k]*elts[p])*gens[b]
        od;
      od;
    elif len=1 then 
      for j in wordsoflen[len] do  # loop over all words of length <1>
        b:=final[j];
        for k in genstoapply do 
          left[j][k]:=right[genslookup[k]][b];
          # gens[k]*elts[j]=gens[k]*gens[b]
        od;
      od;
    fi;
    len:=len+1;
    ind:=1;
    k:=0;
  od;
  
  data!.nr:=nr;    
  data!.nrrules:=nrrules;
  data!.one:=one;  
  data!.pos:=pos;
  data!.ind:=k+1; 
  data!.maxwordlen:=maxwordlen;

  if len>maxwordlen then
    data!.len:=maxwordlen;
    SetFilterObj(data, IsClosedSEEData);
  else 
    data!.len:=len;
  fi;

  return data;
end);

#

InstallMethod(Position, "for SEE data, an associative element, zero cyc",
[IsSEEData, IsAssociativeElement, IsZeroCyc], 
function(data, x, n)
  local pos, lookfunc;

  pos:=HTValue(data!.ht, x);
  
  if pos<>fail then 
    return pos;
  else
    lookfunc:=function(data, i)
      return data!.elts[i]=x;
    end;
    
    pos:=Enumerate(data, infinity, lookfunc)!.found;
    if pos<>false then 
      return pos;
    fi;
  fi;

  return fail;
end);

#

InstallMethod(Length, "for SEE data", [IsSEEData], 
function(data)
  return Length(data!.elts);
end);

#

InstallMethod(ELM_LIST, "for SEE data, and pos int",
[IsSEEData, IsPosInt], 
function(data, nr)
  return data!.elts[nr];
end);

#

InstallMethod(ViewObj, [IsSEEData], 
function(data)
  Print("<");

  if IsClosedSEEData(data) then 
    Print("closed ");
  else 
    Print("open ");
  fi;

  Print("semigroup data with ", Length(data!.elts), " elements, ");
  Print(Length(data!.rules), " relations, ");
  Print("max word length ", data!.maxwordlen, ">");
  return;
end);

#

InstallMethod(PrintObj, [IsSEEData], 2, # to beat the method for an enumerator!
function(data)
  local recnames, com, i, nam;
  
  recnames:=[ "elts", "final", "first", "found", "gens", "genslookup", "genstoapply", 
  "ht", "left", "len", "wordsoflen", "maxwordlen", "nr", "nrrules", "one",
  "pos", "prefix", "reduced", "right", "rules", "stopper", "suffix", "words",
  "nrwordsoflen", "ind"];
  
  for nam in ["leftscc", "rightscc", "leftrightscc", "hclasses", "idempotents"] do 
    if IsBound(data!.(nam)) then 
      Add(recnames, nam);
    fi;
  od;

  Print("\>\>rec(\n\>\>");
  com := false;
  i := 1;
  for nam in Set(recnames) do
      if com then
          Print("\<\<,\n\>\>");
      else
          com := true;
      fi;
      SET_PRINT_OBJ_INDEX(i);
      i := i+1;
      Print(nam, "\< := \>");
      if nam="ht" then 
        ViewObj(data!.(nam));
      else 
        PrintObj(data!.(nam));
      fi;
  od;
  Print(" \<\<\<\<)"); 
  
  return;
end);


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
