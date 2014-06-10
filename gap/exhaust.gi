###########################################################################
##
#W  exhaust.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains the main algorithms for computing semigroups belonging to
# IsExhaustiveSemigroup. 

#  for details see:
#
#  V. Froidure, and J.-E. Pin, Algorithms for computing finite semigroups.
#  Foundations of computational mathematics (Rio de Janeiro, 1997), 112-126,
#  Springer, Berlin,  1997.

# methods for things declared in the GAP library

# different method for ideals

InstallMethod(Enumerator, "for an exhaustive semigroup with generators",
[IsExhaustiveSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local data, record;
  
  record:=rec();

  record.NumberElement:=function(enum, elt)
    return Position(ExhaustiveData(S), elt);
  end;

  record.ElementNumber:=function(enum, nr)
    data:=ExhaustiveData(S);
    if not IsBound(data!.elts[nr]) then 
      Enumerate(data, nr);
    fi;
    return data!.elts[nr];
  end;

  record.Length:=enum -> Size(S);

  record.AsList:=enum -> Enumerate(ExhaustiveData(S))!.elts;

  record.Membership:=function(enum, elt)
    return Position(ExhaustiveData(S), elt)<>fail;
  end;

  record.IsBound\[\]:=function(enum, nr)
    return IsBound(ExhaustiveData(S)!.elts[nr]);
  end;

  return EnumeratorByFunctions(S, record);
end);    

# different method for ideals

InstallMethod(Size, "for an exhaustive semigroup with generators", 
[IsExhaustiveSemigroup and HasGeneratorsOfSemigroup],
function(S)
  return Length(Enumerate(ExhaustiveData(S), infinity, ReturnFalse)!.elts);
end);

# different method for ideals

InstallMethod(\in, "for an associative element and finite semigroup with generators",
[IsAssociativeElement, IsExhaustiveSemigroup and HasGeneratorsOfSemigroup],
function(x, S)
  return Position(ExhaustiveData(S), x)<>fail;
end);

# different method for ideals
# JDM: can probably do better than this by considering Green's classes.

InstallMethod(Idempotents, "for an exhaustive semigroup with generators",
[IsExhaustiveSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local data, elts, idempotents, nr, i;

  data:=Enumerate(ExhaustiveData(S));

  if not IsBound(data!.idempotents) then 

    elts:=data!.elts;
    idempotents:=EmptyPlist(Length(elts));
    nr:=0;

    for i in [1..Length(elts)] do 
      if elts[i]*elts[i]=elts[i] then 
        nr:=nr+1;
        idempotents[nr]:=i;
      fi;
    od;
    
    data!.idempotents:=idempotents;
    ShrinkAllocationPlist(idempotents);
  fi;

  return data!.elts{data!.idempotents};
end);


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

# same method for ideals

InstallMethod(ExhaustiveData, "for a finite semigroup",
[IsFinite and IsSemigroup], 
function(S)
  local data, hashlen, nrgens, nr, val, i;

  data:=rec( elts := [  ], final := [  ], first := [  ], found := false, 
    genslookup := [  ], left := [  ], len := 1, lenindex := [  ], 
    nrrules := 0, prefix := [  ], reduced := [ [  ] ], right := [  ], 
    rules := [  ], stopper := false, suffix := [  ], words := [  ] );
  
  if HasSemigroupOptions(S) then 
    hashlen:=SemigroupOptions(S).hashlen.L;
  else
    hashlen:=SEMIGROUPS_OptionsRec.hashlen.L;
  fi;

  if IsMonoid(S) then 
    data.gens:=ShallowCopy(GeneratorsOfMonoid(S));
    nrgens:=Length(data.gens);
    data.ht:=HTCreate(One(S), rec(treehashsize:=hashlen));
    nr:=1; 
    HTAdd(data.ht, One(S), 1); 
    data.elts[1]:=One(S);
    data.words[1]:=[];   
    data.first[1]:=0;    
    data.final[1]:=0;
    data.prefix[1]:=0;   
    data.suffix[1]:=0;
    #data.reduced[1]:=BlistList([1..nrgens], [1..nrgens]);
    data.reduced[1]:=List([1..nrgens], ReturnTrue);
    data.one:=1;
    data.pos:=2; # we don't apply generators to the One(S)
    data.left[1]:=data.genslookup;    
    data.right[1]:=data.genslookup;
    data.lenindex[1]:=2;
  else
    data.gens:=ShallowCopy(GeneratorsOfSemigroup(S));
    nrgens:=Length(data.gens);
    data.ht:=HTCreate(data.gens[1], rec(treehashsize:=hashlen));
    nr:=0;
    data.one:=false;
    data.pos:=1;
    data.lenindex[1]:=1;
  fi;

  data.genstoapply:=[1..nrgens];

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
      data.left[nr]:=EmptyPlist(nrgens);    
      data.right[nr]:=EmptyPlist(nrgens);
      data.genslookup[i]:=nr;
      data.reduced[nr]:=List([1..nrgens], ReturnFalse);
      #data.reduced[nr]:=BlistList(data.genstoapply, []);
      
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

  return Objectify(NewType(FamilyObj(S), IsExhaustiveData and IsMutable and
   IsAttributeStoringRep), data);
end);

# the main algorithm

InstallMethod(Enumerate, "for SEE data", [IsExhaustiveData], 
function(data)
  return Enumerate(data, infinity, ReturnFalse);
end);

#

InstallMethod(Enumerate, "for SEE data and cyclotomic", [IsExhaustiveData, IsCyclotomic], 
function(data, limit)
  return Enumerate(data, limit, ReturnFalse);
end);

# <lookfunc> has arguments <data=S!.semigroupe> and an index <j> in
# <[1..Length(data!.elts)]>.

if IsBound(ENUMERATE_SEE_DATA) then 
  InstallMethod(Enumerate, "for SEE data, cyclotomic, function",
  [IsExhaustiveData, IsCyclotomic, IsFunction], 
  function(data, limit, lookfunc)
    
    data:=ENUMERATE_SEE_DATA(data, limit, lookfunc, lookfunc<>ReturnFalse);

    if data!.pos>data!.nr then
      SetFilterObj(data, IsClosedData);
    fi;

    return data;
  end);
else

  InstallMethod(Enumerate, "for SEE data, cyclotomic, function",
  [IsExhaustiveData, IsCyclotomic, IsFunction], 
  function(data, limit, lookfunc)
    local looking, found, i, nr, len, one, stopper, nrrules, elts, gens, nrgens,
    genstoapply, genslookup, lenindex, first, final, prefix, suffix, words,
    right, left, reduced, ht, rules, htadd, htvalue, stop, lentoapply, b, s, r,
    new, newword, val, p, j, k;
    
    if lookfunc<>ReturnFalse then 
      looking:=true;              # only applied to new elements, not old ones!!!
      data!.found:=false;         # in case we previously looked for something and found it
    else
      looking:=false;
    fi;
    
    found:=false; 
    
    i:=data!.pos;                     # current position we are about to apply gens to ...
    nr:=data!.nr;                     # number of elements found so far...
   
    if i>nr then
      SetFilterObj(data, IsClosedData);
      return data;
    fi;
    
    len:=data!.len;                 # current word length
    one:=data!.one;                 # <elts[one]> is the mult. neutral element
    stopper:=data!.stopper;         # stop when we have applied generators to elts[stopper] 
    nrrules:=data!.nrrules;         # Length(rules)
    
    elts:=data!.elts;               # the so far enumerated elements
    gens:=data!.gens;               # the generators
    nrgens:=Length(gens);
    genstoapply:=data!.genstoapply; # list of indices of generators to apply in inner loop
    genslookup:=data!.genslookup;   # genslookup[i]=Position(elts, gens[i])
                                    # this is not always <i+1>!
    lenindex:=data!.lenindex;       # lenindex[len]=position in <words> and <elts> of
                                    # first element of length <len>
    first:=data!.first;             # elts[i]=gens[first[i]]*elts[suffix[i]], first letter 
    final:=data!.final;             # elts[i]=elts[prefix[i]]*gens[final[i]]
    prefix:=data!.prefix;           # see final, 0 if prefix is empty i.e. elts[i] is a gen
    suffix:=data!.suffix;           # see first, 0 if suffix is empty i.e. elts[i] is a gen
    words:=data!.words;             # words[i] is a word in the gens equal to elts[i]
   
    right:=data!.right;             # elts[right[i][j]]=elts[i]*gens[j], right Cayley graph
    left:=data!.left;               # elts[left[i][j]]=gens[j]*elts[i], left Cayley graph
    reduced:=data!.reduced;         # words[right[i][j]] is reduced if reduced[i][j]=true
    ht:=data!.ht;                   # HTValue(ht, x)=Position(elts, x)
    rules:=data!.rules;             # the relations

    if IsBoundGlobal("ORBC") then 
      htadd:=HTAdd_TreeHash_C;
      htvalue:=HTValue_TreeHash_C;
    else
      htadd:=HTAdd;
      htvalue:=HTValue;
    fi;

    stop:=false;
    
    while i<=nr and not stop do 
      lentoapply:=[1..len];
      while i<=nr and Length(words[i])=len and not stop do 
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
              htadd(ht, new, nr);

              if one=false and ForAll(gens, y-> new*y=y and y*new=y) then
                one:=nr;
              fi;
              
              if s<>0 then 
                suffix[nr]:=right[s][j];
              else 
                suffix[nr]:=genslookup[j];
              fi;
              
              elts[nr]:=new;        
              words[nr]:=newword;   
              first[nr]:=b;         
              final[nr]:=j;
              prefix[nr]:=i;        
              right[nr]:=EmptyPlist(nrgens);        
              left[nr]:=EmptyPlist(nrgens);         
              reduced[nr]:=BlistList(genstoapply, []);
              
              right[i][j]:=nr;    
              reduced[i][j]:=true; 
              
              if looking and (not found) and lookfunc(data, nr) then
                found:=true;
                stop:=true;
                data!.found := nr;
              else
                stop:=nr>=limit;
              fi;
            fi;
          fi;
        od; # finished applying gens to <elts[i]>
        stop:=(stop or i=stopper);
        i:=i+1;
      od; # finished words of length <len> or <looking and found>
      if i>nr or Length(words[i])<>len then 
        # process words of length <len> into <left>
        if len>1 then 
          for j in [lenindex[len]..i-1] do # loop over all words of length <len-1>
            p:=prefix[j]; b:=final[j];
            for k in genstoapply do 
              left[j][k]:=right[left[p][k]][b];
              # gens[k]*elts[j]=(gens[k]*elts[p])*gens[b]
            od;
          od;
        elif len=1 then 
          for j in [lenindex[len]..i-1] do  # loop over all words of length <1>
            b:=final[j];
            for k in genstoapply do 
              left[j][k]:=right[genslookup[k]][b];
              # gens[k]*elts[j]=elts[k]*gens[b]
            od;
          od;
        fi;
        len:=len+1;
        lenindex[len]:=i;
      fi;
    od;
    
    data!.nr:=nr;    
    data!.nrrules:=nrrules;
    data!.one:=one;  
    data!.pos:=i;
    data!.len:=len;

    if i>nr then
      SetFilterObj(data, IsClosedData);
      # Unbind some of the unnecessary components here!
    fi;


    return data;
  end);
fi;

#

InstallMethod(Position, "for SEE data, an associative element, zero cyc",
[IsExhaustiveData, IsAssociativeElement, IsZeroCyc], 
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

InstallMethod(Length, "for SEE data", [IsExhaustiveData], 
function(data)
  return Length(data!.elts);
end);

#

InstallMethod(ELM_LIST, "for SEE data, and pos int",
[IsExhaustiveData, IsPosInt], 
function(data, nr)
  return data!.elts[nr];
end);

#

InstallMethod(ViewObj, [IsExhaustiveData], 
function(data)
  Print("<");

  if IsClosedData(data) then 
    Print("closed ");
  else 
    Print("open ");
  fi;

  Print("semigroup data with ", Length(data!.elts), " elements, ");
  Print(data!.nrrules, " relations, ");
  Print("max word length ", Length(data!.words[data!.nr]), ">");
  return;
end);

#

InstallMethod(PrintObj, [IsExhaustiveData], 2, # to beat the method for an enumerator!
function(data)
  local recnames, com, i, nam;
  
  recnames:=[ "elts", "final", "first", "found", "gens", "genslookup",
    "genstoapply", "ht", "left", "len", "lenindex", "nr", "nrrules", "one", "pos",
    "prefix", "reduced", "right", "rules", "stopper", "suffix", "words"];
  
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


