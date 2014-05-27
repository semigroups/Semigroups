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

InstallMethod(SEEData, "for a finite semigroup with generators",
[IsFinite and IsSemigroup and HasGeneratorsOfSemigroup], 
function(S)
  local data, nr, val, i;

  # JDM: add a test to see if the elements of S are hashable, in the sense that
  # the value returned by ChooseHashFunction(S.1).func is not
  # ORB_HashFunctionReturn1. If it turns out that the version of this algorithm
  # which uses sets (to be implemented in the library) rather than hash tables
  # is faster in this case, then we should use that method instead. 

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

  return Objectify(NewType(FamilyObj(S), IsSEEData and IsMutable), data);
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
            if len+1=Length(words[right[i][j]]) and j=words[right[i][j]][len+1] then 
              reduced[i][j]:=true;
              for k in lentoapply do 
                if words[i][k]<>words[right[i][j]][k] then 
                  reduced[i][j]:=false;
                  break;
                fi;
              od;
            fi;
          elif r=one then               # <elts[r]> is the identity
            right[i][j]:=genslookup[b]; 
            reduced[i][j]:=true;        # <elts[i]*gens[j]=b> and it is reduced
          else # prefix[r]=0, i.e. elts[r] is one of the generators
            right[i][j]:=right[genslookup[b]][final[r]];
            # elts[i]*gens[j]=gens[b]*gens[final[r]];
            # reduced[i][j]=([words[i],j]=words[right[i][j]])
            reduced[i][j]:=false;
            if len+1=Length(words[right[i][j]]) and j=words[right[i][j]][len+1] then 
              reduced[i][j]:=true;
              for k in lentoapply do 
                if words[i][k]<>words[right[i][j]][k] then 
                  reduced[i][j]:=false;
                  break;
                fi;
              od;
            fi;
             
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
      if i=stopper or (looking and found) then 
        break;
      fi;
    od; # finished words of length <len> or <looking and found>
    if i=stopper or (looking and found) then 
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

InstallMethod(ClosureSemigroup, 
"for a finite semigroup and associative element collection",
[IsSemigroup and IsFinite, IsAssociativeElementCollection],
function(S, coll)
 
  if IsEmpty(coll) then 
    return S;
  fi;

  if not ElementsFamily(FamilyObj(S))=FamilyObj(Representative(coll)) then 
    Error("the semigroup and collection of elements are not of the same type,");
    return;
  fi;
  
  if IsSemigroup(coll) then 
    coll:=Generators(coll);
  fi;

  return ClosureNonActingSemigroupNC(S, Filtered(coll, x-> not x in S));
end);


# <coll> should consist of elements not in <S>

# we add the new generators <coll> to the data structure for <S>, and we
# rerun Enumerate on the new data structure until we have applied all the
# generators (old and new) to the old elements of <S>. This is more complicated
# than it might first appear since Enumerate relies on having found all the
# words of one length before moving to the next length.

InstallGlobalFunction(ClosureNonActingSemigroupNC, 
function(S, coll)
  local T, data, oldpos, oldnr, oldnrgens, oldwords, nr, elts, gens, genslookup, genstoapply, right, left, ht, first, final, prefix, suffix, reduced, words, wordsoflen, nrwordsoflen, maxwordlen, len, rules, nrrules, newgenstoapply, one, val, htadd, htvalue, seen, nrseen, pos, lentoapply, i, b, s, r, new, newword, p, k, j;
  
  if IsEmpty(coll) then 
    Info(InfoSemigroups, 2, "every element in the collection belong to the ",
    " semigroup,");
    return S;
  fi;

  if IsBound(S!.opts) then 
    if IsMonoid(S) and One(coll)=One(S) then 
      # it can be that these One's differ, and hence we shouldn't call Monoid here
      T:=Monoid(S, coll, S!.opts);    #JDM: safe?
    else
      T:=Semigroup(S, coll, S!.opts); #JDM: safe?
    fi;
  else
    if IsMonoid(S) and One(coll)=One(S) then 
      # it can be that these One's differ, and hence we shouldn't call Monoid here
      T:=Monoid(S, coll); 
    else
      T:=Semigroup(S, coll); 
    fi;
  fi;

  if not HasSEEData(S) then  
    # JDM could add check that SEEData(S) is not just created and nothing has
    # been enumerated nothing is known about <S>
    return T;
  fi;
  
  data:=SEEData(S);

  # parts that will change but where we also require the old values...
  oldpos:=data!.pos;                # so we can tell when we've finished
  oldnr:=data!.nr;                  # so we discriminate old points from new ones
  oldnrgens:=Length(data!.gens);
  oldwords:=data!.words;
  
  data:=StructuralCopy(data);
  
  # parts of the data which stay the same from the old semigroup to the new...
  nr:=data!.nr;                     # nr=Length(elts);
  elts:=data!.elts;                 # the so far enumerated elements
  gens:=data!.gens;                 # the generators
  genslookup:=data!.genslookup;     # genslookup[i]=Position(elts, gens[i])
                                    # this is not always <i>!
  genstoapply:=data!.genstoapply;
  right:=data!.right;               # elts[right[i][j]]=elts[i]*gens[j], right Cayley graph
  left:=data!.left;                 # elts[left[i][j]]=gens[j]*elts[i], left Cayley graph
  nr:=data!.nr;                     # nr=Length(elts);
  ht:=data!.ht;                     # HTValue(ht, x)=Position(elts, x)

  # parts of the data which do not stay the same from the old to the new...
  
  first:=[]; final:=[]; prefix:=[]; suffix:=[]; reduced:=[]; words:=[];

  for i in genslookup do 
    first[i]:=data!.first[i];       final[i]:=data!.final[i];
    prefix[i]:=data!.prefix[i];     suffix[i]:=data!.suffix[i];
    reduced[i]:=data!.reduced[i];   Append(reduced[i], BlistList([1..Length(coll)], []));
    words[i]:=data!.words[i];
  od;

  data!.first:=first;       data!.final:=final;
  data!.prefix:=prefix;     data!.suffix:=suffix;
  data!.reduced:=reduced;   data!.words:=words;               

  wordsoflen:=[data!.wordsoflen[1]];# wordsoflen[len]=list of positions in <words>
  data!.wordsoflen:=wordsoflen;     # of length <len>
                                    
                                    #nrwordsoflen[len]=Length(wordsoflen[len]);
  nrwordsoflen:=[data!.nrwordsoflen[1]];
  data!.nrwordsoflen:=nrwordsoflen;

  maxwordlen:=1;                    # the maximum length of a word...
  len:=1;
  rules:=[];                        # the relations
  nrrules:=0;                       # Length(rules)
 
  # update the generators etc...
  Append(gens, coll);
  newgenstoapply:=[oldnrgens+1..Length(gens)];
  Append(genstoapply, newgenstoapply);

  ResetFilterObj(data, IsClosedSEEData);
  
  # <elts[one]> is the mult. neutral element
  one:=data!.one;
  if one<>false and elts[one]<>One(gens) then 
    one:=false;                 
  fi;
 
  # append the elements of <coll> to <data>
  for i in [1..Length(coll)] do 
    val:=HTValue(ht, coll[i]);
    if val=fail then #still have to check in case there are duplicates in coll
      nr:=nr+1; 
      HTAdd(ht, coll[i], nr); 
      elts[nr]:=coll[i];
      words[nr]:=[oldnrgens+i];  
      first[nr]:=oldnrgens+i;    
      final[nr]:=oldnrgens+i;
      prefix[nr]:=0;   
      suffix[nr]:=0;
      left[nr]:=[];    
      right[nr]:=[];
      genslookup[oldnrgens+i]:=nr;
      reduced[nr]:=BlistList([1..Length(gens)], []);
      nrwordsoflen[1]:=nrwordsoflen[1]+1;
      wordsoflen[1][nrwordsoflen[1]]:=nr;
      if one=false and ForAll(gens, y-> coll[i]*y=y and y*coll[i]=y) then 
        one:=nr;
      fi;
    else 
      genslookup[oldnrgens+i]:=val;
      nrrules:=nrrules+1;
      rules[nrrules]:=[[oldnrgens+i], [val]];
    fi;
  od;
 
  if IsBoundGlobal("ORBC") then 
    htadd:=HTAdd_TreeHash_C;
    htvalue:=HTValue_TreeHash_C;
  else
    htadd:=HTAdd;
    htvalue:=HTValue;
  fi;
  
  seen:=BlistList([1..oldnr], genslookup);
  nrseen:=oldnrgens;
  pos:=0;

  while nrseen<=oldpos and len<=maxwordlen do 
    lentoapply:=[1..len];
    for k in [1..nrwordsoflen[len]] do 
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
            # check if Concatenation(words[i], [j])=words[right[i][j]];
            if len+1=Length(words[right[i][j]]) and j=words[right[i][j]][len+1] then 
              reduced[i][j]:=true;
              for k in lentoapply do 
                if words[i][k]<>words[right[i][j]][k] then 
                  reduced[i][j]:=false;
                  break;
                fi;
              od;
            fi;
          elif r=one then               # <elts[r]> is the identity
            right[i][j]:=genslookup[b]; 
            reduced[i][j]:=true;        # <elts[i]*gens[j]=b> and it is reduced
          else # prefix[r]=0, i.e. elts[r] is one of the generators
            right[i][j]:=right[genslookup[b]][final[r]];
            # elts[i]*gens[j]=gens[b]*gens[final[r]];
            # reduced[i][j]=([words[i],j]=words[right[i][j]])
            reduced[i][j]:=false;
            # check if Concatenation(words[i], [j])=words[right[i][j]];
            if len+1=Length(words[right[i][j]]) and j=words[right[i][j]][len+1] then 
              reduced[i][j]:=true;
              for k in lentoapply do 
                if words[i][k]<>words[right[i][j]][k] then 
                  reduced[i][j]:=false;
                  break;
                fi;
              od;
            fi;
             
          fi;
        elif j<=oldnrgens and IsBound(right[i]) and IsBound(right[i][j]) then 
        #<elts[s]*gens[j]> is reduced and this is an old element multiplied by
        #an old generator
          val:=right[i][j];
          if seen[val] then  
            # there is a new shorter word which is equal to <elts[val]>
            nrrules:=nrrules+1;
            rules[nrrules]:=[oldwords[val], words[val]];
          else
            # <elts[right[i][k]]> equals <oldwords[val]> i.e. it is the
            # same product of the generators as in <S>
            seen[val]:=true;
            nrseen:=nrseen+1;
            if s<>0 then 
              suffix[val]:=right[s][j];
            else 
              suffix[val]:=genslookup[j];
            fi;
            words[val]:=oldwords[val];
            first[val]:=b;
            final[val]:=j;
            prefix[val]:=i;
            reduced[i][j]:=true;
            reduced[val]:=BlistList(genstoapply, []);

            if not IsBound(wordsoflen[len+1]) then 
              maxwordlen:=len+1;
              wordsoflen[len+1]:=[];
              nrwordsoflen[len+1]:=0;
            fi;
            nrwordsoflen[len+1]:=nrwordsoflen[len+1]+1;
            wordsoflen[len+1][nrwordsoflen[len+1]]:=val;
          fi;
          
        else 
          # <elts[s]*gens[j]> is reduced and this is a new element or a new
          # generator
          new:=elts[i]*gens[j];
          # <newword>=<elts[i]*gens[j]>
          newword:=words[i]{lentoapply}; # better than ShallowCopy
          newword[len+1]:=j;             # using Concatenate here is very bad!
          val:=htvalue(ht, new);
          
          if val<>fail then 
            if val>oldnr or seen[val] then 
              nrrules:=nrrules+1;
              rules[nrrules]:=[newword, words[val]];
              right[i][j]:=val;
            else 
              # this is a new-old element, and <newword> has length less than oldwords[val]
              seen[val]:=true;
              if IsBound(right[val]) and IsBound(right[val][1]) then 
                nrseen:=nrseen+1;
              fi;
              if s<>0 then 
                suffix[val]:=right[s][j];
              else 
                suffix[val]:=genslookup[j];
              fi;
              words[val]:=newword;
              first[val]:=b;
              final[val]:=j;
              prefix[val]:=i;
              right[i][j]:=val;
              reduced[i][j]:=true;
              reduced[val]:=BlistList(genstoapply, []);
              # don't have to do right/left[val]:=[] since this is already set!

              if not IsBound(wordsoflen[len+1]) then 
                maxwordlen:=len+1;
                wordsoflen[len+1]:=[];
                nrwordsoflen[len+1]:=0;
              fi;
              nrwordsoflen[len+1]:=nrwordsoflen[len+1]+1;
              wordsoflen[len+1][nrwordsoflen[len+1]]:=right[i][j];
            fi;
            
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
          fi;
        fi;
      od; # finished applying gens to <elts[i]>
      if nrseen=oldpos then 
        break;
      fi;
    od; # finished words of length <len> or <nrseen=oldpos>
    if nrseen=oldpos then 
      break;
    fi;
    # process words of length <len> into <left>
    if len>1 then 
      for j in wordsoflen[len] do # loop over all words of length <len-1>
        p:=prefix[j]; b:=final[j];
        if IsBound(left[j]) and IsBound(left[j][1]) then # old element
          for k in newgenstoapply do #only loop over new gens
            left[j][k]:=right[left[p][k]][b];
            # gens[k]*elts[j]=(gens[k]*elts[p])*gens[b]
          od;
        else
          for k in genstoapply do 
            left[j][k]:=right[left[p][k]][b];
            # gens[k]*elts[j]=(gens[k]*elts[p])*gens[b]
          od;
        fi;
      od;
    elif len=1 then 
      for j in wordsoflen[len] do  # loop over all words of length <1>
        b:=final[j];
        if IsBound(left[j]) and IsBound(left[j][1]) then # old element
          for k in newgenstoapply do #only loop over new gens
            left[j][k]:=right[genslookup[k]][b];
            # gens[k]*elts[j]=(gens[k]*elts[p])*gens[b]
          od;
        else
          for k in genstoapply do 
            left[j][k]:=right[genslookup[k]][b];
            # gens[k]*elts[j]=gens[k]*gens[b]
          od;
        fi;
      od;
    fi;
    len:=len+1;
    k:=0;
  od;
  
  data!.nr:=nr;    
  data!.nrrules:=nrrules;
  data!.one:=one;  
  data!.pos:=pos;
  data!.ind:=k+1; #Position([1..nrwordsoflen[len]], i); # JDM! bad
  data!.maxwordlen:=maxwordlen;
  data!.len:=len;

  if len>maxwordlen then
    data!.len:=maxwordlen;
    SetFilterObj(data, IsClosedSEEData);
  else 
    data!.len:=len;
  fi;

  # remove the parts of the data that shouldn't be there!
  # Note that <data> can be closed at this point if the old data was closed.
  if len<=Length(nrwordsoflen) then 
    Unbind(data!.rightscc);
    Unbind(data!.leftscc);
    Unbind(data!.idempotents); 
    #JDM <idempotents> could be kept...
  else 
    SetFilterObj(data, IsClosedSEEData);
  fi;
  SetSEEData(T, data);
  return T;
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

InstallMethod(PrintObj, [IsSEEData], 
function(data)
  local recnames, com, i, nam;
  
  recnames:=[ "elts", "final", "first", "found", "gens", "genslookup", "genstoapply", 
  "ht", "left", "len", "wordsoflen", "maxwordlen", "nr", "nrrules", "one",
  "pos", "prefix", "reduced", "right", "rules", "stopper", "suffix", "words",
  "nrwordsoflen", "ind"];
  
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

