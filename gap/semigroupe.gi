
#  for details see:
#
#  V. Froidure, and J.-E. Pin, Algorithms for computing finite semigroups.
#  Foundations of computational mathematics (Rio de Janeiro, 1997), 112-126,
#  Springer, Berlin,  1997.

InstallMethod(PinData, "for a finite semigroup with generators",
[IsFinite and IsSemigroup and HasGeneratorsOfSemigroup], 
function(S)
  local gens, ht, nrgens, genstoapply, stopper, nr, elts, words, lenwords, one, genslookup, first, final, left, prefix, suffix, right, rules, nrrules, reduced, lenindex, pos, val, i;

  if IsMonoid(S) then 
    gens:=ShallowCopy(GeneratorsOfMonoid(S));
    ht:=HTCreate(One(S), rec(treehashsize:=SemigroupsOptionsRec.hashlen.L));
  else
    gens:=ShallowCopy(GeneratorsOfSemigroup(S));
    ht:=HTCreate(gens[1], rec(treehashsize:=SemigroupsOptionsRec.hashlen.L));
  fi;

  nrgens:=Length(gens);
  genstoapply:=[1..nrgens];
  
  stopper:=false;   nr:=0;            one:=false;  
  nrrules:=0;
  
  elts:=[];         words:=[];        lenwords:=[]; 
  first:=[];        final:=[];        left:=[];
  prefix:=[];       suffix:=[];       right:=[];  
  rules:=[];        genslookup:=[];   reduced:=[[]];
  
  if IsMonoid(S) then 
    nr:=1; 
    HTAdd(ht, One(S), 1); 
    elts[1]:=One(S);
    words[1]:=[];   lenwords[1]:=0;
    first[1]:=0;    final[1]:=0;
    prefix[1]:=0;   suffix[1]:=0;
    reduced[1]:=BlistList(genstoapply, genstoapply);
    one:=1;
    pos:=2;
  else 
    pos:=1;
  fi;
    
  for i in genstoapply do 
    val:=HTValue(ht, gens[i]);
    if val=fail then 
      nr:=nr+1; 
      HTAdd(ht, gens[i], nr); 
      elts[nr]:=gens[i];
      words[nr]:=[i];  lenwords[nr]:=1;
      first[nr]:=i;    final[nr]:=i;
      prefix[nr]:=0;   suffix[nr]:=0;
      left[nr]:=[];    right[nr]:=[];
      genslookup[i]:=nr;
      reduced[nr]:=BlistList(genstoapply, []);
      if one=false and ForAll(gens, y-> gens[i]*y=y and y*gens[i]=y) then 
        one:=nr;
      fi;
    else 
      genslookup[i]:=val;
      nrrules:=nrrules+1;
      rules[nrrules]:=[[i], [val]];
    fi;
  od;

  if IsMonoid(S) then 
    left[1]:=List(genstoapply, i-> genslookup[i]);    
    right[1]:=List(genstoapply, i-> genslookup[i]);
  fi;

  return Objectify(NewType(FamilyObj(S), IsPinData and IsMutable), 
      rec( ht:=ht, stopper:=stopper,   words:=words, genslookup:=genslookup,
           nr:=nr, lenwords:=lenwords, elts:=elts,   one:=one, 
           first:=first, final:=final, prefix:=prefix, suffix:=suffix,
           left:=left,   right:=right, reduced:=reduced, genstoapply:=genstoapply, 
           gens:=gens, found:=false, rules:=rules, nrrules:=nrrules, pos:=pos, 
           len:=1, lenindex:=[[]]));
end);

# the main algorithm

InstallMethod(Enumerate, "for Pin data", [IsPinData], 
function(data)
  return Enumerate(data, infinity, ReturnFalse);
end);

#

InstallMethod(Enumerate, "for Pin data and cyclotomic", [IsPinData, IsCyclotomic], 
function(data, limit)
  return Enumerate(data, limit, ReturnFalse);
end);

# <lookfunc> has arguments <data=S!.semigroupe> and an index <j> in
# <[1..Length(data!.elts)]>.

InstallMethod(Enumerate, "for Pin data, cyclotomic, function",
[IsPinData, IsCyclotomic, IsFunction], 
function(data, limit, lookfunc)
  local elts, i, nr, looking, found, gens, genstoapply, len, one, right, left, first, final, prefix, suffix, reduced, words, stopper, ht, lenwords, rules, nrrules, lenindex, genslookup, htadd, htvalue, b, s, r, newword, new, pos, p, j, k;

  i:=data!.pos;                 # current position where we apply <gens>
  nr:=data!.nr;                 # nr=Length(elts);
  
  if lookfunc<>ReturnFalse then 
    looking:=true;              # only applied to new elements, not old ones!!!
    data!.found:=false;         # in case we previously looked for something and found it
  else
    looking:=false;
  fi;
  
  found:=false; 
 
  if i>nr then 
    SetFilterObj(data, IsClosedPinData);
    if looking then 
      data!.found:=false;
    fi;
    return data;
  fi;
  
  elts:=data!.elts;             # the so far enumerated elements
  gens:=data!.gens;
  genstoapply:=data!.genstoapply;
  len:=data!.len;               # current word length
  one:=data!.one;               # <elts[one]> is the mult. neutral element
  right:=data!.right;           # elts[right[i][j]]=elts[i]*gens[j], right Cayley graph
  left:=data!.left;             # elts[left[i][j]]=gens[j]*elts[i], left Cayley graph
  first:=data!.first;           # elts[i]=gens[first[i]]*elts[suffix[i]], first letter 
  final:=data!.final;           # elts[i]=elts[prefix[i]]*gens[final[i]]
  prefix:=data!.prefix;         # see final
  suffix:=data!.suffix;         # see first
  reduced:=data!.reduced;       # words[right[i][j]] is reduced if reduced[i][j]=true
  words:=data!.words;           # words[i] is a word in the gens equal to elts[i]
  nr:=data!.nr;                 # nr=Length(elts);
  stopper:=data!.stopper;       # stop when we have applied generators to elts[stopper] 
  ht:=data!.ht;                 # HTValue(ht, x)=Position(elts, x)
  lenwords:=data!.lenwords;     # lenwords[i]=Length(words[i])
  rules:=data!.rules;           # the relations
  nrrules:=data!.nrrules;       # Length(rules)
  lenindex:=data!.lenindex;     # lenindex[len]=list of positions in <words> of length <len>
  genslookup:=data!.genslookup; # genslookup[i]=Position(elts, gens[i])
                                # this is not always <i+1>!
  
  if IsBoundGlobal("ORBC") then 
    htadd:=HTAdd_TreeHash_C;
    htvalue:=HTValue_TreeHash_C;
  else
    htadd:=HTAdd;
    htvalue:=HTValue;
  fi;
  
  while nr<=limit and i<=nr and i<>stopper do 
    while i<=nr and i<>stopper and lenwords[i]=len do 

      b:=first[i];  s:=suffix[i];  # elts[i]=gens[b]*elts[s]

      for j in genstoapply do # consider <elts[i]*gens[j]>
        # <newword> represents <elts[i]*gens[j]>
        if s<>0 and not reduced[s][j] then     # <elts[s]*gens[j]> is not reduced
          r:=right[s][j];             # elts[r]=elts[s]*gens[j]
          if prefix[r]<>0 then 
            right[i][j]:=right[left[prefix[r]][b]][final[r]];
            # elts[i]*gens[j]=gens[b]*elts[prefix[r]]*elts[final[r]];
            newword:=words[i]{[1..lenwords[i]]};
            newword[lenwords[i]+1]:=j;
            reduced[i][j]:=(newword=words[right[i][j]]);     
            # elts[i]*gens[j]=words[right[i][j]]; 
          elif r=one then               # <elts[r]> is the identity
            right[i][j]:=genslookup[b]; 
            reduced[i][j]:=true;      # <elts[i]*gens[j]=b> and it is reduced
          else # prefix[r]=0, i.e. elts[r] is one of the generators
            right[i][j]:=right[b][final[r]];
            # elts[i]*gens[j]=gens[b]*elts[final[r]];
            # JDM: there is no need to create a new object here! 
            newword:=words[i]{[1..lenwords[i]]};
            newword[lenwords[i]+1]:=j;
            reduced[i][j]:=(newword=words[right[i][j]]);     
            # elts[i]*gens[j]=words[right[i][j]]; 
             
          fi;
        else # <elts[s]*gens[j]> is reduced
          new:=elts[i]*gens[j];
          newword:=words[i]{[1..lenwords[i]]}; # better than ShallowCopy
          newword[lenwords[i]+1]:=j;           # using Concatenate here is very bad!
          pos:=htvalue(ht, new);
          
          if pos<>fail then 
            nrrules:=nrrules+1;
            rules[nrrules]:=[newword, words[pos]];
            right[i][j]:=pos;
            # <newword> and <words[pos]> represent the same element (but are not
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
            words[nr]:=newword;   lenwords[nr]:=lenwords[i]+1;
            first[nr]:=b;         final[nr]:=j;
            prefix[nr]:=i;        right[nr]:=[];        
            left[nr]:=[];         right[i][j]:=nr;      
            reduced[i][j]:=true;  reduced[nr]:=BlistList(genstoapply, []);
            
            Add(lenindex[len], nr);
            
            if looking and not found then
              if lookfunc(data, nr) then
                found:=true;
                data!.found := nr;
              fi;
            fi;
          fi;
        fi;
      od; # finished applying gens to <elts[i]>
      i:=i+1;
      if looking and found then 
        break;
      fi;
    od; # finished words of length <len> or <looking and found>
    if looking and found then 
      break;
    fi;
    # process words of length <len> into <left>
    if len>1 then 
      for j in lenindex[len] do # loop over all words of length <len-1>
        p:=prefix[j]; b:=final[j];
        for k in genstoapply do 
          left[j][k]:=right[left[p][k]][b];
          # gens[k]*elts[j]=(gens[k]*elts[p])*gens[b]
        od;
      od;
    elif len=1 then 
      for j in lenindex[len] do  # loop over all words of length <1>
        b:=final[j];
        for k in genstoapply do 
          left[j][k]:=right[genslookup[k]][b];
          # gens[k]*elts[j]=gens[k]*gens[b]
        od;
      od;
    fi;
    len:=len+1;
    if not IsBound(lenindex[len]) then 
      lenindex[len]:=[];
    fi;
  od;
  
  data!.nr:=nr;    
  data!.nrrules:=nrrules;
  data!.one:=one;  
  data!.pos:=i;
  data!.len:=len;

  if i>nr then 
    SetFilterObj(data, IsClosedPinData);
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

InstallGlobalFunction(ClosureNonActingSemigroupNC, 
function(S, coll)
  local T, data, oldnrgens, nr, val, oldlen, i;
  
  if IsEmpty(coll) then 
    Info(InfoSemigroups, 2, "every element in the collection belong to the ",
    " semigroup,");
    return S;
  fi;
  if IsBound(S!.opts) then 
    if IsMonoid(S) and One(coll)=One(S) then 
      # it can be that these One's differ, and hence we shouldn't call Monoid here
      T:=Monoid(S, coll, S!.opts); #JDM: safe?
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
  
  data:=PinData(S);

  if data!.pos=0 then #nothing is known about <S>
    return T;
  fi;

  data:=StructuralCopy(data);
  oldnrgens:=Length(data!.gens);
  
  Append(data!.gens, coll);
  
  ResetFilterObj(data, IsClosedPinData);
 
  # install the elements in <coll> in <data>
  nr:=data!.nr;
  
  for i in [1..Length(coll)] do 
    val:=HTValue(data!.ht, coll[i]);
    if val=fail then #still have to check in case there are duplicates in coll
      nr:=nr+1; 
      HTAdd(data!.ht, coll[i], nr); 
      data!.elts[nr]:=coll[i];
      data!.words[nr]:=[i];  
      data!.lenwords[nr]:=1;
      data!.first[nr]:=i;    
      data!.final[nr]:=i;
      data!.prefix[nr]:=0;   
      data!.suffix[nr]:=0;
      data!.left[nr]:=[];    
      data!.right[nr]:=[];
      data!.genslookup[oldnrgens+i]:=nr;
      data!.reduced[nr]:=BlistList([1..Length(data!.gens)], []);
      Add(data!.lenindex[1], nr);
      if data!.one=false and ForAll(data!.gens, y-> coll[i]*y=y and y*coll[i]=y) then 
        data!.one:=nr;
      fi;
    else 
      data!.genslookup[oldnrgens+i]:=val;
      data!.nrrules:=data!.nrrules+1;
      data!.rules[data!.nrrules]:=[[i], [val]];
    fi;
  od;
  
  data!.nr:=nr; #put <nr> back in <data>
 
  # apply the new generators to the existing elements
  data!.stopper := data!.pos;
  if IsMonoid(S) then 
    data!.pos := 2;
  else
    data!.pos := 1;
  fi;
  data!.genstoapply := [oldnrgens+1..Length(data!.gens)];
  oldlen:=data!.len;
  data!.len:=1;

  Enumerate(data);
  if data!.pos <> data!.stopper then
      Error("Unexpected case!");
  fi;
  data!.stopper := false;
  data!.genstoapply := [1..Length(data!.gens)];
  data!.len:=oldlen;
  
  # remove the parts of the data that shouldn't be there!
  if not IsClosedPinData(data) then 
    Unbind(data!.rightscc);
    Unbind(data!.leftscc);
    Unbind(data!.idempotents); #JDM this could be kept...
  fi;
  
  SetPinData(T, data);
  return T;
end);

#

InstallMethod(Position, "for Pin data, an associative element, zero cyc",
[IsPinData, IsAssociativeElement, IsZeroCyc], 
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

InstallMethod(Length, "for Pin data", [IsPinData], 
function(data)
  return Length(data!.elts);
end);

#

InstallMethod(ViewObj, [IsPinData], 
function(data)
  Print("<");

  if IsClosedPinData(data) then 
    Print("closed ");
  else 
    Print("open ");
  fi;
  Print("Pin ");

  Print("data with ", Length(data!.elts), " elements, ");
  Print(Length(data!.rules), " relations, ");
  Print("max length of a word is ", data!.lenwords[data!.nr], ">");
  return;
end);

#

