
# TODO: use lookfunc, include logs like in Orb so that we can have
# ClosureSemigroup, IsomorphismFpSemigroup, a method for Factorization,
# \in methods, tie this in with Enumerator, etc...

#  for details see:
#
#  V. Froidure, and J.-E. Pin, Algorithms for computing finite semigroups.
#  Foundations of computational mathematics (Rio de Janeiro, 1997), 112-126,
#  Springer, Berlin,  1997.
#
#  JDM shouldn't produce fp representation if input is fp semigroup!

InstallGlobalFunction(InitSemigroupe,
function(S)
  local gens, nrgens, genstoapply, ht, stopper, nr, elts, words, lenwords, one, genslookup, first, final, left, prefix, suffix, right, reduced, pos, record, i;

  gens:=GeneratorsOfSemigroup(S);
  nrgens:=Length(gens);
  genstoapply:=[1..nrgens];
  ht:=HTCreate(gens[1], rec(treehashsize:=S!.opts!.hashlen.L));

  stopper:=false;  nr:=1;       elts:=[];       words:=[];  
  lenwords:=[];    one:=false;  genslookup:=[];
  
  first:=[];    final:=[];    left:=[genstoapply+1];
  prefix:=[];   suffix:=[];   right:=[genstoapply+1];  
  
  reduced:=[BlistList(genstoapply, genstoapply)];

  for i in genstoapply do 
    pos:=HTValue(ht, gens[i]);
    if pos=fail then 
      nr:=nr+1; 
      HTAdd(ht, gens[i], nr); 
      elts[nr]:=gens[i];
      words[nr]:=[i];  lenwords[nr]:=1;
      first[nr]:=i;    final[nr]:=i;
      prefix[nr]:=1;   suffix[nr]:=1;
      left[nr]:=[];    right[nr]:=[];
      genslookup[i]:=nr;
      reduced[nr]:=BlistList(genstoapply, []);
      if one=false and ForAll(gens, y-> gens[i]*y=y and y*gens[i]=y) then 
        one:=nr;
      fi;
    else 
      genslookup[i]:=pos;
    fi;
  od;

  record:=rec( ht:=ht, stopper:=stopper,   words:=words, genslookup:=genslookup,
               nr:=nr, lenwords:=lenwords, elts:=elts,   one:=one, 
               first:=first, final:=final, prefix:=prefix, suffix:=suffix,
               left:=left,   right:=right, reduced:=reduced);

  record.rules:=[];  record.nrrules:=0;  record.pos:=2;                        
  record.len:=1;     record.lenindex:=[2];   

  S!.semigroupe:=record; 
  return record;
end);

# the main algorithm

InstallMethod(Enumerate, 
"for a finite semigroup with generators, cyclotomic, function",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup, IsCyclotomic, IsFunction], 
function(S, limit, lookfunc)
  local gens, nrgens, genstoapply, data, i, len, one, right, left, first, final, prefix, suffix, reduced, words, elts, nr, stopper, ht, lenwords, rules, nrrules, lenindex, genslookup, htadd, htvalue, b, s, r, newword, new, pos, p, j, k;

  gens:=GeneratorsOfSemigroup(S);
  nrgens:=Length(gens);
  genstoapply:=[1..nrgens];
  
  if not IsBound(S!.semigroupe) then
    data:=InitSemigroupe(S);
  else
    data:=S!.semigroupe;
  fi;
  
  i:=data!.pos;             # current position where we apply <gens>
  len:=data!.len;           # current word length
  one:=data!.one;           # <elts[one]> is the mult. neutral element
  right:=data!.right;       # elts[right[i][j]]=elts[i]*gens[j], right Cayley graph
  left:=data!.left;         # elts[left[i][j]]=gens[j]*elts[i], left Cayley graph
  first:=data!.first;       # elts[i]=gens[first[i]]*elts[suffix[i]], first letter 
  final:=data!.final;       # elts[i]=elts[prefix[i]]*gens[final[i]]
  prefix:=data!.prefix;     # see final
  suffix:=data!.suffix;     # see first
  reduced:=data!.reduced;   # words[right[i][j]] is reduced if reduced[i][j]=true
  words:=data!.words;       # words[i] is a word in the gens equal to elts[i]
  elts:=data!.elts;         # the so far enumerated elements
  nr:=data!.nr;             # nr=Length(elts);
  stopper:=data!.stopper;   # JDM not currenly used
  ht:=data!.ht;             # HTValue(ht, x)=Position(elts, x)
  lenwords:=data!.lenwords; # lenwords[i]=Length(words[i])
  rules:=data!.rules;       # the relations
  nrrules:=data!.nrrules;   # Length(rules)
  lenindex:=data!.lenindex; # lenindex[len]=position in <words> and <elts> of
                            # first element of length <len>
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
    while i<=nr and lenwords[i]=len do 
      b:=first[i];  s:=suffix[i];  # elts[i]=gens[b]*elts[s]

      for j in genstoapply do # consider <elts[i]*gens[j]>
        # <newword> represents <elts[i]*gens[j]>
        if not reduced[s][j] then     # <elts[s]*gens[j]> is not reduced
          r:=right[s][j];             # elts[r]=elts[s]*gens[i]
          if r=one then               # <elts[r]> is the identity
            right[i][j]:=genslookup[b]; 
            reduced[i][j]:=true;      # <elts[i]*gens[j]=b> and it is reduced
          else
            right[i][j]:=right[left[prefix[r]][b]][final[r]];
            # elts[i]*gens[j]=gens[b]*elts[prefix[r]]*elts[final[r]];
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
            elts[nr]:=new;        htadd(ht, new, nr);
            words[nr]:=newword;   lenwords[nr]:=lenwords[i]+1;
            first[nr]:=b;         final[nr]:=j;
            prefix[nr]:=i;        suffix[nr]:=right[s][j];
            right[nr]:=[];        left[nr]:=[];
            right[i][j]:=nr;      reduced[i][j]:=true;
            reduced[nr]:=BlistList(genstoapply, []);
          fi;
        fi;
      od; # finished applying gens to <elts[i]>
      i:=i+1;
    od; # finished words of length <len>
    # process words of length <len> into <left>
    len:=len+1;
    lenindex[len]:=i;                  # words of length <len> start at <nr+1>
    for j in [lenindex[len-1]..i-1] do # loop over all words of length <len-1>
      p:=prefix[j]; b:=final[j];
      for k in genstoapply do 
        left[j][k]:=right[left[p][k]][b];
        # gens[k]*elts[j]=(gens[k]*elts[p])*gens[b]
      od;
    od;
  od;

  data!.nr:=nr;   data!.nrrules:=nrrules;
  data!.one:=one; data!.pos:=i;

  return data;
end);


