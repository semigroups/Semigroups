

#  for details see:
#
#  V. Froidure, and J.-E. Pin, Algorithms for computing finite semigroups.
#  Foundations of computational mathematics (Rio de Janeiro, 1997), 112-126,
#  Springer, Berlin,  1997.
#
#  JDM shouldn't produce fp representation if input is fp semigroup!

InstallMethod(Enumerate, 
"for a finite semigroup with generators, cyclotomic, function",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup, IsCyclotomic, IsFunction], 
function(S, limit, lookfunc)
  local stopper, ht, gens, nrgens, nr, elts, words, lenwords, one, rules, first, final, prefix, suffix, right, left, reduced, i, len, lenindex, j, b, s, newword, r, new, pos, nrrules, p, x, k;

  if not IsBound(S!.semigroupe) then
    stopper:=false; #JDM not currently in use 
    ht:=HTCreate(gens[1], rec(treehashsize:=S!.opts!.hashlen.M));
    gens:=GeneratorsOfSemigroup(S);
    nrgens:=Length(gens);
    nr:=0;                                   # length of <elts>
    elts:=[];
    words:=[];
    lenwords:=[];                            # lenwords[i]=Length(words[i])
    one:=false;
    
    # remove duplicate <gens> and initialise <ht> and <elts>
    for x in gens do 
      if HTValue(ht, x)=fail then 
        nr:=nr+1; 
        HTAdd(ht, x, nr);
        elts[nr]:=x;
        words[nr]:=[nr];
        lenwords[nr]:=1;
        if IsOne(x) then 
          one:=nr;
        fi;
      fi;
    od;

    # <u> is an element in <S> represented as a word in <gens>
    rules:=[];                          # the rules
    first:=[1..nr];                     # position of first letter in <gens>
    final:=[1..nr];                     # position of last letter in <gens>
    prefix:=List([1..nr], ReturnFail);  # position of prefix of length |u|-1
    suffix:=List([1..nr], ReturnFail);  # position of suffix of length |u|-1
    right:=List([1..nr], x-> []);       # position of u*gens[i] in <elts>
    left:=List([1..nr], x-> []);        # position of gens[i]*u in <elts>
    reduced:=List([1..nr], x-> BlistList([1..nrgens], []));
    # <true> if <u*gens[i]> corresponds is a reduced word (i.e. the reduced word
    # corresponding to <u*gens[i]> is the word corresponding to <u> product <gens[i]>
    # in the free semigroup 
    i:=1;                               # position where we apply <gens>
    len:=1;                             # current word length
    lenindex:=[1];                      # lenindex[i]=position in <words> of
                                        # first element of length i
  else
    # set the above local variables using <S!.semigroupe>
  fi;

  j:=i;               # place holder
  
 
  while nr<=limit and i<nr and i<>stopper do 
    while i<nr and lenwords[i]=len do 
      b:=first[i];  s:=suffix[i];  # elts[i]=gens[b]*elts[s]

      for j in [1..nrgens] do # consider <elts[i]*gens[j]>
        newword:=Concatenation(words[i], [j]);
        #newword:=fpelts[u]*freegens[j]; # newword=u*a_j=elts[i]*gens[j]

        if not reduced[s][j] then     # <elts[s]*gens[j]> is not reduced
          r:=right[s][j];             # elts[r]=elts[s]*gens[i]
          if r=one then               # <elts[r]> is the identity
            right[i][j]:=b; 
            reduced[i][j]:=true;      # <elts[i]*gens[j]=b> and it is reduced
          else
            right[i][j]:=right[left[prefix[r]][b]][final[r]];
            # elts[i]*gens[j]=gens[b]*elts[prefix[r]]*elts[final[r]];
            # \rho(u*a_i)=\rho(\rho(b*r)*l(r)), or ua=btc
            reduced[i][j]:=(newword=words[right[i][j]]);     
            # elts[i]*gens[j]=words[right[i][j]]; 
            # if \rho(u*a_i)=u*a_i then true
          fi;
        else # <elts[s]*gens[j]> is reduced
          
          new:=elts[i]*gens[j];
          pos:=HTValue(ht, new);
          if pos<>fail then 
            nrrules:=nrrules+1;
            rules[nrrules]:=[newword, words[pos]];
            right[i][j]:=pos;
            # <newword> and <words[pos]> represent the same element (but are not
            # equal) and so <newword> is not reduced

          else #<new> is a new element!
            nr:=nr+1;
           
            if one<>false and IsOne(new) then 
              one:=nr;
            fi;

            elts[nr]:=new;
            HTAdd(ht, new, nr);

            words[nr]:=newword;
            lenwords[nr]:=lenwords[i]+1;
            
            first[nr]:=b; 
            final[nr]:=j;
            prefix[nr]:=i; 
            suffix[nr]:=right[s][j];
            right[nr]:=[];
            reduced[nr]:=[];
            left[nr]:=[];

            right[i][j]:=nr;
            reduced[i][j]:=true;
          fi;
        fi;
        i:=i+1;
      od; # finished words of length <len>

      # process words of length <len> into <left>
      len:=len+1;
      lenindex[len]:=nr+1;              # words of length <len> start at <nr+1>
      for j in [lenindex[len-1]..nr] do # loop over all words of length <len-1>
        p:=prefix[j]; b:=final[j];
        for k in [1..nrgens] do 
          left[j][k]:=right[left[p][k]][b];
          # gens[k]*elts[j]=(gens[k]*elts[p])*gens[p]
        od;
      od;
    od;
  od;

end);


