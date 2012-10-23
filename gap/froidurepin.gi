

####################
####################
##
#M  FroidurePinExtendedAlg(<semigroup>);
##
##  for details of this algorithm see:
##
##  V. Froidure, and J.-E. Pin, Algorithms for computing finite semigroups.
##  Foundations of computational mathematics (Rio de Janeiro, 1997), 112-126,
##  Springer, Berlin,  1997.
##

#InstallMethod(FroidurePinExtended, "for a semigroup with generators",
#[IsMonoid and HasGeneratorsOfSemigroup],
FroidurePinExtended:=function(s)
  local gens, k, limit, elts, fpelts, ht, rules, len_rules, first, final, prefix, suffix, postmult, reducedflags, premult, length, u, v, Last, currentlength, b, r, len, newword, newelt, old, p, i;

  gens:=GeneratorsOfMonoid(s);
  k:=Length(gens);
  limit:=100003;
  #limit:=SemigroupsOptionsRec.hashlen.M;
  elts:=Concatenation([One(s)], gens);
  fpelts:=List([1..k+1], x-> [x]);
  ht:=HTCreate(elts[1], rec(treehashsize:=limit));
  
  for i in [1..k+1] do 
    HTAdd(ht, elts[k], k);
  od;

  #output
  rules:=[]; len_rules:=0;
  # position of first letter in <gens>
  first:=[0..k];
  # position of last letter in <gens>
  final:=[0..k];
  # position of prefix of length |u|-1 in <fpelts>
  prefix:=List([1..k+1], x->1);
  # position of suffix of length |u|-1 in <fpelts>
  suffix:=List([1..k+1], x->1);
  # position of u*gens[i] in <fpelts>
  postmult:=Concatenation([[2..k+1]], List([1..k], x-> []));
  # true if u*gens[i] is the same word as fpelts[i]
  reducedflags:=Concatenation([List([1..k], x->  true)], List([1..k], x-> []));
  # position of gens[i]*u in <fpelts>
  premult:=Concatenation([[2..k+1]],  List([1..k], x-> []));
  # length of <u>
  length:=Concatenation([0], List([1..k], x->1));

  # initialize loop

  u:=2;               # position of the first generator
  v:=u;               # place holder
  Last:=k+1;          # the current position of the last element in <fpelts>
  currentlength:=1;   # current length of words under consideration

  # loop
  while u<=Last do 
    while u<=Last and length[u]=currentlength do

      b:=first[u];
      s:=suffix[u];

      for i in [1..k] do #loop over generators

	if not reducedflags[s][i] then  # if s*a_i is not reduced
	  r:=postmult[s][i];            # r=s*a_i
	  if fpelts[r]=0 then   # r=1
	    postmult[u][i]:=b+1; 
            reducedflags[u][i]:=true;   # u*a_i=b and it is reduced
	  else
	    postmult[u][i]:=postmult[premult[prefix[r]][b]][final[r]];
            #\rho(u*a_i)=\rho(\rho(b*r)*l(r))
            len:=LEN_LIST(fpelts[u]);
            newword:=fpelts[u]{[1..len]};
            newword[len+1]:=i;
            # newword=u*a_i
	    reducedflags[u][i]:=(newword=fpelts[postmult[u][i]]); 
            # if \rho(u*a_i)=u*a_i then true
	  fi;
        else
          len:=LEN_LIST(fpelts[u]);
          newword:=fpelts[u]{[1..len]};
          newword[len+1]:=i;

          # newword=u*a_i
	  newelt:=elts[u]*gens[i];      # newelt=nu(u*a_i)
          old:=HTValue_TreeHash_C(ht, newelt);
          if old=fail then  
            # add all its info to the table
	    Last:=Last+1;
            fpelts[Last]:=newword; 
            first[Last]:=b; 
            final[Last]:=i;   
            prefix[Last]:=u; 
            suffix[Last]:=postmult[suffix[u]][i];   
            # u=b*suffix(u)*a_i
            postmult[Last]:=[]; 
            reducedflags[Last]:=[]; 
            premult[Last]:=[];
            length[Last]:=length[u]+1; 
            elts[Last]:=newelt;
	    postmult[u][i]:=Last; 
            reducedflags[u][i]:=true;   
            # the word u*a_i is a new elt
            # and is hence reduced
            HTAdd_TreeHash_C(ht, newelt, Last);
          else
            len_rules:=len_rules+1;
            rules[len_rules]:=[newword, fpelts[old]];
	    postmult[u][i]:=old; 
            reducedflags[u][i]:=false;  # u*a_i represents the same elt as
                                        # fpelts[j] and is (hence) not reduced
	  fi;
       fi;
     od;
      u:=u+1;
    od;
    u:=v;  # go back to the first elt with length=currentlength

    while u<=Last and length[u]=currentlength do
      p:=prefix[u];
      for i in [1..k] do
	premult[u][i]:=postmult[premult[p][i]][final[u]];
        # \rho(a_i*u)=\rho(\rho(a_i*p)*final(u))
      od;
      u:=u+1;
    od;
    v:=u;
    currentlength:=currentlength+1;
  od;
  return elts;
end;



















