
Obj FuncPIN(Obj self, Obj gens, Obj ht, Obj limit){
  Int  k, i, last, currentlen, u, v, a, b, s, old, new, r;
  Obj  elts, first, final, prefix, suffix, postmult, premult, flags, length, newelt, p, htval;
    
  k=LEN_PLIST(gens);
  
  elts=NEW_PLIST(T_PLIST, INT_INTOBJ(limit));
  first=NEW_PLIST(T_PLIST_CYC, INT_INTOBJ(limit));
  final=NEW_PLIST(T_PLIST_CYC, INT_INTOBJ(limit));
  prefix=NEW_PLIST(T_PLIST_CYC, INT_INTOBJ(limit));
  suffix=NEW_PLIST(T_PLIST_CYC, INT_INTOBJ(limit));
  postmult=NEW_PLIST(T_PLIST_CYC, INT_INTOBJ(limit));
  premult=NEW_PLIST(T_PLIST_CYC, INT_INTOBJ(limit));
  flags=NEW_PLIST(T_PLIST_CYC, INT_INTOBJ(limit));
  length=NEW_PLIST(T_PLIST_CYC, INT_INTOBJ(limit));
 
  for(i=1;i<=k+1;i++){
    if(i==1){
      SET_ELM_PLIST(elts, i, ONE(ELM_PLIST(gens, 1)));
      SET_ELM_PLIST(length, i, 0);
      HTAdd_TreeHash_C(self, ht, ONE(ELM_PLIST(gens, 1)), INTOBJ_INT(i));
    }else{
      SET_ELM_PLIST(elts, i, ELM_PLIST(gens, i-1));
      SET_ELM_PLIST(length, i, INTOBJ_INT(1));
      SET_ELM_PLIST(postmult, i, INTOBJ_INT(i));
      SET_ELM_PLIST(premult, i, INTOBJ_INT(i));
      HTAdd_TreeHash_C(self, ht, ELM_PLIST(gens, i-1), INTOBJ_INT(i));
    }
    SET_ELM_PLIST(first, i, INTOBJ_INT(i-1));
    SET_ELM_PLIST(final, i, INTOBJ_INT(i-1));
    SET_ELM_PLIST(prefix, i, INTOBJ_INT(1));
    SET_ELM_PLIST(suffix, i, INTOBJ_INT(1));
    SET_ELM_PLIST(flags, i, INTOBJ_INT(0));
  } 

  u=2; v=u; last=k+1; currentlen=1;

  while(u<=last){
    while(u<=last&&INT_INTOBJ(ELM_PLIST(length, u))==currentlen){
      b=INT_INTOBJ(ELM_PLIST(first, u));
      s=INT_INTOBJ(ELM_PLIST(suffix, u))-1;
      for(i=1;i<=k;i++){
        if(!ELM_PLIST(flags, s*k+i)){
          r=INT_INTOBJ(ELM_PLIST(postmult, s*k+i));
          if(r==1){
            SET_ELM_PLIST(postmult, (u-1)*k+i, INTOBJ_INT(b+1));
            SET_ELM_PLIST(flags, (u-1)*k+i, INTOBJ_INT(0));
          }else{
            a=INT_INTOBJ(ELM_PLIST(premult, (INT_INTOBJ(ELM_PLIST(prefix, r))-1)*k+b)-1);
            SET_ELM_PLIST(postmult, (u-1)*k+i, INTOBJ_INT(a*k+INT_INTOBJ(ELM_PLIST(final, r)))); 
            old=INT_INTOBJ(ELM_PLIST(postmult, (u-1)*k+i));
            if(INT_INTOBJ(ELM_PLIST(final, old))!=i){
              SET_ELM_PLIST(flags, (u-1)*k+i, INTOBJ_INT(0));
            }else{
              new=u;
              old=INT_INTOBJ(ELM_PLIST(prefix, old));
              while(new>1&&old>1){
                if(ELM_PLIST(final, old)!=ELM_PLIST(final, new)){
                  SET_ELM_PLIST(flags, (u-1)*k+i, INTOBJ_INT(0));
                  break;
                }
                old=INT_INTOBJ(ELM_PLIST(prefix, old));
                new=INT_INTOBJ(ELM_PLIST(prefix, new));
              }
              if(new==1&&old==1&&ELM_PLIST(flags, (u-1)*k+i)!=NULL){
                SET_ELM_PLIST(flags, (u-1)*k+i, INTOBJ_INT(1));
              }else{
                SET_ELM_PLIST(flags, (u-1)*k+i, INTOBJ_INT(0));
              }
            }
          }
        }else{
          newelt=PROD(ELM_PLIST(elts, u), ELM_PLIST(gens, i));
          htval=HTValue_TreeHash_C(self, ht, newelt);
          if(htval==Fail){
            last++;
            SET_ELM_PLIST(first, last, INTOBJ_INT(b));
            SET_ELM_PLIST(final, last, INTOBJ_INT(i));
            SET_ELM_PLIST(prefix, last, INTOBJ_INT(u));
            SET_ELM_PLIST(suffix, last, 
             ELM_PLIST(postmult, (INT_INTOBJ(ELM_PLIST(suffix, u))-1)*k+i));
            SET_ELM_PLIST(length, last, ELM_PLIST(length, u)+1);
            SET_ELM_PLIST(elts, last, newelt);
            SET_ELM_PLIST(postmult, (u-1)*k+i, INTOBJ_INT(last));
            SET_ELM_PLIST(flags, (u-1)*k+i, INTOBJ_INT(1));
            HTAdd_TreeHash_C(self, ht, newelt, INTOBJ_INT(last));
          }else{
            SET_ELM_PLIST(postmult, (u-1)*k+i, htval);
            SET_ELM_PLIST(flags, (u-1)*k+i, INTOBJ_INT(0));
          }
        }
      }
      u++;
    }
    u=v;
    while(u<=last&&ELM_PLIST(length, u)==INTOBJ_INT(currentlen)){
      p=ELM_PLIST(prefix, u);
      for(i=1;i<=k;i++){
        SET_ELM_PLIST(premult, (u-1)*k+1, 
         ELM_PLIST(postmult, (INT_INTOBJ(ELM_PLIST(premult, 
          (INT_INTOBJ(p)-1)*k+i)-1))*k+INT_INTOBJ(ELM_PLIST(final, u))));
      }
      u=u+1;
    }
    v=u;
    currentlen++;
  }
  SET_LEN_PLIST(elts, last);
  return elts;
}














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
  local gens, k, limit, elts, ht, rules, len_rules, first, final, prefix, suffix, postmult, flags, premult, length, u, v, Last, currentlen, b, r, old, new, newelt, p, i;

  gens:=GeneratorsOfMonoid(s);
  k:=Length(gens);
  limit:=1000003;
  elts:=Concatenation([One(s)], gens);
  ht:=HTCreate(elts[1], rec(treehashsize:=limit));
  for i in [1..k+1] do 
    HTAdd(ht, elts[i], k);
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
  flags:=List([1..k], x-> true);
  # position of gens[i]*u in <fpelts>
  premult:=Concatenation([[2..k+1]],  List([1..k], x-> []));
  # length of <u>
  length:=Concatenation([0], List([1..k], x->1));

  # initialize loop

  u:=2;               # position of the first generator
  v:=u;               # place holder
  Last:=k+1;          # the current position of the last element in <fpelts>
  currentlen:=1;   # current length of words under consideration

  # loop
  while u<=Last do 
    while u<=Last and length[u]=currentlen do

      b:=first[u];
      s:=suffix[u];

      for i in [1..k] do #loop over generators

	if not flags[(s-1)*k+i] then  # if s*a_i is not reduced
	  r:=postmult[s][i];            # r=s*a_i
	  if r=1 then   # r=1
	    postmult[u][i]:=b+1; 
            flags[(u-1)*k+i]:=true;   # u*a_i=b and it is reduced
	  else
	    postmult[u][i]:=postmult[premult[prefix[r]][b]][final[r]];
            #\rho(u*a_i)=\rho(\rho(b*r)*l(r))
            #new=u*a_i
            old:=postmult[u][i];
            if final[old]<>i then 
              flags[(u-1)*k+i]:=false;
            else
              new:=u;
              old:=prefix[old];
              while new>1 and old>1 do 
                if final[old]<>final[new] then 
                  flags[(u-1)*k+i]:=false;
                  break;
                fi;
                old:=prefix[old];
                new:=prefix[new];
              od;
            
              if new=1 and old=1 and not IsBound(flags[(u-1)*k+i]) then 
                flags[(u-1)*k+i]:=true;
              else
                flags[(u-1)*k+i]:=false;
              fi;
            fi;
              # if \rho(u*a_i)=u*a_i then true
	  fi;
        else
	  newelt:=elts[u]*gens[i];      # newelt=nu(u*a_i)
          old:=HTValue_TreeHash_C(ht, newelt);
          if old=fail then  
            # add all its info to the table
	    Last:=Last+1;
            first[Last]:=b; 
            final[Last]:=i;   
            prefix[Last]:=u; 
            suffix[Last]:=postmult[suffix[u]][i];   
            # u=b*suffix(u)*a_i
            postmult[Last]:=[]; 
            premult[Last]:=[];
            length[Last]:=length[u]+1; 
            elts[Last]:=newelt;
	    postmult[u][i]:=Last; 
            flags[(u-1)*k+i]:=true;   
            # the word u*a_i is a new elt
            # and is hence reduced
            HTAdd_TreeHash_C(ht, newelt, Last);
          else
            len_rules:=len_rules+1;
            rules[len_rules]:=[u,i,old];
	    postmult[u][i]:=old; 
            flags[(u-1)*k+i]:=false;  # u*a_i represents the same elt as
                                        # fpelts[j] and is (hence) not reduced
	  fi;
       fi;
     od;
      u:=u+1;
    od;
    u:=v;  # go back to the first elt with length=currentlen

    while u<=Last and length[u]=currentlen do
      p:=prefix[u];
      for i in [1..k] do
	premult[u][i]:=postmult[premult[p][i]][final[u]];
        # \rho(a_i*u)=\rho(\rho(a_i*p)*final(u))
      od;
      u:=u+1;
    od;
    v:=u;
    currentlen:=currentlen+1;
  od;
  return [elts, ht];
end;



















