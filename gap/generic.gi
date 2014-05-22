
# code for generic semigroups, ties in the Froidure-Pin Semigroupe algorithm to
# GAP methods

#

InstallMethod(IsomorphismFpMonoid, "for a finite monoid with generators",
[IsMonoid and IsFinite and HasGeneratorsOfMonoid],
function(S)
  local rules, F, A, rels, Q, B;
 
  rules:=Enumerate(PinData(S))!.rules;
  
  F:=FreeMonoid(Length(GeneratorsOfMonoid(S)));
  A:=GeneratorsOfMonoid(F);
  rels:=List(rules, x-> [EvaluateWord(A, x[1]), EvaluateWord(A, x[2])]);
  
  Q:=F/rels; 
  B:=GeneratorsOfMonoid(Q);
  return MagmaIsomorphismByFunctionsNC(S, Q, 
   x -> EvaluateWord(B, Factorization(S, x)), 
   x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfMonoid(S)));

end);

#

InstallMethod(IsomorphismFpSemigroup, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local rules, F, A, rels, Q, B;
  
  rules:=Enumerate(PinData(S))!.rules;
  
  F:=FreeSemigroup(Length(GeneratorsOfSemigroup(S)));
  A:=GeneratorsOfSemigroup(F);
  rels:=List(rules, x-> [EvaluateWord(A, x[1]), EvaluateWord(A, x[2])]);
  
  Q:=F/rels; 
  B:=GeneratorsOfSemigroup(Q);
  return MagmaIsomorphismByFunctionsNC(S, Q, 
   x -> EvaluateWord(B, Factorization(S, x)), 
   x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfSemigroup(S)));
end);

#

InstallMethod(IsomorphismFpSemigroup, "for a finite monoid with generators",
[IsMonoid and IsFinite and HasGeneratorsOfMonoid],
function(S)
  local rules, lookup, convert, F, A, rels, one, Q, B, i;
 
  if GeneratorsOfSemigroup(S)=GeneratorsOfMonoid(S) then 
    return IsomorphismFpMonoid(S);
  fi;

  lookup:=List(GeneratorsOfMonoid(S), x-> Position(GeneratorsOfSemigroup(S), x));
  one:=Position(GeneratorsOfSemigroup(S), One(S));
  # if One(S) appears more than once in the generators of S, then this causes
  # problems here... JDM
  convert:=function(w)
    if not IsEmpty(w) then 
      return List(w, i-> lookup[i]); 
    else
      return [one];
    fi;
  end;
  #convert words in generators of monoid to words in generators of semigroup

  rules:=Enumerate(PinData(S))!.rules;
  
  F:=FreeSemigroup(Length(GeneratorsOfSemigroup(S)));
  A:=GeneratorsOfSemigroup(F);
  rels:=Set(rules, x-> [EvaluateWord(A, convert(x[1])), 
   EvaluateWord(A, convert(x[2]))]);

  # add relations for the identity
  AddSet(rels, [A[one]^2, A[one]]);
  for i in [1..Length(GeneratorsOfMonoid(S))] do 
    AddSet(rels, [A[lookup[i]]*A[one], A[lookup[i]]]);
    AddSet(rels, [A[one]*A[lookup[i]], A[lookup[i]]]);
  od;
  
  Q:=F/rels; 
  B:=GeneratorsOfSemigroup(Q);
  return MagmaIsomorphismByFunctionsNC(S, Q, 
   x -> EvaluateWord(B, convert(Factorization(S, x))), 
  # Factorization returns a word in the monoid generators of S
   x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfSemigroup(S)));
end);

#

InstallMethod(Enumerator, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data, record;
  
  record:=rec();

  record.NumberElement:=function(enum, elt)
    return Position(PinData(S), elt);
  end;

  record.ElementNumber:=function(enum, nr)
    data:=PinData(S);
    if not IsBound(data!.elts[nr]) then 
      Enumerate(data, nr);
    fi;
    return data!.elts[nr];
  end;

  record.Length:=enum -> Size(S);

  record.Membership:=function(enum, elt)
    return Position(PinData(S), elt)<>fail;
  end;

  record.IsBound\[\]:=function(enum, nr)
    return IsBound(PinData(S)!.elts[nr]);
  end;

  return EnumeratorByFunctions(S, record);
end);    

#

InstallMethod(Size, "for a finite semigroup with generators", 
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Length(Enumerate(PinData(S), infinity, ReturnFalse)!.elts);
end);

#

InstallMethod(RightCayleyGraphSemigroup, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Enumerate(PinData(S))!.right;
end);

#

InstallMethod(LeftCayleyGraphSemigroup, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Enumerate(PinData(S))!.left;
end);

#

InstallMethod(\in, "for an associative element and finite semigroup with generators",
[IsAssociativeElement, IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(x, S)
  return Position(S, x)<>fail;
end);

#

InstallMethod(Factorization,
"for a finite semigroup with generators and an associative element",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(S, x)
  local pos;
  pos:=Position(S, x); 
  if pos=fail then 
    return fail;
  fi;
  return PinData(S)!.words[pos];
end);

# JDM: probably have a global function or attribute RightCayleyGraphSCC or
# similar.

InstallMethod(NrRClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(PinData(S));
  if not IsBound(data!.rightscc) then 
    data!.rightscc:=GABOW_SCC(data!.right);
  fi;
    
  return data!.rightscc.count;
end);

#

InstallMethod(NrLClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(PinData(S));
  if not IsBound(data!.leftscc) then 
    if not IsBound(data!.rightscc) then 
      data!.rightscc:=GABOW_SCC(data!.right);
    fi;

    data!.leftscc:=GABOW_SCC_DCLASSES(data!.left, data!.rightscc);
  fi;
    
  return data!.leftscc.count;
end);

#

InstallMethod(NrDClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(PinData(S));
  if not IsBound(data!.leftscc) then 
    if not IsBound(data!.rightscc) then 
      data!.rightscc:=GABOW_SCC(data!.right);
    fi;

    data!.leftscc:=GABOW_SCC_DCLASSES(data!.left, data!.rightscc);
  fi;
    
  return data!.leftscc.nrdclasses;
end);

#

InstallMethod(MinimalIdeal, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data;
  data:=Enumerate(PinData(S));
  if not IsBound(data!.rightscc) then 
    data!.rightscc:=GABOW_SCC(data!.right);
  fi;
  return SemigroupIdeal(S, data!.elts[data!.rightscc.min]);
end);

# JDM: this could have a c method...

InstallMethod(NrHClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data, nr, rid, rnr, count, sorted, lid, lnr, hid, hlookup, now, hindex, j, cur, i;
  
  data:=Enumerate(PinData(S));
  
  if not IsBound(data!.rightscc) then 
    data!.rightscc:=GABOW_SCC(data!.right);
  fi;
  if not IsBound(data!.leftscc) then 
    data!.leftscc:=GABOW_SCC_DCLASSES(data!.left, data!.rightscc);
  fi;

  nr:=Length(data!.elts);
  rid:=data!.rightscc.id;      # lookup for index of R-class containing <elts[i]>
  rnr:=data!.rightscc.count;   # number of R-classes
  count:=[1..rnr+1]*0;   
  count[1]:=1;

  # count the number of elements in each R-class
  for i in [1..nr] do 
    count[rid[i]+1]:=count[rid[i]+1]+1;
  od;
  # count[rid[i]] is the next available position to contain an element of R-class
  # with index rid[i]. 
  for i in [2..rnr] do 
    count[i]:=count[i-1]+count[i];
  od;
  
  # List(sorted, i-> rid[i])= rid sorted, without the last element...
  sorted:=EmptyPlist(nr);
  for i in [1..nr] do 
    sorted[count[rid[i]]]:=i;
    count[rid[i]]:=count[rid[i]]+1;
  od;

  lid:=data!.leftscc.id; # lookup for L-class indices
  hid:=[1..nr]*0;       # lookup for H-class indices
  now:=0;               # current R-class 
  hindex:=0;            # current H-class index
  hlookup:=[1..data!.leftscc.count]*0; 
  # H-class corresponding to L-class in the current R-class <now>

  # browse the L-class table...
  for i in [1..nr] do
    j:=sorted[i];
    if rid[j]>now then # new R-class
      now:=rid[j]; 
      cur:=hindex; 
      # H-class indices for elements of R-class <now> start at <cur+1>
    fi;
    if hlookup[lid[j]]<=cur then 
      # then we have a new H-class, otherwise, this is an 
      hindex:=hindex+1;          # existing H-class in the current R-class. 
      hlookup[lid[j]]:=hindex;
    fi;
    hid[j]:=hlookup[lid[j]];
  od;
  data!.hid:=hid;

  return hindex;
end);

# JDM: can probably do better than this by considering Green's classes.

InstallMethod(Idempotents, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data, elts, idempotents, nr, i;

  data:=Enumerate(PinData(S));

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


    




