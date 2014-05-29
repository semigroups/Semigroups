###########################################################################
##
#W  generic-lib.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Code for generic semigroups (i.e. ones which know \*), ties in the
# Froidure-Pin Semigroupe algorithm to GAP methods.

# Methods for things declared in the Semigroups package...

InstallMethod(NrRClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Length(SCCRightCayleyGraph(SEEData(S)).comps);
end);

#

InstallMethod(NrLClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Length(SCCLeftCayleyGraph(SEEData(S)).comps);
end);

#

InstallMethod(NrDClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Length(SCCLeftRightCayleyGraph(SEEData(S)).comps);
end);

#JDM: this doesn't yet work...

InstallMethod(MinimalIdeal, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data, scc;
  data:=Enumerate(SEEData(S));
  scc:=SCCRightCayleyGraph(data);
  return SemigroupIdeal(S, data!.elts[scc.comps[1][1]]);
end);

# JDM: this should be a c method...

InstallMethod(NrHClasses, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data, right, rightid, left, leftid, comps, nextpos, len, sorted, hindex, rindex, id, lookup, j, init, i;
  
  data:=Enumerate(SEEData(S));
  right:=SCCRightCayleyGraph(data);
  rightid:=right.id;
  left:=SCCLeftCayleyGraph(data);
  leftid:=left.id;

  comps:=right.comps;
  nextpos:=EmptyPlist(Length(comps));
  nextpos[1]:=1;
  for i in [2..Length(comps)] do 
    nextpos[i]:=nextpos[i-1]+Length(comps[i-1]);
  od;

  # List(sorted, i-> right.id[i])= right.id sorted
  len:=Length(data!.elts);
  sorted:=EmptyPlist(len);
  for i in [1..len] do 
    sorted[nextpos[rightid[i]]]:=i;
    nextpos[rightid[i]]:=nextpos[rightid[i]]+1;
  od;

  hindex:=0;            # current H-class index
  rindex:=0;            # current R-class index
  id:=[1..len]*0;       # id component for H-class data structure
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

# Methods for things declared in the GAP library

InstallMethod(IsomorphismFpMonoid, "for a finite monoid with generators",
[IsMonoid and IsFinite and HasGeneratorsOfMonoid],
function(S)
  local rules, F, A, rels, Q, B;
 
  rules:=Enumerate(SEEData(S))!.rules;
  
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
  
  rules:=Enumerate(SEEData(S))!.rules;
  
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

  rules:=Enumerate(SEEData(S))!.rules;
  
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
    return Position(SEEData(S), elt);
  end;

  record.ElementNumber:=function(enum, nr)
    data:=SEEData(S);
    if not IsBound(data!.elts[nr]) then 
      Enumerate(data, nr);
    fi;
    return data!.elts[nr];
  end;

  record.Length:=enum -> Size(S);

  record.Membership:=function(enum, elt)
    return Position(SEEData(S), elt)<>fail;
  end;

  record.IsBound\[\]:=function(enum, nr)
    return IsBound(SEEData(S)!.elts[nr]);
  end;

  return EnumeratorByFunctions(S, record);
end);    

#

InstallMethod(Size, "for a finite semigroup with generators", 
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Length(Enumerate(SEEData(S), infinity, ReturnFalse)!.elts);
end);

#

InstallMethod(RightCayleyGraphSemigroup, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Enumerate(SEEData(S))!.right;
end);

#

InstallMethod(LeftCayleyGraphSemigroup, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Enumerate(SEEData(S))!.left;
end);

#

InstallMethod(\in, "for an associative element and finite semigroup with generators",
[IsAssociativeElement, IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(x, S)
  return Position(SEEData(S), x)<>fail;
end);

#

InstallMethod(Factorization,
"for a finite semigroup with generators and an associative element",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(S, x)
  local pos;
  pos:=Position(SEEData(S), x); 
  if pos=fail then 
    return fail;
  fi;
  return SEEData(S)!.words[pos];
end);


# JDM: can probably do better than this by considering Green's classes.

InstallMethod(Idempotents, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  local data, elts, idempotents, nr, i;

  data:=Enumerate(SEEData(S));

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

