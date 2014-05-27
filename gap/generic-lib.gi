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
# Froidure-Pin Semigroupe algorithm to GAP methods. This file only contains
# methods for operations (etc) that are available in the gap library. 

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
  return Position(PinData(S), x)<>fail;
end);

#

InstallMethod(Factorization,
"for a finite semigroup with generators and an associative element",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(S, x)
  local pos;
  pos:=Position(PinData(S), x); 
  if pos=fail then 
    return fail;
  fi;
  return PinData(S)!.words[pos];
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


    




