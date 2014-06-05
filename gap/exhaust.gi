###########################################################################
##
#W  generic.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Code for generic semigroups (i.e. ones which know \*), ties in the
# Froidure-Pin Semigroupe algorithm to GAP methods.

# methods for things declared in the GAP library

# different method for ideals

InstallMethod(Enumerator, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
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

InstallMethod(Size, "for a finite semigroup with generators", 
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(S)
  return Length(Enumerate(ExhaustiveData(S), infinity, ReturnFalse)!.elts);
end);

# different method for ideals

InstallMethod(\in, "for an associative element and finite semigroup with generators",
[IsAssociativeElement, IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
function(x, S)
  return Position(ExhaustiveData(S), x)<>fail;
end);

# different method for ideals
# JDM: can probably do better than this by considering Green's classes.

InstallMethod(Idempotents, "for a finite semigroup with generators",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup],
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

# Green's classes: the idea is to only store the index of the equivalence class
# corresponding to the Green's class, then look everything up in the data. The
# equivalence class data structures for R-, L-, H-, D-classes of a finite
# semigroup <S> are stored in the !.data component of the corresponding Green's
# relation. 

# same method for ideals

InstallMethod(GreensRRelation, "for a semigroup", [IsSemigroup], 
function(S)
  local fam, rel, sc;

  fam := GeneralMappingsFamily( ElementsFamily(FamilyObj(S)),
           ElementsFamily(FamilyObj(S)) );

  rel := Objectify(NewType(fam,
           IsEquivalenceRelation and IsEquivalenceRelationDefaultRep
           and IsGreensRRelation), rec());

  SetSource(rel, S);
  SetRange(rel, S);
  SetIsLeftSemigroupCongruence(rel,true);
  if HasIsFinite(S) and IsFinite(S) then
   SetIsFiniteSemigroupGreensRelation(rel, true);
  fi;
  if not IsNonExhaustiveSemigroup(S) then  
    rel!.data:=GABOW_SCC(RightCayleyGraphSemigroup(S));
  fi;
  return rel;
end);

# same method for ideals

InstallMethod(GreensLRelation, "for a semigroup",  [IsSemigroup], 
function(S)
  local fam, rel;

  fam := GeneralMappingsFamily( ElementsFamily(FamilyObj(S)),
          ElementsFamily(FamilyObj(S)) );

  rel := Objectify(NewType(fam,
          IsEquivalenceRelation and IsEquivalenceRelationDefaultRep
          and IsGreensLRelation), rec());

  SetSource(rel, S);
  SetRange(rel, S);
  SetIsRightSemigroupCongruence(rel,true);
  if HasIsFinite(S) and IsFinite(S) then
    SetIsFiniteSemigroupGreensRelation(rel, true);
  fi;

  if not IsNonExhaustiveSemigroup(S) then  
    rel!.data:=GABOW_SCC(LeftCayleyGraphSemigroup(S));
  fi;
  return rel;
end);

# same method for ideals

InstallMethod(GreensJRelation, "for a finite semigroup",  [IsSemigroup and IsFinite], 
GreensDRelation);

# same method for ideals

InstallMethod(GreensDRelation, "for a semigroup",  [IsSemigroup], 
function(S)
    local fam, rel;

  fam := GeneralMappingsFamily( ElementsFamily(FamilyObj(S)),
          ElementsFamily(FamilyObj(S)) );

  rel := Objectify(NewType(fam,
          IsEquivalenceRelation and IsEquivalenceRelationDefaultRep
          and IsGreensDRelation), rec());

  SetSource(rel, S);
  SetRange(rel, S);

  if HasIsFinite(S) and IsFinite(S) then
    SetIsFiniteSemigroupGreensRelation(rel, true);
  fi;
  if not IsNonExhaustiveSemigroup(S) then 
    rel!.data:=SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS(GreensRRelation(S)!.data,
    GreensLRelation(S)!.data);
  fi;
  return rel;
end);

# same method for ideals

InstallMethod(GreensHRelation, "for a semigroup",  [IsSemigroup], 
function(S)
    local fam, rel;

  fam := GeneralMappingsFamily( ElementsFamily(FamilyObj(S)),
          ElementsFamily(FamilyObj(S)) );
  rel := Objectify(NewType(fam,
          IsEquivalenceRelation and IsEquivalenceRelationDefaultRep
          and IsGreensHRelation), rec());

  SetSource(rel, S);
  SetRange(rel, S);

  if HasIsFinite(S) and IsFinite(S) then
    SetIsFiniteSemigroupGreensRelation(rel, true);
  fi;
  if not IsNonExhaustiveSemigroup(S) then 
    rel!.data:=FIND_HCLASSES(GreensRRelation(S)!.data, GreensLRelation(S)!.data);
  fi;
  return rel;
end);

# same method for ideals

InstallMethod(IsomorphismFpMonoid, "for a finite monoid",
[IsMonoid and IsFinite],
function(S)
  local rules, F, A, rels, Q, B;
 
  rules:=Enumerate(ExhaustiveData(S))!.rules;
  
  F:=FreeMonoid(Length(GeneratorsOfMonoid(S)));
  A:=GeneratorsOfMonoid(F);
  rels:=List(rules, x-> [EvaluateWord(A, x[1]), EvaluateWord(A, x[2])]);
  
  Q:=F/rels; 
  B:=GeneratorsOfMonoid(Q);
  return MagmaIsomorphismByFunctionsNC(S, Q, 
   x -> EvaluateWord(B, Factorization(S, x)), 
   x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfMonoid(S)));

end);

# same method for ideals

InstallMethod(IsomorphismFpSemigroup, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  local rules, F, A, rels, Q, B;
  
  rules:=Enumerate(ExhaustiveData(S))!.rules;
  
  F:=FreeSemigroup(Length(GeneratorsOfSemigroup(S)));
  A:=GeneratorsOfSemigroup(F);
  rels:=List(rules, x-> [EvaluateWord(A, x[1]), EvaluateWord(A, x[2])]);
  
  Q:=F/rels; 
  B:=GeneratorsOfSemigroup(Q);
  return MagmaIsomorphismByFunctionsNC(S, Q, 
   x -> EvaluateWord(B, Factorization(S, x)), 
   x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfSemigroup(S)));
end);

# same method for ideals

InstallMethod(IsomorphismFpSemigroup, "for a finite monoid",
[IsMonoid and IsFinite],
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

  rules:=Enumerate(ExhaustiveData(S))!.rules;
  
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

# same method for ideals

InstallMethod(RightCayleyGraphSemigroup, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  return Enumerate(ExhaustiveData(S))!.right;
end);

# same method for ideals

InstallMethod(LeftCayleyGraphSemigroup, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  return Enumerate(ExhaustiveData(S))!.left;
end);


# same method for ideals
# this is declared in the library, but there is no method for semigroups in the
# library.

InstallMethod(Factorization,
"for a finite semigroup with generators and an associative element",
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(S, x)
  local pos;
  pos:=Position(ExhaustiveData(S), x); 
  if pos=fail then 
    return fail;
  fi;
  return ExhaustiveData(S)!.words[pos];
end);

#

InstallMethod(Enumerator, "for a Green's class",  [IsGreensClass], 
function(C)
  local data, rel;
  data:=ExhaustiveData(Parent(C));
  rel:=EquivalenceClassRelation(C);
  return data!.elts{rel!.data.comps[C!.index]};
end);

#

InstallMethod(Size, "for a Green's class", [IsGreensClass], 
function(C)
  return Length(EquivalenceClassRelation(C)!.data.comps[C!.index]);
end);

#

InstallMethod(\in, "for an associative element and Green's class",  
[IsAssociativeElement, IsGreensClass], 
function(x, C)
  local pos;

  pos:=Position(ExhaustiveData(Parent(C)), x);
  return pos<>fail and EquivalenceClassRelation(C)!.data.id[pos]=C!.index;
end);

#

BindGlobal("SEMIGROUPS_EquivalentClassOfElementNC", 
function(rel, rep, IsGreensXClass)
  local pos, type, out;

  pos:=Position(ExhaustiveData(Source(rel)), rep);
  if pos=fail then 
    Error("usage: the element in the 2nd argument does not belong to the ", 
    "semigroup,");
    return;
  fi;

  type:=NewType(CollectionsFamily(FamilyObj(rep)), 
     IsEquivalenceClass and IsEquivalenceClassDefaultRep and IsGreensXClass);
  out:=rec(); 
  ObjectifyWithAttributes(out, type, EquivalenceClassRelation, rel,
    Representative, rep, ParentAttr, Source(rel));
  
  out!.index:=rel!.data.id[pos];

  return out;
end);

InstallMethod(EquivalenceClassOfElementNC, 
"for a Green's R-relation and an associative element", 
[IsGreensRRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalentClassOfElementNC(rel, rep, IsGreensRClass);
end);

InstallMethod(EquivalenceClassOfElementNC, 
"for a Green's L-relation and an associative element", 
[IsGreensLRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalentClassOfElementNC(rel, rep, IsGreensLClass);
end);

InstallMethod(EquivalenceClassOfElementNC, 
"for a Green's H-relation and an associative element", 
[IsGreensHRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalentClassOfElementNC(rel, rep, IsGreensHClass);
end);

InstallMethod(EquivalenceClassOfElementNC, 
"for a Green's D-relation and an associative element", 
[IsGreensDRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalentClassOfElementNC(rel, rep, IsGreensDClass);
end);

#

BindGlobal("SEMIGROUPS_GreensXClasses", 
function(S, GreensXRelation, GreensXClassOfElement)
  local comps, elts, out, C, i;

  comps:=GreensXRelation(S)!.data.comps;
  elts:=ExhaustiveData(S)!.elts;
  out:=EmptyPlist(Length(comps));

  for i in [1..Length(comps)] do
    C:=GreensXClassOfElement(S, elts[i]);
    C!.index:=i;
    out[i]:=C;
  od;
  return out;
end);

#

InstallMethod(GreensLClasses, "for a finite semigroup with generators",  
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup], 
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensLRelation, GreensLClassOfElement);
end);

#

InstallMethod(GreensRClasses, "for a finite semigroup with generators",  
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup], 
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensRRelation, GreensRClassOfElement);
end);

#

InstallMethod(GreensHClasses, "for a finite semigroup with generators",  
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup], 
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensHRelation, GreensHClassOfElement);
end);

#

InstallMethod(GreensDClasses, "for a finite semigroup with generators",  
[IsSemigroup and IsFinite and HasGeneratorsOfSemigroup], 
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensDRelation, GreensDClassOfElement);
end);

#

BindGlobal("SEMIGROUPS_GreensXClassesOfClass", 
function(C, GreensXRelation, GreensXClassOfElement)
  local S, comp, id, seen, elts, out, i;
  
  S:=Parent(C);
  comp:=EquivalenceClassRelation(C)!.data.comps[C!.index];
  id:=GreensXRelation(Parent(C))!.data.id;
  seen:=BlistList([1..Length(id)], []);
  elts:=ExhaustiveData(S)!.elts;
  out:=EmptyPlist(Length(comp));
  
  for i in comp do 
    if not seen[id[i]] then 
      seen[id[i]]:=true;
      C:=GreensXClassOfElement(S, elts[i]);
      C!.index:=id[i];
      Add(out, C);
    fi;
  od;

  return out;
end);

#

InstallMethod(GreensLClasses, "for a Green's D-class", [IsGreensDClass], 
function(C)
  return SEMIGROUPS_GreensXClassesOfClass(C, GreensLRelation, GreensLClassOfElement);
end);

InstallMethod(GreensRClasses, "for a Green's D-class", [IsGreensDClass], 
function(C)
  return SEMIGROUPS_GreensXClassesOfClass(C, GreensRRelation, GreensRClassOfElement);
end);

InstallMethod(GreensHClasses, "for Green's class", [IsGreensClass], 2, 
# to beat the library method
function(C)
  if IsGreensRClass(C) or IsGreensLClass(C) or IsGreensDClass(C) then 
    return SEMIGROUPS_GreensXClassesOfClass(C, GreensHRelation, GreensHClassOfElement);
  fi;
  TryNextMethod();
end);

# Methods for things declared in the Semigroups package but not in the GAP
# library

# same method for ideals

InstallMethod(NrIdempotents, "for a semigroup",
[IsSemigroup], S-> Length(Idempotents(S)));

# same method for ideals

InstallMethod(NrRClasses, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  return Length(GreensRRelation(S)!.data.comps);
end);

InstallMethod(NrRClasses, "for a Green's D-class",
[IsGreensDClass],
function(D)
  return Length(GreensRClasses(D));
end);

# same method for ideals

InstallMethod(NrLClasses, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  return Length(GreensLRelation(S)!.data.comps);
end);

InstallMethod(NrLClasses, "for a Green's D-class",
[IsGreensDClass],
function(D)
  return Length(GreensLClasses(D));
end);

# same method for ideals

InstallMethod(NrDClasses, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  return Length(GreensDRelation(S)!.data.comps);
end);

InstallMethod(NrDClasses, "for a semigroup", [IsSemigroup],
function(S)
  return Length(GreensDClasses(S));
end);

# same method for ideals

InstallMethod(NrHClasses, "for a finite semigroup",
[IsSemigroup and IsFinite],
function(S)
  return Length(GreensHRelation(S)!.data.comps);
end);

InstallMethod(NrHClasses, "for a Green's class",
[IsGreensClass],
function(C)
  return Length(GreensHClasses(C));
end);

# same method for ideals

InstallMethod(MinimalIdeal, "for a finite semigroup",
[IsSemigroup],
function(S)
  local data, scc;
  data:=Enumerate(ExhaustiveData(S));
  scc:=GreensRRelation(S)!.data;
  return SemigroupIdeal(S, data!.elts[scc.comps[1][1]]);
end);

