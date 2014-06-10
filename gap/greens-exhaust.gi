#############################################################################
##
#W  greens-exhaust.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# this file contains methods for Green's relations and classes of semigroups
# that satisfy IsExhaustiveSemigroup.

# Green's classes: the idea is to only store the index of the equivalence class
# corresponding to the Green's class, then look everything up in the data. The
# equivalence class data structures for R-, L-, H-, D-classes of a finite
# semigroup <S> are stored in the !.data component of the corresponding Green's
# relation.

# same method for ideals

InstallMethod(GreensRRelation, "for a semigroup", [IsExhaustiveSemigroup], 
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

InstallMethod(GreensLRelation, "for a semigroup",  [IsExhaustiveSemigroup], 
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

InstallMethod(GreensJRelation, "for an exhaustive semigroup",  [IsExhaustiveSemigroup], 
GreensDRelation);

# same method for ideals

InstallMethod(GreensDRelation, "for a semigroup",  [IsExhaustiveSemigroup], 
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

InstallMethod(GreensHRelation, "for a semigroup",  [IsExhaustiveSemigroup], 
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

# same method for ideals

InstallMethod(GreensLClasses, "for an exhaustive semigroup",  
[IsExhaustiveSemigroup], 
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensLRelation, GreensLClassOfElement);
end);

# same method for ideals

InstallMethod(GreensRClasses, "for an exhaustive semigroup",  
[IsExhaustiveSemigroup], 
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensRRelation, GreensRClassOfElement);
end);

# same method for ideals

InstallMethod(GreensHClasses, "for an exhaustive semigroup",  
[IsExhaustiveSemigroup], 
function(S)
  return SEMIGROUPS_GreensXClasses(S, GreensHRelation, GreensHClassOfElement);
end);

# same method for ideals

InstallMethod(GreensDClasses, "for an exhaustive semigroup",  
[IsExhaustiveSemigroup], 
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

InstallMethod(NrRClasses, "for an exhaustive semigroup",
[IsExhaustiveSemigroup],
function(S)
  return Length(GreensRRelation(S)!.data.comps);
end);

InstallMethod(NrRClasses, "for a Green's D-class",
[IsGreensDClass],
function(D)
  return Length(GreensRClasses(D));
end);

# same method for ideals

InstallMethod(NrLClasses, "for an exhaustive semigroup",
[IsExhaustiveSemigroup],
function(S)
  return Length(GreensLRelation(S)!.data.comps);
end);

InstallMethod(NrLClasses, "for a Green's D-class",
[IsGreensDClass],
function(D)
  return Length(GreensLClasses(D));
end);

# same method for ideals

InstallMethod(NrDClasses, "for an exhaustive semigroup",
[IsExhaustiveSemigroup],
function(S)
  return Length(GreensDRelation(S)!.data.comps);
end);


# same method for ideals

InstallMethod(NrHClasses, "for an exhaustive semigroup",
[IsExhaustiveSemigroup],
function(S)
  return Length(GreensHRelation(S)!.data.comps);
end);

InstallMethod(NrHClasses, "for a Green's class",
[IsGreensClass],
function(C)
  return Length(GreensHClasses(C));
end);

