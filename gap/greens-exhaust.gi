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

InstallMethod(GreensRRelation, "for an exhaustive semigroup", 
[IsExhaustiveSemigroup], 
function(S)
  local fam, rel, sc;

  fam := GeneralMappingsFamily( ElementsFamily(FamilyObj(S)),
           ElementsFamily(FamilyObj(S)) );

  rel := Objectify(NewType(fam,
           IsEquivalenceRelation and IsEquivalenceRelationDefaultRep
           and IsGreensRRelation and IsExhaustiveSemigroupGreensRelation),
           rec());

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

InstallMethod(GreensLRelation, "for an exhaustive semigroup",
[IsExhaustiveSemigroup], 
function(S)
  local fam, rel;

  fam := GeneralMappingsFamily( ElementsFamily(FamilyObj(S)),
          ElementsFamily(FamilyObj(S)) );

  rel := Objectify(NewType(fam,
          IsEquivalenceRelation and IsEquivalenceRelationDefaultRep
          and IsGreensLRelation and IsExhaustiveSemigroupGreensRelation), 
          rec());

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

InstallMethod(GreensJRelation, "for an exhaustive semigroup",
[IsExhaustiveSemigroup], GreensDRelation);

# same method for ideals

InstallMethod(GreensDRelation, "for an exhaustive semigroup",
[IsExhaustiveSemigroup], 
function(S)
    local fam, rel;

  fam := GeneralMappingsFamily( ElementsFamily(FamilyObj(S)),
          ElementsFamily(FamilyObj(S)) );

  rel := Objectify(NewType(fam,
          IsEquivalenceRelation and IsEquivalenceRelationDefaultRep
          and IsGreensDRelation and IsExhaustiveSemigroupGreensRelation), 
          rec());

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

InstallMethod(GreensHRelation, "for an exhaustive semigroup",  
[IsExhaustiveSemigroup], 
function(S)
    local fam, rel;

  fam := GeneralMappingsFamily( ElementsFamily(FamilyObj(S)),
          ElementsFamily(FamilyObj(S)) );
  rel := Objectify(NewType(fam,
          IsEquivalenceRelation and IsEquivalenceRelationDefaultRep
          and IsGreensHRelation and IsExhaustiveSemigroupGreensRelation),
          rec());

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

InstallMethod(Enumerator, "for an exhaustive semigroup Green's class",
[IsExhaustiveSemigroupGreensClass], 
function(C)
  local data, rel;
  data:=ExhaustiveData(Parent(C));
  rel:=EquivalenceClassRelation(C);
  return data!.elts{rel!.data.comps[C!.index]};
end);

#

InstallMethod(Size, "for an exhaustive semigroup Green's class",
[IsExhaustiveSemigroupGreensClass], 
function(C)
  return Length(EquivalenceClassRelation(C)!.data.comps[C!.index]);
end);

#

InstallMethod(\in, 
"for an associative element and an exhaustive semigroup Green's class",  
[IsAssociativeElement, IsExhaustiveSemigroupGreensClass], 
function(x, C)
  local pos;

  pos:=Position(ExhaustiveData(Parent(C)), x);
  return pos<>fail and EquivalenceClassRelation(C)!.data.id[pos]=C!.index;
end);

# there are not methods for EquivalenceClassOfElementNC in this case since we
# require the variable <pos> below, and if it can't be determined, then we can't
# make the Green's class. 

BindGlobal("SEMIGROUPS_EquivalenceClassOfElement", 
function(rel, rep, type)
  local pos, out;

  pos:=Position(ExhaustiveData(Source(rel)), rep);
  if pos=fail then 
    Error("usage: the element in the 2nd argument does not belong to the ", 
    "semigroup,");
    return;
  fi;

  out:=rec(); 
  ObjectifyWithAttributes(out, type, EquivalenceClassRelation, rel,
    Representative, rep, ParentAttr, Source(rel));
  
  out!.index:=rel!.data.id[pos];

  return out;
end);

#

InstallMethod(EquivalenceClassOfElement, 
"for an exhaustive semigroup Green's R-relation and an associative element", 
[IsGreensRRelation and IsExhaustiveSemigroupGreensRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalenceClassOfElement(rel, rep, RClassType(Source(rel));
end);

InstallMethod(EquivalenceClassOfElement, 
"for an exhaustive semigroup Green's L-relation and an associative element", 
[IsGreensLRelation and IsExhaustiveSemigroupGreensRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalenceClassOfElement(rel, rep, IsGreensLClass);
end);

InstallMethod(EquivalenceClassOfElement, 
"for an exhaustive semigroup Green's H-relation and an associative element", 
[IsGreensHRelation and IsExhaustiveSemigroupGreensRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalenceClassOfElement(rel, rep, IsGreensHClass);
end);

InstallMethod(EquivalenceClassOfElement, 
"for an exhaustive semigroup Green's D-relation and an associative element", 
[IsGreensDRelation and IsExhaustiveSemigroupGreensRelation, IsAssociativeElement],
function(rel, rep)
  return SEMIGROUPS_EquivalenceClassOfElement(rel, rep, IsGreensDClass);
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

InstallMethod(GreensLClasses, "for a Green's D-class", 
[IsGreensDClass and IsExhaustiveSemigroupGreensClass], 
function(C)
  return SEMIGROUPS_GreensXClassesOfClass(C, GreensLRelation, GreensLClassOfElement);
end);

InstallMethod(GreensRClasses, "for a Green's D-class", 
[IsGreensDClass and IsExhaustiveSemigroupGreensClass], 
function(C)
  return SEMIGROUPS_GreensXClassesOfClass(C, GreensRRelation, GreensRClassOfElement);
end);

InstallMethod(GreensHClasses, "for an exhaustive semigroup Green's class",
[IsExhaustiveSemigroupGreensClass],
function(C)
  if IsGreensRClass(C) or IsGreensLClass(C) or IsGreensDClass(C) then 
    return SEMIGROUPS_GreensXClassesOfClass(C, GreensHRelation, GreensHClassOfElement);
  fi;
  Error("usage: the argument should be a Greens R-, L-, or D-class,");
  return;
end);

# Methods for things declared in the Semigroups package but not in the GAP
# library

# same method for ideals

InstallMethod(NrRClasses, "for an exhaustive semigroup",
[IsExhaustiveSemigroup],
function(S)
  return Length(GreensRRelation(S)!.data.comps);
end);

# same method for ideals

InstallMethod(NrLClasses, "for an exhaustive semigroup",
[IsExhaustiveSemigroup],
function(S)
  return Length(GreensLRelation(S)!.data.comps);
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

#

InstallMethod(DClassType, "for an exhaustive semigroup",
[IsExhaustiveSemigroup],
function(S)
  return NewType( FamilyObj(S), IsEquivalenceClass and
          IsEquivalenceClassDefaultRep and IsGreensDClass and
          IsExhaustiveSemigroupGreensClass);
end);

InstallMethod(HClassType, "for an exhaustive semigroup",
[IsExhaustiveSemigroup],
function(S)
 return NewType( FamilyObj(S), IsEquivalenceClass and
  IsEquivalenceClassDefaultRep and IsGreensHClass and
  IsExhaustiveSemigroupGreensClass);
end);

InstallMethod(LClassType, "for a exhaustive semigroup",
[IsExhaustiveSemigroup],
function(S)
  return NewType( FamilyObj(S), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensLClass and
         IsExhaustiveSemigroupGreensClass);
end);

InstallMethod(RClassType, "for a exhaustive semigroup",
[IsExhaustiveSemigroup],
function(S)
  return NewType( FamilyObj(S), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensRClass and
         IsExhaustiveSemigroupGreensClass);
end);
