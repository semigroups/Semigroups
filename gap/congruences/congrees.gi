############################################################################
##
##  congruences/congrees.gi
##  Copyright (C) 2015-2022                               Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for Rees congruences; i.e. semigroup congruences
## defined by a two-sided ideal.  See Howie 1.7
##

#############################################################################
# Testers + fundamental representation specific attributes
#############################################################################

# The following requires CanComputeEquivalenceRelationPartition, because
# otherwise, it applies to other congruences, which don't have a method for
# NrEquivalenceClasses.

InstallMethod(IsReesCongruence, "for a semigroup congruence",
[CanComputeEquivalenceRelationPartition],
9,  # to beat the library method
function(C)
  local S, classes, nontrivial, i, class, I;
  if not IsSemigroupCongruence(C) then
    return false;
  fi;
  # This function is adapted from code in the library
  S := Range(C);
  if NrEquivalenceClasses(C) = Size(S) then
    # Trivial congruence - only possible I is zero
    if MultiplicativeZero(S) <> fail then
      SetSemigroupIdealOfReesCongruence(C, MinimalIdeal(S));
      return true;
    else
      return false;
    fi;
  else
    # Search for non-trivial classes
    classes := EquivalenceClasses(C);
    nontrivial := 0;
    for i in [1 .. Length(classes)] do
      if Size(classes[i]) > 1 then
        if nontrivial = 0 then
          nontrivial := i;
        else
          # Two non-trivial classes
          return false;
        fi;
      fi;
    od;

    # Only one non-trivial class - check it is an I
    class := classes[nontrivial];
    I := SemigroupIdeal(S, AsList(class));
    if Size(class) = Size(I) then
      SetSemigroupIdealOfReesCongruence(C, I);
      return true;
    else
      return false;
    fi;
  fi;
end);

InstallMethod(ReesCongruenceOfSemigroupIdeal, "for a semigroup ideal",
[IsSemigroupIdeal],
function(I)
  local S, fam, type, C;
  S := Parent(I);
  # Construct the object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  type := NewType(fam,
                  IsSemigroupCongruence
                  and IsMagmaCongruence
                  and IsAttributeStoringRep
                  and CanComputeEquivalenceRelationPartition);
  C := Objectify(type, rec());
  # Set some attributes
  SetSource(C, S);
  SetRange(C, S);
  SetSemigroupIdealOfReesCongruence(C, I);
  SetIsReesCongruence(C, true);
  return C;
end);

InstallMethod(AsSemigroupCongruenceByGeneratingPairs, "for a Rees congruence",
[IsReesCongruence],
function(C)
  local S, gens, min, pairs, y, x;
  S := Range(C);
  gens := MinimalIdealGeneratingSet(SemigroupIdealOfReesCongruence(C));
  min := MinimalIdeal(S);
  pairs := [];
  C := SemigroupCongruence(S, pairs);
  for y in min do
    for x in gens do
      if not [x, y] in C then
        Add(pairs, [x, y]);
        C := SemigroupCongruence(S, pairs);
      fi;
    od;
  od;
  return C;
end);

InstallMethod(GeneratingPairsOfMagmaCongruence, "for a Rees congruence",
[IsReesCongruence],
function(C)
  C := AsSemigroupCongruenceByGeneratingPairs(C);
  return GeneratingPairsOfSemigroupCongruence(C);
end);

InstallMethod(ViewObj, "for a Rees congruence",
[IsReesCongruence],
function(C)
  Print("<Rees congruence of ");
  ViewObj(SemigroupIdealOfReesCongruence(C));
  Print(" over ");
  ViewObj(Range(C));
  Print(">");
end);

InstallMethod(PrintObj, "for a Rees congruence",
[IsReesCongruence],
function(C)
  Print("ReesCongruenceOfSemigroupIdeal( ");
  Print(SemigroupIdealOfReesCongruence(C));
  Print(" )");
end);

#############################################################################
# The mandatory things that must be implemented for
# congruences belonging to CanComputeEquivalenceRelationPartition
#############################################################################

InstallMethod(EquivalenceRelationPartition, "for a Rees congruence",
[IsReesCongruence], C -> [AsList(SemigroupIdealOfReesCongruence(C))]);

InstallMethod(ImagesElm,
"for a Rees congruence and a multiplicative element",
[IsReesCongruence, IsMultiplicativeElement],
function(C, x)
  if not x in Range(C) then
    ErrorNoReturn("the 2nd argument (a mult. elt.) does not belong to ",
                  "the range of the 1st argument (a Rees congruence)");
  elif x in SemigroupIdealOfReesCongruence(C) then
    return Elements(SemigroupIdealOfReesCongruence(C));
  else
    return [x];
  fi;
end);

InstallMethod(CongruenceTestMembershipNC,
"for Rees congruence and two multiplicative elements",
[IsReesCongruence, IsMultiplicativeElement, IsMultiplicativeElement],
function(C, lhop, rhop)
  local I;
  I := SemigroupIdealOfReesCongruence(C);
  return (lhop = rhop) or (lhop in I and rhop in I);
end);

########################################################################
# The non-mandatory things where we have a superior method for this particular
# representation.
########################################################################

########################################################################
# Operators
########################################################################

InstallMethod(\=, "for two Rees congruences",
[IsReesCongruence, IsReesCongruence],
function(lhop, rhop)
  return SemigroupIdealOfReesCongruence(lhop) =
         SemigroupIdealOfReesCongruence(rhop);
end);

InstallMethod(IsSubrelation, "for two Rees congruences",
[IsReesCongruence, IsReesCongruence],
function(lhop, rhop)
  local I1, I2;
  # Tests whether rhop is a subcongruence of lhop
  if Range(lhop) <> Range(rhop) then
    Error("the 1st and 2nd arguments are congruences over different",
          " semigroups");
  fi;
  I1 := SemigroupIdealOfReesCongruence(lhop);
  I2 := SemigroupIdealOfReesCongruence(rhop);
  return ForAll(GeneratorsOfSemigroupIdeal(I2), gen -> gen in I1);
end);

InstallMethod(JoinSemigroupCongruences, "for two Rees congruences",
[IsReesCongruence, IsReesCongruence],
function(lhop, rhop)
  local gens1, gens2, I;
  if Range(lhop) <> Range(rhop) then
    Error("cannot form the join of congruences over different semigroups");
  fi;
  gens1 := GeneratorsOfSemigroupIdeal(SemigroupIdealOfReesCongruence(lhop));
  gens2 := GeneratorsOfSemigroupIdeal(SemigroupIdealOfReesCongruence(rhop));
  I := SemigroupIdeal(Range(lhop), Concatenation(gens1, gens2));
  I := SemigroupIdeal(Range(lhop), MinimalIdealGeneratingSet(I));
  return ReesCongruenceOfSemigroupIdeal(I);
end);

########################################################################
# Equivalence classes
########################################################################

InstallMethod(NrEquivalenceClasses, "for a Rees congruence",
[IsReesCongruence],
C -> Size(Range(C)) - Size(SemigroupIdealOfReesCongruence(C)) + 1);

InstallMethod(EquivalenceClasses, "for a Rees congruence", [IsReesCongruence],
function(C)
  local classes, I, next, x;
  classes := EmptyPlist(NrEquivalenceClasses(C));
  I := SemigroupIdealOfReesCongruence(C);
  classes[1] := EquivalenceClassOfElementNC(C, I.1);
  next := 2;
  for x in Range(C) do
    if not (x in I) then
      classes[next] := EquivalenceClassOfElementNC(C, x);
      next := next + 1;
    fi;
  od;
  return classes;
end);

InstallMethod(NonTrivialEquivalenceClasses, "for a Rees congruence",
[IsReesCongruence],
function(C)
  local I;
  I := SemigroupIdealOfReesCongruence(C);
  if Size(I) = 1 then
    return [];
  fi;
  return [EquivalenceClassOfElementNC(C, I.1)];
end);

InstallMethod(EquivalenceClassOfElementNC,
"for a Rees congruence and a multiplicative element",
[IsReesCongruence, IsMultiplicativeElement],
function(C, x)
  local is_ideal_class, fam, class;
  # Ensure consistency of representatives
  if x in SemigroupIdealOfReesCongruence(C) then
    is_ideal_class := true;
    x := SemigroupIdealOfReesCongruence(C).1;
  else
    is_ideal_class := false;
  fi;

  # Construct the object
  fam := CollectionsFamily(FamilyObj(x));
  class := Objectify(NewType(fam,
                             IsReesCongruenceClass
                             and IsLeftRightOrTwoSidedCongruenceClass),
                     rec());
  if is_ideal_class then
    SetSize(class, Size(SemigroupIdealOfReesCongruence(C)));
  else
    SetSize(class, 1);
  fi;
  SetParentAttr(class, Range(C));
  SetEquivalenceClassRelation(class, C);
  SetRepresentative(class, x);
  return class;
end);

########################################################################
# Operators for classes
########################################################################

InstallMethod(\*, "for two Rees congruence classes",
[IsReesCongruenceClass, IsReesCongruenceClass],
function(lhop, rhop)
  if EquivalenceClassRelation(lhop) <> EquivalenceClassRelation(rhop) then
    ErrorNoReturn("the arguments (cong. classes) are not classes of the same ",
                  "congruence");
  elif Size(lhop) > 1 then
    return lhop;
  elif Size(rhop) > 1 then
    return rhop;
  fi;
  return EquivalenceClassOfElementNC(EquivalenceClassRelation(lhop),
                                     Representative(lhop) *
                                     Representative(rhop));
end);

InstallMethod(\=, "for two Rees congruence classes",
[IsReesCongruenceClass, IsReesCongruenceClass],
function(lhop, rhop)
  return EquivalenceClassRelation(lhop) = EquivalenceClassRelation(rhop)
    and (Representative(lhop) = Representative(rhop))
         or (Size(lhop) > 1 and Size(rhop) > 1);
end);

InstallMethod(\in,
"for a multiplicative element and a Rees congruence class",
[IsMultiplicativeElement, IsReesCongruenceClass],
function(x, class)
  local rel;
  if Size(class) > 1 then
    rel := EquivalenceClassRelation(class);
    return x in SemigroupIdealOfReesCongruence(rel);
  else
    return x = Representative(class);
  fi;
end);
