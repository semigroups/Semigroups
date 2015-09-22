############################################################################
##
#W  rees-cong.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for Rees congruences; i.e. semigroup congruences
## defined by a two-sided ideal.
##

InstallMethod(IsReesCongruence,
"for a semigroup congruence",
[IsSemigroupCongruence],
1, #FIXME why is this here?
function(cong)
  local s, classes, sizes, pos, class, ideal;
  # This function is adapted from code in the library
  s := Range(cong);
  if NrCongruenceClasses(cong) = Size(s) then
    # Trivial congruence - only possible ideal is zero
    if MultiplicativeZero(s) <> fail then
      SetSemigroupIdealOfReesCongruence(cong, MinimalIdeal(s));
      return true;
    else
      return false;
    fi;
  else
    # Find all the non-trivial classes
    classes := EquivalenceClasses(cong);
    sizes := List(classes, Size);
    pos := PositionsProperty(sizes, n -> n > 1);
    if Length(pos) > 1 then
      return false;
    fi;
    # Only one non-trivial class - check it is an ideal
    class := classes[pos[1]];
    ideal := SemigroupIdeal(s, AsList(class));
    if Size(class) = Size(ideal) then
      SetSemigroupIdealOfReesCongruence(cong, ideal);
      return true;
    else
      return false;
    fi;
  fi;
end);

#

InstallMethod(ReesCongruenceOfSemigroupIdeal,
"for a semigroup ideal",
[IsSemigroupIdeal],
1, #FIXME why is this here?
function(i)
  local s, fam, type, cong;
  s := Parent(i);
  # Construct the object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(s)),
                               ElementsFamily(FamilyObj(s)));
  type := NewType(fam, IsSemigroupCongruence and IsAttributeStoringRep);
  cong := Objectify(type, rec());
  # Set some attributes
  SetSource(cong, s);
  SetRange(cong, s);
  SetSemigroupIdealOfReesCongruence(cong, i);
  SetIsReesCongruence(cong, true);
  return cong;
end);

#

InstallMethod(ViewObj,
"for a Rees congruence",
[IsReesCongruence],
function(cong)
  Print("<Rees congruence of ");
  ViewObj(SemigroupIdealOfReesCongruence(cong));
  Print(" over ");
  ViewObj(Range(cong));
  Print(">");
end);

#

InstallMethod(PrintObj,
"for a Rees congruence",
[IsReesCongruence],
function(cong)
  Print("ReesCongruenceOfSemigroupIdeal( ");
  PrintObj(SemigroupIdealOfReesCongruence(cong));
  Print(" )");
end);

#

InstallMethod(NrCongruenceClasses,
"for a Rees congruence",
[IsReesCongruence],
cong -> Size(Range(cong)) - Size(SemigroupIdealOfReesCongruence(cong)) + 1);

#

InstallMethod(\=,
"for two Rees congrunces",
[IsReesCongruence, IsReesCongruence],
function(c1, c2)
  return SemigroupIdealOfReesCongruence(c1) =
         SemigroupIdealOfReesCongruence(c2);
end);

#

InstallMethod(\in,
"for an associative element collection and a Rees congruence",
[IsAssociativeElementCollection, IsReesCongruence],
function(pair, cong)
  local s, i;
  # Check for validity
  if Size(pair) <> 2 then
    Error("Semigroups: \in: usage,\n",
          "the first arg <pair> must be a list of length 2,");
    return;
  fi;
  s := Range(cong);
  if not ForAll(pair, x -> x in s) then
    Error("Semigroups: \in: usage,\n",
          "the elements of 1st arg <pair> ",
          "must be in the range of 2nd arg <cong>,");
    return;
  fi;
  i := SemigroupIdealOfReesCongruence(cong);
  return (pair[1] = pair[2]) or (pair[1] in i and pair[2] in i);
end);

#

InstallMethod(ImagesElm,
"for a Rees congruence and an associative element",
[IsReesCongruence, IsAssociativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    Error("Semigroups: ImagesElm: usage,\n",
          "the args <cong> and <elm> must refer to the same semigroup,");
    return;
  fi;
  if elm in SemigroupIdealOfReesCongruence(cong) then
    return Elements(SemigroupIdealOfReesCongruence(cong));
  else
    return [elm];
  fi;
end);

#

InstallMethod(JoinSemigroupCongruences,
"for two Rees congruences",
[IsReesCongruence, IsReesCongruence],
function(c1, c2)
  local gens1, gens2, i;
  if Range(c1) <> Range(c2) then
    Error("Semigroups: JoinSemigroupCongruences: usage,\n",
          "the args <c1> and <c2> must be congruences of the same semigroup,");
    return;
  fi;
  gens1 := GeneratorsOfSemigroupIdeal(SemigroupIdealOfReesCongruence(c1));
  gens2 := GeneratorsOfSemigroupIdeal(SemigroupIdealOfReesCongruence(c2));
  i := SemigroupIdeal(Range(c1), Concatenation(gens1, gens2));
  i := SemigroupIdeal(Range(c1), MinimalIdealGeneratingSet(i));
  return ReesCongruenceOfSemigroupIdeal(i);
end);

#

#InstallMethod(MeetSemigroupCongruences,
#"for two Rees congruences",
#[IsReesCongruence, IsReesCongruence],
#function(c1, c2)
#
#end);

#

InstallMethod(EquivalenceClasses,
"for a Rees congruence",
[IsReesCongruence],
function(cong)
  local classes, i, next, x;
  classes := EmptyPlist(NrCongruenceClasses(cong));
  i := SemigroupIdealOfReesCongruence(cong);
  classes[1] := EquivalenceClassOfElementNC(cong, i.1);
  next := 2;
  for x in Range(cong) do
    if not (x in i) then
      classes[next] := EquivalenceClassOfElementNC(cong, x);
      next := next + 1;
    fi;
  od;
  return classes;
end);

#

InstallMethod(EquivalenceClassOfElement,
"for a Rees congruence and an associative element",
[IsReesCongruence, IsAssociativeElement],
function(cong, elm)
  # Check that the args make sense
  if not elm in Range(cong) then
    Error("Semigroups: EquivalenceClassOfElement: usage,\n",
          "the second arg <elm> must be ",
          "in the semigroup of first arg <cong>,");
    return;
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

#

InstallMethod(EquivalenceClassOfElementNC,
"for a Rees congruence and an associative element",
[IsReesCongruence, IsAssociativeElement],
function(cong, elm)
  local is_ideal_class, fam, class;
  # Ensure consistency of representatives
  if elm in SemigroupIdealOfReesCongruence(cong) then
    is_ideal_class := true;
    elm := SemigroupIdealOfReesCongruence(cong).1;
  else
    is_ideal_class := false;
  fi;

  # Construct the object
  fam := CollectionsFamily(FamilyObj(elm));
  class := Objectify(NewType(fam, IsReesCongruenceClass),
                     rec(is_ideal_class := is_ideal_class));
  SetParentAttr(class, cong);
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

#

InstallMethod(\in,
"for an associative element and a Rees congruence class",
[IsAssociativeElement, IsReesCongruenceClass],
function(elm, class)
  if class!.is_ideal_class then
    return elm in SemigroupIdealOfReesCongruence(Parent(class));
  else
    return elm = Representative(class);
  fi;
end);

#

InstallMethod(\*,
"for two Rees congruence classes",
[IsReesCongruenceClass, IsReesCongruenceClass],
function(c1, c2)
  if not Parent(c1) = Parent(c2) then
    Error("Semigroups: \*: usage,\n",
          "the args <c1> and <c2> must be classes of the same congruence,");
    return;
  fi;
  if c1!.is_ideal_class then
    return c1;
  fi;
  if c2!.is_ideal_class then
    return c2;
  fi;
  return EquivalenceClassOfElementNC(Parent(c1),
                                     Representative(c1) * Representative(c2));
end);

#

InstallMethod(Size, "for a Rees congruence class",
[IsReesCongruenceClass],
function(class)
  local rel;

  if class!.is_ideal_class then
    rel := EquivalenceClassRelation(class);
    return Size(SemigroupIdealOfReesCongruence(rel));
  fi;
  return 1;
end);

#

InstallMethod(\=,
"for two Rees congruence classes",
[IsReesCongruenceClass, IsReesCongruenceClass],
function(c1, c2)
  return (Representative(c1) = Representative(c2))
         or (c1!.is_ideal_class and c2!.is_ideal_class);
end);

#

InstallMethod(AsSemigroupCongruenceByGeneratingPairs,
"for a Rees congruence",
[IsReesCongruence],
function(cong)
  local s, gens, min, nrclasses, pairs, y, x;
  s := Range(cong);
  gens := MinimalIdealGeneratingSet(SemigroupIdealOfReesCongruence(cong));
  min := MinimalIdeal(s);
  nrclasses := NrCongruenceClasses(cong);
  pairs := [];
  cong := SemigroupCongruence(s, pairs);
  for y in min do
    for x in gens do
      if not [x, y] in cong then
        Add(pairs, [x, y]);
        cong := SemigroupCongruence(s, pairs);
      fi;
    od;
  od;
  return cong;
end);

#

InstallMethod(GeneratingPairsOfMagmaCongruence,
"for a Rees congruence",
[IsReesCongruence],
function(cong)
  cong := AsSemigroupCongruenceByGeneratingPairs(cong);
  return GeneratingPairsOfSemigroupCongruence(cong);
end);

#

InstallMethod(OneImmutable,
"for a congruence class",
[IsCongruenceClass],
function(class)
  local cong, one;
  cong := EquivalenceClassRelation(class);
  one := One(Range(cong));
  if one <> fail then
    return EquivalenceClassOfElementNC(cong, one);
  fi;
  return fail;
end);

#

InstallMethod(Enumerator,
"for a Rees congruence class",
[IsReesCongruenceClass],
function(class)
  local cong;
  if class!.is_ideal_class then
    cong := EquivalenceClassRelation(class);
    return Enumerator(SemigroupIdealOfReesCongruence(cong));
  fi;
  return AsList(class);
end);
