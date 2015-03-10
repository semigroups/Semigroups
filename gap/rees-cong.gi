InstallMethod(ReesCongruenceOfSemigroupIdeal,
"for a semigroup ideal",
[IsSemigroupIdeal],
1,  # Use this method instead of the library
function(i)
  local s, fam, type, cong;
  s := Parent(i);
  # Construct the object
  fam := GeneralMappingsFamily(
                 ElementsFamily(FamilyObj(s)),
                 ElementsFamily(FamilyObj(s)) );
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

InstallMethod(NrCongruenceClasses,
"for a Rees congruence",
[IsReesCongruence],
function(cong)
  return Size(Range(cong)) - Size(SemigroupIdealOfReesCongruence(cong)) + 1;
end);

#

InstallMethod( \=,
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
          "the elements of the first arg <pair> ",
          "must be in the range of the second arg <cong>,");
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
  
end);

#
  
InstallMethod(MeetSemigroupCongruences,
"for two Rees congruences",
[IsReesCongruence, IsReesCongruence],
function(c1, c2)
  
end);

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
          "in the semigroup of first arg <cong>");
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
  fam := CollectionsFamily( FamilyObj(elm) );
  class := Objectify(NewType(fam, IsReesCongruenceClass),
      rec(is_ideal_class := is_ideal_class) );
  SetParentAttr(class, cong);
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

#

InstallMethod( \in,
"for an associative element and a Rees congruence class",
[IsAssociativeElement, IsReesCongruenceClass],
function(elm, class)
  if class!.is_ideal_class then
    return elm in SemigroupIdealOfReesCongruence(class);
  else
    return elm = Representative(class);
  fi;
end);

#

InstallMethod( \*,
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

InstallMethod(Size,
"for a Rees congruence class",
[IsReesCongruenceClass],
function(class)
  if class!.is_ideal_class then
    return Size(SemigroupIdealOfReesCongruence(
                   EquivalenceClassRelation(class) ));
  fi;
  return 1;
end);

#

InstallMethod( \=,
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
      if not [x,y] in cong then
        Add(pairs, [x,y]);
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
  return GeneratingPairsOfSemigroupCongruence(
                 AsSemigroupCongruenceByGeneratingPairs(cong) );
end);
