InstallMethod(ReesCongruenceOfSemigroupIdeal,
"for semigroup ideal",
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
"for a rees congruence",
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
"for a rees congruence",
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
"for associative element collection and Rees congruence",
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
"for Rees congruence and associative element",
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
"for Rees congruence",
[IsReesCongruence],
function(cong)
  classes := EmptyPlist(NrCongruenceClasses(cong));
  i := SemigroupIdealOfReesCongruence(cong);
  classes[1] := EquivalenceClassOfElementNC(cong, i.1);
  next := 2;
  for x in Range(cong) do
    if not (x in i) then
      classes[next] := EquivalenceClassOfElementNC(cong, x);
    fi;
  od;
  return classes;
end);

#

InstallMethod(EquivalenceClassOfElement,
"for Rees congruence and an associative element",
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
"for Rees congruence and an associative element",
[IsReesCongruence, IsAssociativeElement],
function(cong, elm)
  fam := CollectionsFamily( FamilyObj(elm) );
  class := Objectify(NewType(fam, IsReesCongruenceClass),
      rec(is_ideal_class := elm in SemigroupIdealOfReesCongruence(cong)) );
  SetParentAttr(class, cong);
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

#

#InstallMethod( \in,
#"for associative element and a Rees congruence class",
#[IsAssociativeElement, IsReesCongruenceClass],
#function(elm, class)
#  i := SemigroupIdealOfReesCongruence(class);
#  rep := Representative(class);
#  return elm = rep or (elm in i and rep in i);
#end);
#
#

#InstallMethod( \*,
#"for two RMS congruence classes by linked triple",
#[IsRMSCongruenceClassByLinkedTriple, IsRMSCongruenceClassByLinkedTriple],
#function(c1, c2)
#  
#end);
#
#
#
#InstallMethod(Size,
#"for Rees congruence class",
#[],
#function(class)
#end);
#
#InstallMethod( \=,
#"for two congruence classes by linked triple",
#[IsRMSCongruenceClassByLinkedTriple, IsRMSCongruenceClassByLinkedTriple],
        
InstallMethod(AsSemigroupCongruenceByGeneratingPairs,
"for Rees congruence",
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
"for Rees congruence",
[IsReesCongruence],
function(cong)
  return GeneratingPairsOfSemigroupCongruence(
                 AsSemigroupCongruenceByGeneratingPairs(cong) );
end);
