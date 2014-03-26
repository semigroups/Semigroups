InstallGlobalFunction(UniversalSemigroupCongruence,
function(s)
  local fam, cong;
  fam := GeneralMappingsFamily(
                 ElementsFamily(FamilyObj(s)),
                 ElementsFamily(FamilyObj(s)) );
  cong := Objectify(NewType(fam, IsUniversalSemigroupCongruence), rec());
  SetSource(cong, s);
  SetRange(cong, s);
  return cong;
end);
        
#

InstallMethod(ViewObj,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  Print("<universal semigroup congruence>");
end);

#

InstallMethod( \=,
"for two universal semigroup congruences",
[IsUniversalSemigroupCongruence, IsUniversalSemigroupCongruence],
function(cong1, cong2)
  return( Range(cong1) = Range(cong2) );
end);

#

InstallMethod( \=,
"for universal semigroup congruence and Rees 0-matrix semigroup congruence by linked triple",
[IsUniversalSemigroupCongruence, IsRZMSCongruenceByLinkedTriple],
function(ucong, cong)
  return false;
end);

#

InstallMethod( \=,
"for Rees 0-matrix semigroup congruence by linked triple and universal semigroup congruence",
[IsRZMSCongruenceByLinkedTriple, IsUniversalSemigroupCongruence],
function(cong, ucong)
  return false;
end);

#

InstallMethod( \in,
"for dense list and universal semigroup congruence",
[IsDenseList, IsUniversalSemigroupCongruence],
function(pair, cong)
  return( Size(pair) = 2
          and pair[1] in Range(cong)
          and pair[2] in Range(cong) );
end);

#

InstallMethod(ImagesElm,
"for universal semigroup congruence and element",
[IsUniversalSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    Error("usage: 2nd argument <elm> must be in <cong>'s semigroup"); return; 
  fi;
  return Elements(Range(cong));
end);

#

InstallMethod(NrCongruenceClasses,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  return 1;
end);

#

InstallMethod(JoinSemigroupCongruences,
"for two universal semigroup congruences",
[IsUniversalSemigroupCongruence, IsUniversalSemigroupCongruence],
function(ucong1, ucong2)
  return ucong1;
end);

#

InstallMethod(JoinSemigroupCongruences,
"for Rees 0-matrix semigroup congruence by linked triple and universal semigroup congruence",
[IsRZMSCongruenceByLinkedTriple, IsUniversalSemigroupCongruence],
function(cong, ucong)
  return ucong;
end);

#

InstallMethod(JoinSemigroupCongruences,
"for universal semigroup congruence and Rees 0-matrix semigroup congruence by linked triple",
[IsUniversalSemigroupCongruence, IsRZMSCongruenceByLinkedTriple],
function(ucong, cong)
  return ucong;
end);

#

InstallMethod(MeetSemigroupCongruences,
"for two universal semigroup congruences",
[IsUniversalSemigroupCongruence, IsUniversalSemigroupCongruence],
function(ucong1, ucong2)
  return ucong1;
end);

#

InstallMethod(MeetSemigroupCongruences,
"for Rees 0-matrix semigroup congruence by linked triple and universal semigroup congruence",
[IsRZMSCongruenceByLinkedTriple, IsUniversalSemigroupCongruence],
function(cong, ucong)
  return cong;
end);

#

InstallMethod(MeetSemigroupCongruences,
"for universal semigroup congruence and Rees 0-matrix semigroup congruence by linked triple",
[IsUniversalSemigroupCongruence, IsRZMSCongruenceByLinkedTriple],
function(ucong, cong)
  return cong;
end);

#

InstallMethod(EquivalenceClasses,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  return [EquivalenceClassOfElement(cong, Representative(Range(cong)))];
end);

#

InstallMethod(EquivalenceClassOfElement,
"for universal semigroup congruence and associative element",
[IsUniversalSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  # Check that the arguments make sense
  if not elm in Range(cong) then
    Error("usage: 2nd argument <elm> should be ",
          "in the semigroup of 1st argument <cong>");
    return;
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

#

InstallMethod(EquivalenceClassOfElementNC,
"for universal semigroup congruence and associative element",
[IsUniversalSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  local fam, class;
  fam := CollectionsFamily( FamilyObj(elm) );
  class := Objectify(NewType(fam, IsUniversalSemigroupCongruenceClass), rec() );
  SetParentAttr(class, cong);
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  return class;
end);
        
#

InstallMethod( \in,
"for associative element and universal semigroup congruence class",
[IsAssociativeElement, IsUniversalSemigroupCongruenceClass],
function(elm, class)
  return( elm in Range(ParentAttr(class)) );
end);

#

InstallMethod( \*,
"for two universal semigroup congruence classes",
[IsUniversalSemigroupCongruenceClass, IsUniversalSemigroupCongruenceClass],
function(c1, c2)
  if ParentAttr(c1) <> ParentAttr(c2) then
    Error("arguments <c1> and <c2> must be over the same congruence"); return;
  fi;
  return c1;
end);

#

InstallMethod(Size,
"for universal semigroup congruence class",
[IsUniversalSemigroupCongruenceClass],
function(class)
  return 1;
end);

#

InstallMethod( \=,
"for two universal semigroup congruence classes",
[IsUniversalSemigroupCongruenceClass, IsUniversalSemigroupCongruenceClass],
function(c1, c2)
  return( ParentAttr(c1) = ParentAttr(c2) );
end);

#

InstallMethod(GeneratingPairsOfMagmaCongruence,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  local s;
  s := Range(cong);
  return List(Elements(s), x-> [x, Representative(s)]);
end);

#