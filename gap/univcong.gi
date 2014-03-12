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
#  Print("<universal congruence on ");
#  ViewObj(Range(cong));
#  Print(">");
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
[IsUniversalSemigroupCongruence, IsRMSCongruenceByLinkedTriple],
function(u, c)
  return( Range(u) = Range(c)
          and c!.nCoset = UnderlyingSemigroup(Range(c))
          and Size(c!.colBlocks) = 1
          and Size(c!.rowBlocks) = 1 );
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

InstallMethod(EquivalenceClasses,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  #TODO: Only one class
end);

#
