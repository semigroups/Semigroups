InstallGlobalFunction(SimpleSemigroupCongruenceNC,
function(s, rmscong)
  local iso, r, fam, cong;
  # Find the isomorphism from s to r
  iso := IsomorphismReesMatrixSemigroup(s);
  r := Range(rmscong);

  # Construct the object
  fam := GeneralMappingsFamily(
                 ElementsFamily(FamilyObj(s)),
                 ElementsFamily(FamilyObj(s)) );
  cong := Objectify( NewType(fam, IsSimpleSemigroupCongruence),
                     rec(rmscong := rmscong, iso := iso) );
  SetSource(cong, s);
  SetRange(cong, s);
  return cong;
end);

#

InstallGlobalFunction(SimpleSemigroupCongruenceClassNC,
function(cong, rmsclass)
  local iso, fam, class;
  iso := IsomorphismReesMatrixSemigroup(Range(cong));
  fam := FamilyObj(Range(cong));
  class := Objectify( NewType(fam, IsSimpleSemigroupCongruenceClass),
                      rec(rmsclass := rmsclass, iso := iso) );
  SetParentAttr(class, cong);
  SetRepresentative(class, Representative(rmsclass)^InverseGeneralMapping(iso));
  SetEquivalenceClassRelation(class, cong);
  return class;
end);

#

InstallMethod(ViewObj,
"for a simple semigroup congruence",
[IsSimpleSemigroupCongruence],
function(cong)
  Print("<simple semigroup congruence (",
        StructureDescription(cong!.rmscong!.n:short), ",",
        Size(cong!.rmscong!.colBlocks), ",",
        Size(cong!.rmscong!.rowBlocks),")>");
end);

#

InstallMethod(CongruencesOfSemigroup,
"for a simple semigroup",
[IsSimpleSemigroup and IsFinite],
function(s)
  return List( CongruencesOfSemigroup(Range(IsomorphismReesMatrixSemigroup(s))),
               cong-> SimpleSemigroupCongruenceNC(s, cong) );
end);

#

InstallMethod(\=,
"for two simple semigroup congruences",
[IsSimpleSemigroupCongruence, IsSimpleSemigroupCongruence],
function(cong1, cong2)
  return (Range(cong1) = Range(cong2) and cong1!.rmscong = cong2!.rmscong);
end);

#

InstallMethod(\in,
"for a Rees matrix semigroup element collection and a simple semigroup congruence",
[IsAssociativeElementCollection, IsSimpleSemigroupCongruence],
function(pair, cong)
  local s;
  # Check for validity
  if Size(pair) <> 2 then
    Error("usage: 1st argument <pair> must be a list of length 2,");
    return;
  fi;
  s := Range(cong);
  if not ForAll(pair, x-> x in s) then
    Error("usage: the elements of the 1st argument <pair> ",
          "must be in the range of the 2nd argument <cong>,");
    return;
  fi;
  return [pair[1]^cong!.iso, pair[2]^cong!.iso] in cong!.rmscong;
end);

#

InstallMethod(ImagesElm,
"for a simple semigroup congruence and an associative element",
[IsSimpleSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  return List( ImagesElm(cong!.rmscong, elm^cong!.iso),
               x-> x^InverseGeneralMapping(cong!.iso) );
end);

#

InstallMethod(EquivalenceClasses,
"for a simple semigroup congruence",
[IsSimpleSemigroupCongruence],
function(cong)
  return List( EquivalenceClasses(cong!.rmscong),
               c-> SimpleSemigroupCongruenceClassNC(cong, c) );
end);

#

InstallMethod(EquivalenceClassOfElementNC,
"for a simple semigroup congruence",
[IsSimpleSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  return SimpleSemigroupCongruenceClassNC(cong,
      EquivalenceClassOfElementNC(cong!.rmscong, elm^cong!.iso) );
end);

#

InstallMethod(\in,
"for an associative element and a simple semigroup congruence class",
[IsAssociativeElement, IsSimpleSemigroupCongruenceClass],
function(elm, class)
  return (elm^EquivalenceClassRelation(class)!.iso in class!.rmsclass);
end);

#

InstallMethod(\*,
"for two simple semigroup congruence classes",
[IsSimpleSemigroupCongruenceClass, IsSimpleSemigroupCongruenceClass],
function(c1, c2)
  return SimpleSemigroupCongruenceClassNC( EquivalenceClassRelation(c1),
                                           c1!.rmsclass * c2!.rmsclass );
end);

#

InstallMethod(Size,
"for a simple semigroup congruence class",
[IsSimpleSemigroupCongruenceClass],
function(class)
  return Size(class!.rmsclass);
end);

#

InstallMethod( \=,
"for two simple semigroup congruence classes",
[IsSimpleSemigroupCongruenceClass, IsSimpleSemigroupCongruenceClass],
function(c1, c2)
  return EquivalenceClassRelation(c1) = EquivalenceClassRelation(c2) and
         c1!.rmsclass = c2!.rmsclass;
end);

#

InstallMethod(GeneratingPairsOfMagmaCongruence,
"for a simple semigroup congruence",
[IsSimpleSemigroupCongruence],
function(cong)
  local map;
  map := InverseGeneralMapping(cong!.iso);
  return List( GeneratingPairsOfMagmaCongruence(cong!.rmscong),
               x-> [x[1]^map, x[2]^map] );
end);

#

