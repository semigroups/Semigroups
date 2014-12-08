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
  iso := IsomorphismReesMatrixSemigroup(Range(cong));
  fam := FamilyObj(Range(cong));
  class := Objectify( NewType(fam, IsSimpleSemigroupCongruenceClass),
                      rec(rmsclass := rmsclass, iso := iso) );
  SetParentAttr(class, cong);
  SetRepresentative(class, Representative(rmsclass)^InverseGeneralMapping(iso));
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
[IsSimpleSemigroup]
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
  return List( ImagesElm(cong!.rmscong, elm^iso),
               x-> x^InverseGeneralMapping(iso) );
end);

#

InstallMethod(EquivalenceClasses,
"for a simple semigroup congruence",
[IsSimpleSemigroupCongruence],
function(cong)
  
end);