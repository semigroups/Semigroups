InstallGlobalFunction(SimpleSemigroupCongruenceNC,
function(s, ltcong)
  local iso, r, fam, cong;
  # Find the isomorphism from s to r
  iso := IsomorphismReesMatrixSemigroup(s);
  r := Range(ltcong);
  
  # Construct the object
  fam := GeneralMappingsFamily(
                 ElementsFamily(FamilyObj(s)),
                 ElementsFamily(FamilyObj(s)) );
  cong := Objectify( NewType(fam, IsSimpleSemigroupCongruence),
                     rec(rmscong := ltcong, iso := iso) );
  SetSource(cong, s);
  SetRange(cong, s);
  return cong;
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
  #TODO
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