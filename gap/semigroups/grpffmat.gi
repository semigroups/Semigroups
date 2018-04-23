############################################################################
##
##  grpffmat.gi
##  Copyright (C) 2013-15                                James D. Mitchell
##                                                       Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# TODO special cases for 0 dimensional matrices over finite fields

InstallMethod(IsomorphismPermGroup, "for a matrix over finite field group",
[IsMatrixOverFiniteFieldGroup],
function(G)
  local iso1, iso2;

  iso1 := IsomorphismMatrixGroup(G);
  iso2 := IsomorphismPermGroup(Range(iso1));
  return GroupHomomorphismByFunction(G,
                                     Range(iso2),
                                     x -> (x ^ iso1) ^ iso2,
                                     x -> (x ^ InverseGeneralMapping(iso2)) ^
                                          InverseGeneralMapping(iso1));
end);

# TODO ViewString

InstallMethod(ViewObj,
"for a matrix over finite field group with generators",
[IsMatrixOverFiniteFieldGroup],
function(G)
  local gens, deg;

  if HasGeneratorsOfGroup(G) then
    gens := GeneratorsOfGroup(G);
  elif HasGeneratorsOfSemigroup(G) then
    gens := GeneratorsOfSemigroup(G);
  fi;
  deg := DimensionOfMatrixOverSemiring(gens[1]);
  Print("<group of ");
  Print(deg, "x", deg);
  Print(" matrices over ", BaseDomain(G));
  Print(" with ", Length(gens), " generator");
  if Length(gens) > 1 then
    Print("s");
  fi;
  Print(">");
end);

InstallMethod(IsGeneratorsOfMagmaWithInverses,
"for a matrix over finite field collection",
[IsMatrixOverFiniteFieldCollection],
function(coll)
  return ForAll(coll, x -> Inverse(x) <> fail);
end);

InstallMethod(GeneratorsOfGroup,
"for a matrix over finite field group with semigroup generators",
[IsMatrixOverFiniteFieldGroup and HasGeneratorsOfSemigroup],
GeneratorsOfSemigroup);

InstallMethod(GeneratorsOfSemigroup,
"for a matrix over finite field group with group generators",
[IsMatrixOverFiniteFieldGroup and HasGeneratorsOfGroup], GeneratorsOfGroup);

InstallMethod(IsomorphismMatrixGroup,
"for a matrix over finite field group as semigroup",
[IsMatrixOverFiniteFieldSemigroup],
function(G)
  local H, gens;

  if not IsGroupAsSemigroup(G) then
    ErrorNoReturn("Semigroups: IsomorphismMatrixGroup: usage,\n",
                  "the argument must be a group (as semigroup),");
  fi;

  if DimensionOfMatrixOverSemiringCollection(G) = 0 then
    H := TrivialGroup();
    return GroupHomomorphismByFunction(G, H, x -> One(H), x -> One(G));
  fi;

  if HasGeneratorsOfGroup(G) then
    gens := GeneratorsOfGroup(G);
  else
    gens := GeneratorsOfSemigroup(G);
    if not IsGeneratorsOfMagmaWithInverses(gens) then
      TryNextMethod();
    fi;
  fi;

  # Do not delete the next if statement it is required, run
  # extreme/semiffmat.tst
  if Length(gens) = 0 then
    H := Group(AsList(One(G)));
    return GroupHomomorphismByFunction(G, H, x -> One(H), x -> One(G));
  fi;
  return GroupHomomorphismByFunction(G,
                                     Group(List(gens, AsList)),
                                     AsList,
                                     g -> MatrixNC(Representative(G), g));
end);

InstallMethod(IsomorphismSemigroup,
"for IsMatrixOverFiniteFieldSemigroup and a matrix group with gens",
[IsMatrixOverFiniteFieldSemigroup, IsMatrixGroup and HasGeneratorsOfGroup],
function(filt, G)
  return IsomorphismSemigroup(filt, DefaultFieldOfMatrixGroup(G), G);
end);

InstallMethod(IsomorphismSemigroup,
"for IsMatrixOverFiniteFieldSemigroup, ring, and a matrix group with gens",
[IsMatrixOverFiniteFieldSemigroup,
 IsRing,
 IsMatrixGroup and HasGeneratorsOfGroup],
function(filt, R, G)
  local gens, iso;
  gens := GeneratorsOfGroup(G);
  iso := g -> NewMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
                                       R,
                                       g);
  return GroupHomomorphismByFunction(G,
                                     Group(List(gens, iso)),
                                     iso,
                                     AsList);
end);

InstallMethod(AsMatrixGroup,
"for a matrix over finite field group as semigroup",
[IsMatrixOverFiniteFieldSemigroup],
G -> Range(IsomorphismMatrixGroup(G)));

InstallMethod(Size, "for a matrix over finite field group as semigroup",
[IsMatrixOverFiniteFieldSemigroup and HasGeneratorsOfSemigroup and
 IsGeneratorsOfMagmaWithInverses],
S -> Size(Range(IsomorphismMatrixGroup(S))));

InstallMethod(Size, "for a matrix over finite field group",
[IsMatrixOverFiniteFieldGroup and HasGeneratorsOfGroup],
S -> Size(Range(IsomorphismMatrixGroup(S))));

InstallMethod(\in,
"for a matrix and matrix over finite field group as semigroup",
[IsMatrixOverFiniteField, IsMatrixOverFiniteFieldSemigroup and
 IsGroupAsSemigroup],
function(x, G)
  if BaseDomain(G) <> BaseDomain(x)
      or DimensionOfMatrixOverSemiringCollection(G)
         <> DimensionOfMatrixOverSemiring(x) then
    return false;
  elif DimensionOfMatrixOverSemiringCollection(G) = 0
      and DimensionOfMatrixOverSemiring(x) = 0 then
    return true;
  fi;
  return AsList(x) in AsMatrixGroup(G);
end);

InstallMethod(\^,
"for a matrix over finite field group and matrix over finite field",
[IsMatrixOverFiniteFieldGroup, IsMatrixOverFiniteField],
function(G, x)
  if BaseDomain(G) <> BaseDomain(x)
      or DimensionOfMatrixOverSemiringCollection(G)
         <> DimensionOfMatrixOverSemiring(x) then
    ErrorNoReturn("Semigroups: \\^ (for matrix over finite field ",
                  "group and matrix over finite field): usage,\n",
                  " the args must have the same base domain, degree, and\n",
                  " the second arg must be invertible,");
  elif IsOne(x) or DimensionOfMatrixOverSemiring(x) = 0 then
    return G;
  fi;
  return Range(IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup,
                                    AsMatrixGroup(G) ^ AsList(x)));
end);

InstallMethod(ClosureGroup,
"for a matrix over finite field group and matrix over finite field",
[IsMatrixOverFiniteFieldGroup, IsMatrixOverFiniteField],
function(G, x)
  if BaseDomain(G) <> BaseDomain(x)
      or DimensionOfMatrixOverSemiringCollection(G)
         <> DimensionOfMatrixOverSemiring(x)
      or Inverse(x) = fail then
    ErrorNoReturn("Semigroups: ClosureGroup (for matrix over finite",
                  " field group and matrix over finite field): usage,\n",
                  " the args must have the same base domain, degree, and\n",
                  " the second arg must be invertible,");
  fi;
  return ClosureGroup(G, [x]);
end);

InstallMethod(ClosureGroup,
"for a matrix over finite field group and collection",
[IsMatrixOverFiniteFieldGroup, IsMatrixOverFiniteFieldCollection],
function(G, coll)
  if BaseDomain(G) <> BaseDomain(coll)
      or DimensionOfMatrixOverSemiringCollection(G) <>
         DimensionOfMatrixOverSemiringCollection(coll)
      or ForAny(coll, x -> Inverse(x) = fail) then
    ErrorNoReturn("Semigroups: ClosureGroup (for matrix over ",
                  "finite field group",
                  " and matrix over finite field): usage,\n",
                  " the args must have the same base domain, degree, and\n",
                  " every matrix in the second arg must be invertible,");
  elif DimensionOfMatrixOverSemiringCollection(G) = 0 then
    return G;
  fi;
  return Range(IsomorphismSemigroup(IsMatrixOverFiniteFieldSemigroup,
                                    BaseDomain(G),
                                    ClosureGroup(AsMatrixGroup(G),
                                                 List(coll, AsList))));
end);
