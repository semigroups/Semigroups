############################################################################
##
#W  grpffmat.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##                                                       Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# TODO special cases for 0 dimensional matrices over finite fields

InstallMethod(IsomorphismPermGroup, "for an matrix over finite field group",
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
  #else
  #  TryNextMethod(); #FIXME ok?
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
"for an matrix over finite field collection",
[IsMatrixOverFiniteFieldCollection],
function(coll)
  return ForAll(coll, x -> Inverse(x) <> fail);
end);

InstallMethod(GeneratorsOfGroup,
"for an matrix over finite field group with semigroup generators",
[IsMatrixOverFiniteFieldGroup and HasGeneratorsOfSemigroup],
GeneratorsOfSemigroup);

InstallMethod(GeneratorsOfSemigroup,
"for an matrix over finite field group with group generators",
[IsMatrixOverFiniteFieldGroup and HasGeneratorsOfGroup], GeneratorsOfGroup);

InstallMethod(IsomorphismMatrixGroup, "for an matrix over finite field group",
[IsMatrixOverFiniteFieldGroup],
function(G)
  local H, gens;

  if DimensionOfMatrixOverSemiringCollection(G) = 0 then
    H := TrivialGroup();
    return GroupHomomorphismByFunction(G, H, x -> One(H), x -> One(G));
  fi;
  gens := GeneratorsOfGroup(G);
  #if Length(gens) = 0 then
  #  H := Group(AsList(One(G)));
  #  return GroupHomomorphismByFunction(G, H, x -> One(H), x -> One(G));
  #fi;
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
  # FIXME delete this or find an example where it applies
  #if Length(gens) = 0 then
  #  ErrorNoReturn("Semigroups: IsomorphismSemigroup: usage,\n",
  #                "the group must have at least one generator,");
  #fi;
  iso := g -> NewMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
                                       R,
                                       g);
  return GroupHomomorphismByFunction(G,
                                     Group(List(gens, iso)),
                                     iso,
                                     AsList);
end);

InstallMethod(AsMatrixGroup, "for an matrix over finite field group",
[IsMatrixOverFiniteFieldGroup], G -> Range(IsomorphismMatrixGroup(G)));

InstallMethod(Size, "for an matrix over finite field group",
[IsMatrixOverFiniteFieldGroup and HasGeneratorsOfSemigroup],
S -> Size(Range(IsomorphismMatrixGroup(S))));

InstallMethod(Size, "for an matrix over finite field group",
[IsMatrixOverFiniteFieldGroup and HasGeneratorsOfGroup],
S -> Size(Range(IsomorphismMatrixGroup(S))));

InstallMethod(\in,
"for an matrix over finite field and matrix over finite field group",
[IsMatrixOverFiniteField, IsMatrixOverFiniteFieldGroup],
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
"for an matrix over finite field group and matrix over finite field",
[IsMatrixOverFiniteFieldGroup, IsMatrixOverFiniteField],
function(G, x)
  if BaseDomain(G) <> BaseDomain(x)
      or DimensionOfMatrixOverSemiringCollection(G)
         <> DimensionOfMatrixOverSemiring(x) then
    ErrorNoReturn("Semigroups: \^ (for matrix over finite field ",
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
"for an matrix over finite field group and matrix over finite field",
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
"for an matrix over finite field group and collection",
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
