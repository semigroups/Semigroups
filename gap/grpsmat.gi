############################################################################
##
#W  grpsmat.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##                                                       Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# TODO special cases for 0 dimensional s-matrices

InstallMethod(IsomorphismPermGroup, "for an s-matrix group",
[IsSMatrixGroup], 
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
"for a s-matrix group with generators",
[IsSMatrixGroup],
function(G)
  local gens, deg;

  if HasGeneratorsOfGroup(G) then 
    gens := GeneratorsOfGroup(G);
  elif HasGeneratorsOfSemigroup(G) then  
    gens := GeneratorsOfSemigroup(G);
  else 
    TryNextMethod(); #FIXME ok?
  fi;
  deg := DegreeOfSMatrix(gens[1]);
  Print("<group of ");
  Print(deg, "x", deg);
  Print(" s-matrices over ", BaseDomain(G));
  Print(" with ", Length(gens), " generator");
  if Length(gens) > 1 then
    Print("s");
  fi;
  Print(">");
end);

InstallMethod(IsGeneratorsOfMagmaWithInverses, "for an s-matrix collection", 
[IsSMatrixCollection], 
function(coll)
  return ForAll(coll, x -> Inverse(x) <> fail);
end);

InstallMethod(GeneratorsOfGroup,  
"for an s-matrix group with semigroup generators",
[IsSMatrixGroup and HasGeneratorsOfSemigroup], GeneratorsOfSemigroup);

InstallMethod(GeneratorsOfSemigroup, 
"for an s-matrix group with group generators",
[IsSMatrixGroup and HasGeneratorsOfGroup], GeneratorsOfGroup);

InstallMethod(IsomorphismMatrixGroup, "for an s-matrix group",
[IsSMatrixGroup], 
function(G)
  local H, gens;

  if DegreeOfMatrixSemigroup(G) = 0 then 
    H := TrivialGroup();
    return GroupHomomorphismByFunction(G, H, x-> One(H), x-> One(G));
  fi;
  gens := GeneratorsOfGroup(G);
  return GroupHomomorphismByFunction(G, Group(List(gens, AsMatrix)), 
    AsMatrix, 
    g -> AsSMatrix(Representative(G), g));
end);

InstallMethod(IsomorphismMatrixSemigroup, "for a matrix group",
[IsMatrixGroup and HasGeneratorsOfGroup], 
function(G)
  return IsomorphismMatrixSemigroup(G, DefaultFieldOfMatrixGroup(G));
end);

InstallMethod(IsomorphismMatrixSemigroup, "for a matrix group and ring",
[IsMatrixGroup and HasGeneratorsOfGroup, IsRing], 
function(G, R)
  local gens, iso;
  gens := GeneratorsOfGroup(G);
  if Length(gens) = 0 then 
    Error("not yet implemented");
  fi;
  iso := g -> NewSMatrix(IsPlistSMatrixRep, R, 
                         DimensionOfMatrixGroup(G), g);
  return GroupHomomorphismByFunction(G, 
                                     Group(List(gens, iso)), 
                                     iso,
                                     AsMatrix);
end);

InstallMethod(AsMatrixGroup, "for an s-matrix group", 
[IsSMatrixGroup], G -> Range(IsomorphismMatrixGroup(G)));

InstallMethod(Size, "for an s-matrix group",
[IsSMatrixGroup and HasGeneratorsOfSemigroup],
S -> Size(Range(IsomorphismMatrixGroup(S))));

InstallMethod(Size, "for an s-matrix group",
[IsSMatrixGroup and HasGeneratorsOfGroup],
S -> Size(Range(IsomorphismMatrixGroup(S))));

InstallMethod(\in, "for an s-matrix and s-matrix group",
[IsSMatrix, IsSMatrixGroup],
function(x, G)
  if BaseDomain(G) <> BaseDomain(x) 
      or DegreeOfMatrixSemigroup(G) <> DegreeOfSMatrix(x) then 
    return false;
  else
    return AsMatrix(x) in AsMatrixGroup(G);
  fi;
end);

InstallMethod(\^, "for an s-matrix group and s-matrix",
[IsSMatrixGroup, IsSMatrix],
function(G, x)
  if BaseDomain(G) <> BaseDomain(x) 
      or DegreeOfMatrixSemigroup(G) <> DegreeOfSMatrix(x) 
      or Inverse(x) = fail then 
    Error("Semigroups: ^ (for s-matrix group and s-matrix): usage\n",
          " the args must have the same base domain, degree, and\n",
          " the second arg must be invertible");
    return;
  elif IsOne(x) or DegreeOfSMatrix(x) = 0 then 
    return G;
  else
    return Range(IsomorphismMatrixSemigroup(AsMatrixGroup(G) ^ AsMatrix(x)));
  fi;
end);

InstallMethod(ClosureGroup, "for an s-matrix group and s-matrix",
[IsSMatrixGroup, IsSMatrix],
function(G, x) 
  if BaseDomain(G) <> BaseDomain(x) 
      or DegreeOfMatrixSemigroup(G) <> DegreeOfSMatrix(x) 
      or Inverse(x) = fail then 
    Error("Semigroups: ClosureGroup (for s-matrix group and s-matrix): usage\n",
          " the args must have the same base domain, degree, and\n",
          " the second arg must be invertible");
    return;
  fi;
  return ClosureGroup(G, [x]);
end);

InstallMethod(ClosureGroup, "for an s-matrix group and collection",
[IsSMatrixGroup, IsSMatrixCollection],
function(G, coll) 
  if BaseDomain(G) <> BaseDomain(coll) 
      or DegreeOfMatrixSemigroup(G) <> DegreeOfSMatrixCollection(coll) 
      or ForAny(coll, x-> Inverse(x) = fail) then 
    Error("Semigroups: ClosureGroup (for s-matrix group and s-matrix): usage\n",
          " the args must have the same base domain, degree, and\n",
          " every matrix in the second arg must be invertible");
    return;
  elif DegreeOfMatrixSemigroup(G) = 0 then 
    return G;
  fi;
  return Range(IsomorphismMatrixSemigroup(ClosureGroup(AsMatrixGroup(G),
                                                       List(coll, AsMatrix)), BaseDomain(G)));
end);
