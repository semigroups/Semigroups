############################################################################
##
#W  grpmatobj.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##                                                       Markus Pfeiffer
##
##  Licensing inforxion can be found in the README file of this package.
##
#############################################################################
##

# FIXME this should be a method for IsMatrixSemigroup

InstallMethod(BaseDomain, "for a matrix obj group", 
[SEMIGROUPS_IsMatrixObjGroup and HasGeneratorsOfSemigroup], 
G -> BaseDomain(G.1));

InstallMethod(IsomorphismMatrixGroup, "for a matrix obj group",
[SEMIGROUPS_IsMatrixObjGroup], 
function(G)
  local gens;
  gens := GeneratorsOfGroup(G);
  return GroupHomomorphismByFunction(G, Group(List(gens, AsMatrix)), 
    AsMatrix, 
    g -> NewMatrix(IsPlistMatrixRep, BaseDomain(G), Length(g), g));
end);

InstallMethod(IsomorphismMatrixObjGroup, "for a matrix group",
[IsMatrixGroup], 
function(G)
  local gens, iso;
  gens := GeneratorsOfGroup(G);
  iso := g -> NewMatrix(IsPlistMatrixRep, DefaultFieldOfMatrixGroup(G),
                        Length(g), g);
  return GroupHomomorphismByFunction(G, Semigroup(List(gens, iso)), iso,
                                     AsMatrix);
end);

InstallMethod(AsMatrixGroup, "for a matrix obj group", 
[SEMIGROUPS_IsMatrixObjGroup], 
G -> Range(IsomorphismMatrixGroup(G)));

InstallMethod(Size, "for a matrix obj group",
[SEMIGROUPS_IsMatrixObjGroup and HasGeneratorsOfSemigroup],
SEMIGROUPS_MatrixObjGroupRankIncrement,
S -> Size(Range(IsomorphismMatrixGroup(S))));

InstallMethod(\in, "for a matrix obj and matrix obj group",
[IsMatrixObj, SEMIGROUPS_IsMatrixObjGroup],
SEMIGROUPS_MatrixObjGroupRankIncrement,
function(x, G)
  if BaseDomain(G) <> BaseDomain(x) then 
    return false;
  else
    return AsMatrix(x) in AsMatrixGroup(G);
  fi;
end);

InstallMethod(\^, "for a matrix obj group and matrix obj",
[SEMIGROUPS_IsMatrixObjGroup, IsMatrixObj],
SEMIGROUPS_MatrixObjGroupRankIncrement,
function(G, x)
  if DimensionsMat(x) <> DimensionsOfMatrixSemigroup(G) 
      or BaseDomain(x) <> BaseDomain(G) then 
    Error("can't do it");
    return;
  elif IsOne(x) or DimensionsMat(x) = [0, 0] then 
    return G;
  elif x ^ -1 <> fail then 
    return Range(IsomorphismMatrixObjGroup(AsMatrixGroup(G) ^ AsMatrix(x)));
  else 
    Error("can't do it");
    return;
  fi;
end);

InstallMethod(ClosureGroup, "for a matrix obj group and matrix obj",
[SEMIGROUPS_IsMatrixObjGroup, IsMatrixObj],
SEMIGROUPS_MatrixObjGroupRankIncrement,
function(G, x) 
  if DimensionsMat(x) <> DimensionsOfMatrixSemigroup(G) 
      or BaseDomain(x) <> BaseDomain(G) then 
    Error("can't do it");
    return;
  fi;
  return ClosureGroupNC(G, [x]);
end);

InstallMethod(ClosureGroup, "for a matrix obj group and collection",
[SEMIGROUPS_IsMatrixObjGroup, IsCollection],
SEMIGROUPS_MatrixObjGroupRankIncrement,
function(G, coll) 
  if not (IsSMatrixCollection(coll) 
      and IsGeneratorsOfSemigroup(coll) 
      and DimensionsMat(coll[1]) = DimensionsOfMatrixSemigroup(G) 
      and BaseDomain(coll[1]) = BaseDomain(G)) then 
    Error("can't do it");
    return;
  fi;
  return ClosureGroupNC(G, coll);
end);

InstallMethod(ClosureGroupNC, "for a matrix obj group and collection",
[SEMIGROUPS_IsMatrixObjGroup, IsCollection],
SEMIGROUPS_MatrixObjGroupRankIncrement,
function(G, coll) 
  return Range(IsomorphismMatrixObjGroup(ClosureGroup(AsMatrixGroup(G),
                                                      List(coll, AsMatrix))));
end);
