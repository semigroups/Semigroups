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

InstallMethod(GeneratorsOfGroup, 
"for an s-matrix group with semigroup generators",
[IsSMatrixGroup and HasGeneratorsOfSemigroup], GeneratorsOfSemigroup);

InstallMethod(IsomorphismMatrixGroup, "for an s-matrix group",
[IsSMatrixGroup], 
function(G)
  local gens;
  gens := GeneratorsOfGroup(G);
  return GroupHomomorphismByFunction(G, Group(List(gens, AsMatrix)), 
    AsMatrix, 
    g -> AsSMatrix(Representative(G), g));
end);

InstallMethod(IsomorphismMatrixSemigroup, "for a matrix group",
[IsMatrixGroup], 
function(G)
  local gens, iso;
  gens := GeneratorsOfGroup(G);
  iso := g -> AsSMatrix(Representative(G), g);
  return GroupHomomorphismByFunction(G, 
                                     Semigroup(List(gens, iso)), 
                                     iso,
                                     AsMatrix);
end);

InstallMethod(AsMatrixGroup, "for an s-matrix group", 
[IsSMatrixGroup], G -> Range(IsomorphismMatrixGroup(G)));

InstallMethod(Size, "for an s-matrix group",
[IsSMatrixGroup and HasGeneratorsOfSemigroup],
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
[IsSMatrixGroup, IsMatrixObj],
function(G, x)
  if BaseDomain(G) <> BaseDomain(x) 
      or DegreeOfMatrixSemigroup(G) <> DegreeOfSMatrix(x) 
      or Inverse(x) = fail then 
    Error("can't do it");
    return;
  elif IsOne(x) or DegreeOfSMatrix(x) = 0 then 
    return G;
  else
    return Range(IsomorphismMatrixSemigroup(AsMatrixGroup(G) ^ AsMatrix(x)));
  fi;
end);

InstallMethod(ClosureGroup, "for an s-matrix group and s-matrix",
[IsSMatrixGroup, IsMatrixObj],
function(G, x) 
  if BaseDomain(G) <> BaseDomain(x) 
      or DegreeOfMatrixSemigroup(G) <> DegreeOfSMatrix(x) 
      or Inverse(x) = fail then 
    Error("can't do it");
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
    Error("can't do it");
    return;
  fi;
  return Range(IsomorphismMatrixSemigroup(ClosureGroup(AsMatrixGroup(G),
                                                       List(coll, AsMatrix))));
end);
