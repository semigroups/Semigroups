############################################################################
##
#W  grpmatobj.gd
#Y  Copyright (C) 2013-15                                James D. Mitchell
##                                                       Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(IsomorphismMatrixGroup, "for a matrix obj group",
[SEMIGROUPS_IsMatrixObjGroup], 
function(G)
  local gens;

  gens := GeneratorsOfGroup(G);
  return GroupHomomorphismByFunction(G, Group(List(gens, g -> g![ROWSPOS])), 
    g -> g![ROWSPOS], 
    g -> NewMatrix(IsPlistMatrixRep, BaseDomain(G), Length(g), g));
end);

