#############################################################################
##
#W  attributes-exhaust.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains methods for finding attributes of semigroups satisfying
# IsExhaustiveSemigroup.

# same method for ideals

InstallMethod(MinimalIdeal, "for an generic semigroup",
[IsExhaustiveSemigroup],
function(S)
  local data, scc;
  data:=Enumerate(GenericSemigroupData(S));
  scc:=GreensRRelation(S)!.data;
  return SemigroupIdeal(S, data!.elts[scc.comps[1][1]]);
  # the first component (i.e. the inner most) of the strongly connected
  # components of the right Cayley graph corresponds the minimal ideal. 

end);

