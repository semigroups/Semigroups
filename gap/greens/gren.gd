#############################################################################
##
#W  gren.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

## This file contains methods for Green's classes/relations for semigroups
## in IsEnumerableSemigroupRep.

# Green's classes in this representation have a single component which contains
# the strongly connected compontents of the appropriate Cayley graph, it
# contains two subcomponents consisting of the actual components and a lookup
# table <t> such that the value of <t[i]> is the index of the class containing
# EnumeratorCanonical(S)[i], where S is the semigroup.

DeclareRepresentation("IsEnumerableSemigroupGreensClassRep",
                      IsComponentObjectRep and IsGreensClass,
                      ["data"]);

DeclareRepresentation("IsEnumerableSemigroupGreensRelationRep",
                      IsComponentObjectRep and IsGreensRelation,
                      ["data"]);

DeclareAttribute("DClassType", IsEnumerableSemigroupRep);
DeclareAttribute("HClassType", IsEnumerableSemigroupRep);
DeclareAttribute("LClassType", IsEnumerableSemigroupRep);
DeclareAttribute("RClassType", IsEnumerableSemigroupRep);
