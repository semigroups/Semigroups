#############################################################################
##
##  semigroups/semipperm.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("DigraphOfActionOnPoints", IsPartialPermSemigroup);
DeclareOperation("DigraphOfActionOnPoints",
                 [IsPartialPermSemigroup, IsInt]);

DeclareAttribute("FixedPointsOfPartialPermSemigroup",
                 IsPartialPermSemigroup);
DeclareAttribute("CyclesOfPartialPermSemigroup",
                 IsPartialPermSemigroup);
DeclareAttribute("ComponentRepsOfPartialPermSemigroup",
                 IsPartialPermSemigroup);
DeclareAttribute("ComponentsOfPartialPermSemigroup",
                 IsPartialPermSemigroup);

DeclareAttribute("ComponentRepresentatives", IsSemigroup);

DeclareAttribute("SmallerDegreePartialPermRepresentation",
                 IsInverseSemigroup and IsGeneratorsOfInverseSemigroup);
