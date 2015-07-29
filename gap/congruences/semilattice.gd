############################################################################
##
#W  cong-semilattice.gd
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on semilattices.
##

DeclareProperty("IsSemilatticeCongruence",
                IsSemigroupCongruence and IsAttributeStoringRep);
DeclareCategory("IsSemilatticeCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep and
                IsAssociativeElement);
DeclareAttribute("MeetsOfGeneratingPairs", IsSemilatticeCongruence);
DeclareAttribute("BlockCoincidenceTable", IsSemilatticeCongruence);

DeclareCategory("SEMIGROUPS_SemilatticeCongruenceClass",
                IsCongruenceClass and IsAttributeStoringRep and
                IsAssociativeElement);
DeclareGlobalFunction("SEMIGROUPS_SemilatticeCongClassNoOfElm");

DeclareGlobalFunction("SemilatticeElementsBetween");
DeclareGlobalFunction("SemilatticeElementsBetweenNC");
