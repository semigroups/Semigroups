###########################################################################
##
#W  semigroupe.gd
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareAttribute("SEEData", IsFinite and IsSemigroup and
HasGeneratorsOfSemigroup, "mutable");
DeclareCategory("IsSEEData", IsList);
DeclareFilter("IsClosedSEEData", IsSEEData);

DeclareOperation("Enumerate", [IsSEEData]);
DeclareOperation("Enumerate", [IsSEEData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsSEEData, IsCyclotomic, IsFunction]);

# we use the attribute GreensXClasses to store the data structure for the sccs
# corresponding to Green's X-classes in the SEE data
DeclareAttribute("GreensHClasses", IsSEEData);
DeclareAttribute("GreensRClasses", IsSEEData);
DeclareAttribute("GreensLClasses", IsSEEData);
DeclareAttribute("GreensDClasses", IsSEEData);

