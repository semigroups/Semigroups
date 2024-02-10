############################################################################
##
##  semigroups/semicons.gd
##  Copyright (C) 2015-2022                                 Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
##

DeclareGlobalFunction("TrivialSemigroup");
DeclareConstructor("TrivialSemigroupCons", [IsSemigroup, IsInt]);
DeclareGlobalFunction("RectangularBand");
DeclareConstructor("RectangularBandCons", [IsSemigroup, IsPosInt, IsPosInt]);
DeclareGlobalFunction("FreeSemilattice");
DeclareConstructor("FreeSemilatticeCons", [IsSemigroup, IsPosInt]);
DeclareGlobalFunction("MonogenicSemigroup");
DeclareConstructor("MonogenicSemigroupCons", [IsSemigroup, IsPosInt, IsPosInt]);
DeclareGlobalFunction("ZeroSemigroup");
DeclareConstructor("ZeroSemigroupCons", [IsSemigroup, IsPosInt]);
DeclareGlobalFunction("LeftZeroSemigroup");
DeclareGlobalFunction("RightZeroSemigroup");
DeclareConstructor("BrandtSemigroupCons", [IsSemigroup, IsGroup, IsPosInt]);
DeclareGlobalFunction("BrandtSemigroup");

DeclareCategory("IsSSSE", IsAssociativeElement);
DeclareCategoryCollections("IsSSSE");

# Objects in IsSSERep have 3 slots:
# 1) the strong semilattice of semigroup of which this is an element;
# 2) the node of the digraph;
# 3) the underlying semigroup element itself.
DeclareRepresentation("IsSSSERep", IsSSSE and IsPositionalObjectRep, 3);

DeclareOperation("StrongSemilatticeOfSemigroups", [IsDigraph, IsList, IsList]);
DeclareOperation("UnderlyingSemilatticeOfSemigroups", [IsSSSERep]);

DeclareCategory("IsStrongSemilatticeOfSemigroups",
                IsSemigroup and IsSSSECollection);
DeclareAttribute("SemilatticeOfStrongSemilatticeOfSemigroups",
                 IsStrongSemilatticeOfSemigroups);
DeclareAttribute("SemigroupsOfStrongSemilatticeOfSemigroups",
                 IsStrongSemilatticeOfSemigroups);
DeclareAttribute("HomomorphismsOfStrongSemilatticeOfSemigroups",
                 IsStrongSemilatticeOfSemigroups);
DeclareAttribute("ElementTypeOfStrongSemilatticeOfSemigroups",
                 IsStrongSemilatticeOfSemigroups);

DeclareOperation("SSSE",
                 [IsStrongSemilatticeOfSemigroups,
                  IsPosInt,
                  IsAssociativeElement]);
