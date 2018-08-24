#############################################################################
##
##  dual.gd
##  Copyright (C) 2018                                      James D. Mitchell
##                                                          Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsDualSemigroupElement", IsAssociativeElement);
DeclareCategoryCollections("IsDualSemigroupElement");
DeclareAttribute("DualSemigroup", IsSemigroup);

# Every semigroup is mathematically a dual semigroup
# What we care about is whether it is represented as one
DeclareRepresentation("IsDualSemigroupRep",
                      IsEnumerableSemigroupRep and
                        IsDualSemigroupElementCollection,
                      []);

DeclareAttribute("DualSemigroupOfFamily", IsFamily);
DeclareAttribute("AntiIsomorphismDualSemigroup", IsSemigroup);
DeclareGlobalFunction("UnderlyingElementOfDualSemigroupElement");

InstallTrueMethod(IsDualSemigroupRep,
                  IsSemigroup and IsDualSemigroupElementCollection);
