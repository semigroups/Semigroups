#############################################################################
##
##  attributes/dual.gd
##  Copyright (C) 2018-2022                              James D. Mitchell
##                                                              Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsDualSemigroupElement", IsAssociativeElement);
DeclareCategoryCollections("IsDualSemigroupElement");
DeclareAttribute("DualSemigroup", IsSemigroup);

# TODO (major release): remove "Rep" and update docs
DeclareCategory("IsDualSemigroupRep",
                IsDualSemigroupElementCollection and IsSemigroup);

DeclareAttribute("DualSemigroupOfFamily", IsFamily);
DeclareAttribute("AntiIsomorphismDualSemigroup", IsSemigroup);
DeclareGlobalFunction("UnderlyingElementOfDualSemigroupElement");

InstallTrueMethod(IsDualSemigroupRep,
                  IsSemigroup and IsDualSemigroupElementCollection);
