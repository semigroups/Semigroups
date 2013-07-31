###############################################################################
##
#W  freeinverse.gd
#Y  Copyright (C) 2013                                 Julius Jonusas
## 
##  Licensing information can be foundin the README file of this package.
##
###############################################################################

DeclareCategory("IsFreeInverseSemigroupElement",
  IsAssociativeElementWithUniqueSemigroupInverse);
DeclareCategoryCollections("IsFreeInverseSemigroupElement");
DeclareProperty("IsFreeInverseSemigroup", IsSemigroup);
DeclareGlobalFunction("FreeInverseSemigroup");
DeclareAttribute("CanonicalForm", IsFreeInverseSemigroupElement);
