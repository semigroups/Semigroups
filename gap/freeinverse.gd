###############################################################################
##
#W  freeinverse.gd
#Y  Copyright (C) 2013-14                                  Julius Jonusas
## 
##  Licensing information can be foundin the README file of this package.
##
###############################################################################

DeclareUserPreference(rec(
  name:= "FreeInverseSemigroupElementDisplay",
  description:= ["options for the display of free inverse semigroup elements"],
  default:= "minimal",
  values := ["minimal", "canonical"],
  multi := false,
  package := "semigroups"));

DeclareCategory("IsFreeInverseSemigroupElement", IsAssociativeElement);
DeclareCategoryCollections("IsFreeInverseSemigroupElement");
DeclareProperty("IsFreeInverseSemigroup", IsSemigroup);
DeclareGlobalFunction("FreeInverseSemigroup");
DeclareAttribute("MinimalWord", IsFreeInverseSemigroupElement);
DeclareAttribute("CanonicalForm", IsFreeInverseSemigroupElement);
