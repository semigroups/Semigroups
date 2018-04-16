###############################################################################
##
##  freeinverse.gd
##  Copyright (C) 2013-15                                  Julius Jonusas
##
##  Licensing information can be foundin the README file of this package.
##
###############################################################################

DeclareUserPreference(rec(
  name := "FreeInverseSemigroupElementDisplay",
  description := ["options for the display of free inverse semigroup elements"],
  default := "minimal",
  values := ["minimal", "canonical"],
  multi := false,
  package := "semigroups"));

DeclareCategory("IsFreeInverseSemigroupElement", IsAssociativeElement);
DeclareCategoryCollections("IsFreeInverseSemigroupElement");

DeclareCategory("IsFreeInverseSemigroupCategory",
                IsInverseSemigroup
                and IsGeneratorsOfInverseSemigroup
                and IsFreeInverseSemigroupElementCollection);
DeclareProperty("IsFreeInverseSemigroup", IsSemigroup);

DeclareGlobalFunction("FreeInverseSemigroup");

DeclareAttribute("MinimalWord", IsFreeInverseSemigroupElement);
DeclareAttribute("CanonicalForm", IsFreeInverseSemigroupElement);
