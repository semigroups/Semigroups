###############################################################################
##
#W  freeinverse.gd
#Y  Copyright (C) 2013                                 Julius Jonusas
## 
##  Licensing information can be foundin the README file of this package.
##
###############################################################################

DeclareUserPreference(rec(
  name:= "FreeInverseSemigroupElementDisplay",
  description:= ["options for the display of free inverse semigroup elements"],
  default:= "minimal",
  check:=function(a) return (IsString(a) and a in ["minimal", "canonical"]);end));

DeclareCategory("IsFreeInverseSemigroupElement",
  IsAssociativeElementWithUniqueSemigroupInverse);
DeclareCategoryCollections("IsFreeInverseSemigroupElement");
DeclareProperty("IsFreeInverseSemigroup", IsSemigroup);
DeclareGlobalFunction("FreeInverseSemigroup");
DeclareAttribute("MinimalWord", IsFreeInverseSemigroupElement);
DeclareAttribute("CanonicalForm", IsFreeInverseSemigroupElement);
