#############################################################################
##
##  semigroups/semifp.gd
##  Copyright (C) 2020-2022                                   Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareUserPreference(rec(
  name := "FpSemigroupView",
  description := ["options for the display of fp semigroups and monoids"],
  default := "gap-library",
  values := ["semigroups-pkg", "gap-library"],
  multi := false,
  package := "semigroups"));

DeclareOperation("ElementOfFpSemigroup", [IsFpSemigroup, IsAssocWord]);
DeclareOperation("ElementOfFpMonoid", [IsFpMonoid, IsAssocWord]);
DeclareOperation("ParseRelations", [IsDenseList, IsString]);
DeclareAttribute("UnderlyingCongruence", IsFpSemigroup);
DeclareAttribute("UnderlyingCongruence", IsFpMonoid);
