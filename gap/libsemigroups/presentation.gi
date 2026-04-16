#############################################################################
##
##  libsemigroups/presentation.gi
##  Copyright (C) 2025                                      Joseph Edwards
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallGlobalFunction("LibsemigroupsPresentation",
function(S)
  local F, SS, R, add_rule, pair;

  Assert(1, IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
            or IsFpMonoid(S) or (HasIsFreeMonoid(S) and IsFreeMonoid(S)));

  if IsBound(S!.LibsemigroupsPresentation)
      and IsValidGapbind14Object(S!.LibsemigroupsPresentation) then
    return S!.LibsemigroupsPresentation;
  fi;
  Unbind(S!.LibsemigroupsPresentation);
  if IsFpSemigroup(S) then
    F := FreeSemigroupOfFpSemigroup(S);
  elif IsFpMonoid(S) then
    F := FreeMonoidOfFpMonoid(S);
  else
    # Free semigroup or monoid
    F := S;
  fi;

  SS := libsemigroups.Presentation.make();
  libsemigroups.Presentation.set_alphabet_size(
    SS,
    Size(GeneratorsOfSemigroup(S)));

  if IsMonoid(S) then
    # The identity must be 0 so that this corresponds to what happens in
    # FroidurePin, where GeneratorsOfSemigroup(S) is used and the identity is
    # the first entry.
    libsemigroups.presentation_add_identity_rules(SS, 0);
    R := RelationsOfFpMonoid(S);
  else
    R := RelationsOfFpSemigroup(S);
  fi;

  add_rule := libsemigroups.presentation_add_rule;
  for pair in R do
    add_rule(SS,
             Factorization(F, pair[1]) - 1,
             Factorization(F, pair[2]) - 1);
  od;
  S!.LibsemigroupsPresentation := SS;
  return SS;
end);
