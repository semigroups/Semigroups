#############################################################################
##
##  libsemigroups/fpsemi.gi
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallGlobalFunction("LibsemigroupsFpSemigroup",
function(S)
  local F, SS, R, add_rule, pair;

  Assert(1, IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
            or IsFpMonoid(S) or (HasIsFreeMonoid(S) and IsFreeMonoid(S)));

  if IsBound(S!.LibsemigroupsFpSemigroup)
      and IsValidGapbind14Object(S!.LibsemigroupsFpSemigroup) then
    return S!.LibsemigroupsFpSemigroup;
  fi;
  Unbind(S!.LibsemigroupsFpSemigroup);
  if IsFpSemigroup(S) then
    F := FreeSemigroupOfFpSemigroup(S);
  elif IsFpMonoid(S) then
    F := FreeMonoidOfFpMonoid(S);
  else
    # Free semigroup or monoid
    F := S;
  fi;

  SS := libsemigroups.FpSemigroup.make();
  libsemigroups.FpSemigroup.set_alphabet(SS, Size(GeneratorsOfSemigroup(S)));

  if IsMonoid(S) then
    # The identity must be 0 so that this corresponds to what happens in
    # FroidurePin, where GeneratorsOfSemigroup(S) is used and the identity is
    # the first entry.
    libsemigroups.FpSemigroup.set_identity(SS, 0);
    R := RelationsOfFpMonoid(S);
  else
    R := RelationsOfFpSemigroup(S);
  fi;

  add_rule := libsemigroups.FpSemigroup.add_rule;
  for pair in R do
    add_rule(SS,
             Factorization(F, pair[1]) - 1,
             Factorization(F, pair[2]) - 1);
  od;
  S!.LibsemigroupsFpSemigroup := SS;
  return SS;
end);
