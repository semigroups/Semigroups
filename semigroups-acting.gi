#############################################################################
##
#W  semigroups-acting.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## Methods for some standard things for acting semigroups. 

# different method for inverse/regular, same for ideals
# FIXME move this to another file
InstallMethod(Random, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local data, gens, i, w, x, n, m, o, rep, g;

  data := SemigroupData(s);

  if not IsClosedData(data) then
    if HasGeneratorsOfSemigroup(s) then
      gens := GeneratorsOfSemigroup(s);
      i := Random([1 .. 2 * Length(gens)]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));
      return EvaluateWord(gens, w);
    elif IsSemigroupIdeal(s) and HasGeneratorsOfSemigroupIdeal(s) then
      x := Random([1 .. Length(GeneratorsOfSemigroupIdeal(s))]);
      gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(s));

      i := Random([1 .. Length(gens)]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));

      x := x * EvaluateWord(gens, w);

      i := Random([1 .. Length(gens)]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));
      return EvaluateWord(gens, w) * x;
    fi;
  fi;

  n := Random([2 .. Length(data!.orbit)]);
  m := data[n][2];
  o := data[n][3];
  rep := data[n][4];

  g := Random(LambdaOrbSchutzGp(o, m));
  i := Random(OrbSCC(o)[m]);
  return StabilizerAction(s)(rep,g) * LambdaOrbMult(o, m, i)[1];
end);
