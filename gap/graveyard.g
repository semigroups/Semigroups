# Here lies some dead code, may it rest in peace

# LambdaRhoLookup(d)[i]=j if orbit[j][4] in reps[i] (orbit[j][4] is one of the
# R-reps of the D-class d) and LambdaRhoLookup(d) is only bound for those
# indices i where there is an R-rep in the scc of the D-class in reps[i]

DeclareAttribute("LambdaRhoLookup", IsGreensDClass and
IsActingSemigroupGreensClass);

# this won't work for ideals, but isn't currently used for anything

InstallMethod(LambdaRhoLookup, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local data, orb_scc, orblookup1, orblookup2, out, i;

  data := SemigroupData(Parent(d));

  # scc of R-reps corresponding to d
  orb_scc := SemigroupDataSCC(d);

  # positions in reps containing R-reps in d
  orblookup1 := data!.orblookup1;
  orblookup2 := data!.orblookup2;

  out := [];
  for i in orb_scc do
    if not IsBound(out[orblookup1[i]]) then
      out[orblookup1[i]] := [];
    fi;
    Add(out[orblookup1[i]], orblookup2[i]);
  od;

  return out;
end);
