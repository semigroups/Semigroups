

#GGG

#NNN

# new for 0.7! - NaturalPartialOrder - "for an inverse semigroup"
##############################################################################
# C function for me!

InstallMethod(NaturalPartialOrder, "for an inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  local elts, n, out, i, j;

  elts:=Elements(s);  n:=Length(elts);
  out:=List([1..n], x-> EmptyPlist(n));
  for i in [n, n-1..1] do
    for j in [i-1,i-2 ..1] do
      if NaturalLeqPP(elts[j], elts[i]) then
        AddSet(out[i], j);
      fi;
    od;
  od;
  Perform(out, ShrinkAllocationPlist);
  return out;
end);

# new for 1.0! - NrIdempotents - for an acting semigroup with inversion
##############################################################################

InstallOtherMethod(NrIdempotents, "for an acting semigroup with inversion",
[IsActingSemigroupWithInversion], 
function(s)
  return Length(Enumerate(LambdaOrb(s), infinity))-
   ActingSemigroupModifier(s);     
end);

# mod for 1.0! - NrRClasses - for an acting semigroup with inversion
##############################################################################

InstallOtherMethod(NrRClasses, "for an acting semigroup with inversion",
[IsActingSemigroupWithInversion], NrLClasses);

# mod for 1.0! - NrHClasses - for an acting semigroup with inversion
##############################################################################

InstallOtherMethod(NrHClasses, "for an acting semigroup with inversion",
[IsActingSemigroupWithInversion],
function(s)
  local o, scc;
  o:=Enumerate(LambdaOrb(s), infinity);
  scc:=OrbSCC(o);

  return Sum(List(scc, m-> Length(m)^2))-ActingSemigroupModifier(s);
end);


