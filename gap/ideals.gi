#############################################################################
##
#W  ideals.gi
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
##

# new for 0.5! - PrintObj - "for an ideal of a trans. semigp."
#############################################################################

InstallMethod(PrintObj, "for an ideal of a trans. semigp.",
[IsSemigroupIdeal and IsTransformationSemigroup],
function(i)
  Print("<semigroup ideal with ", Length( GeneratorsOfMagmaIdeal( i ) ),
  " generators>");
  return;
end);

# new for 0.5! - ViewObj - "for an ideal of a trans. semigp."
#############################################################################

InstallMethod(ViewObj, "for an ideal of a trans. semigp.",
[IsSemigroupIdeal and IsTransformationSemigroup],
function(i)
  Print("<semigroup ideal with ", Length( GeneratorsOfMagmaIdeal( i ) ),
  " generators>");
  return;
end);

InstallOtherMethod(Degree, "for an ideal of a transformation semigroup", 
[IsSemigroupIdeal and IsTransformationSemigroup],
i -> Degree(GeneratorsOfMagmaIdeal(i)[1]));

# new for 0.5! - GeneratorsOfSemigroup - "for min. ideal of trans. semi."
############################################################################

InstallMethod(GeneratorsOfSemigroup, "for min. ideal of trans. semi.",
[IsMinimalIdeal], 
function(i)
#  local gens, g;

#  gens:=Union(GreensRClassReps(UnderlyingDClassOfMinIdeal(i)), 
#   GreensLClassReps(UnderlyingDClassOfMinIdeal(i)));
 
#  if Size(Semigroup(gens))=Size(UnderlyingDClassOfMinIdeal(i)) then 
#    return gens;
#  fi;

#  g:=SchutzenbergerGroup(UnderlyingDClassOfMinIdeal(i));
#  g:=List(Generators(g), x-> AsTransformation(x, Degree(gens[1])));

#  return Union(gens, g);
end);

############################################################################

InstallMethod(Representative, "for an ideal of a transformation semigroup",
[IsMinimalIdeal], i-> GeneratorsOfMagmaIdeal(i)[1]);

############################################################################

InstallMethod(SemigroupIdeal, "for a trans. semigroup",
[IsTransformationSemigroup, IsTransformation],
function(s, f)
  SemigroupIdealByGenerators(s, [f]);
end);

#EOF


