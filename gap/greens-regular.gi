#############################################################################
##
#W  greens-regular.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## This file contains methods for Green's classes of regular acting semigroups.
## See the start of greens-acting.gi for details of how to create Green's
## classes of acting semigroups.

#############################################################################
## This file contains methods for Green's classes etc for acting semigroups.
## It is organized as follows:
##
##   1. Helper functions for the creation of Green's classes, and lambda-rho
##      stuff.
##
##   2. Individual Green's classes (constructors, size, membership)
##
##   3. Collections of Green's classes (GreensXClasses, XClassReps, NrXClasses)
##
##   4. Idempotents and NrIdempotents
##
##   5. Regularity of Green's classes
##
##   6. Iterators and enumerators
##
#############################################################################

#############################################################################
## 1. Helper functions for the creation of Green's classes . . .
#############################################################################

InstallMethod(RhoCosets, "for a regular class of an acting semigroup",
[IsRegularClass and IsActingSemigroupGreensClass],
function(C)
  local S;
  S := Parent(C);
  return [LambdaIdentity(S)(LambdaRank(S)(LambdaFunc(S)(Representative(C))))];
end);

InstallMethod(LambdaCosets, "for a regular class of an acting semigroup",
[IsRegularClass and IsActingSemigroupGreensClass], RhoCosets);

# same method for inverse

InstallMethod(SchutzenbergerGroup, "for D-class of regular acting semigroup",
[IsRegularClass and IsGreensDClass and IsActingSemigroupGreensClass],
D -> LambdaOrbSchutzGp(LambdaOrb(D), LambdaOrbSCCIndex(D)));

# same method for inverse

InstallMethod(SchutzenbergerGroup, "for H-class of regular acting semigroup",
[IsActingSemigroupGreensClass and IsHClassOfRegularSemigroup and
 IsGreensHClass],
function(H)
  local S, rep, p;
  S := Parent(H);
  rep := Representative(H);
  SEMIGROUPS_RectifyLambda(H);
  p := LambdaConjugator(S)(H!.rep, rep);
  return LambdaOrbSchutzGp(LambdaOrb(H), LambdaOrbSCCIndex(H)) ^ p;
end);

#############################################################################
## 2. Individual classes . . .
#############################################################################

InstallMethod(Size, "for a regular D-class of an acting semigroup",
[IsRegularClass and IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  return Size(SchutzenbergerGroup(D)) * Length(LambdaOrbSCC(D))
   * Length(RhoOrbSCC(D));
end);

#############################################################################
## 3. Collections of classes, and reps
#############################################################################

# different method for inverse.

# Note that these are not rectified!

# this method could apply to regular ideals but is not used since the rank of
# IsActingSemigroup and IsSemigroupIdeal is higher than the rank for this
# method. Anyway the data of an ideal must be enumerated to the end to know the
# DClassReps, and to know the LambdaOrb, so these methods are equal.

InstallMethod(DClassReps, "for a regular acting semigroup with generators",
[IsRegularSemigroup and IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local o, out, m;
  o := LambdaOrb(S);
  out := EmptyPlist(Length(OrbSCC(o)));
  for m in [2 .. Length(OrbSCC(o))] do
    out[m - 1] := LambdaOrbRep(o, m);
  od;
  return out;
end);

# different method for inverse/ideals

InstallMethod(GreensDClasses, "for a regular acting semigroup with generators",
[IsRegularSemigroup and IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local o, scc, out, D, i;

  o := LambdaOrb(S);
  scc := OrbSCC(o);
  out := EmptyPlist(Length(scc) - 1);
  Enumerate(RhoOrb(S));

  for i in [2 .. Length(scc)] do
    # don't use GreensDClassOfElementNC here to avoid rectifying lambda
    D := SEMIGROUPS_CreateDClass(S, LambdaOrbRep(o, i), false);
    SetLambdaOrb(D, o);
    SetLambdaOrbSCCIndex(D, i);
    SEMIGROUPS_SetRho(D);
    SEMIGROUPS_RectifyRho(D);
    out[i - 1] := D;
  od;
  return out;
end);

# same method for inverse/ideals

InstallMethod(RClassReps, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
S -> Concatenation(List(GreensDClasses(S), RClassReps)));

# same method for inverse/ideals

InstallMethod(GreensRClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
S -> Concatenation(List(GreensDClasses(S), GreensRClasses)));

#############################################################################

# same method for inverse/ideals

InstallMethod(NrDClasses, "for a regular acting semigroup with generators",
[IsActingSemigroup and IsRegularSemigroup and HasGeneratorsOfSemigroup],
S -> Length(OrbSCC(LambdaOrb(S))) - 1);

# same method for inverse semigroups, same for ideals

InstallMethod(NrLClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
S -> Length(Enumerate(LambdaOrb(S))) - 1);

# same method for inverse semigroups

InstallMethod(NrLClasses, "for a D-class of regular acting semigroup",
[IsActingSemigroupGreensClass and IsRegularClass and IsGreensDClass],
D -> Length(LambdaOrbSCC(D)));

# different method for inverse semigroups, same for ideals

InstallMethod(NrRClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
S -> Length(Enumerate(RhoOrb(S), infinity)) - 1);

# different method for inverse semigroups

InstallMethod(NrRClasses, "for a D-class of regular acting semigroup",
[IsActingSemigroupGreensClass and IsRegularClass and IsGreensDClass],
D -> Length(RhoOrbSCC(D)));

# different method for inverse semigroups

InstallMethod(NrHClasses, "for a D-class of regular acting semigroup",
[IsActingSemigroupGreensClass and IsRegularClass and IsGreensRClass],
R -> Length(LambdaOrbSCC(R)) * Length(RhoOrbSCC(R)));

# different method for inverse semigroups

InstallMethod(NrHClasses, "for a L-class of regular acting semigroup",
[IsActingSemigroupGreensClass and IsRegularClass and IsGreensLClass],
L -> Length(RhoOrbSCC(L)));

# same method for inverse semigroups

InstallMethod(NrHClasses, "for a R-class of regular acting semigroup",
[IsActingSemigroupGreensClass and IsRegularClass and IsGreensRClass],
R -> Length(LambdaOrbSCC(R)));

# different method for inverse/ideals

InstallMethod(PartialOrderOfDClasses,
"for a regular acting semigroup with generators",
[IsActingSemigroup and IsRegularSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local D, n, out, o, gens, lookup, lambdafunc, i, x, y;

  D := GreensDClasses(S);
  n := Length(D);
  out := List([1 .. n], x -> EmptyPlist(n));
  o := LambdaOrb(S);
  gens := o!.gens;
  lookup := OrbSCCLookup(o);
  lambdafunc := LambdaFunc(S);

  for i in [1 .. n] do
    for x in gens do
      for y in RClassReps(D[i]) do
        AddSet(out[i], lookup[Position(o, lambdafunc(x * y))] - 1);
      od;
      for y in LClassReps(D[i]) do
        AddSet(out[i], lookup[Position(o, lambdafunc(y * x))] - 1);
      od;
    od;
  od;

  Perform(out, ShrinkAllocationPlist);
  return out;
end);

#############################################################################
## 4. Idempotents . . .
#############################################################################

# different method for inverse, same for ideals

InstallMethod(NrIdempotents, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(S)
  local nr, tester, rho_o, scc, lambda_o, rhofunc, lookup, rep, rho, j, i, k;

  nr := 0;
  tester := IdempotentTester(S);
  rho_o := RhoOrb(S);
  scc := OrbSCC(rho_o);
  lambda_o := LambdaOrb(S);
  Enumerate(lambda_o, infinity);
  rhofunc := RhoFunc(S);
  lookup := OrbSCCLookup(rho_o);

  for i in [2 .. Length(lambda_o)] do
    rep := EvaluateWord(lambda_o, TraceSchreierTreeForward(lambda_o, i));
    rho := rhofunc(rep);
    j := lookup[Position(rho_o, rho)];
    for k in scc[j] do
      if tester(lambda_o[i], rho_o[k]) then
        nr := nr + 1;
      fi;
    od;
  od;

  return nr;
end);

#############################################################################
## 5. Regular classes . . .
#############################################################################

# same method for inverse semigroups, same for ideals

InstallMethod(NrRegularDClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
NrDClasses);

#############################################################################
## 6. Iterators and enumerators . . .
## FIXME move this whole section to another file
#############################################################################

# different method for inverse

InstallMethod(IteratorOfLClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(S)
  if HasGreensLClasses(S) then
    return IteratorList(GreensLClasses(S));
  fi;

  return IteratorByIterator(IteratorOfLClassReps(S),
                            x -> GreensLClassOfElementNC(S, x),
                            [IsIteratorOfLClasses]);
end);

# same method for inverse

InstallMethod(IteratorOfDClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(S)
  if HasGreensDClasses(S) then
    return IteratorList(GreensDClasses(S));
  fi;
  return IteratorByIterator(IteratorOfDClassReps(S),
                            x -> GreensDClassOfElementNC(S, x),
                            [IsIteratorOfDClasses]);
end);

# different method for inverse

# Notes: the only purpose for this is the method for NumberElement.  Otherwise
# use (if nothing much is known) IteratorOfRClasses or if everything is know
# just use RClasses.

InstallMethod(EnumeratorOfRClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(S)
  # gaplint: ignore 35
  return EnumeratorByFunctions(S, rec(

    parent := S,

    Length := enum -> NrRClasses(enum!.parent),

    Membership := function(R, enum)
      return Representative(R) in enum!.parent;
    end,

    NumberElement := function(enum, R)
      local pos;
      pos := Position(RhoOrb(enum!.parent),
       RhoFunc(enum!.parent)(Representative(R)));
      if pos = fail then
        return fail;
      fi;
      return pos - 1;
    end,

   ElementNumber := function(enum, nr)
    local S, o, m;
    S := enum!.parent;
    o := RhoOrb(S);
    m := OrbSCCLookup(o)[nr + 1];
    return
      GreensRClassOfElementNC(S,
                              RhoOrbMult(o, m, nr + 1)[1] * RhoOrbRep(o, m));
   end,

   PrintObj := function(enum)
     Print( "<enumerator of R-classes of ", ViewString(S), ">");
     return;
   end));
end);
