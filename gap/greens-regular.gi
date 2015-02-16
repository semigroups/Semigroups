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
##   2. Technical Green's stuff (types, representative, etc)
##
##   3. Individual Green's classes (constructors, size, membership)
##
##   4. Collections of Green's classes (GreensXClasses, XClassReps, NrXClasses)
##
##   5. Idempotents and NrIdempotents
##
##   6. Regularity of Green's classes
##
##   7. Iterators and enumerators
## 
#############################################################################

#############################################################################
## 1. Helper functions for the creation of Green's classes . . .
#############################################################################

InstallMethod(RhoCosets, "for a regular class of an acting semigroup",
[IsRegularClass and IsActingSemigroupGreensClass],
function(x)
  return [()];
end);

InstallMethod(LambdaCosets, "for a regular class of an acting semigroup",
[IsRegularClass and IsActingSemigroupGreensClass],
function(x)
  return [()];
end);

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
  p := LambdaConjugator(S)(Representative(RClassOfHClass(H)), rep);
  return LambdaOrbSchutzGp(LambdaOrb(H), LambdaOrbSCCIndex(H)) ^ p;
end);

#############################################################################
## 2. Technical Green's classes stuff . . .
#############################################################################

# different method for inverse semigroups

InstallMethod(DClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(S)
  return NewType( FamilyObj(S), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsRegularClass and IsGreensDClass
         and IsActingSemigroupGreensClass);
end);

# different method for inverse semigroups

InstallMethod(LClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(S)
  return NewType( FamilyObj(S), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsRegularClass and IsGreensLClass and
         IsActingSemigroupGreensClass);
end);

# different method for inverse semigroups

InstallMethod(RClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(S)
  return NewType( FamilyObj(S), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsRegularClass and IsGreensRClass and
         IsActingSemigroupGreensClass);
end);

# different method for inverse semigroups

InstallMethod(HClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(S);
 return NewType( FamilyObj(S), IsEquivalenceClass and
  IsEquivalenceClassDefaultRep and IsGreensHClass and
  IsHClassOfRegularSemigroup and IsActingSemigroupGreensClass);
end);

#############################################################################
## 3. Individual classes . . .
#############################################################################

InstallMethod(Size, "for a regular D-class of an acting semigroup",
[IsRegularClass and IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  return Size(SchutzenbergerGroup(D)) * Length(LambdaOrbSCC(D))
   * Length(RhoOrbSCC(D));
end);

#############################################################################
## 4. Collections of classes, and reps
#############################################################################

# same method for inverse.

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
## 5. Idempotents . . .
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
## 6. Regular classes . . .
#############################################################################

# same method for inverse semigroups, same for ideals

InstallMethod(NrRegularDClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
NrDClasses);

#############################################################################
## 7. Iterators and enumerators . . .
#############################################################################

# JDM here

# different method for inverse

InstallMethod(IteratorOfLClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
s -> IteratorByIterator(IteratorOfLClassData(s), x ->
CallFuncList(SEMIGROUPS_CreateLClassNC, x), [IsIteratorOfLClasses]));

# same method for inverse

InstallMethod(IteratorOfDClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  if HasGreensDClasses(s) then
    return IteratorList(GreensDClasses(s));
  fi;
  return IteratorByIterator(IteratorOfDClassData(s), x ->
   CallFuncList(SEMIGROUPS_CreateDClassNC, x), [IsIteratorOfDClasses]);
end);

# different method for inverse

# Notes: the only purpose for this is the method for NumberElement.  Otherwise
# use (if nothing much is known) IteratorOfRClasses or if everything is know
# just use RClasses.

InstallMethod(EnumeratorOfRClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local o;

  o := RhoOrb(s);
  Enumerate(o, infinity);

  return EnumeratorByFunctions(s, rec(

    parent := s,

    Length := enum -> NrRClasses(enum!.parent),

    Membership := function(r, enum)
      return Representative(r) in enum!.parent;
    end,

    NumberElement := function(enum, r)
      local pos;
      pos := Position(RhoOrb(enum!.parent),
       RhoFunc(enum!.parent)(Representative(r)));
      if pos = fail then
        return fail;
      fi;
      return pos - 1;
    end,

   ElementNumber := function(enum, nr)
    local s, o, m;
    s := enum!.parent;
    o := RhoOrb(s);
    m := OrbSCCLookup(o)[nr + 1];
    return SEMIGROUPS_CreateRClass(s, m, LambdaOrb(s),
     RhoOrbMult(o, m, nr + 1)[1] * RhoOrbRep(o, m), false);
   end,
   PrintObj := function(enum)
     Print( "<enumerator of R-classes of ", ViewString(s), ">");
     return;
   end));
end);

#############################################################################
#############################################################################
#############################################################################

# different method for inverse, same method for ideals
# FIXME move this

InstallMethod(\in, "for an associative element and regular acting semigroup",
[IsAssociativeElement, IsActingSemigroup and IsRegularSemigroup],
function(f, s)
  local lambda_o, lambda_l, rho_o, rho_l, m, schutz, g, n, rep;

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f)
    or (IsActingSemigroupWithFixedDegreeMultiplication(s)
        and ActionDegree(f) <> ActionDegree(s))
    or ActionDegree(f) > ActionDegree(s) then
    return false;
  fi;

  if not (IsMonoid(s) and IsOne(f)) then
    if Length(Generators(s)) > 0 and
      ActionRank(s)(f) > MaximumList(List(Generators(s), f -> ActionRank(s)(f)))
     then
      Info(InfoSemigroups, 2, "element has larger rank than any element of ",
       "semigroup.");
      return false;
    fi;
  fi;

  if HasMinimalIdeal(s) then
    if ActionRank(s)(f) < ActionRank(s)(Representative(MinimalIdeal(s))) then
      Info(InfoSemigroups, 2, "element has smaller rank than any element of ",
       "semigroup.");
      return false;
    fi;
  fi;

  if HasAsSSortedList(s) then
    return f in AsSSortedList(s);
  fi;

  lambda_o := LambdaOrb(s);
  Enumerate(lambda_o, infinity);
  lambda_l := Position(lambda_o, LambdaFunc(s)(f));

  if lambda_l = fail then
    return false;
  fi;

  rho_o := RhoOrb(s);
  rho_l := EnumeratePosition(rho_o, RhoFunc(s)(f), false);
  # this is worth it in the case that schutz=true below! For example, in the
  # full transformation monoid on 12 points (see Issue 22 in testinstall.tst)

  if rho_l = fail then
    return false;
  fi;

  m := OrbSCCLookup(lambda_o)[lambda_l];
  schutz := LambdaOrbStabChain(lambda_o, m);

  if schutz = true then
    return true;
  fi;

  g := f;

  if lambda_l <> OrbSCC(lambda_o)[m][1] then
    g := g * LambdaOrbMult(lambda_o, m, lambda_l)[2];
  fi;

  Enumerate(rho_o, infinity); # in case <s> is an ideal...
  n := OrbSCCLookup(rho_o)[rho_l];

  if rho_l <> OrbSCC(rho_o)[n][1] then
    g := RhoOrbMult(rho_o, n, rho_l)[2] * g;
  fi;

  if IsIdempotent(g) then
    return true;
  fi;

  rep := RectifyRho(s, rho_o, LambdaOrbRep(lambda_o, m)).rep;

  if rep = g then
    return true;
  elif schutz = false then
    return false;
  fi;

  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g)) = ();
end);

# different method for inverse, same method for ideals
# FIXME move this
InstallMethod(Random, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(S)
  local gens, i, w, x, o, m;

  if not IsClosed(LambdaOrb(S)) or not IsClosed(RhoOrb(S)) then
    if HasGeneratorsOfSemigroup(S) then
      gens := GeneratorsOfSemigroup(S);
      i := Random([1 .. 2 * Int(Length(gens))]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));
      return EvaluateWord(gens, w);
    else
      x := Random(GeneratorsOfSemigroupIdeal(S));
      gens := GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));

      i := Random([1 .. Length(gens)]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));

      x := x * EvaluateWord(gens, w);

      i := Random([1 .. Length(gens)]);
      w := List([1 .. i], x -> Random([1 .. Length(gens)]));
      return EvaluateWord(gens, w) * x;
    fi;
  fi;

  o := LambdaOrb(S);
  i := Random([2 .. Length(o)]);
  m := OrbSCCLookup(o)[i];
  x := LambdaOrbRep(o, m) * Random(LambdaOrbSchutzGp(o, m))
   * LambdaOrbMult(o, m, i)[1];

  o := RhoOrb(S);
  m := OrbSCCLookup(o)[Position(o, RhoFunc(S)(x))];
  i := Random(OrbSCC(o)[m]);

  return RhoOrbMult(o, m, i)[1] * x;
end);

# different method for inverse semigroups
# FIXME move this 

InstallMethod(Size, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  local lambda_o, rho_o, nr, lambda_scc, rho_scc, r, rhofunc, lookup, start,
  rho, m;

  lambda_o := Enumerate(LambdaOrb(s), infinity);
  rho_o := Enumerate(RhoOrb(s), infinity);

  nr := 0;
  lambda_scc := OrbSCC(lambda_o);
  rho_scc := OrbSCC(rho_o);
  r := Length(lambda_scc);
  rhofunc := RhoFunc(s);
  lookup := OrbSCCLookup(rho_o);

  for m in [2 .. r] do
    rho := rhofunc(LambdaOrbRep(lambda_o, m));
    nr := nr + Length(lambda_scc[m]) * Size(LambdaOrbSchutzGp(lambda_o, m)) *
     Length(rho_scc[lookup[Position(rho_o, rho)]]);
  od;

  return nr;
end);
