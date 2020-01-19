#############################################################################
##
##  grac.gi
##  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

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
## Green's classes are created as follows:
##
##   1. Create the class C using SEMIGROUPS.CreateXClass, this sets the type,
##      equivalence relations, whether or not the class is NC, the parent, and
##      the element used to create the class (stored in C!.rep)
##
##   2. Copy or set the lambda/rho orbits using SEMIGROUPS.CopyLambda/Rho and
##      SEMIGROUPS.SetLambda/Rho. After calling any combination of these two
##      functions Lambda/RhoOrb and Lambda/RhoOrbSCCIndex for the class are
##      set.
##
##      The former simply copies the orbit and the index of the strongly
##      connected component containing the lambda/rho value of the element used
##      to create the class. This is useful, for example, when trying to create
##      an R-class of a D-class, when the lambda orbit and scc index are just
##      the same as those for the D-class.
##
##      The latter uses the lambda/rho orbit of the whole semigroup if it is
##      known and the class is not nc. Otherwise it creates/finds the graded
##      lambda/rho orbit. If the graded lambda/rho orbit is created/found, then
##      the position of the lambda/rho value of C!.rep (the element used to
##      create the class) in the orbit is stored in C!.Lambda/RhoPos.
##
##   3. Rectify the lambda/rho value of the representative. The representative
##      of an R-class, for example, must have its lambda-value in the first
##      position of the scc containing it. Representatives of L-classes must
##      have the rho-value must be in the first position of the scc containing
##      it, D-class reps must have both lambda- and rho-values in the
##      respective first positions, the lambda- and rho-values of H-class reps
##      must not be modified. The functions SEMIGROUPS.RectifyLambda/Rho can
##      used to modify C!.rep (in place) so that its lambda/rho-value is in the
##      first place of its scc. These functions depend on the previous 2 steps
##      being called.
##
## Note that a Green's class does not satisfies IsGreensClassNC if it is known
## that its representative is an element of the underlying semigroup, not if it
## is known to be an element of another Green's class. Also a class in
## IsGreensClassNC makes the assumption that the representative belongs to the
## containing object, and reuses the lambda/rho orbit from the container. So,
## it might be that the representative of a class created using an NC method,
## has lambda/rho value not belonging to the lambda/rho orbit of the class.
#############################################################################

#############################################################################
## 1. Helper functions for the creation of Green's classes . . .
#############################################################################

# same method for regular/different for inverse

SEMIGROUPS.CreateXClass := function(args, type, rel)
  local S, nc, rep, C;

  if Length(args) = 1 then  # arg is a Green's class
    # for creating bigger classes containing smaller ones
    S := Parent(args[1]);
    nc := IsGreensClassNC(args[1]);
    rep := Representative(args[1]);
  else  # arg is semigroup/Green's class, rep, and boolean
        # for creating smaller classes inside bigger ones
    if IsGreensClass(args[1]) then
      S := Parent(args[1]);
    else
      S := args[1];
    fi;
    rep := args[2];
    nc := args[3];
  fi;

  C := rec(rep := rep);
  ObjectifyWithAttributes(C, type(S),
                          ParentAttr, S,
                          IsGreensClassNC, nc,
                          EquivalenceClassRelation, rel(S));

  if IsInverseActingSemigroupRep(S) then
    SetFilterObj(C, IsInverseActingRepGreensClass);
  elif IsRegularActingSemigroupRep(S) then
    SetFilterObj(C, IsRegularActingRepGreensClass);
  elif HasIsRegularSemigroup(S) and IsRegularSemigroup(S) then
    if type <> HClassType then
      SetIsRegularGreensClass(C, true);
    fi;
  fi;

  return C;
end;

# same method for regular/inverse

SEMIGROUPS.CreateDClass := function(arg)
  return SEMIGROUPS.CreateXClass(arg, DClassType, GreensDRelation);
end;

# same method for regular/inverse

SEMIGROUPS.CreateRClass := function(arg)
  return SEMIGROUPS.CreateXClass(arg, RClassType, GreensRRelation);
end;

# same method for regular/inverse

SEMIGROUPS.CreateLClass := function(arg)
  return SEMIGROUPS.CreateXClass(arg, LClassType, GreensLRelation);
end;

# same method for regular/inverse

SEMIGROUPS.CreateHClass := function(arg)
  return SEMIGROUPS.CreateXClass(arg, HClassType, GreensHRelation);
end;

SEMIGROUPS.SetLambda := function(C)
  local S, o;

  S := Parent(C);
  if HasLambdaOrb(S) and IsClosedOrbit(LambdaOrb(S))
      and not IsGreensClassNC(C) then
    SetLambdaOrb(C, LambdaOrb(S));
    SetLambdaOrbSCCIndex(C, OrbSCCIndex(LambdaOrb(S), LambdaFunc(S)(C!.rep)));
  else
    o := GradedLambdaOrb(S, C!.rep, IsGreensClassNC(C) <> true, C);
    SetLambdaOrb(C, o);
    SetLambdaOrbSCCIndex(C, OrbSCCLookup(o)[C!.LambdaPos]);
  fi;
end;

SEMIGROUPS.SetRho := function(C)
  local S, o;

  S := Parent(C);
  if HasRhoOrb(S) and IsClosedOrbit(RhoOrb(S)) and not IsGreensClassNC(C) then
    SetRhoOrb(C, RhoOrb(S));
    SetRhoOrbSCCIndex(C, OrbSCCIndex(RhoOrb(S), RhoFunc(S)(C!.rep)));
  else
    o := GradedRhoOrb(S, C!.rep, IsGreensClassNC(C) <> true, C);
    SetRhoOrb(C, o);
    SetRhoOrbSCCIndex(C, OrbSCCLookup(o)[C!.RhoPos]);
  fi;
end;

SEMIGROUPS.CopyLambda := function(old, new)
  SetLambdaOrb(new, LambdaOrb(old));
  SetLambdaOrbSCCIndex(new, LambdaOrbSCCIndex(old));
  return;
end;

SEMIGROUPS.CopyRho := function(old, new)
  SetRhoOrb(new, RhoOrb(old));
  SetRhoOrbSCCIndex(new, RhoOrbSCCIndex(old));
  return;
end;

SEMIGROUPS.RectifyLambda := function(C)
  local o, i, m;

  o := LambdaOrb(C);

  if not IsBound(C!.LambdaPos) then
    i := Position(o, LambdaFunc(Parent(C))(C!.rep));
  else
    i := C!.LambdaPos;
  fi;

  m := LambdaOrbSCCIndex(C);

  if i <> OrbSCC(o)[m][1] then
    C!.rep := C!.rep * LambdaOrbMult(o, m, i)[2];
    # don't set Representative in case we must also rectify Rho
  fi;

  return;
end;

SEMIGROUPS.RectifyRho := function(C)
  local o, i, m;

  o := RhoOrb(C);

  if not IsBound(C!.RhoPos) then
    i := Position(o, RhoFunc(Parent(C))(C!.rep));
  else
    i := C!.RhoPos;
  fi;

  m := RhoOrbSCCIndex(C);

  if i <> OrbSCC(o)[m][1] then
    C!.rep := RhoOrbMult(o, m, i)[2] * C!.rep;
    # don't set Representative in case we must also rectify Lambda
  fi;

  return;
end;

# helper function, for: Green's class, lambda/rho value, rho/lambda scc,
# rho/lambda orbit, and boolean

SEMIGROUPS.Idempotents := function(x, value, scc, o, onright)
  local S, out, j, tester, creator, i;

  if HasIsRegularGreensClass(x) and not IsRegularGreensClass(x) then
    return [];
  fi;

  S := Parent(x);

  if IsActingSemigroupWithFixedDegreeMultiplication(S)
      and IsMultiplicativeElementWithOneCollection(S)
      and ActionRank(S)(Representative(x))
          = ActionDegree(Representative(x)) then
    return [One(S)];
  fi;

  out := EmptyPlist(Length(scc));
  j := 0;
  tester := IdempotentTester(S);
  creator := IdempotentCreator(S);

  if onright then
    for i in scc do
      if tester(o[i], value) then
        j := j + 1;
        out[j] := creator(o[i], value);
      fi;
    od;
  else
    for i in scc do
      if tester(value, o[i]) then
        j := j + 1;
        out[j] := creator(value, o[i]);
      fi;
    od;
  fi;

  if not HasIsRegularGreensClass(x) then
    SetIsRegularGreensClass(x, j <> 0);
  fi;

  # can't set NrIdempotents here since we sometimes input a D-class here.

  ShrinkAllocationPlist(out);
  return out;
end;

# helper function, for: Green's class, lambda/rho value, rho/lambda scc,
# rho/lambda orbit, and boolean

SEMIGROUPS.NrIdempotents := function(x, value, scc, o, onright)
  local S, data, m, nr, tester, i;

  if HasIsRegularGreensClass(x) and not IsRegularGreensClass(x) then
    return 0;
  fi;

  S := Parent(x);

  # check if we already know this...
  if HasSemigroupDataIndex(x)
      and not (HasIsRegularGreensClass(x) and IsRegularGreensClass(x)) then
    data := SemigroupData(S);
    m := LambdaOrbSCCIndex(x);
    if data!.repslens[m][data!.orblookup1[SemigroupDataIndex(x)]] > 1 then
      return 0;
    fi;
  fi;

  # is r the group of units...
  if IsActingSemigroupWithFixedDegreeMultiplication(S) and
      ActionRank(S)(Representative(x)) = ActionDegree(Representative(x)) then
    return 1;
  fi;

  nr := 0;
  tester := IdempotentTester(S);

  if onright then
    for i in scc do
      if tester(o[i], value) then
        nr := nr + 1;
      fi;
    od;
  else
    for i in scc do
      if tester(value, o[i]) then
        nr := nr + 1;
      fi;
    od;
  fi;

  if not HasIsRegularGreensClass(x) then
    SetIsRegularGreensClass(x, nr <> 0);
  fi;

  return nr;
end;

# helper function, for: Green's class, lambda/rho value, rho/lambda scc,
# rho/lambda orbit, and boolean

SEMIGROUPS.IsRegularGreensClass := function(x, value, scc, o, onright)
  local S, data, m, tester, i;

  if HasNrIdempotents(x) then
    return NrIdempotents(x) <> 0;
  fi;

  S := Parent(x);

  if HasSemigroupDataIndex(x) then
    data := SemigroupData(S);
    m := LambdaOrbSCCIndex(x);
    if data!.repslens[m][data!.orblookup1[SemigroupDataIndex(x)]] > 1 then
      return false;
    fi;
  fi;

  # is x the group of units...
  if IsActingSemigroupWithFixedDegreeMultiplication(S) and
      ActionRank(S)(Representative(x)) = ActionDegree(Representative(x)) then
    return true;
  fi;

  tester := IdempotentTester(S);

  if onright then
    for i in scc do
      if tester(o[i], value) then
        return true;
      fi;
    od;
  else
    for i in scc do
      if tester(value, o[i]) then
        return true;
      fi;
    od;
  fi;

  return false;
end;

# Lambda-Rho stuff

# same method for regular/inverse/ideals

InstallMethod(LambdaOrbSCC, "for a Green's class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensClass],
C -> OrbSCC(LambdaOrb(C))[LambdaOrbSCCIndex(C)]);

# same method for regular/inverse/ideals

InstallMethod(RhoOrbSCC, "for a Green's class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensClass],
C -> OrbSCC(RhoOrb(C))[RhoOrbSCCIndex(C)]);

# not required for regular/inverse, same method for ideals

InstallMethod(LambdaCosets, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  return RightTransversal(LambdaOrbSchutzGp(LambdaOrb(D),
                                            LambdaOrbSCCIndex(D)),
                          SchutzenbergerGroup(D));
end);

# not required for regular/inverse, same method for ideals

InstallMethod(RhoCosets, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  local o, m, S;
  o := RhoOrb(D);
  m := RhoOrbSCCIndex(D);
  S := Parent(D);
  return RightTransversal(RhoOrbSchutzGp(o, m) ^
                          LambdaConjugator(S)(RhoOrbRep(o, m),
                                              Representative(D)),
                          SchutzenbergerGroup(D));
end);

# note that the RhoCosets of the D-class containing an L-class are not the same
# as the RhoCosets of the L-class. The RhoCosets of a D-class correspond to the
# lambda value of the rep of the D-class (which is in the first place of its
# scc) and the rep of the D-class but the RhoCosets of the L-class should
# correspond to the lambda value of the rep of the L-class which is not nec in
# the first place of its scc.

# different method for regular, same method for ideals

InstallMethod(RhoCosets, "for a L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local D, S, rep, m, o, pos, x, conj;

  D := DClassOfLClass(L);
  S := Parent(L);
  o := LambdaOrb(D);
  m := LambdaOrbSCCIndex(D);

  if IsRegularGreensClass(L) or Length(RhoCosets(D)) = 1 then
    # Maybe <L> is regular and doesn't know it!
    return [LambdaIdentity(S)(LambdaRank(S)(o[OrbSCC(o)[m][1]]))];
  fi;

  rep := Representative(L);
  pos := Position(o, LambdaFunc(S)(rep));

  if pos = OrbSCC(o)[m][1] then
    return RhoCosets(D);
  else
    x := rep * LambdaOrbMult(o, m, pos)[2];
    conj := LambdaConjugator(S)(x, rep) * LambdaPerm(S)(x, Representative(D));
    return List(RhoCosets(D), x -> x ^ conj);
  fi;
end);

# different method for regular/inverse, same for ideals

InstallMethod(RhoOrbStabChain, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
D -> RhoOrbStabChain(GreensLClassOfElementNC(D, Representative(D))));

# same method for regular, not required for inverse, same for ideals

InstallMethod(RhoOrbStabChain, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local G;

  G := SchutzenbergerGroup(L);

  if IsTrivial(G) then
    return false;
  elif IsNaturalSymmetricGroup(G) and
      NrMovedPoints(G) = ActionRank(Parent(L))(Representative(L)) then
    return true;
  fi;

  if IsPermGroup(G) then
    return StabChainImmutable(G);
  else  # if IsMatrixGroup(g)
    return G;
  fi;
end);

InstallMethod(SemigroupDataIndex,
"for an acting semigroup Green's class",
[IsActingSemigroupGreensClass],
function(C)
  return Position(SemigroupData(Parent(C)), Representative(C));
end);

# different method for regular/inverse/ideals

InstallMethod(SchutzenbergerGroup, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  local o, m, lambda_schutz, lambda_stab, rho_schutz, rho_stab, schutz, p;

  o := LambdaOrb(D);
  m := LambdaOrbSCCIndex(D);
  lambda_schutz := LambdaOrbSchutzGp(o, m);
  lambda_stab := LambdaOrbStabChain(o, m);

  o := RhoOrb(D);
  m := RhoOrbSCCIndex(D);
  rho_schutz := RhoOrbSchutzGp(o, m);
  rho_stab := RhoOrbStabChain(o, m);

  if rho_stab = true then
    schutz := lambda_schutz;
    if lambda_stab = true then
      SetRhoOrbStabChain(D, true);
      # Right transversal required so can use PositionCanonical
      SetRhoCosets(D, RightTransversal(schutz, schutz));
      return lambda_schutz;
    fi;
  elif rho_stab = false then
    SetRhoOrbStabChain(D, false);
    SetRhoCosets(D, RightTransversal(rho_schutz, rho_schutz));
    return rho_schutz;
  fi;

  p := LambdaConjugator(Parent(D))(RhoOrbRep(o, m), Representative(D));
  rho_schutz := rho_schutz ^ p;

  # FIXME should make the following case distinction unnecessary
  if IsPermGroup(rho_schutz) then
    SetRhoOrbStabChain(D, StabChainImmutable(rho_schutz));
  else  # if IsMatrixGroup(g)
    SetRhoOrbStabChain(D, rho_schutz);
  fi;

  if lambda_stab = false then
    SetRhoCosets(D, Enumerator(rho_schutz));
    return lambda_schutz;
  elif lambda_stab = true then
    schutz := rho_schutz;
  else
    schutz := Intersection(lambda_schutz, rho_schutz);
  fi;

  SetRhoCosets(D, RightTransversal(rho_schutz, schutz));
  return schutz;
end);

# same method for regular/inverse/ideals

InstallMethod(SchutzenbergerGroup, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
R -> LambdaOrbSchutzGp(LambdaOrb(R), LambdaOrbSCCIndex(R)));

# same method for regular/ideals, different method for inverse

InstallMethod(SchutzenbergerGroup, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local o, m, p;
  o := RhoOrb(L);
  m := RhoOrbSCCIndex(L);
  p := LambdaConjugator(Parent(L))(RhoOrbRep(o, m), Representative(L));
  return RhoOrbSchutzGp(o, m) ^ p;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(SchutzenbergerGroup, "for a H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local lambda_o, lambda_m, lambda_schutz, lambda_stab, rho_o, rho_m,
   rho_schutz, rho_stab, S, rep, lambda_mult, lambda_p, rho_mult, rho_p;

  lambda_o := LambdaOrb(H);
  lambda_m := LambdaOrbSCCIndex(H);
  lambda_schutz := LambdaOrbSchutzGp(lambda_o, lambda_m);
  lambda_stab := LambdaOrbStabChain(lambda_o, lambda_m);

  if lambda_stab = false then
    return lambda_schutz;
  fi;

  rho_o := RhoOrb(H);
  rho_m := RhoOrbSCCIndex(H);
  rho_schutz := RhoOrbSchutzGp(rho_o, rho_m);
  rho_stab := RhoOrbStabChain(rho_o, rho_m);

  if rho_stab = false then
    return rho_schutz;
  fi;

  S := Parent(H);
  rep := Representative(H);

  lambda_mult := LambdaOrbMult(lambda_o,
                               lambda_m,
                               Position(lambda_o, LambdaFunc(S)(rep)))[2];

  # the points acted on by LambdaSchutzGp mapped to the lambda value of rep
  lambda_p := LambdaConjugator(S)(rep * lambda_mult, rep);

  if rho_stab = true then
    return lambda_schutz ^ lambda_p;
  fi;

  rho_mult := RhoOrbMult(rho_o, rho_m, Position(rho_o, RhoFunc(S)(rep)))[2];
  # the points acted on by RhoSchutzGp mapped to the corresponding points for
  # rep (the rho value mapped through the rep so that it is on the right)
  rho_p := LambdaConjugator(S)(RhoOrbRep(rho_o, rho_m), rho_mult * rep);

  # if lambda_stab = true then
  #   return rho_schutz ^ rho_p;
  # fi;

  return Intersection(lambda_schutz ^ lambda_p, rho_schutz ^ rho_p);
end);

#############################################################################
## 2. Technical Green's classes stuff . . .
#############################################################################

# Different method for regular/inverse

InstallMethod(\<, "for Green's D-classes of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass,
 IsGreensDClass and IsActingSemigroupGreensClass],
function(x, y)
  local scc;
  if Parent(x) <> Parent(y) or x = y then
    return false;
  fi;
  scc  := OrbSCCLookup(SemigroupData(Parent(x)));
  return scc[SemigroupDataIndex(x)] < scc[SemigroupDataIndex(y)];
end);

# Different method for regular/inverse

InstallMethod(\<, "for Green's R-classes of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass,
 IsGreensRClass and IsActingSemigroupGreensClass],
function(x, y)
  if Parent(x) <> Parent(y) or x = y then
    return false;
  fi;
  return SemigroupDataIndex(x) < SemigroupDataIndex(y);
end);

# TODO a method for L-classes?

InstallMethod(IsRegularDClass, "for a Green's D-class",
[IsGreensDClass and IsActingSemigroupGreensClass], IsRegularGreensClass);

InstallMethod(Representative,
"for an acting semigroup Green's class",
[IsActingSemigroupGreensClass], C -> C!.rep);

# different method for regular/inverse

InstallMethod(DClassType, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  return NewType(FamilyObj(S), IsEquivalenceClass
                               and IsEquivalenceClassDefaultRep
                               and IsGreensDClass
                               and IsActingSemigroupGreensClass);
end);

# different method for regular/inverse

InstallMethod(HClassType, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  return NewType(FamilyObj(S), IsEquivalenceClass
                               and IsEquivalenceClassDefaultRep
                               and IsGreensHClass
                               and IsActingSemigroupGreensClass);
end);

# different method for regular/inverse

InstallMethod(LClassType, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  return NewType(FamilyObj(S), IsEquivalenceClass
                               and IsEquivalenceClassDefaultRep
                               and IsGreensLClass
                               and IsActingSemigroupGreensClass);
end);

# different method for regular/inverse

InstallMethod(RClassType, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  return NewType(FamilyObj(S), IsEquivalenceClass
                               and IsEquivalenceClassDefaultRep
                               and IsGreensRClass
                               and IsActingSemigroupGreensClass);
end);

#############################################################################
## 3. Individual classes . . .
#############################################################################

# same method for regular/ideals, different method for inverse

InstallMethod(DClassOfLClass, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local D;
  D := SEMIGROUPS.CreateDClass(L);
  SEMIGROUPS.CopyRho(L, D);
  SEMIGROUPS.SetLambda(D);
  SEMIGROUPS.RectifyLambda(D);
  return D;
end);

# same method for regular/ideals, different method for inverse

InstallMethod(DClassOfRClass, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(R)
  local D;
  D := SEMIGROUPS.CreateDClass(R);
  SEMIGROUPS.CopyLambda(R, D);
  SEMIGROUPS.SetRho(D);
  SEMIGROUPS.RectifyRho(D);
  return D;
end);

# same method for regular, different method for inverse semigroups,
# same for ideals

InstallMethod(DClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local D;
  D := SEMIGROUPS.CreateDClass(H);
  SEMIGROUPS.CopyLambda(H, D);
  SEMIGROUPS.RectifyLambda(D);
  SEMIGROUPS.CopyRho(H, D);
  SEMIGROUPS.RectifyRho(D);
  return D;
end);

# same method for regular/ideals, different method for inverse

InstallMethod(LClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local L;
  L := SEMIGROUPS.CreateLClass(H);
  SEMIGROUPS.CopyRho(H, L);
  SEMIGROUPS.RectifyRho(L);
  return L;
end);

# same method for regular/inverse/ideals

InstallMethod(RClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local R;
  R := SEMIGROUPS.CreateRClass(H);
  SEMIGROUPS.CopyLambda(H, R);
  SEMIGROUPS.RectifyLambda(R);
  return R;
end);

# same method for regular/ideals/inverse

InstallMethod(GreensDClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsMultiplicativeElement],
function(S, x)
  if not x in S then
    ErrorNoReturn("Semigroups: GreensDClassOfElement: usage,\n",
                  "the element does not belong to the semigroup,");
  fi;
  return GreensDClassOfElementNC(S, x, false);
end);

# same method for regular/ideals/inverse

InstallMethod(GreensDClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsMultiplicativeElement],
function(S, x)
  return GreensDClassOfElementNC(S, x, true);
end);

# same method for regular/ideals, different method for inverse

InstallMethod(GreensDClassOfElementNC,
"for an acting semigroup, element, and bool",
[IsActingSemigroup, IsMultiplicativeElement, IsBool],
function(S, x, isGreensClassNC)
  local D;
  D := SEMIGROUPS.CreateDClass(S, x, isGreensClassNC);
  SEMIGROUPS.SetLambda(D);
  SEMIGROUPS.RectifyLambda(D);
  SEMIGROUPS.SetRho(D);
  SEMIGROUPS.RectifyRho(D);
  return D;
end);

# same method for regular/ideals/inverse

InstallMethod(GreensLClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsMultiplicativeElement],
function(S, x)
  if not x in S then
    ErrorNoReturn("Semigroups: GreensLClassOfElement: usage,\n",
                  "the element does not belong to the semigroup,");
  fi;
  return GreensLClassOfElementNC(S, x, false);
end);

# same method for regular/ideals/inverse

InstallMethod(GreensLClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsMultiplicativeElement],
function(S, x)
  return GreensLClassOfElementNC(S, x, true);
end);

# same method for regular/ideals, different method for inverse

InstallMethod(GreensLClassOfElementNC,
"for an acting semigroup, element, and bool",
[IsActingSemigroup, IsMultiplicativeElement, IsBool],
function(S, x, isGreensClassNC)
  local L;
  L := SEMIGROUPS.CreateLClass(S, x, isGreensClassNC);
  SEMIGROUPS.SetRho(L);
  SEMIGROUPS.RectifyRho(L);
  return L;
end);

# same method for regular/ideals/inverse

InstallMethod(GreensLClassOfElement,
"for D-class of acting semigroup and element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsMultiplicativeElement],
function(D, x)
  if not x in D then
    ErrorNoReturn("Semigroups: GreensLClassOfElement: usage,\n",
                  "the element does not belong to the D-class,");
  fi;
  return GreensLClassOfElementNC(D, x, IsGreensClassNC(D));
end);

# same method for regular/ideals/inverse

InstallMethod(GreensLClassOfElementNC, "for D-class and multiplicative element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsMultiplicativeElement],
function(D, x)
  return GreensLClassOfElementNC(D, x, true);
end);

# same method for regular/ideals, different method for inverse

InstallMethod(GreensLClassOfElementNC,
"for D-class, multiplicative element, and bool",
[IsGreensDClass and IsActingSemigroupGreensClass, IsMultiplicativeElement,
 IsBool],
function(D, x, isGreensClassNC)
  local L;
  L := SEMIGROUPS.CreateLClass(D, x, isGreensClassNC);
  # this is a special case, D might not be an inverse-op class but
  # L might be an inverse-op class.
  if IsInverseActingRepGreensClass(L) then
    SEMIGROUPS.CopyLambda(D, L);
    SEMIGROUPS.InverseRectifyRho(L);
  else
    SEMIGROUPS.CopyRho(D, L);
    SEMIGROUPS.RectifyRho(L);
  fi;
  SetDClassOfLClass(L, D);
  return L;
end);

# same method for regular/inverse/ideals

InstallMethod(GreensRClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsMultiplicativeElement],
function(S, x)
  if not x in S then
    ErrorNoReturn("Semigroups: GreensRClassOfElement: usage,\n",
                  "the element does not belong to the semigroup,");
  fi;
  return GreensRClassOfElementNC(S, x, false);
end);

# same method for regular/inverse/ideals

InstallMethod(GreensRClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsMultiplicativeElement],
function(S, x)
  return GreensRClassOfElementNC(S, x, true);
end);

# same method for regular/inverse/ideals

InstallMethod(GreensRClassOfElementNC,
"for an acting semigroup, multiplicative element, and bool",
[IsActingSemigroup, IsMultiplicativeElement, IsBool],
function(S, x, isGreensClassNC)
  local R;
  R := SEMIGROUPS.CreateRClass(S, x, isGreensClassNC);
  SEMIGROUPS.SetLambda(R);
  SEMIGROUPS.RectifyLambda(R);
  return R;
end);

# same method for regular/inverse/ideals

InstallMethod(GreensRClassOfElement,
"for an acting semigroup D-class and multiplicative element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsMultiplicativeElement],
function(D, x)
  if not x in D then
    ErrorNoReturn("Semigroups: GreensRClassOfElement: usage,\n",
                  "the element does not belong to the D-class,");
  fi;
  return GreensRClassOfElementNC(D, x, IsGreensClassNC(D));
end);

# same method for regular/inverse/ideals

InstallMethod(GreensRClassOfElementNC, "for D-class and multiplicative element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsMultiplicativeElement],
function(D, x)
  return GreensRClassOfElementNC(D, x, true);
end);

# same method for regular/inverse/ideals

InstallMethod(GreensRClassOfElementNC,
"for a D-class, multiplicative element, and bool",
[IsGreensDClass and IsActingSemigroupGreensClass, IsMultiplicativeElement,
 IsBool],
function(D, x, isGreensClassNC)
  local R;
  R := SEMIGROUPS.CreateRClass(D, x, isGreensClassNC);
  SEMIGROUPS.CopyLambda(D, R);
  SEMIGROUPS.RectifyLambda(R);
  SetDClassOfRClass(R, D);
  return R;
end);

# same method for regular/ideals/inverse

InstallMethod(GreensHClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsMultiplicativeElement],
function(S, x)
  if not x in S then
    ErrorNoReturn("Semigroups: GreensHClassOfElement: usage,\n",
                  "the element does not belong to the semigroup,");
  fi;
  return GreensHClassOfElementNC(S, x, false);
end);

# same method for regular/ideals/inverse

InstallMethod(GreensHClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsMultiplicativeElement],
function(S, x)
  return GreensHClassOfElementNC(S, x, true);
end);

# same method for regular/ideals, different for inverse

InstallMethod(GreensHClassOfElementNC,
"for an acting semigroup, element, and bool",
[IsActingSemigroup, IsMultiplicativeElement, IsBool],
function(S, x, isGreensClassNC)
  local H;
  H := SEMIGROUPS.CreateHClass(S, x, isGreensClassNC);
  SEMIGROUPS.SetLambda(H);
  SEMIGROUPS.SetRho(H);
  return H;
end);

# same method for regular/ideals/inverse

InstallMethod(GreensHClassOfElement, "for a D/H-class and element",
[IsActingSemigroupGreensClass and IsGreensClass, IsMultiplicativeElement],
function(C, x)
  if not x in C then
    ErrorNoReturn("Semigroups: GreensHClassOfElement: usage,\n",
                  "the element does not belong to the D-class,");
  fi;
  return GreensHClassOfElementNC(C, x, IsGreensClassNC(C));
end);

# same method for regular/ideals/inverse

InstallMethod(GreensHClassOfElementNC, "for a D/H-class and element",
[IsActingSemigroupGreensClass and IsGreensClass, IsMultiplicativeElement],
function(C, x)
  return GreensHClassOfElementNC(C, x, true);
end);

# same method for regular/ideals, different method for inverse

InstallMethod(GreensHClassOfElementNC, "for a D/H-class, element, and bool",
[IsActingSemigroupGreensClass and IsGreensClass,
 IsMultiplicativeElement,
 IsBool],
function(C, x, isGreensClassNC)
  local H;
  H := SEMIGROUPS.CreateHClass(C, x, isGreensClassNC);
  SEMIGROUPS.CopyLambda(C, H);
  SEMIGROUPS.CopyRho(C, H);
  if IsGreensDClass(C) then
    SetDClassOfHClass(H, C);
  fi;
  return H;
end);

# same method for regular/ideals, different method for inverse

InstallMethod(GreensHClassOfElementNC, "for an L-class, element, and bool",
[IsActingSemigroupGreensClass and IsGreensLClass, IsMultiplicativeElement,
 IsBool],
function(L, x, isGreensClassNC)
  local H;
  H := SEMIGROUPS.CreateHClass(L, x, isGreensClassNC);
  SEMIGROUPS.CopyRho(L, H);
  SEMIGROUPS.SetLambda(H);
  SetLClassOfHClass(H, L);
  return H;
end);

# same method for regular/ideals, different method for inverse

InstallMethod(GreensHClassOfElementNC, "for an R-class, element, and bool",
[IsActingSemigroupGreensClass and IsGreensRClass, IsMultiplicativeElement,
 IsBool],
function(R, x, isGreensClassNC)
  local H;
  H := SEMIGROUPS.CreateHClass(R, x, isGreensClassNC);
  SEMIGROUPS.CopyLambda(R, H);
  SEMIGROUPS.SetRho(H);
  SetRClassOfHClass(H, R);
  return H;
end);

# different method for inverse/regular, same for ideals

InstallMethod(Size, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  local L, R;
  L := LambdaOrbSchutzGp(LambdaOrb(D), LambdaOrbSCCIndex(D));
  R := RhoOrbSchutzGp(RhoOrb(D), RhoOrbSCCIndex(D));
  return Size(R) * Size(L) * Length(LambdaOrbSCC(D)) * Length(RhoOrbSCC(D)) /
   Size(SchutzenbergerGroup(D));
end);

# same method for inverse/regular/ideals

InstallMethod(Size, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
R -> Size(SchutzenbergerGroup(R)) * Length(LambdaOrbSCC(R)));

# same method for regular/ideals, different method of inverse

InstallMethod(Size, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
L -> Size(SchutzenbergerGroup(L)) * Length(RhoOrbSCC(L)));

# same method for inverse/regular/ideals

InstallMethod(Size, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
H -> Size(SchutzenbergerGroup(H)));

# same method for regular/ideals, different for inverse

InstallMethod(\in, "for multiplicative element and D-class of acting semigroup",
[IsMultiplicativeElement, IsGreensDClass and IsActingSemigroupGreensClass],
function(x, D)
  local S, rep, o, m, scc, l, schutz, cosets, membership, one, p;

  S := Parent(D);
  rep := Representative(D);

  # FIXME ActionRank method selection causes slow down here...
  if ElementsFamily(FamilyObj(S)) <> FamilyObj(x)
      or (IsActingSemigroupWithFixedDegreeMultiplication(S)
          and ActionDegree(x) <> ActionDegree(rep))
      or ActionRank(S)(x) <> ActionRank(S)(rep) then
    return false;
  fi;

  o := LambdaOrb(D);
  m := LambdaOrbSCCIndex(D);
  scc := OrbSCC(o);
  l := Position(o, LambdaFunc(S)(x));

  if l = fail or OrbSCCLookup(o)[l] <> m then
    return false;
  elif l <> scc[m][1] then
    x := x * LambdaOrbMult(o, m, l)[2];
  fi;

  o := RhoOrb(D);
  m := RhoOrbSCCIndex(D);
  scc := OrbSCC(o);
  l := Position(o, RhoFunc(S)(x));
  schutz := RhoOrbStabChain(D);

  if l = fail or OrbSCCLookup(o)[l] <> m then
    return false;
  elif schutz = true then
    return true;
  elif l <> scc[m][1] then
    x := RhoOrbMult(o, m, l)[2] * x;
  fi;

  cosets := LambdaCosets(D);
  x := LambdaPerm(S)(rep, x);
  membership := SchutzGpMembership(S);
  one := LambdaIdentity(S)(ActionRank(S)(rep));

  if schutz <> false then
    for p in cosets do
      if membership(schutz, x / p) then
        return true;
      fi;
    od;
  else
    for p in cosets do
      if x / p = one then
        return true;
      fi;
    od;
  fi;

  return false;
end);

# same method for regular/ideals, different method for inverse

InstallMethod(\in, "for multiplicative element and L-class of acting semigroup",
[IsMultiplicativeElement, IsGreensLClass and IsActingSemigroupGreensClass],
function(x, L)
  local S, rep, o, m, scc, l, schutz;

  S := Parent(L);
  rep := Representative(L);

  if ElementsFamily(FamilyObj(S)) <> FamilyObj(x)
      or (IsActingSemigroupWithFixedDegreeMultiplication(S)
          and ActionDegree(x) <> ActionDegree(rep))
      or ActionRank(S)(x) <> ActionRank(S)(rep)
      or LambdaFunc(S)(x) <> LambdaFunc(S)(rep) then
    return false;
  fi;

  o := RhoOrb(L);
  m := RhoOrbSCCIndex(L);
  scc := OrbSCC(o);
  l := Position(o, RhoFunc(S)(x));
  schutz := RhoOrbStabChain(L);

  if l = fail or OrbSCCLookup(o)[l] <> m then
    return false;
  elif schutz = true then
    return true;
  elif l <> scc[m][1] then
    x := RhoOrbMult(o, m, l)[2] * x;
  fi;

  if x = rep then
    return true;
  elif schutz = false then
    return false;
  fi;

  return SchutzGpMembership(S)(schutz, LambdaPerm(S)(rep, x));
end);

# same method for regular/inverse/ideals

InstallMethod(\in, "for multiplicative element and R-class of acting semigroup",
[IsMultiplicativeElement, IsGreensRClass and IsActingSemigroupGreensClass],
function(x, R)
  local S, rep, o, m, scc, l, schutz;

  S := Parent(R);
  rep := Representative(R);

  if ElementsFamily(FamilyObj(S)) <> FamilyObj(x)
      or (IsActingSemigroupWithFixedDegreeMultiplication(S)
          and ActionDegree(x) <> ActionDegree(rep))
      or ActionRank(S)(x) <> ActionRank(S)(rep)
      or RhoFunc(S)(x) <> RhoFunc(S)(rep) then
    return false;
  fi;

  o := LambdaOrb(R);
  m := LambdaOrbSCCIndex(R);
  scc := OrbSCC(o);
  l := Position(o, LambdaFunc(S)(x));
  schutz := LambdaOrbStabChain(o, m);

  if l = fail or OrbSCCLookup(o)[l] <> m then
    return false;
  elif schutz = true then
    return true;
  elif l <> scc[m][1] then
    x := x * LambdaOrbMult(o, m, l)[2];
  fi;

  if x = rep then
    return true;
  elif schutz = false then
    return false;
  fi;

  return SchutzGpMembership(S)(schutz, LambdaPerm(S)(rep, x));
end);

# same method for regular/inverse/ideals

InstallMethod(\in, "for element and acting semigroup H-class",
[IsMultiplicativeElement, IsGreensHClass and IsActingSemigroupGreensClass],
function(x, H)
  local S, rep;

  S := Parent(H);
  rep := Representative(H);

  if ElementsFamily(FamilyObj(S)) <> FamilyObj(x)
      or (IsActingSemigroupWithFixedDegreeMultiplication(S)
          and ActionDegree(x) <> ActionDegree(rep))
      or ActionRank(S)(x) <> ActionRank(S)(rep)
      or RhoFunc(S)(x) <> RhoFunc(S)(rep)
      or LambdaFunc(S)(x) <> LambdaFunc(S)(rep) then
    return false;
  fi;

  return LambdaPerm(S)(rep, x) in SchutzenbergerGroup(H);
end);

#############################################################################
## 4. Collections of classes, and reps
#############################################################################

# These are not rho-rectified!

# different methods for regular/inverse/ideals

InstallMethod(DClassReps, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local data, scc, out, i;

  data := Enumerate(SemigroupData(S), infinity, ReturnFalse);
  scc := OrbSCC(data);
  out := EmptyPlist(Length(scc) - 1);

  for i in [2 .. Length(scc)] do
    out[i - 1] := data[scc[i][1]][4];
  od;
  return out;
end);

# different method for regular/inverse/ideals

InstallMethod(GreensDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local data, scc, out, CreateDClass, RectifyRho, next, D, i;

  data         := Enumerate(SemigroupData(S), infinity, ReturnFalse);
  scc          := OrbSCC(data);
  out          := EmptyPlist(Length(scc));
  CreateDClass := SEMIGROUPS.CreateDClass;
  RectifyRho   := SEMIGROUPS.RectifyRho;

  for i in [2 .. Length(scc)] do
    next := data[scc[i][1]];
    # don't use GreensDClassOfElementNC here since we don't have to lookup scc
    # indices or rectify lambda
    D := CreateDClass(S, next[4], false);
    SetLambdaOrb(D, LambdaOrb(S));
    SetLambdaOrbSCCIndex(D, next[2]);
    SetRhoOrb(D, RhoOrb(S));
    SetRhoOrbSCCIndex(D, OrbSCCLookup(RhoOrb(S))[data!.rholookup[next[6]]]);
    RectifyRho(D);
    SetSemigroupDataIndex(D, next[6]);
    out[i - 1] := D;
  od;

  return out;
end);

# same method for regular/inverse/ideals

InstallMethod(LClassReps, "for an acting semigroup",
[IsActingSemigroup],
S -> Concatenation(List(GreensDClasses(S), LClassReps)));

# same method for regular/inverse/ideals

InstallMethod(GreensLClasses, "for an acting semigroup",
[IsActingSemigroup],
S -> Concatenation(List(GreensDClasses(S), GreensLClasses)));

# same method for regular/inverse/ideals

InstallMethod(LClassReps, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  local scc, mults, cosets, rep, act, out, nr, x, p, i;

  scc := LambdaOrbSCC(D);
  mults := LambdaOrbMults(LambdaOrb(D), LambdaOrbSCCIndex(D));
  cosets := LambdaCosets(D);
  rep := Representative(D);
  act := StabilizerAction(Parent(D));
  out := EmptyPlist(Length(scc) * Length(cosets));
  nr := 0;

  for p in cosets do
    x := act(rep, p);
    for i in scc do
      nr := nr + 1;
      # don't use GreensLClassOfElementNC cos we don't need to rectify the
      # rho-value
      out[nr] := x * mults[i][1];
    od;
  od;

  return out;
end);

# same method for regular/ideals, different for inverse

InstallMethod(GreensLClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
function(D)
  local reps, out, CreateLClass, CopyRho, i;

  reps := LClassReps(D);
  out := EmptyPlist(Length(reps));
  CreateLClass := SEMIGROUPS.CreateLClass;
  CopyRho      := SEMIGROUPS.CopyRho;

  for i in [1 .. Length(reps)] do
    # don't use GreensLClassOfElementNC cos we don't need to rectify the
    # rho-value
    out[i] := CreateLClass(D, reps[i], IsGreensClassNC(D));
    CopyRho(D, out[i]);
    SetDClassOfLClass(out[i], D);
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(RClassReps, "for an acting semigroup", [IsActingSemigroup],
function(S)
  local data, out, i;

  data := Enumerate(SemigroupData(S));
  out := EmptyPlist(Length(data) - 1);

  for i in [2 .. Length(data)] do
    out[i - 1] := data[i][4];
  od;
  return out;
end);

# different method for regular/inverse, same for ideals

InstallMethod(GreensRClasses, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local reps, out, data, CreateRClass, i;

  reps := RClassReps(S);
  out := EmptyPlist(Length(reps));
  data := SemigroupData(S);
  CreateRClass := SEMIGROUPS.CreateRClass;
  for i in [1 .. Length(reps)] do
    # don't use GreensRClassOfElementNC cos we don't need to rectify the
    # lambda-value
    out[i] := SEMIGROUPS.CreateRClass(S, reps[i], false);
    SetLambdaOrb(out[i], LambdaOrb(S));
    SetLambdaOrbSCCIndex(out[i], data[i + 1][2]);
    SetSemigroupDataIndex(out[i], i + 1);
  od;
  return out;
end);

# same method for regular/ideals, different method for inverse

InstallMethod(RClassReps, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
function(D)
  local scc, mults, cosets, rep, act, out, nr, x, i, p;

  scc := RhoOrbSCC(D);
  mults := RhoOrbMults(RhoOrb(D), RhoOrbSCCIndex(D));
  cosets := RhoCosets(D);
  rep := Representative(D);
  act := StabilizerAction(Parent(D));
  out := EmptyPlist(Length(scc) * Length(cosets));
  nr := 0;

  for p in cosets do
    x := act(rep, p ^ -1);
    for i in scc do
      nr := nr + 1;
      out[nr] := mults[i][1] * x;
    od;
  od;

  return out;
end);

# same method for regular/inverse/ideals

InstallMethod(GreensRClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
function(D)
  local reps, out, CreateRClass, CopyLambda, i;

  reps         := RClassReps(D);
  out          := EmptyPlist(Length(reps));
  CreateRClass := SEMIGROUPS.CreateRClass;
  CopyLambda   := SEMIGROUPS.CopyLambda;

  for i in [1 .. Length(reps)] do
    # don't use GreensRClassOfElementNC cos we don't need to rectify the
    # lambda-value
    out[i] := CreateRClass(D, reps[i], IsGreensClassNC(D));
    CopyLambda(D, out[i]);
    SetDClassOfRClass(out[i], D);
  od;
  return out;
end);

# same method for regular/inverse/ideals

InstallMethod(HClassReps, "for an acting semigroup",
[IsActingSemigroup], S -> Concatenation(List(GreensRClasses(S), HClassReps)));

# same method for regular/inverse/ideals

InstallMethod(GreensHClasses, "for an acting semigroup",
[IsActingSemigroup],
S -> Concatenation(List(GreensDClasses(S), GreensHClasses)));

# same method for regular/inverse/ideals

InstallMethod(HClassReps, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
D -> Concatenation(List(GreensRClasses(D), HClassReps)));

# same method for regular/inverse/ideals

InstallMethod(GreensHClasses, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
D -> Concatenation(List(GreensRClasses(D), GreensHClasses)));

# same method for regular/inverse/ideals

InstallMethod(HClassReps, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(R)
  local scc, mults, cosets, rep, act, out, nr, x, i, p;

  scc := LambdaOrbSCC(R);
  mults := LambdaOrbMults(LambdaOrb(R), LambdaOrbSCCIndex(R));
  cosets := LambdaCosets(DClassOfRClass(R));
  rep := Representative(R);
  act := StabilizerAction(Parent(R));
  out := EmptyPlist(Length(scc) * Length(cosets));
  nr := 0;

  for p in cosets do
    x := act(rep, p);
    for i in scc do
      nr := nr + 1;
      out[nr] := x * mults[i][1];
    od;
  od;
  return out;
end);

# same method for regular/inverse/ideals

InstallMethod(GreensHClasses, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(R)
  local reps, out, i;
  reps := HClassReps(R);
  out := [GreensHClassOfElementNC(R, reps[1], IsGreensClassNC(R))];
  # do this rather than GreensHClassOfElement(R, reps[i]) to
  # avoid recomputing the RhoOrb.
  for i in [2 .. Length(reps)] do
    out[i] := GreensHClassOfElementNC(out[1], reps[i], IsGreensClassNC(R));
    SetRClassOfHClass(out[i], R);
  od;
  return out;
end);

# same method for regular/ideals, different method for inverse

InstallMethod(HClassReps, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local scc, mults, cosets, rep, act, out, nr, x, p, i;

  scc := RhoOrbSCC(L);
  mults := RhoOrbMults(RhoOrb(L), RhoOrbSCCIndex(L));
  cosets := RhoCosets(L);
  # These are the rho cosets of the D-class containing L rectified so that they
  # correspond to the lambda value of the rep of L and not the lambda value of
  # the rep of the D-class.
  rep := Representative(L);
  act := StabilizerAction(Parent(L));
  out := EmptyPlist(Length(scc) * Length(cosets));
  nr := 0;

  for p in cosets do
    x := act(rep, p);
    for i in scc do
      nr := nr + 1;
      out[nr] := mults[i][1] * x;
    od;
  od;
  return out;
end);

# same method for regular/ideals, different for inverse

InstallMethod(GreensHClasses, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local reps, out, i;
  reps := HClassReps(L);
  out := [GreensHClassOfElementNC(L, reps[1], IsGreensClassNC(L))];
  # do this rather than GreensHClassOfElement(L, reps[i]) to
  # avoid recomputing the RhoOrb.
  for i in [2 .. Length(reps)] do
    out[i] := GreensHClassOfElementNC(out[1], reps[i], IsGreensClassNC(L));
    SetLClassOfHClass(out[i], L);
  od;
  return out;
end);

#############################################################################

# different method for regular/inverse, same for ideals

InstallMethod(NrDClasses, "for an acting semigroup",
[IsActingSemigroup],
S -> Length(OrbSCC(SemigroupData(S))) - 1);

# different method for regular/inverse, same for ideals

InstallMethod(NrLClasses, "for an acting semigroup",
[IsActingSemigroup], S -> Sum(List(GreensDClasses(S), NrLClasses)));

# different method for regular/inverse, same for ideals

InstallMethod(NrLClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
D -> Length(LambdaCosets(D)) * Length(LambdaOrbSCC(D)));

# different method for regular/inverse, same for ideals

InstallMethod(NrRClasses, "for an acting semigroup", [IsActingSemigroup],
S -> Length(Enumerate(SemigroupData(S), infinity, ReturnFalse)) - 1);

# different method for regular/inverse, same for ideals

InstallMethod(NrRClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
D -> Length(RhoCosets(D)) * Length(RhoOrbSCC(D)));

# same method for regular/inverse/ideals

InstallMethod(NrHClasses, "for an acting semigroup", [IsActingSemigroup],
function(S)
  return Sum(List(GreensDClasses(S), NrHClasses));
end);

# same method for regular/ideals, different method for inverse

InstallMethod(GroupHClassOfGreensDClass,
"for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  local S, rho, o, scc, tester, i;

  if HasIsRegularGreensClass(D) and not IsRegularGreensClass(D) then
    return fail;
  fi;

  S := Parent(D);
  rho := RhoFunc(S)(Representative(D));
  o := LambdaOrb(D);
  scc := LambdaOrbSCC(D);
  tester := IdempotentTester(S);

  for i in scc do
    if tester(o[i], rho) then
      if not HasIsRegularGreensClass(D) then
        SetIsRegularGreensClass(D, true);
      fi;
      return GreensHClassOfElementNC(D, IdempotentCreator(S)(o[i], rho),
                                     IsGreensClassNC(D));
    fi;
  od;

  if not HasIsRegularGreensClass(D) then
    SetIsRegularGreensClass(D, false);
  fi;
  return fail;
end);

# same method for regular/inverse/ideals

InstallMethod(IsGroupHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local S, x;
  S := Parent(H);
  x := Representative(H);
  return IdempotentTester(S)(LambdaFunc(S)(x), RhoFunc(S)(x));
end);

# same method for regular/inverse/ideals

InstallMethod(IsomorphismPermGroup, "for H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local map, iso, inv;

  if not IsGroupHClass(H) then
    ErrorNoReturn("Semigroups: IsomorphismPermGroup: usage,\n",
                  "the H-class is not a group,");
  fi;

  if not IsPermGroup(SchutzenbergerGroup(H)) then
    map := IsomorphismPermGroup(SchutzenbergerGroup(H));
  else
    map := IdentityMapping(SchutzenbergerGroup(H));
  fi;

  iso := function(x)
    if not x in H then
      ErrorNoReturn("argument does not belong to the domain of the ",
                    "function,");
    fi;
    return LambdaPerm(Parent(H))(MultiplicativeNeutralElement(H), x) ^ map;
  end;
  inv := function(x)
    if not x in Image(map) then
      ErrorNoReturn("argument does not belong to the domain of the ",
      "function,");
    fi;
    return StabilizerAction(Parent(H))(MultiplicativeNeutralElement(H),
                                       x ^ InverseGeneralMapping(map));
  end;
  return MappingByFunction(H, SchutzenbergerGroup(H), iso, inv);
end);

# different method for regular/inverse/ideals

InstallMethod(PartialOrderOfDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local d, n, out, data, gens, graph, lambdarhoht, datalookup, reps, repslens,
  ht, repslookup, lambdafunc, rhofunc, lambdaperm, o, orho, scc, lookup,
  schutz, mults, f, l, m, val, j, i, k, x;

  d := GreensDClasses(S);
  n := Length(d);
  out := List([1 .. n], x -> []);

  data := SemigroupData(S);
  gens := data!.gens;
  graph := data!.graph;
  lambdarhoht := data!.lambdarhoht;
  datalookup := OrbSCCLookup(data) - 1;
  reps := data!.reps;
  repslens := data!.repslens;
  ht := data!.ht;
  repslookup := data!.repslookup;

  lambdafunc := LambdaFunc(S);
  rhofunc := RhoFunc(S);
  lambdaperm := LambdaPerm(S);

  o := LambdaOrb(S);
  orho := RhoOrb(S);
  scc := OrbSCC(o);
  lookup := OrbSCCLookup(o);
  Perform([2 .. Length(scc)], x -> LambdaOrbSchutzGp(o, x));
  Perform([2 .. Length(scc)], x -> LambdaOrbMults(o, x));
  schutz := o!.schutzstab;
  mults := o!.mults;

  for i in [1 .. n] do
    # collect info about left multiplying R-class reps of d[i] by gens
    for j in OrbSCC(data)[OrbSCCLookup(data)[SemigroupDataIndex(d[i])]] do
      for k in graph[j] do
        AddSet(out[i], datalookup[k]);
      od;
    od;

    for x in gens do
      for f in LClassReps(d[i]) do
        # the below is an expanded version of Position(data, f * x)
        f := f * x;
        l := Position(o, lambdafunc(f));
        m := lookup[l];
        val := lambdarhoht[Position(orho, rhofunc(f))][m];
        if not IsBound(schutz[m]) then
          LambdaOrbSchutzGp(o, m);
        fi;
        if schutz[m] = true then
          j := repslookup[m][val][1];
        else
          if l <> scc[m][1] then
            f := f * mults[l][2];
          fi;
          if schutz[m] = false then
            j := HTValue(ht, f);
          else
            n := 0;
            j := 0;
            repeat
              n := n + 1;
              if SchutzGpMembership(S)(schutz[m],
                                       lambdaperm(reps[m][val][n], f)) then
                j := repslookup[m][val][n];
              fi;
            until j <> 0;
          fi;
        fi;
        AddSet(out[i], datalookup[j]);
      od;
    od;
  od;
  Perform(out, ShrinkAllocationPlist);
  return out;
end);

#############################################################################
## 5. Idempotents . . .
#############################################################################

# same method for regular/ideals, different method for inverse

InstallMethod(Idempotents, "for an acting semigroup", [IsActingSemigroup],
function(S)
  local out, nr, tester, creator, rho_o, scc, lambda_o, gens, rhofunc, lookup,
   rep, rho, j, i, k;

  if HasRClasses(S) or not IsRegularSemigroup(S) then
    return Concatenation(List(GreensRClasses(S), Idempotents));
  fi;

  out := [];
  nr := 0;
  tester := IdempotentTester(S);
  creator := IdempotentCreator(S);
  rho_o := RhoOrb(S);
  scc := OrbSCC(rho_o);
  lambda_o := LambdaOrb(S);
  Enumerate(lambda_o, infinity);
  gens := lambda_o!.gens;
  rhofunc := RhoFunc(S);
  lookup := OrbSCCLookup(rho_o);

  for i in [2 .. Length(lambda_o)] do
    # TODO this could be better, just take the product with the next
    # schreiergen every time
    rep := EvaluateWord(lambda_o, TraceSchreierTreeForward(lambda_o, i));
    rho := rhofunc(rep);
    j := lookup[Position(rho_o, rho)];

    for k in scc[j] do
      if tester(lambda_o[i], rho_o[k]) then
        nr := nr + 1;
        out[nr] := creator(lambda_o[i], rho_o[k]);
      fi;
    od;
  od;

  if not HasNrIdempotents(S) then
    SetNrIdempotents(S, nr);
  fi;
  return out;
end);

# same method for regular/ideals, different method for inverse

InstallMethod(Idempotents,
"for an acting semigroup and a positive integer",
[IsActingSemigroup, IsInt],
function(S, n)
  local out, nr, tester, creator, rho_o, scc, lambda_o, gens, rhofunc, lookup,
   rank, rep, rho, j, i, k;

  if n < 0 then
    ErrorNoReturn("Semigroups: Idempotents: usage,\n",
                  "the second argument <n> must be a non-negative integer,");
  elif n > Maximum(List(GeneratorsOfSemigroup(S), x -> ActionRank(S)(x))) then
    return [];
  fi;

  if HasIdempotents(S) or not IsRegularSemigroup(S) then
    return Filtered(Idempotents(S), x -> ActionRank(S)(x) = n);
  fi;

  out      := [];
  nr       := 0;
  tester   := IdempotentTester(S);
  creator  := IdempotentCreator(S);
  rho_o    := RhoOrb(S);
  scc      := OrbSCC(rho_o);
  lambda_o := LambdaOrb(S);
  gens     := lambda_o!.gens;
  rhofunc  := RhoFunc(S);
  lookup   := OrbSCCLookup(rho_o);
  rank     := RhoRank(S);

  Enumerate(lambda_o, infinity);

  for i in [2 .. Length(lambda_o)] do
    # TODO this could be better, just take the product with the next
    # schreiergen every time
    rep := EvaluateWord(lambda_o, TraceSchreierTreeForward(lambda_o, i));
    rho := rhofunc(rep);
    j := lookup[Position(rho_o, rho)];
    if rank(rho_o[scc[j][1]]) = n then
      for k in scc[j] do
        if tester(lambda_o[i], rho_o[k]) then
          nr := nr + 1;
          out[nr] := creator(lambda_o[i], rho_o[k]);
        fi;
      od;
    fi;
  od;

  return out;
end);

# same method for regular/ideals, different method for inverse

InstallMethod(Idempotents, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  if HasIsRegularGreensClass(D) and not IsRegularGreensClass(D) then
    # this avoids creating the R-classes, which is an unnecessary overhead
    return [];
  fi;
  return Concatenation(List(GreensRClasses(D), Idempotents));
end);

# same method for regular/ideals, different method for inverse

InstallMethod(Idempotents, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
L -> SEMIGROUPS.Idempotents(L, LambdaFunc(Parent(L))(Representative(L)),
                            RhoOrbSCC(L), RhoOrb(L), false));

# same method for regular/ideals, different method for inverse

InstallMethod(Idempotents, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
R -> SEMIGROUPS.Idempotents(R, RhoFunc(Parent(R))(Representative(R)),
                            LambdaOrbSCC(R), LambdaOrb(R), true));

# same method for regular/inverse/ideals

InstallMethod(Idempotents, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local S, x;

  if not IsGroupHClass(H) then
    return [];
  fi;

  S := Parent(H);
  x := Representative(H);
  return [IdempotentCreator(S)(LambdaFunc(S)(x), RhoFunc(S)(x))];
end);

# Number of idempotents . . .

# different method for regular/inverse, same for ideals

InstallMethod(NrIdempotents, "for an acting semigroup", [IsActingSemigroup],
function(S)
  local data, lambda, rho, scc, lenreps, repslens, rholookup, repslookup,
  tester, nr, rhoval, m, ind, i;

  if HasIdempotents(S) then
    return Length(Idempotents(S));
  fi;

  data := Enumerate(SemigroupData(S), infinity, ReturnFalse);

  lambda := LambdaOrb(S);
  rho := RhoOrb(S);
  scc := OrbSCC(lambda);

  lenreps := data!.lenreps;
  repslens := data!.repslens;
  rholookup := data!.rholookup;
  repslookup := data!.repslookup;

  tester := IdempotentTester(S);

  nr := 0;
  for m in [2 .. Length(scc)] do
    for ind in [1 .. lenreps[m]] do
      if repslens[m][ind] = 1 then
        rhoval := rho[rholookup[repslookup[m][ind][1]]];
        for i in scc[m] do
          if tester(lambda[i], rhoval) then
            nr := nr + 1;
          fi;
        od;
      fi;
    od;
  od;

  return nr;
end);

# same method for regular/inverse/ideals

InstallMethod(NrIdempotents, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
D -> Sum(List(GreensRClasses(D), NrIdempotents)));

# same method for regular/ideals, different method for inverse

InstallMethod(NrIdempotents, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
L -> SEMIGROUPS.NrIdempotents(L, LambdaFunc(Parent(L))(Representative(L)),
                              RhoOrbSCC(L), RhoOrb(L), false));

# same method for regular/ideals, different method inverse

InstallMethod(NrIdempotents, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
R -> SEMIGROUPS.NrIdempotents(R, RhoFunc(Parent(R))(Representative(R)),
                              LambdaOrbSCC(R), LambdaOrb(R), true));

# same method for regular/inverse/ideals

InstallMethod(NrIdempotents, "for a H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  if IsGroupHClass(H) then
    return 1;
  fi;
  return 0;
end);

#############################################################################
## 6. Regular classes . . .
#############################################################################

# not required for regular/inverse, same for ideals

InstallMethod(IsRegularGreensClass, "for an D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
D -> SEMIGROUPS.IsRegularGreensClass(D,
                                     RhoFunc(Parent(D))(Representative(D)),
                                     LambdaOrbSCC(D), LambdaOrb(D), true));

# not required for regular/inverse, same for ideals

InstallMethod(IsRegularGreensClass, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
L -> SEMIGROUPS.IsRegularGreensClass(L,
                                     LambdaFunc(Parent(L))(Representative(L)),
                                     RhoOrbSCC(L), RhoOrb(L), false));

# not required for regular/inverse, same for ideals

InstallMethod(IsRegularGreensClass, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
R -> SEMIGROUPS.IsRegularGreensClass(R,
                                     RhoFunc(Parent(R))(Representative(R)),
                                     LambdaOrbSCC(R), LambdaOrb(R), true));

# same method for regular/inverse, same method for ideals

InstallMethod(IsRegularGreensClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass], IsGroupHClass);

# different method for regular/inverse/ideals

InstallMethod(NrRegularDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local data, datascc, rho, lambda, scc, rholookup, tester, nr, j, rhoval, i,
   k;

  data := SemigroupData(S);
  datascc := OrbSCC(data);
  rho := RhoOrb(S);
  lambda := LambdaOrb(S);
  scc := OrbSCC(lambda);
  rholookup := data!.rholookup;
  tester := IdempotentTester(S);
  nr := 0;

  for i in [2 .. Length(datascc)] do
    j := datascc[i][1];
    # data of the first R-class in the D-class corresponding to x
    rhoval := rho[rholookup[j]];
    for k in scc[data[j][2]] do
      if tester(lambda[k], rhoval) then
        nr := nr + 1;
        break;
      fi;
    od;
  od;
  return nr;
end);

#############################################################################
## 7. Iterators and enumerators . . .
#############################################################################

# FIXME move to separate file

# TODO improve the performance of this

# Notes: the only purpose for this is the method for NumberElement.  Otherwise
# use (if nothing much is known) IteratorOfRClasses or if everything is know
# just use RClasses.

# different method for regular/inverse

InstallMethod(EnumeratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  return EnumeratorByFunctions(CollectionsFamily(FamilyObj(S)),
  rec(ElementNumber := function(enum, pos)
        return GreensRClasses(S)[pos];
      end,

      NumberElement := function(enum, R)
        return Position(SemigroupData(S), Representative(R)) - 1;
      end,

      Length := enum -> NrRClasses(S),

      PrintObj := function(enum)
        Print("<enumerator of R-classes of ", ViewString(S), ">");
        return;
      end));

end);

# same method for regular/inverse

InstallMethod(IteratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local iter, CreateRClass, convert;

  if HasGreensRClasses(S) then
    iter := IteratorList(GreensRClasses(S));
    return iter;
  fi;

  CreateRClass := SEMIGROUPS.CreateRClass;

  convert := function(x)
    local R;
    # don't use GreensRClassOfElementNC since we don't need to rectify lambda
    R := CreateRClass(S, x[4], false);
    SetLambdaOrb(R, x[3]);
    SetLambdaOrbSCCIndex(R, x[2]);
    return R;
  end;
  return IteratorByIterator(IteratorOfRClassData(S),
                            convert,
                            [],
                            fail,
                            rec(PrintObj := function(iter)
                                  Print("<iterator of R-classes>");
                                  return;
                                end));
end);

# TODO this should be improved at some point

# different method for regular/inverse

InstallMethod(IteratorOfDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local iter;

  if IsClosedData(SemigroupData(S)) then
    iter := IteratorList(GreensDClasses(S));
    return iter;
  fi;
  return IteratorByIterator(
    IteratorOfRClassData(S),  # baseiter
    function(iter, x)         # convert
      local D, convert;
      convert := function(x)
        local R;
        # Don't use GreensRClassOfElementNC since we don't need to rectify
        # lambda.
        R := SEMIGROUPS.CreateRClass(S, x[4], false);
        SetLambdaOrb(R, x[3]);
        SetLambdaOrbSCCIndex(R, x[2]);
        return R;
      end;
      D := DClassOfRClass(convert(x));
      Add(iter!.classes, D);
      return D;
    end,
    [],
    function(iter, x)         # isnew FIXME ugh!!
      return x = fail or ForAll(iter!.classes, D -> not x[4] in D);
     end,
     rec(classes := [],
         PrintObj := function(iter)
           Print("<iterator of D-classes>");
           return;
         end));
end);
