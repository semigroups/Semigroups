#############################################################################
##
#W  greens-inverse.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for inverse acting semigroups consisting of
# associative elements with a method for InverseOp.

# Methods here are similar to methods in greens-regular but without any use of
# RhoAnything!

#############################################################################
## 1. Helper functions for the creation of Green's classes . . .
#############################################################################

# the following is only for inverse op semigroups!

BindGlobal("SEMIGROUPS_DClassOfXClass", 
function(X)
  local D;
  D := SEMIGROUPS_CreateDClass(X);
  SEMIGROUPS_CopyLambda(X, D);
  SEMIGROUPS_RectifyLambda(D);
  D!.rep := RightOne(D!.rep); 
  # so that lambda and rho are rectified!
  return D;
end);

#

BindGlobal("SEMIGROUPS_InverseRectifyRho",
function(C)
  local o, i, m;

  o := LambdaOrb(C);

  if not IsClosed(o) then
    Enumerate(o, infinity);
  fi;

  if not IsBound(C!.RhoPos) then
    i := Position(o, RhoFunc(Parent(C))(C!.rep));
  else
    i := C!.RhoPos;
  fi;

  m := LambdaOrbSCCIndex(C);

  if i <> OrbSCC(o)[m][1] then
    C!.rep := LambdaOrbMult(o, m, l)[1] * C!.rep;
    # don't set Representative in case we must also rectify Lambda
  fi;

  return;
end);

# same method for inverse ideals

InstallMethod(SchutzenbergerGroup, "for an inverse op L-class",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local o, m, p;

  o := LambdaOrb(L);
  m := LambdaOrbSCCIndex(L);

  if not IsGreensClassNC(L) then
    # go from the lambda-value in scc 1 to the lambda value of the rep of <l>
    p := LambdaConjugator(Parent(L))(RightOne(LambdaOrbRep(o, m)),
          Representative(L));
    return LambdaOrbSchutzGp(o, m) ^ p;
  else 
    return LambdaOrbSchutzGp(o, m);
  fi;
end);

#############################################################################
## 2. Technical Green's classes stuff . . .
#############################################################################

InstallMethod(IsInverseOpClass, "for a Green's class",
[IsGreensClass],
function(class)
  return IsActingSemigroupWithInverseOp(Parent(class));
end);

#

InstallMethod(DClassType, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsInverseOpClass and IsGreensDClass
         and IsActingSemigroupGreensClass);
end);

#

InstallMethod(HClassType, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsInverseOpClass and IsGreensHClass
         and IsActingSemigroupGreensClass and IsHClassOfRegularSemigroup);
end);

#

InstallMethod(LClassType, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsInverseOpClass and IsGreensLClass
         and IsActingSemigroupGreensClass);
end);

#

InstallMethod(RClassType, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsInverseOpClass and IsGreensRClass
         and IsActingSemigroupGreensClass);
end);

#############################################################################
## 3. Individual classes . . .
#############################################################################


InstallMethod(DClassOfLClass, 
"for an L-class of an inverse op acting semigroup",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
SEMIGROUPS_DClassOfXClass);

#

InstallMethod(DClassOfRClass, 
"for an R-class of an inverse op acting semigroup",
[IsInverseOpClass and IsGreensRClass and IsActingSemigroupGreensClass],
SEMIGROUPS_DClassOfXClass);

#

InstallMethod(DClassOfHClass, 
"for an H-class of an inverse op acting semigroup",
[IsInverseOpClass and IsGreensHClass and IsActingSemigroupGreensClass],
SEMIGROUPS_DClassOfXClass);

#

InstallMethod(LClassOfHClass, 
"for an H-class of an inverse op acting semigroup",
[IsInverseOpClass and IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local L;
  L := SEMIGROUPS_CreateLClass(L);
  SEMIGROUPS_CopyLambda(H, L);
  SEMIGROUPS_InverseRectifyRho(L);
  return L;
end);

# same method for inverse ideals

InstallMethod(GreensDClassOfElement,
"for acting semigroup with inverse op and element",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(S, x)
  local D;
  if not x in S then
    Error("Semigroups: GreensDClassOfElement: usage,\n",
          "the element does not belong to the semigroup,");
    return;
  fi;
  D := SEMIGROUPS_CreateDClass(S, x, false);
  SetLambdaOrb(D, LambdaOrb(S));
  SEMIGROUPS_RectifyLambda(D);
  D!.rep := RightOne(D!.rep);
  return D;
end);

# same method for inverse ideals

InstallMethod(GreensDClassOfElementNC, 
"for an inverse op acting semigroup and element",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(S, x)
  local D;
  D := SEMIGROUPS_CreateDClass(S, RightOne(x), true);
  SetLambdaOrb(D, GradedLambdaOrb(S, x, false));
  SetLambdaOrbSCCIndex(D, 1);
  return D;
end);

# same method for inverse ideals

InstallMethod(GreensLClassOfElement, 
"for an inverse op acting semigroup and element",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(S, x)
  local L;

  if not x in S then
    Error("Semigroups: GreensLClassOfElement: usage,\n",
          "the element does not belong to the semigroup,");
    return;
  fi;

  L := SEMIGROUPS_CreateLClass(S, x, false);
  SEMIGROUPS_SetLambda(L);
  SEMIGROUPS_InverseRectifyRho(L);
  return L;
end);

# same method for inverse ideals

InstallMethod(GreensLClassOfElementNC, 
"for an inverse op acting semigroup and element",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(S, x)
  local L;
  L := SEMIGROUPS_CreateLClass(S, x, true);
  SEMIGROUPS_SetLambda(L);
  SEMIGROUPS_InverseRectifyRho(L);
  return L;
end);

# same method for inverse ideals

InstallMethod(GreensLClassOfElementNC, "for D-class and associative element",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(D, x)
  local L;
  L := SEMIGROUPS_CreateLClass(D, x);
  SEMIGROUPS_CopyLambda(D, L);
  SEMIGROUPS_InverseRectifyRho(L);
  SetDClassOfLClass(L, D);
  return L;
end);

# same method for inverse ideals

InstallMethod(GreensHClassOfElement, "for an inverse op acting semigroup and element",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(S, x)
  local H;

  if not x in S then
    Error("Semigroups: GreensHClassOfElement: usage,\n",
          "the element does not belong to the semigroup,");
    return;
  fi;

  H := SEMIGROUPS_CreateHClass(S, x, false);
  SetLambdaOrb(H, LambdaOrb(S));
  SetLambdaOrbSCCIndex(H, OrbSCCIndex(LambdaOrb(S), LambdaFunc(S)(x)));
  return H;
end);

# same method for inverse ideals

InstallMethod(GreensHClassOfElementNC, "for an inverse op acting semigroup and element",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(S, x)
  local H;
  H := SEMIGROUPS_CreateHClass(S, x, false);
  SetLambdaOrb(H, GradedLambdaOrb(S, x, false));
  SetLambdaOrbSCCIndex(H, 1);
  return H;
end);

# same method for inverse ideals

InstallMethod(GreensHClassOfElementNC, "for a inverse op class and element",
[IsInverseOpClass and IsActingSemigroupGreensClass and IsGreensDClass, IsAssociativeElement],
function(C, x)
  local H;
  H := SEMIGROUPS_CreateHClass(C, x);
  SEMIGROUPS_CopyLambda(C, H);
  
  if IsGreensLClass(C) then
    SetLClassOfHClass(H, C);
  elif IsGreensRClass(C) then
    SetRClassOfHClass(H, C);
  elif IsGreensDClass(C) then
    SetDClassOfHClass(H, C);
  fi;
  return H;
end);

# same method for inverse ideals

InstallMethod(Size, "for an inverse op D-class",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass],
D -> Size(SchutzenbergerGroup(D)) * Length(LambdaOrbSCC(D)) ^ 2);

# same method for inverse ideals

InstallMethod(Size, "for an inverse op L-class",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
L -> Size(SchutzenbergerGroup(L)) * Length(LambdaOrbSCC(L)));

#

InstallMethod(\in, "for inverse op D-class",
[IsAssociativeElement,
IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass],
function(x, D)
  local S, rep, m, o, scc, l, schutz;

  S := Parent(D);
  rep := Representative(D);

  if ElementsFamily(FamilyObj(S)) <> FamilyObj(x)
    or (IsActingSemigroupWithFixedDegreeMultiplication(S) and
        ActionDegree(x) <> ActionDegree(rep)) 
    or ActionRank(S)(x) <> ActionRank(S)(rep) then
    return false;
  fi;

  m := LambdaOrbSCCIndex(D);
  o := LambdaOrb(D);
  scc := OrbSCC(o);
  l := Position(o, RhoFunc(S)(x));

  if l = fail or OrbSCCLookup(o)[l] <> m then
    return false;
  elif l <> scc[m][1] then 
    x := LambdaOrbMult(o, m, l)[1] * x;
  fi;
  
  l := Position(o, LambdaFunc(S)(x));
  if l = fail or OrbSCCLookup(o)[l] <> m then
    return false;
  elif l <> scc[m][1] then 
    x := x * LambdaOrbMult(o, m, l)[2];
  fi;
  
  schutz := LambdaOrbStabChain(o, m);
  if schutz = true then
    return true;
  elif x = rep then
    return true;
  elif schutz = false then
    return false;
  fi;

  return SiftedPermutation(schutz, LambdaPerm(S)(rep, x)) = ();
end);

InstallMethod(\in,
"for associative element and inverse op L-class of acting semigroup.",
[IsAssociativeElement, IsInverseOpClass and IsGreensLClass and
IsActingSemigroupGreensClass],
function(x, L)
  local S, rep, o, m, scc, l, schutz;

  S := Parent(L);
  rep := Representative(L);

  if ElementsFamily(FamilyObj(S)) <> FamilyObj(x)
    or ActionRank(S)(x) <> ActionRank(S)(rep)
    or (IsActingSemigroupWithFixedDegreeMultiplication(S) and
        ActionDegree(x) <> ActionDegree(rep))
    or LambdaFunc(S)(x) <> LambdaFunc(S)(rep) then
    return false;
  fi;

  o := LambdaOrb(L);
  m := LambdaOrbSCCIndex(L);
  scc := OrbSCC(o);
  l := Position(o, RhoFunc(S)(x));
  schutz := LambdaOrbStabChain(o, m);
  
  if l = fail or OrbSCCLookup(o)[l] <> m then
    return false;
  elif schutz = true then
    return true;
  elif l <> scc[m][1] then
    x := LambdaOrbMult(o, m, l)[1] * x;
  fi;

  if x = rep then
    return true;
  elif schutz = false then
    return false;
  fi;

  return SiftedPermutation(schutz,  LambdaPerm(S)(rep, x)) = ();
end);

#############################################################################
## 4. Collections of classes, and reps
#############################################################################

# same method for inverse ideals

InstallMethod(GreensDClasses, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(S)
  local o, scc, out, D, i;

  o := LambdaOrb(S);
  scc := OrbSCC(o);
  out := EmptyPlist(Length(scc) - 1);

  for i in [2 .. Length(scc)] do
    D := SEMIGROUPS_CreateDClass(S, RightOne(LambdaOrbRep(o, i)), false);
    SetLambdaOrb(D, o);
    SetLambdaOrbSCCIndex(D, i);
    out[i - 1] := D;
  od;
  return out;
end);

# same method for inverse ideals

InstallMethod(GreensLClasses, "for inverse op D-class of acting semigroup",
[IsActingSemigroupGreensClass and IsInverseOpClass and IsGreensDClass],
function(D)
  local reps, out, i;
  reps := LClassReps(D);
  out := EmptyPlist(Length(reps));
  for i in [ 1 .. Length(reps) ] do
    # don't use GreensLClassOfElementNC cos we don't need to rectify the
    # rho-value
    out[i] := SEMIGROUPS_CreateLClass(D, reps[i]);
    SEMIGROUPS_CopyLambda(D, out[i]);
    SetDClassOfLClass(out[i], D);
  od;
  return out;
end);

# same method for inverse ideals

InstallMethod(RClassReps, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp], S -> List(LClassReps(S), x -> Inverse(x)));

# same method for inverse ideals

InstallMethod(RClassReps, "for a D-class of an inverse op acting semigroup",
[IsActingSemigroupGreensClass and IsInverseOpClass and IsGreensDClass],
D -> List(LClassReps(D), x -> Inverse(x)));

# same method for inverse ideals

InstallMethod(HClassReps, "for an inverse op L-class",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local o, m, scc, mults, rep, out, nr, i;
  o := LambdaOrb(L);
  m := LambdaOrbSCCIndex(L);
  scc := OrbSCC(o)[m];
  mults := LambdaOrbMults(o, m);
  rep := Representative(l);
  out := EmptyPlist(Length(scc));
  nr := 0;
  for i in scc do
    nr := nr + 1;
    out[nr] := mults[i][2] * rep;
  od;
  return out;
end);

#############################################################################

# same method for inverse ideals

InstallMethod(NrRClasses, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp], NrLClasses);

# same method for inverse ideals

InstallMethod(NrRClasses, "for inverse op D-class",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass],
NrLClasses);

# same method for inverse ideals

InstallMethod(NrHClasses, "for an inverse op D-class of acting semigroup",
[IsActingSemigroupGreensClass and IsInverseOpClass and IsGreensDClass],
D -> Length(LambdaOrbSCC(D)) ^ 2);

# same method for inverse ideals

InstallMethod(NrHClasses, "for an inverse op L-class of acting semigroup",
[IsActingSemigroupGreensClass and IsInverseOpClass and IsGreensLClass],
L -> Length(LambdaOrbSCC(L)));

# same method for inverse ideals

InstallMethod(NrHClasses, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
S -> 
return Sum(List(OrbSCC(Enumerate(LambdaOrb(S))), x -> Length(x) ^ 2)) - 1));

# same method for inverse ideals

InstallMethod(GroupHClassOfGreensDClass, "for an inverse op D-class",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  local H;
  H := GreensHClassOfElementNC(D, Representative(D));
  SetIsGroupHClass(H, true);
  return H;
end);

# same method for inverse ideals

InstallMethod(PartialOrderOfDClasses, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(S)
  local D, n, out, o, gens, lookup, lambdafunc, i, x, f;

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
        AddSet(out[i], lookup[Position(o, lambdafunc(Inverse(y) * x))] - 1);
      od;
    od;
  od;

  Perform(out, ShrinkAllocationPlist);
  return out;
end);

#############################################################################
## 5. Idempotents . . .
#############################################################################

# same method for inverse ideals

InstallMethod(Idempotents, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local o, creator, r, out, i;

  o := LambdaOrb(s);
  if not IsClosed(o) then
    Enumerate(o, infinity);
  fi;

  creator := IdempotentCreator(s);
  r := Length(o);
  out := EmptyPlist(r - 1);

  for i in [2 .. r] do
    out[i - 1] := creator(o[i], o[i]);
  od;
  return out;
end);

# same method for inverse ideals

InstallMethod(Idempotents,
"for acting semigroup with inverse op and non-negative integer",
[IsActingSemigroupWithInverseOp, IsInt],
function(S, n)
  local o, creator, out, rank, nr, i;

  if n < 0 then
    Error("Semigroups: Idempotents: usage,\n",
          "the second argument <n> must be a non-negative integer,");
    return;
  fi;

  if HasIdempotents(S) then
    return Filtered(Idempotents(S), x -> ActionRank(S)(x) = n);
  fi;

  o := LambdaOrb(S);
  
  if not IsClosed(o) then
    Enumerate(o, infinity);
  fi;

  creator := IdempotentCreator(S);
  out := EmptyPlist(Length(o) - 1);
  rank := LambdaRank(S);
  nr := 0;

  for i in [2 .. Length(o)] do
    if rank(o[i]) = n then
      nr := nr + 1;
      out[nr] := creator(o[i], o[i]);
    fi;
  od;
  return out;
end);

# same method for inverse ideals

InstallMethod(Idempotents, "for an inverse op D-class",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  local creator, o;
  creator := IdempotentCreator(Parent(D));
  o := LambdaOrb(D);
  return List(LambdaOrbSCC(D), x -> creator(o[x], o[x]));
end);

# same method for inverse ideals

InstallMethod(Idempotents, "for an inverse op L-class",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
L -> [RightOne(Representative(L))]);

# same method for inverse ideals

InstallMethod(Idempotents, "for an inverse op R-class",
[IsInverseOpClass and IsGreensRClass and IsActingSemigroupGreensClass],
R -> [LeftOne(Representative(R))]);

# Number of idempotents . . .

# same method for inverse ideals

InstallMethod(NrIdempotents, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
S -> Length(Enumerate(LambdaOrb(S))) - 1);

# same method for inverse ideals

InstallMethod(NrIdempotents, "for an inverse op D-class",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass],
NrLClasses);

# same method for inverse ideals

InstallMethod(NrIdempotents, "for an inverse op L-class",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass], L -> 1);

# same method for inverse ideals

InstallMethod(NrIdempotents, "for an inverse op R-class",
[IsInverseOpClass and IsGreensRClass and IsActingSemigroupGreensClass], R -> 1);

#############################################################################
## 6. Iterators and enumerators . . .
#############################################################################

# FIXME move this elsewhere

# JDM again this method might not nec. if inverse op semigroups have RhoOrb
# method

# Notes: the only purpose for this is the method for NumberElement.  Otherwise
# use (if nothing much is known) IteratorOfRClasses or if everything is know
# just use RClasses.

InstallMethod(EnumeratorOfRClasses, "for an inverse op acting semigroup",
[IsActingSemigroupWithInverseOp],
function(S)

  Enumerate(LambdaOrb(S));

  return EnumeratorByFunctions(S, rec(

    parent := S,

    Length := enum -> NrRClasses(enum!.parent),

    Membership := function(R, enum)
      return Representative(R) in enum!.parent;
    end,

    NumberElement := function(enum, R)
      local pos;
      pos := Position(LambdaOrbOrb(enum!.parent),
       RhoFunc(enum!.parent)(Representative(R)));
      if pos = fail then
        return fail;
      fi;
      return pos - 1;
    end,

   ElementNumber := function(enum, nr)
    local S, o;
    S := enum!.parent;
    o := LambdaOrb(S);
    return GreensRClassOfElementNC(S,
      Inverse(EvaluateWord(o!.gens, TraceSchreierTreeForward(o, nr))));
    end,

   PrintObj := function(enum)
     Print( "<enumerator of R-classes of ", ViewString(S), ">");
     return;
   end));
end);
