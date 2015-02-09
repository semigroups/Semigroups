#############################################################################
##
#W  greens-acting.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Notes:
# - D-class reps must have rectified lambda and rho value

DeclareGlobalFunction("SEMIGROUPS_IsRegularClass");
DeclareGlobalFunction("SEMIGROUPS_Idempotents");
DeclareGlobalFunction("SEMIGROUPS_NrIdempotents");

# same method for regular/different for inverse

BindGlobal("SEMIGROUPS_CreateGreensClassNC",
function(S, isGreensClassNC, record, type)
  local C;

  C := ObjectifyWithAttributes(record,          type, 
                               Parent,          S,  
                               IsGreensClassNC, isGreensClassNC, 
                               Representative,  record.rep);
  if IsGreensDClass(C) then 
    SetEquivalenceClassRelation(C, GreensDRelation(S));
  elif IsGreensRClass(C) then 
    SetEquivalenceClassRelation(C, GreensRRelation(S));
  elif IsGreensLClass(C) then 
    SetEquivalenceClassRelation(C, GreensLRelation(S));
  elif IsGreensHClass(C) then 
    SetEquivalenceClassRelation(C, GreensHRelation(S));
  fi;

  return C;
end);

InstallMethod(LambdaOrb, "for a Green's class", 
[IsActingSemigroupGreensClass], C -> C!.LambdaOrb);

InstallMethod(LambdaOrb, "for a Green's L-class", 
[IsGreensLClass and IsActingSemigroupGreensClass], 
function(L)
  Error("Semigroups: LambdaOrb: usage\n", 
        "L-classes do not have this attribute,");
end);

InstallMethod(LambdaOrbSCCIndex, "for a Green's class", 
[IsActingSemigroupGreensClass], C -> C!.LambdaOrbSCCIndex);

InstallMethod(LambdaOrbSCCIndex, "for a Green's L-class", 
[IsGreensLClass and IsActingSemigroupGreensClass], 
function(L)
  Error("Semigroups: LambdaOrbSCCIndex: usage\n", 
        "L-classes do not have this attribute,");
end);

InstallMethod(RhoOrb, "for a Green's class", 
[IsActingSemigroupGreensClass], 
function(C)
  return C!.RhoOrb;
end);

InstallMethod(RhoOrb, "for a Green's L-class", 
[IsGreensRClass and IsActingSemigroupGreensClass], 
function(R)
  Error("Semigroups: RhoOrb: usage\n", 
        "R-classes do not have this attribute,");
end);

InstallMethod(RhoOrbSCCIndex, "for a Green's class", 
[IsActingSemigroupGreensClass], C -> C!.RhoOrbSCCIndex);

InstallMethod(RhoOrbSCCIndex, "for a Green's L-class", 
[IsGreensLClass and IsActingSemigroupGreensClass], 
function(L)
  Error("Semigroups: RhoOrbSCCIndex: usage\n", 
        "L-classes do not have this attribute,");
end);

# same method for regular/inverse/ideals

InstallMethod(LambdaOrbSCC, "for a Green's class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensClass],
C -> OrbSCC(LambdaOrb(C))[LambdaOrbSCCIndex(C)]);

# same method for regular/inverse/ideals

InstallMethod(RhoOrbSCC, "for a Green's class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensClass],
C -> OrbSCC(RhoOrb(C))[RhoOrbSCCIndex(C)]);

InstallMethod(SemigroupDataIndex,
"for an acting semigroup Green's class",
[IsActingSemigroupGreensClass],
function(C)
  return Position(SemigroupData(Parent(C)), Representative(C));
end);

# TODO improve the performance of this

# Notes: the only purpose for this is the method for NumberElement.  Otherwise
# use (if nothing much is known) IteratorOfRClasses or if everything is know
# just use RClasses.

# different method for regular/inverse

InstallMethod(EnumeratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup],
function(S)

  return EnumeratorByFunctions(S, rec(

    ElementNumber := function(enum, pos)
      return GreensRClasses(S)[pos];
    end,

    NumberElement := function(enum, R)
      return Position(SemigroupData(S), Representative(R)) - 1;
    end,

    Membership := function(R, enum)
      return Position(enum, R) <> fail;
    end,

    Length := enum -> NrRClasses(S),

    PrintObj := function(enum)
      Print( "<enumerator of R-classes of ", ViewString(S), ">");
      return;
    end));

end);

# same method for regular/inverse
# FIXME is this necessary?
InstallMethod(GreensJClassOfElement, "for acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement], GreensDClassOfElement);

# different method for regular/inverse

InstallMethod(DClassType, "for an acting semigroup",
[IsActingSemigroup],
function( S )
  return NewType( FamilyObj( S ), IsEquivalenceClass and
          IsEquivalenceClassDefaultRep and IsGreensDClass and
          IsActingSemigroupGreensClass );
end);

# different method for regular/inverse

InstallMethod(HClassType, "for an acting semigroup",
[IsActingSemigroup],
function( S )
 return NewType( FamilyObj( S ), IsEquivalenceClass and
  IsEquivalenceClassDefaultRep and IsGreensHClass and
  IsActingSemigroupGreensClass);
end);

# different method for regular/inverse

InstallMethod(LClassType, "for an acting semigroup",
[IsActingSemigroup],
function( S )
  return NewType( FamilyObj( S ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensLClass and
         IsActingSemigroupGreensClass);
end);

# different method for regular/inverse

InstallMethod(RClassType, "for an acting semigroup",
[IsActingSemigroup],
function( S )
  return NewType( FamilyObj( S ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensRClass and
         IsActingSemigroupGreensClass);
end);

# not required for regular/inverse, same method for ideals

InstallMethod(LambdaCosets, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(D)
  return RightTransversal(LambdaOrbSchutzGp(LambdaOrb(D),
   LambdaOrbSCCIndex(D)), SchutzenbergerGroup(D));
end);

# not required for regular/inverse, same method for ideals
# TODO refactor this to remove the call to SchutzenbergerGroup

InstallMethod(RhoCosets, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  SchutzenbergerGroup(d);
  return RhoCosets(d);
end);

# note that the RhoCosets of the D-class containing an L-class are not the same
# as the RhoCosets of the L-class. The RhoCosets of a D-class correspond to the
# lambda value of the rep of the D-class (which is in the first place of its
# scc) and the rep of the D-class but the RhoCosets of the L-class should
# correspond to the lambda value of the rep of the L-class which is not nec in
# the first place of its scc.

# different method for regular, same method for ideals
# TODO refactor this
InstallMethod(RhoCosets, "for a L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local D, S, o, rep, pos, m, x, conj;

  D := DClassOfLClass(L);
  if IsRegularClass(L) or Length(RhoCosets(D)) = 1 then
    #maybe <L> is regular and doesn't know it!
    return [()];
  fi;

  S := Parent(L);
  rep := Representative(L);
  m := LambdaOrbSCCIndex(D);
  o := LambdaOrb(D);

  pos := Position(o, LambdaFunc(S)(rep));

  if pos <> OrbSCC(o)[m][1] then
    x := rep * LambdaOrbMult(o, m, pos)[2];
    conj := LambdaConjugator(S)(x, rep);
    conj := conj * LambdaPerm(S)(x, Representative(D));
    return List(RhoCosets(D), x -> x ^ conj);
  fi;
  return RhoCosets(D);
end);


# different method for regular/inverse, same for ideals
# TODO refactor this
InstallMethod(RhoOrbStabChain, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  SchutzenbergerGroup(d);
  return RhoOrbStabChain(d);
end);

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
  return StabChainImmutable(G);
end);

# main

# different method for regular/inverse, same for ideals

InstallMethod(\in, "for associative element and D-class of acting semigroup",
[IsAssociativeElement, IsGreensDClass and IsActingSemigroupGreensClass],
function(f, d)
  local rep, s, g, m, o, scc, l, schutz, cosets, x;

  rep := Representative(d);
  s := Parent(d);

  # <ActionRank> method selection causes slow down here...
  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f)
    or (IsActingSemigroupWithFixedDegreeMultiplication(s)
        and ActionDegree(f) <> ActionDegree(rep))
    or ActionRank(s)(f) <> ActionRank(s)(rep) then
    return false;
  fi;

  g := f;

  m := LambdaOrbSCCIndex(d);
  o := LambdaOrb(d);
  scc := OrbSCC(o);

  l := Position(o, LambdaFunc(s)(g));

  if l = fail or OrbSCCLookup(o)[l] <> m then
    return false;
  fi;

  if l <> scc[m][1] then
    g := g * LambdaOrbMult(o, m, l)[2];
  fi;

  m := RhoOrbSCCIndex(d);
  o := RhoOrb(d);
  scc := OrbSCC(o);

  l := Position(o, RhoFunc(s)(g));

  if l = fail or OrbSCCLookup(o)[l] <> m then
    return false;
  fi;

  schutz := RhoOrbStabChain(d);

  if schutz = true then
    return true;
  fi;

  if l <> scc[m][1] then
    g := RhoOrbMult(o, m, l)[2] * g;
  fi;

  cosets := LambdaCosets(d);
  g := LambdaPerm(s)(rep, g);

  if schutz <> false then
    for x in cosets do
      if SiftedPermutation(schutz, g / x) = () then
        return true;
      fi;
    od;
  else
    for x in cosets do
      if g / x = () then
        return true;
      fi;
    od;
  fi;

  return false;
end);

# same method for regular/inverse, same for ideals

InstallMethod(\in, "for element and acting semigroup H-class",
[IsAssociativeElement, IsGreensHClass and IsActingSemigroupGreensClass],
function(f, h)
  local s, rep;

  s := Parent(h);
  rep := Representative(h);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f)
    or (IsActingSemigroupWithFixedDegreeMultiplication(s) and
        ActionDegree(f) <> ActionDegree(rep))
    or ActionRank(s)(f) <> ActionRank(s)(rep)
    or RhoFunc(s)(f) <> RhoFunc(s)(rep)
    or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) then
    return false;
  fi;

  return LambdaPerm(s)(rep, f) in SchutzenbergerGroup(h);
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(\in, "for associative element and L-class of acting semigroup",
[IsAssociativeElement, IsGreensLClass and IsActingSemigroupGreensClass],
function(f, l)
  local rep, s, m, o, i, schutz, g, p;

  rep := Representative(l);
  s := Parent(l);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f)
    or (IsActingSemigroupWithFixedDegreeMultiplication(s)
        and ActionDegree(f) <> ActionDegree(rep))
    or ActionRank(s)(f) <> ActionRank(s)(rep)
    or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) then
    return false;
  fi;

  m := RhoOrbSCCIndex(l);
  o := RhoOrb(l);

  if not IsClosed(o) then
    Enumerate(o, infinity);
  fi;

  i := Position(o, RhoFunc(s)(f));

  if i = fail or OrbSCCLookup(o)[i] <> m then
    return false;
  fi;

  schutz := RhoOrbStabChain(l);

  if schutz = true then
    Info(InfoSemigroups, 3, "Schutz. group of L-class is symmetric group");
    return true;
  fi;

  if i <> OrbSCC(o)[m][1] then
    g := RhoOrbMult(o, m, i)[2] * f;
  else
    g := f;
  fi;

  if g = rep then
    Info(InfoSemigroups, 3, "element with rectified rho value equals ",
    "L-class representative");
    return true;
  elif schutz = false then
    Info(InfoSemigroups, 3, "Schutz. group of L-class is trivial");
    return false;
  fi;

  return SiftedPermutation(schutz,  LambdaPerm(s)(rep, g)) = ();
end);

# Algorithm E.
# same method for regular/inverse/ideals

InstallMethod(\in, "for associative element and R-class of acting semigroup",
[IsAssociativeElement, IsGreensRClass and IsActingSemigroupGreensClass],
function(f, r)
  local rep, s, m, o, l, schutz, g;

  rep := Representative(r);
  s := Parent(r);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f)
    or (IsActingSemigroupWithFixedDegreeMultiplication(s)
        and ActionDegree(f) <> ActionDegree(rep))
    or ActionRank(s)(f) <> ActionRank(s)(rep)
    or RhoFunc(s)(f) <> RhoFunc(s)(rep) then
    return false;
  fi;

  m := LambdaOrbSCCIndex(r);
  o := LambdaOrb(r);

  if not IsClosed(o) then
    Enumerate(o, infinity);
  fi;

  l := Position(o, LambdaFunc(s)(f));

  if l = fail or OrbSCCLookup(o)[l] <> m then
    return false;
  fi;

  schutz := LambdaOrbStabChain(o, m);

  if schutz = true then
    Info(InfoSemigroups, 3, "Schutz. group of R-class is symmetric group");
    return true;
  fi;

  g := f;

  if l <> OrbSCC(o)[m][1] then
    g := f * LambdaOrbMult(o, m, l)[2];
  fi;

  if g = rep then
    Info(InfoSemigroups, 3, "element with rectified lambda value equals ",
    "R-class representative");
    return true;
  elif schutz = false then
    Info(InfoSemigroups, 3, "Schutz. group of R-class is trivial");
    return false;
  fi;

  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g)) = ();
end);

# this should be removed after the library method for AsSSortedList
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallMethod(AsSSortedList, "for a Green's class of an acting semigroup",
[IsGreensClass and IsActingSemigroupGreensClass],
function(c)
  return ConstantTimeAccessList(EnumeratorSorted(c));
end);

# same method for regular/inverse/ideals

InstallMethod(DClassOfLClass, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local S, x, record, o;

  S := Parent(L);
  x := Representative(L);

  record := rec(RhoOrb := RhoOrb(L), 
                RhoOrbSCCIndex := RhoOrbSCCIndex(L));

  if HasLambdaOrb(S) and IsClosed(LambdaOrb(S)) and not IsGreensClassNC(L) then
    o := LambdaOrb(S);
  else
    o := GradedLambdaOrb(S, x, IsGreensClassNC(L) <> true, record);
  fi;

  SEMIGROUPS_RectifyLambda(S, o, x, record);

  return SEMIGROUPS_CreateGreensClassNC(S, IsGreensClassNC(L), record,
    DClassType(S));
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(DClassOfRClass, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(R)
  local S, x, record, o;

  S := Parent(R);
  x := Representative(R);

  record := rec(LambdaOrb := LambdaOrb(R), 
                LambdaOrbSCCIndex := LambdaOrbSCCIndex(R));

  if HasRhoOrb(S) and IsClosed(RhoOrb(S)) and not IsGreensClassNC(R) then
    o := RhoOrb(S);
  else
    o := GradedRhoOrb(S, x, IsGreensClassNC(R) <> true, record);
  fi;

  SEMIGROUPS_RectifyRho(S, o, x, record);

  return SEMIGROUPS_CreateGreensClassNC(S, IsGreensClassNC(R), record,
    DClassType(S));
end);

# same method for regular, different method for inverse semigroups,
# same for ideals

InstallMethod(DClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local S, x, record;

  S := Parent(H);
  x := Representative(H);
  
  record := rec(RhoOrb := RhoOrb(L), 
                RhoOrbSCCIndex := RhoOrbSCCIndex(L),
                LambdaOrb := LambdaOrb(R), 
                LambdaOrbSCCIndex := LambdaOrbSCCIndex(R));

  SEMIGROUPS_RectifyRho(S, RhoOrb(H), x, record);
  SEMIGROUPS_RectifyLambda(S, LambdaOrb(H), record.rep, record);

  return SEMIGROUPS_CreateGreensClassNC(S, IsGreensClassNC(H), record,
   HClassType(S));
end); 


# same method for regular, different method for inverse, same for ideals

InstallMethod(LClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local record, S;
  
  record := rec(RhoOrb := RhoOrb(H), 
                RhoOrbSCCIndex := RhoOrbSCCIndex(H));

  S := Parent(H);
  SEMIGROUPS_RectifyRho(S, RhoOrb(H), Representative(H), record);

  return SEMIGROUPS_CreateGreensClassNC(S, IsGreensClassNC(H), record,
   HClassType(S));
end);

# same method for regular/inverse semigroups, same for ideals

InstallMethod(RClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(H)
  local record, S;
  
  record := rec(LambdaOrb := LambdaOrb(H), 
                LambdaOrbSCCIndex := LambdaOrbSCCIndex(H));

  S := Parent(H);
  SEMIGROUPS_RectifyLambda(S, LambdaOrb(H), Representative(H), record);

  return SEMIGROUPS_CreateGreensClassNC(S, IsGreensClassNC(H), record,
   HClassType(S));
end);

# different method for regular/inverse/ideals

# TODO check that this isn't too much slower than before. 

InstallMethod(GreensDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local data, scc, out, type, next, record, D, i;

  data := Enumerate(SemigroupData(S), infinity, ReturnFalse);

  scc := OrbSCC(data);
  out := EmptyPlist(Length(scc));
  type := DClassType(S);

  for i in [2 .. Length(scc)] do
    next := data[scc[i][1]];
    
    record := rec(LambdaOrb := LambdaOrb(S), 
                  LambdaOrbSCCIndex := next[2],
                  RhoOrb := RhoOrb(S),
                  RhoPos := data!.rholookup(next[6]));

    SEMIGROUPS_RectifyRho(S, o, next[4], record);
    
    D := SEMIGROUPS_CreateGreensClassNC(S, false, record, type);
    SetSemigroupDataIndex(d, next[6]);
    out[i - 1] := D;
  od;

  return out;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(GreensHClasses, "for an acting semigroup",
[IsActingSemigroup], S -> Concatenation(List(GreensDClasses(S),
GreensHClasses)));

# different method for regular/inverse, same method for ideals

InstallMethod(GreensHClasses, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass], 
D -> Concatenation(List(GreensRClasses(D), GreensHClasses)));

# different method for regular/inverse, same method for ideals

InstallMethod(GreensHClasses, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local D, record, scc, mults, cosets, S, x, out, type, nr, i, j;
  
  D := DClassOfLClass(L);

  record := rec( LambdaOrb := LambdaOrb(D), 
                 LambdaOrbSCCIndex := LambdaOrbSCCIndex(D),
                 RhoOrb := RhoOrb(L), 
                 RhoOrbSCCIndex := RhoOrbSCCIndex(L));

  scc := RhoOrbSCC(L);
  mults := RhoOrbMults(RhoOrb(L), RhoOrbSCCIndex(L));
  cosets := RhoCosets(L);
  S := Parent(L);
  x := Representative(L);
  act := StabilizerAction(S);
  out := EmptyPlist(Length(scc) * Length(cosets));
  type := HClassType(S);
  nr := 0;

  for i in scc do
    record.rep := mults[i][1] * x;
    for j in cosets do
      # TODO check if we need to copy record here!
      record.rep := act(record.rep, j);
      nr := nr + 1;
      out[nr] := 
       SEMIGROUPS_CreateGreensClassNC(S, IsGreensClassNC(L), record, type);
      SetLClassOfHClass(out[nr], L);
      SetDClassOfHClass(out[nr], D);
    od;
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(GreensHClasses, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(R)
  
  D := DClassOfLClass(R);

  record := rec( LambdaOrb := LambdaOrb(R), 
                 LambdaOrbSCCIndex := LambdaOrbSCCIndex(R),
                 RhoOrb := RhoOrb(D), 
                 RhoOrbSCCIndex := RhoOrbSCCIndex(D));

  scc := LambdaOrbSCC(R);
  mults := LambdaOrbMults(LambdaOrb(R), LambdaOrbSCCIndex(R));
  cosets := LambdaCosets(D);
  S := Parent(R);
  x := Representative(R);
  out := EmptyPlist(Length(scc) * Length(cosets));
  type := HClassType(S);
  nr := 0;

  for p in cosets do
    record.rep := act(x, p);
    for i in scc do
      # TODO check if we need to copy record here!
      record.rep := record.rep * mults[i][1];
      nr := nr + 1;
      out[nr] := 
       SEMIGROUPS_CreateGreensClassNC(S, IsGreensClassNC(R), record, type);
      SetRClassOfHClass(out[nr], R);
      SetDClassOfHClass(out[nr], D);
    od;
  od;
  return out;
end);

# different method for regular/inverse, same for ideals

#TODO check that this isn't slower than before. 
InstallMethod(GreensLClasses, "for an acting semigroup",
[IsActingSemigroup], 
S -> Concatenation(List(GreensDClasses(S), GreensLClasses)));

# FIXME

# different method for regular/inverse, same for ideals
#JDM here!
InstallMethod(GreensLClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
function(d)
  local mults, scc, cosets, f, s, o, m, nc, act, out, k, g, j, i;

  mults := LambdaOrbMults(LambdaOrb(d), LambdaOrbSCCIndex(d));
  scc := LambdaOrbSCC(d);
  cosets := LambdaCosets(d);
  f := Representative(d);

  s := Parent(d);
  o := RhoOrb(d);
  m := RhoOrbSCCIndex(d);
  nc := IsGreensClassNC(d);
  act := StabilizerAction(s);

  out := EmptyPlist(Length(scc) * Length(cosets));
  k := 0;
  for j in cosets do
    g := act(f,j);
    for i in scc do
      k := k + 1;
      #use NC since f has rho value in first place of scc
      #JDM maybe don't use SEMIGROUPS_CreateLClassNC here, and rather expand!
      out[k] := SEMIGROUPS_CreateLClassNC(s, m, o, g * mults[i][1], nc);
      SetDClassOfLClass(out[k], d);
    od;
  od;

  return out;
end);

# different method for regular/inverse, same for ideals

InstallMethod(GreensRClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local data, orbit, out, i;

  data := Enumerate(SemigroupData(s), infinity, ReturnFalse);
  orbit := data!.orbit;
  out := EmptyPlist(Length(orbit));

  for i in [2 .. Length(orbit)] do
    out[i - 1] := CallFuncList(SEMIGROUPS_CreateRClassNC, orbit[i]);
  od;
  return out;
end);

# different method for regular/inverse, same for ideals

InstallMethod(GreensRClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
function(d)
  local mults, scc, cosets, f, s, o, m, nc, act, out, k, g, i, j;

  mults := RhoOrbMults(RhoOrb(d), RhoOrbSCCIndex(d));
  scc := RhoOrbSCC(d);
  cosets := RhoCosets(d);
  f := Representative(d);

  s := Parent(d);
  o := LambdaOrb(d);
  m := LambdaOrbSCCIndex(d);
  nc := IsGreensClassNC(d);
  act := StabilizerAction(s);

  out := EmptyPlist(Length(scc) * Length(cosets));
  k := 0;
  for i in scc do
    g := mults[i][1] * f;
    for j in cosets do
      k := k + 1;
      out[k] := SEMIGROUPS_CreateRClassNC(s, m, o, act(g, j ^ - 1), nc);
      SetDClassOfRClass(out[k], d);
    od;
  od;

  return out;
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GreensDClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(S, x)
  local record;

  if not x in S then
    Error("Semigroups: GreensDClassOfElement: usage,\n",
          "the element does not belong to the semigroup,");
    return;
  fi;

  record := rec(LambdaOrb := LambdaOrb(S));
  SEMIGROUPS_RectifyLambda(S, LambdaOrb(S), x, record);

  if HasRhoOrb(S) and IsClosed(RhoOrb(S)) then
    record.RhoOrb := RhoOrb(S);
  else
    record.RhoOrb := GradedRhoOrb(s, record.rep, true, record);
  fi;
  
  SEMIGROUPS_RectifyRho(S, record.RhoOrb, record.rep, record);

  return SEMIGROUPS_CreateGreensClassNC(S, false, record, DClassType(S));
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GreensDClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  return SEMIGROUPS_CreateDClassNC(s, 1, GradedLambdaOrb(s, f, false)[1],
   1, GradedRhoOrb(s, f, false)[1], f, true);
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  local lambda_o, lambda_m, rho_o, i, rho_m;

  if not f in s then
    Error("Semigroups: GreensHClassOfElement: usage,\n",
          "the element does not belong to the semigroup,");
    return;
  fi;

  lambda_o := LambdaOrb(s);
  lambda_m := OrbSCCLookup(lambda_o)[Position(lambda_o, LambdaFunc(s)(f))];

  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then
    rho_o := RhoOrb(s);
    i := Position(rho_o, RhoFunc(s)(f));
  else
    rho_o := GradedRhoOrb(s, f, true);
    i := rho_o[2];
    rho_o := rho_o[1];
  fi;

  rho_m := OrbSCCLookup(rho_o)[i];

  return SEMIGROUPS_CreateHClass(s, lambda_m, lambda_o, rho_m, rho_o, f, false);
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  return SEMIGROUPS_CreateHClass(s, 1, GradedLambdaOrb(s, f, false)[1],
   1, GradedRhoOrb(s, f, false)[1], f, true);
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElement, "for D-class and element",
[IsActingSemigroupGreensClass and IsGreensDClass, IsAssociativeElement],
function(d, f)
  local h;

  if not f in d then
    Error("Semigroups: GreensHClassOfElement: usage,\n",
          "the element does not belong to the D-class,");
    return;
  fi;

  h := SEMIGROUPS_CreateHClass(Parent(d), LambdaOrbSCCIndex(d), LambdaOrb(d),
   RhoOrbSCCIndex(d), RhoOrb(d), f, IsGreensClassNC(d));
  SetDClassOfHClass(h, d);

  return h;
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElementNC, "for a D-class and element",
[IsActingSemigroupGreensClass and IsGreensDClass, IsAssociativeElement],
function(d, f)
  local h;

  h := SEMIGROUPS_CreateHClass(Parent(d), LambdaOrbSCCIndex(d), LambdaOrb(d),
   RhoOrbSCCIndex(d), RhoOrb(d), f, true);
  SetDClassOfHClass(h, d);

  return h;
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElement, "for L-class and element",
[IsActingSemigroupGreensClass and IsGreensLClass, IsAssociativeElement],
function(l, f)
  local s, nc, o, i, h;

  if not f in l then
    Error("Semigroups: GreensHClassOfElement: usage,\n",
          "the element does not belong to the L-class,");
    return;
  fi;

  s := Parent(l);
  nc := IsGreensClassNC(l);

  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then
    o := LambdaOrb(s);
    i := Position(o, LambdaFunc(s)(f));
  else
    o := GradedLambdaOrb(s, f, nc <> true);
    i := o[2];
    o := o[1];
  fi;

  h := SEMIGROUPS_CreateHClass(s, OrbSCCLookup(o)[i], o, RhoOrbSCCIndex(l), RhoOrb(l), f,
   nc);
  SetLClassOfHClass(h, l);

  return h;
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElementNC, "for an L-class and element",
[IsActingSemigroupGreensClass and IsGreensLClass, IsAssociativeElement],
function(l, f)
  local h;

  h := SEMIGROUPS_CreateHClass(Parent(l), 1, GradedLambdaOrb(Parent(l), f, false)[1],
   RhoOrbSCCIndex(l), RhoOrb(l), f, true);
  SetLClassOfHClass(h, l);

  return h;
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElement, "for R-class and element",
[IsActingSemigroupGreensClass and IsGreensRClass, IsAssociativeElement],
function(r, f)
  local s, nc, o, i, h;

  if not f in r then
    Error("Semigroups: GreensHClassOfElement: usage,\n",
          "the element does not belong to the R-class,");
    return;
  fi;

  s := Parent(r);
  nc := IsGreensClassNC(r);

  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then
    o := RhoOrb(s);
    i := Position(o, RhoFunc(s)(f));
  else
    o := GradedRhoOrb(s, f, nc <> true);
    i := o[2];
    o := o[1];
  fi;

  h := SEMIGROUPS_CreateHClass(s, LambdaOrbSCCIndex(r), LambdaOrb(r), OrbSCCLookup(o)[i],
   o, f, nc);
  SetRClassOfHClass(h, r);

  return h;
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElementNC, "for an R-class and element",
[IsActingSemigroupGreensClass and IsGreensRClass, IsAssociativeElement],
function(r, f)
  local h;

  h := SEMIGROUPS_CreateHClass(Parent(r), LambdaOrbSCCIndex(r), LambdaOrb(r),
   1, GradedRhoOrb(Parent(r), f, false)[1], f, true);
  SetRClassOfHClass(h, r);

  return h;
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GreensLClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  local o;

  if not f in s then
    Error("Semigroups: GreensLClassOfElement: usage,\n",
          "the element does not belong to the semigroup,");
    return;
  fi;

  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then
    o := RhoOrb(s);
  else
    o := GradedRhoOrb(s, f, true)[1];
  fi;
  # use non-NC so that rho value of f is rectified.
  return SEMIGROUPS_CreateLClass(s, fail, o, f, false);
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GreensLClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  # use NC since rho value of f has to be in first place of GradedRhoOrb
  # with false as final arg
  return SEMIGROUPS_CreateLClassNC(s, 1, GradedRhoOrb(s, f, false)[1], f, true);
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GreensLClassOfElement,
"for D-class of acting semigroup and element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local l;

  if not f in d then
    Error("Semigroups: GreensLClassOfElement: usage,\n",
          "the element does not belong to the D-class,");
    return;
  fi;

  # use non-NC so that rho value of f is rectified
  l := SEMIGROUPS_CreateLClass(Parent(d), RhoOrbSCCIndex(d), RhoOrb(d), f,
   IsGreensClassNC(d));

  SetDClassOfLClass(l, d);
  return l;
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GreensLClassOfElementNC, "for D-class and associative element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local l;

  # use non-NC so that rho value of f is rectified
  l := SEMIGROUPS_CreateLClass(Parent(d), RhoOrbSCCIndex(d), RhoOrb(d), f, true);
  SetDClassOfLClass(l, d);
  return l;
end);

# different method for regular/inverse, same for ideals

InstallMethod(GreensRClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)

  if not f in s then
    Error("Semigroups: GreensRClassOfElement: usage,\n",
          "the element does not belong to the semigroup,");
    return;
  fi;
  return CallFuncList(SEMIGROUPS_CreateRClassNC,
   SemigroupData(s)[Position(SemigroupData(s), f)]);
end);

# same method for regular/inverse, same for ideals

InstallMethod(GreensRClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  local pos, r;

  if HasSemigroupData(s) and IsClosedData(SemigroupData(s)) then
    pos := Position(SemigroupData(s), f);
    if pos <> fail then
      return CallFuncList(SEMIGROUPS_CreateRClassNC, SemigroupData(s)[pos]);
    fi;
  fi;

  return SEMIGROUPS_CreateRClassNC(s, 1, GradedLambdaOrb(s, f, false)[1], f, true);
end);

# same method for regular/inverse, same for ideals

InstallMethod(GreensRClassOfElement,
"for an acting semigroup D-class and associative element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local r;

  if not f in d then
    Error("Semigroups: GreensRClassOfElement: usage,\n",
          "the element does not belong to the D-class,");
    return;
  fi;

  r := SEMIGROUPS_CreateRClass(Parent(d), LambdaOrbSCCIndex(d), LambdaOrb(d), f,
   IsGreensClassNC(d));
  SetDClassOfRClass(r, d);

  return r;
end);

# same method for regular/inverse, same for ideals

InstallMethod(GreensRClassOfElementNC, "for D-class and associative element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local r;

  r := SEMIGROUPS_CreateRClass(Parent(d), LambdaOrbSCCIndex(d), LambdaOrb(d), f, true);
  SetDClassOfRClass(r, d);
  return r;
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GroupHClassOfGreensDClass, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local s, rho, o, scc, tester, h, i;

  if HasIsRegularClass(d) and not IsRegularClass(d) then
    return fail;
  fi;

  s := Parent(d);
  rho := RhoFunc(s)(Representative(d));
  o := LambdaOrb(d);
  scc := OrbSCC(o)[LambdaOrbSCCIndex(d)];
  tester := IdempotentTester(s);

  for i in scc do
    if tester(o[i], rho) then
      if not HasIsRegularClass(d) then
        SetIsRegularClass(d, true);
      fi;
      h := SEMIGROUPS_CreateHClass(s, LambdaOrbSCCIndex(d), o, RhoOrbSCCIndex(d),
       RhoOrb(d), IdempotentCreator(s)(o[i], rho), IsGreensClassNC(d));
      SetIsGroupHClass(h, true);
      return h;
    fi;
  od;

  if not HasIsRegularClass(d) then
    SetIsRegularClass(d, false);
  fi;
  return fail;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(HClassReps, "for an acting semigroup",
[IsActingSemigroup], s -> Concatenation(List(GreensRClasses(s), HClassReps)));

# different method for regular/inverse, same method for ideals

# JDM this method could be better...

InstallMethod(HClassReps, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return Concatenation(List(GreensRClasses(d), HClassReps));
end);

# different method for regular/inverse, same method for ideals

InstallMethod(HClassReps, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, scc, mults, f, cosets, out, k, act, i, j;

  o := RhoOrb(l);
  m := RhoOrbSCCIndex(l);
  scc := OrbSCC(o)[m];
  mults := RhoOrbMults(o, m);
  f := Representative(l);

  cosets := RhoCosets(l);
  #these are the rho cosets of the D-class containing l rectified so that they
  #correspond to the lambda value of f and not the lambda value of the rep of
  #the D-class.

  out := EmptyPlist(Length(scc) * Length(cosets));
  k := 0;
  act := StabilizerAction(Parent(l));

  for i in scc do
    i := mults[i][1] * f;
    for j in cosets do
      k := k + 1;
      out[k] := act(i, j);
    od;
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(HClassReps, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, scc, mults, f, cosets, out, k, act, i, j;

  o := LambdaOrb(r);
  m := LambdaOrbSCCIndex(r);
  scc := OrbSCC(o)[m];
  mults := LambdaOrbMults(o, m);
  f := Representative(r);

  cosets := LambdaCosets(DClassOfRClass(r));
  out := EmptyPlist(Length(scc) * Length(cosets));
  k := 0;

  act := StabilizerAction(Parent(r));

  for i in cosets do
    i := act(f, i);
    for j in scc do
      k := k + 1;
      out[k] := i * mults[j][1];
    od;
  od;
  return out;
end);

# Notes: that these are not rectified!

# different methods for regular/inverse/ideals

InstallMethod(DClassReps, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local data, scc, r, i, out, j;

  data := Enumerate(SemigroupData(s), infinity, ReturnFalse);
  scc := OrbSCC(data);
  r := Length(scc);
  out := EmptyPlist(r - 1);

  for i in [2 .. r] do
    out[i - 1] := data[scc[i][1]][4];
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals.

InstallMethod(LClassReps, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local D, out, x;
  D := GreensDClasses(s);
  out := [];
  for x in D do
    Append(out, LClassReps(x));
  od;
  return out;
end);

# different method for regular/inverse, same for ideals

InstallMethod(LClassReps, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local o, m, mults, scc, f, cosets, out, act, k, g, i, j;

  o := LambdaOrb(d);
  m := LambdaOrbSCCIndex(d);
  mults := LambdaOrbMults(o, m);
  scc := LambdaOrbSCC(d);
  f := Representative(d);

  cosets := LambdaCosets(d);
  out := EmptyPlist(Length(scc) * Length(cosets));
  act := StabilizerAction(Parent(d));
  k := 0;
  for i in cosets do
    g := act(f, i);
    for j in scc do
      k := k + 1;
      out[k] := g * mults[j][1];
    od;
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(RClassReps, "for an acting semigroup", [IsActingSemigroup],
function(s)
  local data, orbit, nr, r, out, i;

  data := Enumerate(SemigroupData(s));
  orbit := data!.orbit;
  nr := Length(orbit);
  out := EmptyPlist(nr - 1);

  for i in [2 .. nr] do
    out[i - 1] := orbit[i][4];
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(RClassReps, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
function(d)
  local o, m, mults, scc, f, cosets, out, k, act, g, i, j;

  o := RhoOrb(d);
  m := RhoOrbSCCIndex(d);
  mults := RhoOrbMults(o, m);
  scc := RhoOrbSCC(d);
  f := Representative(d);

  cosets := RhoCosets(d);
  out := EmptyPlist(Length(scc) * Length(cosets));

  k := 0;
  act := StabilizerAction(Parent(d));
  for i in scc do
    g := mults[i][1] * f;
    for j in cosets do
      k := k + 1;
      out[k] := act(g, j ^ - 1);
    od;
  od;
  return out;
end);

#

InstallGlobalFunction(SEMIGROUPS_Idempotents,
function(x, value, scc, o, onright)
  local s, out, j, tester, creator, i;

  if HasIsRegularClass(x) and not IsRegularClass(x) then
    return [];
  fi;

  s := Parent(x);

  if IsActingSemigroupWithFixedDegreeMultiplication(s)
   and IsMultiplicativeElementWithOneCollection(s)
   and ActionRank(s)(Representative(x)) = ActionDegree(Representative(x)) then
    return [One(s)];
  fi;

  out := EmptyPlist(Length(scc));
  j := 0;
  tester := IdempotentTester(s);
  creator := IdempotentCreator(s);

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

  if not HasIsRegularClass(x) then
    SetIsRegularClass(x, j <> 0);
  fi;

  # can't set NrIdempotents here since we sometimes input a D-class here.

  ShrinkAllocationPlist(out);
  return out;
end);

# Notes: this could be more compacted but it is not for performance reasons.

# same method for regular, different method for inverse, same for ideals

InstallMethod(Idempotents, "for an acting semigroup", [IsActingSemigroup],
function(s)
  local lambda_o, creator, r, l, out, nr, tester, rho_o, scc, gens, rhofunc,
  lookup, rep, rho, j, i, k;

  if IsRegularSemigroup(s) then

    out := [];
    nr := 0;
    tester := IdempotentTester(s);
    creator := IdempotentCreator(s);
    rho_o := RhoOrb(s);
    scc := OrbSCC(rho_o);
    lambda_o := LambdaOrb(s);
    Enumerate(lambda_o, infinity);
    gens := lambda_o!.gens;
    rhofunc := RhoFunc(s);
    lookup := OrbSCCLookup(rho_o);

    for i in [2 .. Length(lambda_o)] do
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

    if not HasNrIdempotents(s) then
      SetNrIdempotents(s, nr);
    fi;
    return out;
  fi;

  return Concatenation(List(GreensRClasses(s), Idempotents));
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(Idempotents,
"for an acting semigroup and a positive integer",
[IsActingSemigroup , IsInt],
function(s, n)
  local out, nr, tester, creator, rho_o, scc, lambda_o, gens, rhofunc, lookup,
  rank, rep, rho, j, i, k;

  if n < 0 then
    Error("Semigroups: Idempotents: usage,\n",
          "the second argument <n> must be a non-negative integer,");
    return;
  fi;

  if HasIdempotents(s) or not IsRegularSemigroup(s) then
    return Filtered(Idempotents(s), x -> ActionRank(s)(x) = n);
  else

    out := [];
    nr := 0;
    tester := IdempotentTester(s);
    creator := IdempotentCreator(s);
    rho_o := RhoOrb(s);
    scc := OrbSCC(rho_o);
    lambda_o := LambdaOrb(s);
    Enumerate(lambda_o, infinity);
    gens := lambda_o!.gens;
    rhofunc := RhoFunc(s);
    lookup := OrbSCCLookup(rho_o);
    rank := RhoRank(s);

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
  fi;
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(Idempotents, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local out, lambda_o, lambda_scc, rho_o, rho_scc, i;

  out := [];
  lambda_o := LambdaOrb(d);
  lambda_scc := LambdaOrbSCC(d);
  rho_o := RhoOrb(d);
  rho_scc := RhoOrbSCC(d);

  for i in lambda_scc do
    Append(out, SEMIGROUPS_Idempotents(d, lambda_o[i], rho_scc, rho_o, false));
  od;
  return out;
end);

# same method for regular/inverse, same method for ideals

InstallMethod(Idempotents, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local s, f;

  if not IsGroupHClass(h) then
    return [];
  fi;

  s := Parent(h);
  f := Representative(h);
  return [IdempotentCreator(s)(LambdaFunc(s)(f), RhoFunc(s)(f))];
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(Idempotents, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
l -> SEMIGROUPS_Idempotents(l, LambdaFunc(Parent(l))(Representative(l)),
RhoOrbSCC(l), RhoOrb(l), false));

# same method for regular, different method for inverse, same for ideals

InstallMethod(Idempotents, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r -> SEMIGROUPS_Idempotents(r, RhoFunc(Parent(r))(Representative(r)),
LambdaOrbSCC(r), LambdaOrb(r), true));

# same method for regular/inverse, same method for ideals

InstallMethod(IsGroupHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local s, f;
  s := Parent(h);
  f := Representative(h);
  return IdempotentTester(s)(LambdaFunc(s)(f), RhoFunc(s)(f));
end);

# same method for regular/inverse, same method for ideals

InstallMethod(IsomorphismPermGroup, "for H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)

  if not IsGroupHClass(h) then
    Error("Semigroups: IsomorphismPermGroup: usage,\n",
          "the H-class is not a group,");
    return;
  fi;

  return MappingByFunction(h, SchutzenbergerGroup(h),
   x -> LambdaPerm(Parent(h))(Representative(h), x),
   x -> StabilizerAction(Parent(h))(MultiplicativeNeutralElement(h), x));
end);

#

InstallGlobalFunction(SEMIGROUPS_IsRegularClass,
function(x, value, scc, o, onright)
  local s, data, m, tester, i;

  if HasNrIdempotents(x) then
    return NrIdempotents(x) <> 0;
  fi;

  s := Parent(x);

  if HasSemigroupDataIndex(x) then
    data := SemigroupData(s);
    m := LambdaOrbSCCIndex(x);
    if data!.repslens[m][data!.orblookup1[SemigroupDataIndex(x)]] > 1 then
      return false;
    fi;
  fi;

  # is x the group of units...
  if IsActingSemigroupWithFixedDegreeMultiplication(s) and
    ActionRank(s)(Representative(x)) = ActionDegree(Representative(x)) then
    return true;
  fi;

  tester := IdempotentTester(s);

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
end);

# not required for regular/inverse, same for ideals

InstallMethod(IsRegularClass, "for an D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
d -> SEMIGROUPS_IsRegularClass(d, RhoFunc(Parent(d))(Representative(d)),
LambdaOrbSCC(d), LambdaOrb(d), true));

# same method for regular/inverse, same method for ideals

InstallMethod(IsRegularClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass], IsGroupHClass);

# not required for regular/inverse, same for ideals

InstallMethod(IsRegularClass, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
l -> SEMIGROUPS_IsRegularClass(l, LambdaFunc(Parent(l))(Representative(l)),
RhoOrbSCC(l), RhoOrb(l), false));

# not required for regular/inverse, same for ideals

InstallMethod(IsRegularClass, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r -> SEMIGROUPS_IsRegularClass(r, RhoFunc(Parent(r))(Representative(r)),
LambdaOrbSCC(r), LambdaOrb(r), true));

# different method for regular/inverse/ideals

InstallMethod(NrDClasses, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
s -> Length(OrbSCC(SemigroupData(s))) - 1);

# different method for regular/inverse/ideals

InstallMethod(NrRegularDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local data, datascc, rhofunc, tester, nr, r, x, o, scc, rho, i, j;

  data := Enumerate(SemigroupData(s), infinity, ReturnFalse);
  datascc := OrbSCC(data);

  rhofunc := RhoFunc(s);
  tester := IdempotentTester(s);
  nr := 0;

  for i in [2 .. Length(datascc)] do
    # data of the first R-class in the D-class corresponding to x
    x := data[datascc[i][1]];
    o := x[3];
    scc := OrbSCC(o)[x[2]];
    rho := rhofunc(x[4]);

    for j in scc do
      if tester(o[j], rho) then
        nr := nr + 1;
        break;
      fi;
    od;
  od;
  return nr;
end);

# different method for regular/inverse, same for ideals

# could do better not to create the D-classes. Maybe not, we must store the
# schutz gp of the D-class somewhere and so it might as well be the D-class.

InstallMethod(NrLClasses, "for an acting semigroup",
[IsActingSemigroup], S -> Sum(List(GreensDClasses(S), NrLClasses)));

# different method for regular/inverse, same for ideals

InstallMethod(NrLClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
function(d)
  return Length(LambdaCosets(d)) * Length(LambdaOrbSCC(d));
end);

# different method for regular/inverse, same for ideals

InstallMethod(NrRClasses, "for an acting semigroup", [IsActingSemigroup],
function(s)
  local data;

  data := Enumerate(SemigroupData(s), infinity, ReturnFalse);
  return Length(data!.orbit) - 1;
end);

# different method for regular/inverse, same for ideals

InstallMethod(NrRClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
d -> Length(RhoCosets(d)) * Length(RhoOrbSCC(d)));

#

InstallGlobalFunction(SEMIGROUPS_NrIdempotents,
function(x, value, scc, o, onright)
  local s, data, m, nr, tester, i;

  if HasIsRegularClass(x) and not IsRegularClass(x) then
    return 0;
  fi;

  s := Parent(x);

  # check if we already know this...
  if HasSemigroupDataIndex(x) and not (HasIsRegularClass(x) and
   IsRegularClass(x)) then
    data := SemigroupData(s);
    m := LambdaOrbSCCIndex(x);
    if data!.repslens[m][data!.orblookup1[SemigroupDataIndex(x)]] > 1 then
      return 0;
    fi;
  fi;

  # is r the group of units...
  if IsActingSemigroupWithFixedDegreeMultiplication(s) and
   ActionRank(s)(Representative(x)) = ActionDegree(Representative(x)) then
    return 1;
  fi;

  nr := 0;
  tester := IdempotentTester(s);

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

  if not HasIsRegularClass(x) then
    SetIsRegularClass(x, nr <> 0);
  fi;

  return nr;
end);

# same method for regular/inverse, same for ideals

InstallMethod(NrIdempotents, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local nr, rho_o, rho_scc, lambda_o, lambda_scc, i;

  if HasIdempotents(d) then
    return Length(Idempotents(d));
  fi;

  rho_o := RhoOrb(d);
  rho_scc := RhoOrbSCC(d);
  lambda_o := LambdaOrb(d);
  lambda_scc := LambdaOrbSCC(d);

  nr := 0;
  for i in lambda_scc do
    nr := nr + SEMIGROUPS_NrIdempotents(d, lambda_o[i], rho_scc, rho_o, false);
  od;

  return nr;
end);

# same method for regular/inverse, same method for ideals

InstallMethod(NrIdempotents, "for a H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  if IsGroupHClass(h) then
    return 1;
  fi;
  return 0;
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(NrIdempotents, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
l -> SEMIGROUPS_NrIdempotents(l, LambdaFunc(Parent(l))(Representative(l)),
RhoOrbSCC(l), RhoOrb(l), false));

# same method for regular, different method inverse, same for ideals

InstallMethod(NrIdempotents, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r -> SEMIGROUPS_NrIdempotents(r, RhoFunc(Parent(r))(Representative(r)), LambdaOrbSCC(r),
LambdaOrb(r), true));

# different method for regular/inverse, same for ideals

InstallMethod(NrIdempotents, "for an acting semigroup", [IsActingSemigroup],
function(s)
  local data, lambda, rho, scc, lenreps, repslens, rholookup, repslookup,
   tester, nr, rhoval, m, ind, i;

  if HasIdempotents(s) then
    return Length(Idempotents(s));
  fi;

  data := Enumerate(SemigroupData(s), infinity, ReturnFalse);

  lambda := LambdaOrb(s);
  rho := RhoOrb(s);
  scc := OrbSCC(lambda);

  lenreps := data!.lenreps;
  repslens := data!.repslens;
  rholookup := data!.rholookup;
  repslookup := data!.repslookup;

  tester := IdempotentTester(s);

  nr := 0;
  for m in [2 .. Length(scc)] do
    for ind in [1 .. lenreps[m]] do
      if repslens[m][ind] = 1 then
        rhoval := rho[rholookup[repslookup[m][ind][1]]];
        #rhofunc(reps[m][ind][1]);
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

# different method for regular/inverse/ideals

InstallMethod(PartialOrderOfDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local d, n, out, data, gens, graph, lambdarhoht, datalookup, reps, repslens,
  ht, repslookup, lambdafunc, rhofunc, lambdaperm, o, orho, scc, lookup, schutz,
  mults, f, l, m, val, j, i, x, k;

  d := GreensDClasses(s);
  n := Length(d);
  out := List([1 .. n], x -> []);

  data := SemigroupData(s);
  gens := data!.gens;
  graph := data!.graph;
  lambdarhoht := data!.lambdarhoht;
  datalookup := OrbSCCLookup(data) - 1;
  reps := data!.reps;
  repslens := data!.repslens;
  ht := data!.ht;
  repslookup := data!.repslookup;

  lambdafunc := LambdaFunc(s);
  rhofunc := RhoFunc(s);
  lambdaperm := LambdaPerm(s);

  o := LambdaOrb(s);
  orho := RhoOrb(s);
  scc := OrbSCC(o);
  lookup := OrbSCCLookup(o);
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
              if SiftedPermutation(schutz[m],
                lambdaperm(reps[m][val][n], f)) = () then
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

#RRR

# different method for inverse/regular, same for ideals

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

# different method for regular/inverse, same for ideals

InstallMethod(SchutzenbergerGroup, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local o, m, lambda_schutz, lambda_stab, rho_schutz, rho_stab, schutz, p;

  o := LambdaOrb(d);
  m := LambdaOrbSCCIndex(d);
  lambda_schutz := LambdaOrbSchutzGp(o, m);
  lambda_stab := LambdaOrbStabChain(o, m);

  o := RhoOrb(d);
  m := RhoOrbSCCIndex(d);
  rho_schutz := RhoOrbSchutzGp(o, m, infinity);
  rho_stab := RhoOrbStabChain(o, m);

  if rho_stab = true then
    schutz := lambda_schutz;
    if lambda_stab = true then
      SetRhoOrbStabChain(d, true);
      #right transversal required so can use PositionCanonical
      SetRhoCosets(d, RightTransversal(schutz, schutz));
      return lambda_schutz;
    fi;
  elif rho_stab = false then
    SetRhoOrbStabChain(d, false);
    SetRhoCosets(d, RightTransversal(rho_schutz, rho_schutz));
    return rho_schutz;
  fi;

  p := LambdaConjugator(Parent(d))(RhoOrbRep(o, m), Representative(d));
  rho_schutz := rho_schutz ^ p;

  SetRhoOrbStabChain(d, StabChainImmutable(rho_schutz));

  if lambda_stab = false then
    SetRhoCosets(d, Enumerator(rho_schutz));
    return lambda_schutz;
  elif lambda_stab = true then
    schutz := rho_schutz;
  else
    schutz := Intersection(lambda_schutz, rho_schutz);
  fi;

  SetRhoCosets(d, RightTransversal(rho_schutz, schutz));
  return schutz;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(SchutzenbergerGroup, "for a H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local lambda_o, lambda_m, lambda_schutz, lambda_stab, rho_o, rho_m,
  rho_schutz, rho_stab, rep, s, lambda_p, rho_p;

  lambda_o := LambdaOrb(h);
  lambda_m := LambdaOrbSCCIndex(h);
  lambda_schutz := LambdaOrbSchutzGp(lambda_o, lambda_m);
  s := Parent(h);

 lambda_stab := LambdaOrbStabChain(lambda_o, lambda_m);

  if lambda_stab = false then
    return lambda_schutz;
  fi;

  rho_o := RhoOrb(h);
  rho_m := RhoOrbSCCIndex(h);
  rho_schutz := RhoOrbSchutzGp(rho_o, rho_m, infinity);
  rho_stab := RhoOrbStabChain(rho_o, rho_m);

  if rho_stab = false then
    return rho_schutz;
  fi;

  rep := Representative(h);

  lambda_p := LambdaOrbMult(lambda_o, lambda_m, Position(lambda_o,
   LambdaFunc(s)(rep)))[2];
   #LambdaConjugator seems to be used for two different things here!
  lambda_p := LambdaConjugator(s)(rep * lambda_p, rep);

  if rho_stab = true then
    return lambda_schutz ^ lambda_p;
  fi;

  rho_p := RhoOrbMult(rho_o, rho_m, Position(rho_o, RhoFunc(s)(rep)))[2];
  rho_p := LambdaConjugator(s)(RhoOrbRep(rho_o, rho_m), rho_p * rep);

  if lambda_stab = true then
    return rho_schutz ^ rho_p;
  fi;

  return Intersection(lambda_schutz ^ lambda_p, rho_schutz ^ rho_p);
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(SchutzenbergerGroup, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, p;

  o := RhoOrb(l);
  m := RhoOrbSCCIndex(l);

  if not IsGreensClassNC(l) then
    p := LambdaConjugator(Parent(l))(RhoOrbRep(o, m), Representative(l));
    return RhoOrbSchutzGp(o, m, infinity) ^ p;
  fi;
  return RhoOrbSchutzGp(o, m, infinity);
end);

# same method for regular/inverse, same for ideals

InstallMethod(SchutzenbergerGroup, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r -> LambdaOrbSchutzGp(LambdaOrb(r), LambdaOrbSCCIndex(r)));

# different method for inverse/regular, same for ideals

InstallMethod(Size, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local l, r;

  l := LambdaOrbSchutzGp(LambdaOrb(d), LambdaOrbSCCIndex(d));
  r := RhoOrbSchutzGp(RhoOrb(d), RhoOrbSCCIndex(d), infinity);
  return Size(r) * Size(l) * Length(LambdaOrbSCC(d)) * Length(RhoOrbSCC(d)) /
   Size(SchutzenbergerGroup(d));
end);

# same method for inverse/regular, same for ideals

InstallMethod(Size, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
h -> Size(SchutzenbergerGroup(h)));

# same method for inverse/regular, same for ideals

InstallMethod(Size, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r -> Size(SchutzenbergerGroup(r)) * Length(LambdaOrbSCC(r)));

# same method for regular, different method of inverse, same for ideals

InstallMethod(Size, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
l -> Size(SchutzenbergerGroup(l)) * Length(RhoOrbSCC(l)));

# same method for regular/inverse
#TODO refactor

InstallMethod(IteratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local iter;

  if HasGreensRClasses(s) then
    iter := IteratorList(GreensRClasses(s));
    SetIsIteratorOfRClasses(iter, true);
    return iter;
  fi;

  return IteratorByIterator(IteratorOfRClassData(s), x ->
   CallFuncList(SEMIGROUPS_CreateRClassNC, x), [IsIteratorOfRClasses]);
end);

#TODO this should be improved at some point

# different method for regular/inverse
#TODO refactor

InstallMethod(IteratorOfDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local iter;

  if IsClosedData(SemigroupData(s)) then
    iter := IteratorList(GreensDClasses(s));
    SetIsIteratorOfDClasses(iter, true);
    return iter;
  fi;

  return IteratorByIterator(
    IteratorOfRClassData(s),  # baseiter
    function(iter, x)         # convert
      local d;
      d := DClassOfRClass(CallFuncList(SEMIGROUPS_CreateRClassNC, x));
      Add(iter!.classes, d);
      return d;
    end,
    [IsIteratorOfDClasses],
    function(iter, x)         #isnew
      return x = fail or ForAll(iter!.classes, d -> not x[4] in d);
     end,
    rec(classes := []));        #iter
end);
