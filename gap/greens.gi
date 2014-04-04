#############################################################################
##
#W  greens.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Notes:
# - D-class reps must have rectified lambda and rho value

InstallMethod(EquivalenceClassOfElement, 
"for Green's R-relation and associative element", 
[IsGreensRRelation, IsAssociativeElement],
function(R, x)
  return GreensRClassOfElement(UnderlyingDomainOfBinaryRelation(R), x);
end);

InstallMethod(EquivalenceClassOfElement, 
"for Green's L-relation and associative element", 
[IsGreensLRelation, IsAssociativeElement],
function(L, x)
  return GreensLClassOfElement(UnderlyingDomainOfBinaryRelation(L), x);
end);

InstallMethod(EquivalenceClassOfElement, 
"for Green's D-relation and associative element", 
[IsGreensDRelation, IsAssociativeElement],
function(D, x)
  return GreensDClassOfElement(UnderlyingDomainOfBinaryRelation(D), x);
end);

InstallMethod(EquivalenceClassOfElement, 
"for Green's H-relation and associative element", 
[IsGreensHRelation, IsAssociativeElement],
function(H, x)
  return GreensHClassOfElement(UnderlyingDomainOfBinaryRelation(H), x);
end);

# acting...

# this won't work for ideals, but isn't currently used for anything

InstallMethod(LambdaRhoLookup, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass], 
function(d)
  local data, orb_scc, orblookup1, orblookup2, out, i;

  data:=SemigroupData(Parent(d));
  
  # scc of R-reps corresponding to d 
  orb_scc:=SemigroupDataSCC(d);

  # positions in reps containing R-reps in d 
  orblookup1:=data!.orblookup1;
  orblookup2:=data!.orblookup2;

  out:=[]; 
  for i in orb_scc do 
    if not IsBound(out[orblookup1[i]]) then 
      out[orblookup1[i]]:=[];
    fi;
    Add(out[orblookup1[i]], orblookup2[i]);
  od;

  return out;
end);

# not required for regular/inverse, same method for ideals

InstallMethod(LambdaCosets, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return RightTransversal(LambdaOrbSchutzGp(LambdaOrb(d),
   LambdaOrbSCCIndex(d)), SchutzenbergerGroup(d));
end);

# not required for regular/inverse, same method for ideals

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

InstallMethod(RhoCosets, "for a L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(L)
  local D, S, o, rep, pos, m, x, conj;
  
  D:=DClassOfLClass(L);
  if IsRegularClass(L) or Length(RhoCosets(D))=1 then 
    #maybe <L> is regular and doesn't know it!
    return [()];
  fi;
  
  S:=Parent(L);               rep:=Representative(L);
  m:=LambdaOrbSCCIndex(D);    o:=LambdaOrb(D);

  pos:=Position(o, LambdaFunc(S)(rep));
  
  if pos<>OrbSCC(o)[m][1] then
    x:=rep*LambdaOrbMult(o, m, pos)[2];
    conj:=LambdaConjugator(S)(x, rep);
    conj:=conj*LambdaPerm(S)(x, Representative(D));
    return List(RhoCosets(D), x-> x^conj);
  fi;
  return RhoCosets(D);
end);

# same method for regular/inverse/ideals

InstallMethod(LambdaOrbSCC, "for a Green's class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensClass],
x-> OrbSCC(LambdaOrb(x))[LambdaOrbSCCIndex(x)]);

# same method for regular/inverse/ideals

InstallMethod(RhoOrbSCC, "for a Green's class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensClass], 
x-> OrbSCC(RhoOrb(x))[RhoOrbSCCIndex(x)]);

# different method for regular/inverse, same for ideals

InstallMethod(RhoOrbStabChain, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass], 
function(d)
  SchutzenbergerGroup(d);
  return RhoOrbStabChain(d);
end);

# same method for regular, not required for inverse, same for ideals 

InstallMethod(RhoOrbStabChain, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local g;

  g:=SchutzenbergerGroup(l);

  if IsTrivial(g) then 
    return false;
  elif IsNaturalSymmetricGroup(g) and
   NrMovedPoints(g)=ActionRank(Parent(l))(Representative(l)) then 
    return true; 
  fi;
  return StabChainImmutable(g);
end);

# this is useful in PartialOrderOfDClasses, different method required for ideals

InstallMethod(SemigroupDataSCC, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local data;
  if not HasSemigroupDataIndex(d) then 
    return fail;
  fi;
  data:=SemigroupData(Parent(d));

  # scc of R-reps corresponding to d 
  return OrbSCC(data)[OrbSCCLookup(data)[SemigroupDataIndex(d)]];
end);

# main 

# different method for regular/inverse, same for ideals 

InstallMethod(\in, "for associative element and D-class of acting semigroup",
[IsAssociativeElement, IsGreensDClass and IsActingSemigroupGreensClass],
function(f, d)
  local rep, s, g, m, o, scc, l, schutz, cosets, x;
  
  rep:=Representative(d); 
  s:=Parent(d);
 
  # <ActionRank> method selection causes slow down here...
  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) 
    or (IsActingSemigroupWithFixedDegreeMultiplication(s) 
        and ActionDegree(f)<>ActionDegree(rep)) 
    or ActionRank(s)(f)<>ActionRank(s)(rep) then 
    return false;
  fi;

  g:=f;

  m:=LambdaOrbSCCIndex(d); o:=LambdaOrb(d); scc:=OrbSCC(o);

  l:=Position(o, LambdaFunc(s)(g));

  if l = fail or OrbSCCLookup(o)[l]<>m then 
    return false;
  fi;
  
  if l<>scc[m][1] then 
    g:=g*LambdaOrbMult(o, m, l)[2];
  fi;

  m:=RhoOrbSCCIndex(d); o:=RhoOrb(d); scc:=OrbSCC(o); 

  l:=Position(o, RhoFunc(s)(g));

  if l = fail or OrbSCCLookup(o)[l]<>m then 
    return false;
  fi;

  schutz:=RhoOrbStabChain(d);

  if schutz=true then 
    return true;
  fi;

  if l<>scc[m][1] then 
    g:=RhoOrbMult(o, m, l)[2]*g;
  fi;

  cosets:=LambdaCosets(d);
  g:=LambdaPerm(s)(rep, g);

  if schutz<>false then 
    for x in cosets do 
      #if SiftGroupElement(schutz, g/x).isone then 
      if SiftedPermutation(schutz, g/x)=() then 
        return true;
      fi;
    od;
  else #JDM is search really necessary? 
    for x in cosets do 
      if g/x=() then 
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

  s:=Parent(h);
  rep:=Representative(h);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) 
    or (IsActingSemigroupWithFixedDegreeMultiplication(s) and
        ActionDegree(f)<>ActionDegree(rep))
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

  rep:=Representative(l); 
  s:=Parent(l);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) 
    or (IsActingSemigroupWithFixedDegreeMultiplication(s) 
        and ActionDegree(f) <> ActionDegree(rep)) 
    or ActionRank(s)(f) <> ActionRank(s)(rep)  
    or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) then
    return false;
  fi;

  m:=RhoOrbSCCIndex(l);
  o:=RhoOrb(l);
  
  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;

  i:=Position(o, RhoFunc(s)(f));

  if i = fail or OrbSCCLookup(o)[i]<>m then 
    return false;
  fi;

  schutz:=RhoOrbStabChain(l);

  if schutz=true then
    Info(InfoSemigroups, 3, "Schutz. group of L-class is symmetric group");
    return true;
  fi;

  if i<>OrbSCC(o)[m][1] then 
    g:=RhoOrbMult(o, m, i)[2]*f;
  else 
    g:=f;
  fi;

  if g=rep then
    Info(InfoSemigroups, 3, "element with rectified rho value equals ",
    "L-class representative");
    return true;
  elif schutz=false then
    Info(InfoSemigroups, 3, "Schutz. group of L-class is trivial");
    return false;
  fi;
  
  return SiftedPermutation(schutz,  LambdaPerm(s)(rep, g))=();
end);

# Algorithm E. 
# same method for regular/inverse/ideals 

InstallMethod(\in, "for associative element and R-class of acting semigroup",
[IsAssociativeElement, IsGreensRClass and IsActingSemigroupGreensClass],
function(f, r)
  local rep, s, m, o, l, schutz, g;

  rep:=Representative(r); 
  s:=Parent(r);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) 
    or (IsActingSemigroupWithFixedDegreeMultiplication(s) 
        and ActionDegree(f) <> ActionDegree(rep)) 
    or ActionRank(s)(f) <> ActionRank(s)(rep)  
    or RhoFunc(s)(f) <> RhoFunc(s)(rep) then
    return false;
  fi;

  m:=LambdaOrbSCCIndex(r);
  o:=LambdaOrb(r);
  
  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;

  l:=Position(o, LambdaFunc(s)(f));

  if l = fail or OrbSCCLookup(o)[l]<>m then 
    return false;
  fi;

  schutz:=LambdaOrbStabChain(o, m);

  if schutz=true then
    Info(InfoSemigroups, 3, "Schutz. group of R-class is symmetric group");
    return true;
  fi;

  g:=f;
  
  if l<>OrbSCC(o)[m][1] then 
    g:=f*LambdaOrbMult(o, m, l)[2];
  fi;

  if g=rep then
    Info(InfoSemigroups, 3, "element with rectified lambda value equals ",
    "R-class representative");
    return true;
  elif schutz=false then
    Info(InfoSemigroups, 3, "Schutz. group of R-class is trivial");
    return false;
  fi;

  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g))=();
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
# only for L-classes not created during GreensLClasses! Those created via
# GreensLClasses should already know DClassOfLClass

InstallMethod(DClassOfLClass, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local s, f, nc, o, i, m;

  s:=Parent(l); 
  f:=Representative(l);
  nc:=IsGreensClassNC(l);

  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) and not nc then 
    o:=LambdaOrb(s);
    i:=Position(o, LambdaFunc(s)(f));
  else
    o:=GradedLambdaOrb(s, f, nc<>true);
    i:=o[2]; o:=o[1];
  fi;

  if nc then 
    m:=1;
  else
    m:=OrbSCCLookup(o)[i];
    if i<>OrbSCC(o)[m][1] then 
      f:=f*LambdaOrbMult(o, m, i)[2];
    fi;
  fi;
  #JDM is this a good idea? or should I give a separate method 
  if IsInverseOpClass(l) then 
    return CreateDClassNC(s, m, o, fail, fail, f, nc);
  fi;
  return CreateDClassNC(s, m, o, RhoOrbSCCIndex(l), RhoOrb(l), f, nc);
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(DClassOfRClass, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, f, nc, o, i, m;

  s:=Parent(r); 
  f:=Representative(r);
  nc:=IsGreensClassNC(r);

  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) and not nc then 
    o:=RhoOrb(s);
    i:=Position(o, RhoFunc(s)(f));
  else
    o:=GradedRhoOrb(s, f, nc<>true);
    i:=o[2]; o:=o[1];
  fi;

  if nc then 
    m:=1;
  else
    m:=OrbSCCLookup(o)[i];
    if i<>OrbSCC(o)[m][1] then 
      f:=RhoOrbMult(o, m, i)[2]*f;
    fi;
  fi;

  return CreateDClassNC(s, LambdaOrbSCCIndex(r), LambdaOrb(r), m, o, f, nc);
end);

# same method for regular, different method for inverse semigroups,
# same for ideals 

InstallMethod(DClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local s, lambda_o, lambda_m, rho_o, rho_m, rectify;
  
  s:=Parent(h);
  lambda_o:=LambdaOrb(h);
  lambda_m:=LambdaOrbSCCIndex(h);
  rho_o:=RhoOrb(h);
  rho_m:=RhoOrbSCCIndex(h);

  rectify:=RectifyRho(s, rho_o, Representative(h), fail, rho_m);
  rectify:=RectifyLambda(s, lambda_o, rectify.rep, fail, lambda_m);

  return CreateDClassNC(s, lambda_m, lambda_o, rho_m, rho_o, rectify.rep, IsGreensClassNC(h));
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(LClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
# use non-NC so that rho value of f is rectified
h-> CreateLClass(Parent(h), RhoOrbSCCIndex(h), RhoOrb(h),
 Representative(h), IsGreensClassNC(h)));

# same method for regular/inverse semigroups, same for ideals 

InstallMethod(RClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
h-> CreateRClass(Parent(h), LambdaOrbSCCIndex(h), LambdaOrb(h),
 Representative(h), IsGreensClassNC(h)));

# Notes: this could be written in a more compact way but it is not for
# performance reasons. 

# different method for regular/inverse/ideals

InstallMethod(GreensDClasses, "for an acting semigroup",
[IsActingSemigroup], 
function(s)
  local data, scc, out, type, drel, o, arg, d, rectify, i;

  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  
  scc:=OrbSCC(data);
  out:=EmptyPlist(Length(scc));
  type:=DClassType(s);
  drel:=GreensDRelation(s);
  o:=RhoOrb(s);

  for i in [2..Length(scc)] do 
    arg:=data[scc[i][1]];
    d:=Objectify(type, rec());     
    SetParent(d, s);
    SetLambdaOrbSCCIndex(d, arg[2]);
    SetLambdaOrb(d, arg[3]);
    SetSemigroupDataIndex(d, arg[6]);

    #JDM expand!
    rectify:=RectifyRho(arg[1], o, arg[4]);
    
    SetRepresentative(d, rectify.rep);
    SetRhoOrb(d, o);
    SetRhoOrbSCCIndex(d, rectify.m);
    SetEquivalenceClassRelation(d, drel); 
    SetIsGreensClassNC(d, false);

    out[i-1]:=d;
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(GreensHClasses, "for an acting semigroup",
[IsActingSemigroup], 
function(s)
  return Concatenation(List(GreensDClasses(s), GreensHClasses));
end);

# JDM could this be better/more efficient?!

# different method for regular/inverse, same method for ideals 

InstallMethod(GreensHClasses, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return Concatenation(List(GreensRClasses(d), GreensHClasses));
end);

# different method for regular/inverse, same method for ideals

InstallMethod(GreensHClasses, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local rho_o, rho_m, s, scc, mults, d, lambda_o, lambda_m, cosets, f, nc, act, out, k, i, j;

  rho_o:=RhoOrb(l); 
  rho_m:=RhoOrbSCCIndex(l);
  s:=Parent(l);

  scc:=OrbSCC(rho_o)[rho_m];
  mults:=RhoOrbMults(rho_o, rho_m);
  
  d:=DClassOfLClass(l);
  lambda_o:=LambdaOrb(d); 
  lambda_m:=LambdaOrbSCCIndex(d);

  cosets:=RhoCosets(l);
  f:=Representative(l);
  nc:=IsGreensClassNC(l);
  act:=StabilizerAction(s);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;

  for i in scc do 
    i:=mults[i][1]*f;
    for j in cosets do 
      k:=k+1;
      out[k]:=CreateHClass(s, lambda_m, lambda_o, rho_m, rho_o, act(i, j), nc);
      SetLClassOfHClass(out[k], l);
      SetDClassOfHClass(out[k], d);
    od;
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(GreensHClasses, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local lambda_o, lambda_m, s, scc, mults, d, rho_o, rho_m, cosets, f, nc, act, out, k, i, j;

  lambda_o:=LambdaOrb(r); 
  lambda_m:=LambdaOrbSCCIndex(r);
  s:=Parent(r);

  scc:=OrbSCC(lambda_o)[lambda_m];
  mults:=LambdaOrbMults(lambda_o, lambda_m);
  
  d:=DClassOfRClass(r);
  rho_o:=RhoOrb(d); 
  rho_m:=RhoOrbSCCIndex(d);

  cosets:=LambdaCosets(d);
  f:=Representative(r);
  nc:=IsGreensClassNC(r);
  act:=StabilizerAction(Parent(r));

  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;

  for i in cosets do 
    i:=act(f, i);
    for j in scc do 
      k:=k+1;
      # JDM maybe a bad idea to use CreateHClass here, perhaps expand?
      out[k]:=CreateHClass(s, lambda_m, lambda_o, rho_m, rho_o, 
       i*mults[j][1], nc);
      SetRClassOfHClass(out[k], r);
      SetDClassOfHClass(out[k], d);
      #JDM also set schutz gp here!
    od;
  od;
  return out;
end);

# different method for regular/inverse, same for ideals

InstallMethod(GreensLClasses, "for an acting semigroup",
[IsActingSemigroup], 
function(s)
  local D, out, d;

  D:=GreensDClasses(s);
  out:=EmptyPlist(NrLClasses(s));

  for d in D do 
    Append(out, GreensLClasses(d));
  od;
  return out;
end);

# different method for regular/inverse, same for ideals

InstallMethod(GreensLClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass], 
function(d)
  local mults, scc, cosets, f, s, o, m, nc, act, out, k, g, j, i;
 
  mults:=LambdaOrbMults(LambdaOrb(d), LambdaOrbSCCIndex(d));
  scc:=LambdaOrbSCC(d);
  cosets:=LambdaCosets(d);
  f:=Representative(d);
 
  s:=Parent(d);
  o:=RhoOrb(d);
  m:=RhoOrbSCCIndex(d);
  nc:=IsGreensClassNC(d);
  act:=StabilizerAction(s);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;
  for j in cosets do
    g:=act(f,j);
    for i in scc do
      k:=k+1;
      #use NC since f has rho value in first place of scc
      #JDM maybe don't use CreateLClassNC here, and rather expand!
      out[k]:=CreateLClassNC(s, m, o, g*mults[i][1], nc);
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

  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  orbit:=data!.orbit;
  out:=EmptyPlist(Length(orbit));

  for i in [2..Length(orbit)] do
    out[i-1]:=CallFuncList(CreateRClassNC, orbit[i]);
  od;
  return out;
end);

# different method for regular/inverse, same for ideals

InstallMethod(GreensRClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass], 
function(d)
  local mults, scc, cosets, f, s, o, m, nc, act, out, k, g, i, j;
 
  mults:=RhoOrbMults(RhoOrb(d), RhoOrbSCCIndex(d));
  scc:=RhoOrbSCC(d);
  cosets:=RhoCosets(d);
  f:=Representative(d);
 
  s:=Parent(d);
  o:=LambdaOrb(d);
  m:=LambdaOrbSCCIndex(d);
  nc:=IsGreensClassNC(d); 
  act:=StabilizerAction(s);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;
  for i in scc do
    g:=mults[i][1]*f;
    for j in cosets do
      k:=k+1;
      out[k]:=CreateRClassNC(s, m, o, act(g, j^-1), nc);
      SetDClassOfRClass(out[k], d);
    od;
  od;

  return out;
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GreensDClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  local lambda_o, rectify, lambda_m, rep, rho_o, i;

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;

  lambda_o:=LambdaOrb(s);
  rectify:=RectifyLambda(s, lambda_o, f);
  lambda_m:=rectify.m;
  rep:=rectify.rep;   
  
  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then
    rho_o:=RhoOrb(s);
    i:=fail;
  else
    rho_o:=GradedRhoOrb(s, f, true);
    i:=rho_o[2];#Position(o, RhoFunc(s)(f));
    rho_o:=rho_o[1];
  fi;

  rectify:=RectifyRho(s, rho_o, rep, i);

  return CreateDClassNC(s, lambda_m, lambda_o, rectify.m, rho_o, rectify.rep,
   false);
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GreensDClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  return CreateDClassNC(s, 1, GradedLambdaOrb(s, f, false)[1], 
   1, GradedRhoOrb(s, f, false)[1], f, true);
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  local lambda_o, lambda_m, rho_o, i, rho_m;

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;
  
  lambda_o:=LambdaOrb(s);
  lambda_m:=OrbSCCLookup(lambda_o)[Position(lambda_o, LambdaFunc(s)(f))];

  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then
    rho_o:=RhoOrb(s);
    i:=Position(rho_o, RhoFunc(s)(f));
  else
    rho_o:=GradedRhoOrb(s, f, true);
    i:=rho_o[2]; rho_o:=rho_o[1];
  fi;

  rho_m:=OrbSCCLookup(rho_o)[i];     

  return CreateHClass(s, lambda_m, lambda_o, rho_m, rho_o, f, false);
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  return CreateHClass(s, 1, GradedLambdaOrb(s, f, false)[1], 
   1, GradedRhoOrb(s, f, false)[1], f, true);
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElement, "for D-class and element",
[IsActingSemigroupGreensClass and IsGreensDClass, IsAssociativeElement],
function(d, f)
  local h;

  if not f in d then
    Error("the element does not belong to the Green's class,");
    return;
  fi;

  h:=CreateHClass(Parent(d), LambdaOrbSCCIndex(d), LambdaOrb(d),
   RhoOrbSCCIndex(d), RhoOrb(d), f, IsGreensClassNC(d));
  SetDClassOfHClass(h, d);
  
  return h;
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElementNC, "for a D-class and element",
[IsActingSemigroupGreensClass and IsGreensDClass, IsAssociativeElement],
function(d, f)
  local h;
 
  h:=CreateHClass(Parent(d), LambdaOrbSCCIndex(d), LambdaOrb(d),
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
    Error("the element does not belong to the Green's class,");
    return;
  fi;

  s:=Parent(l);
  nc:=IsGreensClassNC(l);

  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s);
    i:=Position(o, LambdaFunc(s)(f));
  else
    o:=GradedLambdaOrb(s, f, nc<>true);
    i:=o[2]; o:=o[1];
  fi;

  h:=CreateHClass(s, OrbSCCLookup(o)[i], o, RhoOrbSCCIndex(l), RhoOrb(l), f,
   nc);
  SetLClassOfHClass(h, l);

  return h;
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElementNC, "for an L-class and element",
[IsActingSemigroupGreensClass and IsGreensLClass, IsAssociativeElement],
function(l, f)
  local h;
 
  h:=CreateHClass(Parent(l), 1, GradedLambdaOrb(Parent(l), f, false)[1], 
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
    Error("the element does not belong to the Green's class,");
    return;
  fi;

  s:=Parent(r);
  nc:=IsGreensClassNC(r);

  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then 
    o:=RhoOrb(s);
    i:=Position(o, RhoFunc(s)(f));
  else
    o:=GradedRhoOrb(s, f, nc<>true);
    i:=o[2]; o:=o[1];
  fi;

  h:=CreateHClass(s, LambdaOrbSCCIndex(r), LambdaOrb(r), OrbSCCLookup(o)[i], 
   o, f, nc);
  SetRClassOfHClass(h, r);

  return h;
end);

# same method for regular, different method for inverse, same method for ideals

InstallMethod(GreensHClassOfElementNC, "for an R-class and element",
[IsActingSemigroupGreensClass and IsGreensRClass, IsAssociativeElement],
function(r, f)
  local h;
  
  h:=CreateHClass(Parent(r), LambdaOrbSCCIndex(r), LambdaOrb(r), 
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
    Error("the element does not belong to the semigroup,");
    return;
  fi;
  
  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then 
    o:=RhoOrb(s);
  else
    o:=GradedRhoOrb(s, f, true)[1];
  fi;
  # use non-NC so that rho value of f is rectified. 
  return CreateLClass(s, fail, o, f, false);
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GreensLClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  # use NC since rho value of f has to be in first place of GradedRhoOrb
  # with false as final arg
  return CreateLClassNC(s, 1, GradedRhoOrb(s, f, false)[1], f, true);
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(GreensLClassOfElement, "for D-class of acting semigroup and element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local l;
    
  if not f in d then
    Error("the element does not belong to the D-class,");
    return;
  fi;
 
  # use non-NC so that rho value of f is rectified
  l:=CreateLClass(Parent(d), RhoOrbSCCIndex(d), RhoOrb(d), f,
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
  l:=CreateLClass(Parent(d), RhoOrbSCCIndex(d), RhoOrb(d), f, true);
  SetDClassOfLClass(l, d);
  return l;
end);

# different method for regular/inverse, same for ideals

InstallMethod(GreensRClassOfElement, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;
  return CallFuncList(CreateRClassNC, 
   SemigroupData(s)[Position(SemigroupData(s), f)]);
end);

# same method for regular/inverse, same for ideals

InstallMethod(GreensRClassOfElementNC, "for an acting semigroup and element",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  local pos, r;
 
  if HasSemigroupData(s) and IsClosedData(SemigroupData(s)) then 
    pos:=Position(SemigroupData(s), f);
    if pos<>fail then
      return CallFuncList(CreateRClassNC, SemigroupData(s)[pos]);
    fi;  
  fi;
  
  return CreateRClassNC(s, 1, GradedLambdaOrb(s, f, false)[1], f, true);
end);

# same method for regular/inverse, same for ideals

InstallMethod(GreensRClassOfElement, 
"for an acting semigroup D-class and associative element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local r;
    
  if not f in d then
    Error("the element does not belong to the D-class,");
    return;
  fi;
 
  r:=CreateRClass(Parent(d), LambdaOrbSCCIndex(d), LambdaOrb(d), f,
   IsGreensClassNC(d));
  SetDClassOfRClass(r, d);

  return r;
end);

# same method for regular/inverse, same for ideals

InstallMethod(GreensRClassOfElementNC, "for D-class and associative element",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local r;

  r:=CreateRClass(Parent(d), LambdaOrbSCCIndex(d), LambdaOrb(d), f, true);
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
  
  s:=Parent(d);
  rho:=RhoFunc(s)(Representative(d));
  o:=LambdaOrb(d);
  scc:=OrbSCC(o)[LambdaOrbSCCIndex(d)];
  tester:=IdempotentTester(s);

  for i in scc do 
    if tester(o[i], rho) then 
      if not HasIsRegularClass(d) then 
        SetIsRegularClass(d, true);
      fi;
      h:=CreateHClass(s, LambdaOrbSCCIndex(d), o, RhoOrbSCCIndex(d),
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
[IsActingSemigroup], s-> Concatenation(List(GreensRClasses(s), HClassReps)));

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

  o:=RhoOrb(l); 
  m:=RhoOrbSCCIndex(l);
  scc:=OrbSCC(o)[m];
  mults:=RhoOrbMults(o, m);
  f:=Representative(l); 
 
  cosets:=RhoCosets(l); 
  #these are the rho cosets of the D-class containing l rectified so that they
  #correspond to the lambda value of f and not the lambda value of the rep of
  #the D-class. 

  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;
  act:=StabilizerAction(Parent(l));

  for i in scc do 
    i:=mults[i][1]*f;
    for j in cosets do 
      k:=k+1;
      out[k]:=act(i, j);
    od;
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(HClassReps, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, scc, mults, f, cosets, out, k, act, i, j;

  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
  scc:=OrbSCC(o)[m];
  mults:=LambdaOrbMults(o, m);
  f:=Representative(r);
 
  cosets:=LambdaCosets(DClassOfRClass(r));
  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;

  act:=StabilizerAction(Parent(r));

  for i in cosets do 
    i:=act(f, i);
    for j in scc do 
      k:=k+1;
      out[k]:=i*mults[j][1];
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

  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  scc:=OrbSCC(data); 
  r:=Length(scc);
  out:=EmptyPlist(r-1);

  for i in [2..r] do 
    out[i-1]:=data[scc[i][1]][4];
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals.

InstallMethod(LClassReps, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local D, out, x;
  D:=GreensDClasses(s);
  out:=[];
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
  
  o:=LambdaOrb(d); 
  m:=LambdaOrbSCCIndex(d);
  mults:=LambdaOrbMults(o, m);
  scc:=LambdaOrbSCC(d);
  f:=Representative(d);
 
  cosets:=LambdaCosets(d);
  out:=EmptyPlist(Length(scc)*Length(cosets));
  act:=StabilizerAction(Parent(d));
  k:=0;
  for i in cosets do
    g:=act(f, i);
    for j in scc do
      k:=k+1;
      out[k]:=g*mults[j][1];
    od;
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals 
 
InstallMethod(RClassReps, "for an acting semigroup", [IsActingSemigroup],
function(s)
  local data, orbit, nr, r, out, i;

  data:=Enumerate(SemigroupData(s)); 
  orbit:=data!.orbit;
  nr:=Length(orbit);
  out:=EmptyPlist(nr-1);

  for i in [2..nr] do 
    out[i-1]:=orbit[i][4];
  od;
  return out;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(RClassReps, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
function(d)
  local o, m, mults, scc, f, cosets, out, k, act, g, i, j;

  o:=RhoOrb(d); 
  m:=RhoOrbSCCIndex(d);
  mults:=RhoOrbMults(o, m);
  scc:=RhoOrbSCC(d);
  f:=Representative(d);

  cosets:=RhoCosets(d);
  out:=EmptyPlist(Length(scc)*Length(cosets));
  
  k:=0;
  act:=StabilizerAction(Parent(d));
  for i in scc do
    g:=mults[i][1]*f;
    for j in cosets do
      k:=k+1;
      out[k]:=act(g, j^-1);
    od;
  od;
  return out;
end);

#

InstallGlobalFunction(Idempotents@, 
function(x, value, scc, o, onright)
  local s, out, j, tester, creator, i;

  if HasIsRegularClass(x) and not IsRegularClass(x) then 
    return [];
  fi;
  
  s:=Parent(x);

  if IsActingSemigroupWithFixedDegreeMultiplication(s) 
   and IsMultiplicativeElementWithOneCollection(s) 
   and ActionRank(s)(Representative(x))=ActionDegree(Representative(x)) then
    return [One(s)];
  fi;

  out:=EmptyPlist(Length(scc)); 
  j:=0;
  tester:=IdempotentTester(s);
  creator:=IdempotentCreator(s);

  if onright then 
    for i in scc do
      if tester(o[i], value) then
        j:=j+1;
        out[j]:=creator(o[i], value);
      fi;
    od;
  else
    for i in scc do
      if tester(value, o[i]) then
        j:=j+1;
        out[j]:=creator(value, o[i]);
      fi;
    od;
  fi;

  if not HasIsRegularClass(x) then 
    SetIsRegularClass(x, j<>0);
  fi;
 
  # can't set NrIdempotents here since we sometimes input a D-class here. 

  ShrinkAllocationPlist(out);
  return out;
end);

# Notes: this could be more compacted but it is not for performance reasons.

# same method for regular, different method for inverse, same for ideals

InstallMethod(Idempotents, "for an acting semigroup", [IsActingSemigroup],
function(s)
  local lambda_o, creator, r, l, out, nr, tester, rho_o, scc, gens, rhofunc, lookup, rep, rho, j, i, k;

  if IsRegularSemigroup(s) then 

    out:=[];
    nr:=0;
    tester:=IdempotentTester(s);
    creator:=IdempotentCreator(s);
    rho_o:=RhoOrb(s);
    scc:=OrbSCC(rho_o);
    lambda_o:=LambdaOrb(s);
    Enumerate(lambda_o, infinity);
    gens:=lambda_o!.gens;
    rhofunc:=RhoFunc(s);
    lookup:=OrbSCCLookup(rho_o);

    for i in [2..Length(lambda_o)] do
      rep:=EvaluateWord(lambda_o, TraceSchreierTreeForward(lambda_o, i));
      rho:=rhofunc(rep);
      j:=lookup[Position(rho_o, rho)];

      for k in scc[j] do
        if tester(lambda_o[i], rho_o[k]) then
          nr:=nr+1;
          out[nr]:=creator(lambda_o[i], rho_o[k]);
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
  local out, nr, tester, creator, rho_o, scc, lambda_o, gens, rhofunc, lookup, rank, rep, rho, j, i, k;

  if n<0 then 
    Error("usage: <n> must be a non-negative integer,");
    return;
  fi;

  if HasIdempotents(s) or not IsRegularSemigroup(s) then
    return Filtered(Idempotents(s), x-> ActionRank(s)(x)=n);
  else

    out:=[];
    nr:=0;
    tester:=IdempotentTester(s);
    creator:=IdempotentCreator(s);
    rho_o:=RhoOrb(s);
    scc:=OrbSCC(rho_o);
    lambda_o:=LambdaOrb(s);
    Enumerate(lambda_o, infinity);
    gens:=lambda_o!.gens;
    rhofunc:=RhoFunc(s);
    lookup:=OrbSCCLookup(rho_o);
    rank:=RhoRank(s);

    for i in [2..Length(lambda_o)] do
      rep:=EvaluateWord(lambda_o, TraceSchreierTreeForward(lambda_o, i));
      rho:=rhofunc(rep);
      j:=lookup[Position(rho_o, rho)];
      if rank(rho_o[scc[j][1]])=n then  
        for k in scc[j] do
          if tester(lambda_o[i], rho_o[k]) then
            nr:=nr+1;
            out[nr]:=creator(lambda_o[i], rho_o[k]);
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

  out:=[];
  lambda_o:=LambdaOrb(d); 
  lambda_scc:=LambdaOrbSCC(d);
  rho_o:=RhoOrb(d); 
  rho_scc:=RhoOrbSCC(d);
  
  for i in lambda_scc do
    Append(out, Idempotents@(d, lambda_o[i], rho_scc, rho_o, false));
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

  s:=Parent(h);
  f:=Representative(h);
  return [IdempotentCreator(s)(LambdaFunc(s)(f), RhoFunc(s)(f))];
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(Idempotents, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
l-> Idempotents@(l, LambdaFunc(Parent(l))(Representative(l)),
RhoOrbSCC(l), RhoOrb(l), false));

# same method for regular, different method for inverse, same for ideals

InstallMethod(Idempotents, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> Idempotents@(r, RhoFunc(Parent(r))(Representative(r)),
LambdaOrbSCC(r), LambdaOrb(r), true));

# same method for regular/inverse, same method for ideals

InstallMethod(IsGroupHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local s, f;
  s:=Parent(h);
  f:=Representative(h);
  return IdempotentTester(s)(LambdaFunc(s)(f), RhoFunc(s)(f));
end);

# same method for regular/inverse, same method for ideals

InstallMethod(IsomorphismPermGroup, "for H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)

  if not IsGroupHClass(h) then
    Error("the H-class is not a group,");
    return;
  fi;

  return MappingByFunction(h, SchutzenbergerGroup(h),
   x-> LambdaPerm(Parent(h))(Representative(h), x),
   x-> StabilizerAction(Parent(h))(MultiplicativeNeutralElement(h), x));
end);

#

InstallGlobalFunction(IsRegularClass@, 
function(x, value, scc, o, onright)
  local s, data, m, tester, i;

  if HasNrIdempotents(x) then 
    return NrIdempotents(x)<>0;
  fi;

  s:=Parent(x);
  
  if HasSemigroupDataIndex(x) then
    data:=SemigroupData(s);
    m:=LambdaOrbSCCIndex(x);
    if data!.repslens[m][data!.orblookup1[SemigroupDataIndex(x)]]>1 then
      return false;
    fi;
  fi; 
  
  # is x the group of units...
  if IsActingSemigroupWithFixedDegreeMultiplication(s) and 
    ActionRank(s)(Representative(x))=ActionDegree(Representative(x)) then
    return true;
  fi;   
 
  tester:=IdempotentTester(s);
  
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
d-> IsRegularClass@(d, RhoFunc(Parent(d))(Representative(d)),
LambdaOrbSCC(d), LambdaOrb(d), true));

# same method for regular/inverse, same method for ideals

InstallMethod(IsRegularClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass], IsGroupHClass);

# not required for regular/inverse, same for ideals

InstallMethod(IsRegularClass, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
l-> IsRegularClass@(l, LambdaFunc(Parent(l))(Representative(l)),
RhoOrbSCC(l), RhoOrb(l), false));

# not required for regular/inverse, same for ideals

InstallMethod(IsRegularClass, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> IsRegularClass@(r, RhoFunc(Parent(r))(Representative(r)),
LambdaOrbSCC(r), LambdaOrb(r), true));

#OOO

# same method for regular/inverse, same method for ideals

InstallMethod(MultiplicativeNeutralElement, 
"for a H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass], 
function(h)

  if not IsGroupHClass(h) then
    return fail;        
  fi;
  return Idempotents(h)[1];
end);

# different method for regular/inverse/ideals 

InstallMethod(NrDClasses, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
s-> Length(OrbSCC(SemigroupData(s)))-1);

InstallMethod(NrDClasses, "for a semigroup",
[IsSemigroup], S-> Length(GreensDClasses(S)));

# different method for regular/inverse/ideals

InstallMethod(NrRegularDClasses, "for an acting semigroup with generators",
[IsActingSemigroup],
function(s)
  local data, datascc, rhofunc, tester, nr, r, x, o, scc, rho, i, j;
  
  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  datascc:=OrbSCC(data);
  
  rhofunc:=RhoFunc(s);
  tester:=IdempotentTester(s);
  nr:=0;

  for i in [2..Length(datascc)] do
    # data of the first R-class in the D-class corresponding to x
    x:=data[datascc[i][1]];
    o:=x[3]; scc:=OrbSCC(o)[x[2]]; 
    rho:=rhofunc(x[4]);

    for j in scc do  
      if tester(o[j], rho) then
        nr:=nr+1;
        break;
      fi;
    od;
  od;
  return nr;
end);

# different method for regular/inverse, same method for ideals

InstallMethod(NrHClasses, "for an acting semigroup", [IsActingSemigroup],
function(s)
  return Sum(List(GreensDClasses(s), NrHClasses));
end);

InstallMethod(NrHClasses, "for a semigroup",
[IsSemigroup], S-> Length(GreensHClasses(S)));

# different method for regular/inverse, same for ideals

InstallMethod(NrHClasses, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return NrRClasses(d)*NrLClasses(d);
end);

# different method for regular/inverse, same for ideals

InstallMethod(NrHClasses, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
l-> NrRClasses(DClassOfLClass(l)));

# different method for regular/inverse, same for ideals

InstallMethod(NrHClasses, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> NrLClasses(DClassOfRClass(r)));

# different method for regular/inverse, same for ideals

# could do better not to create the D-classes. Maybe not, we must store the
# schutz gp of the D-class somewhere and so it might as well be the D-class.

InstallMethod(NrLClasses, "for an acting semigroup",
[IsActingSemigroup], s-> Sum(List(GreensDClasses(s), NrLClasses)));

InstallMethod(NrLClasses, "for a semigroup",
[IsSemigroup], S-> Length(GreensLClasses(S)));

# different method for regular/inverse, same for ideals

InstallMethod(NrLClasses, "for a D-class of an acting semigroup",       
[IsActingSemigroupGreensClass and IsGreensDClass],
function(d)
  return Length(LambdaCosets(d))*Length(LambdaOrbSCC(d));
end);

# different method for regular/inverse, same for ideals

InstallMethod(NrRClasses, "for an acting semigroup", [IsActingSemigroup],
function(s)
  local data;
  
  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  return Length(data!.orbit)-1;
end);

InstallMethod(NrRClasses, "for a semigroup",
[IsSemigroup], S-> Length(GreensRClasses(S)));

# different method for regular/inverse, same for ideals

InstallMethod(NrRClasses, "for a D-class of an acting semigroup",       
[IsActingSemigroupGreensClass and IsGreensDClass],
d-> Length(RhoCosets(d))*Length(RhoOrbSCC(d)));

#

InstallGlobalFunction(NrIdempotents@, 
function(x, value, scc, o, onright)
  local s, data, m, nr, tester, i;

  if HasIsRegularClass(x) and not IsRegularClass(x) then 
    return 0;
  fi;

  s:=Parent(x);     

  # check if we already know this...
  if HasSemigroupDataIndex(x) and not (HasIsRegularClass(x) and
   IsRegularClass(x)) then
    data:=SemigroupData(s);
    m:=LambdaOrbSCCIndex(x);
    if data!.repslens[m][data!.orblookup1[SemigroupDataIndex(x)]]>1 then
      return 0;
    fi;
  fi;

  # is r the group of units...
  if IsActingSemigroupWithFixedDegreeMultiplication(s) and
   ActionRank(s)(Representative(x))=ActionDegree(Representative(x)) then
    return 1;
  fi;

  nr:=0;
  tester:=IdempotentTester(s);

  if onright then 
    for i in scc do
      if tester(o[i], value) then
        nr:=nr+1;
      fi;
    od;
  else
    for i in scc do
      if tester(value, o[i]) then
        nr:=nr+1;
      fi;
    od;
  fi;

  if not HasIsRegularClass(x) then 
    SetIsRegularClass(x, nr<>0);
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

  rho_o:=RhoOrb(d);
  rho_scc:=RhoOrbSCC(d);
  lambda_o:=LambdaOrb(d);
  lambda_scc:=LambdaOrbSCC(d);
  
  nr:=0;
  for i in lambda_scc do
    nr:=nr+NrIdempotents@(d, lambda_o[i], rho_scc, rho_o, false);
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
l-> NrIdempotents@(l, LambdaFunc(Parent(l))(Representative(l)),
RhoOrbSCC(l), RhoOrb(l), false));

# same method for regular, different method inverse, same for ideals

InstallMethod(NrIdempotents, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> NrIdempotents@(r, RhoFunc(Parent(r))(Representative(r)), LambdaOrbSCC(r), LambdaOrb(r), true));

# different method for regular/inverse, same for ideals

InstallMethod(NrIdempotents, "for an acting semigroup", [IsActingSemigroup],
function(s)
  local data, lambda, rho, scc, lenreps, repslens, rholookup, repslookup,
   tester, nr, rhoval, m, ind, i;

  if HasIdempotents(s) then 
    return Length(Idempotents(s));
  fi;

  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  
  lambda:=LambdaOrb(s);    
  rho:=RhoOrb(s);
  scc:=OrbSCC(lambda);
  
  lenreps:=data!.lenreps;
  repslens:=data!.repslens;  
  rholookup:=data!.rholookup;
  repslookup:=data!.repslookup;
 
  tester:=IdempotentTester(s);
  
  nr:=0;
  for m in [2..Length(scc)] do 
    for ind in [1..lenreps[m]] do 
      if repslens[m][ind]=1 then
        rhoval:=rho[rholookup[repslookup[m][ind][1]]]; #rhofunc(reps[m][ind][1]);
        for i in scc[m] do 
          if tester(lambda[i], rhoval) then 
            nr:=nr+1;
          fi;
        od;
      fi;
    od;
  od;

  return nr;
end);

InstallMethod(NrIdempotents, "for a semigroup",
[IsSemigroup], S-> Length(Idempotents(S)));

#PPP

# different method for regular/inverse/ideals

InstallMethod(PartialOrderOfDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local d, n, out, data, gens, graph, lambdarhoht, datalookup, reps, repslens, ht, repslookup, lambdafunc, rhofunc, lambdaperm, o, orho, scc, lookup, schutz, mults, f, l, m, val, j, i, x, k;

  d:=GreensDClasses(s); 
  n:=Length(d);
  out:=List([1..n], x-> []);
  
  data:=SemigroupData(s);
  gens:=data!.gens;
  graph:=data!.graph;
  lambdarhoht:=data!.lambdarhoht;
  datalookup:=OrbSCCLookup(data)-1;  
  reps:=data!.reps;
  repslens:=data!.repslens;
  ht:=data!.ht;
  repslookup:=data!.repslookup;
  
  lambdafunc:=LambdaFunc(s);
  rhofunc:=RhoFunc(s);
  lambdaperm:=LambdaPerm(s);
  
  o:=LambdaOrb(s);
  orho:=RhoOrb(s);
  scc:=OrbSCC(o); 
  lookup:=OrbSCCLookup(o);
  schutz:=o!.schutzstab;
  mults:=o!.mults;

  for i in [1..n] do
    # collect info about left multiplying R-class reps of d[i] by gens
    for j in SemigroupDataSCC(d[i]) do 
      for k in graph[j] do  
        AddSet(out[i], datalookup[k]);
      od;
    od;
    
    for x in gens do
      for f in LClassReps(d[i]) do
        # the below is an expanded version of Position(data, f * x)
        f:=f*x;
        l:=Position(o, lambdafunc(f));
        m:=lookup[l];
        val:=lambdarhoht[Position(orho, rhofunc(f))][m];
        if not IsBound(schutz[m]) then 
          LambdaOrbSchutzGp(o, m);
        fi;
        if schutz[m]=true then 
          j:=repslookup[m][val][1];
        else
          if l<>scc[m][1] then 
            f:=f*mults[l][2];
          fi;
          if schutz[m]=false then 
            j:=HTValue(ht, f);
          else
            n:=0; j:=0;
            repeat 
              n:=n+1;
              if SiftedPermutation(schutz[m], lambdaperm(reps[m][val][n], f))=()
               then 
                j:=repslookup[m][val][n];
              fi;
            until j<>0;
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
  
  data:=SemigroupData(s);
  
  if not IsClosedData(data) then 
    if HasGeneratorsOfSemigroup(s) then
      gens:=GeneratorsOfSemigroup(s);
      i:=Random([1..2*Length(gens)]);
      w:=List([1..i], x-> Random([1..Length(gens)]));
      return EvaluateWord(gens, w);
    elif IsSemigroupIdeal(s) and HasGeneratorsOfSemigroupIdeal(s) then 
      x:=Random([1..Length(GeneratorsOfSemigroupIdeal(s))]);
      gens:=GeneratorsOfSemigroup(SupersemigroupOfIdeal(s));
      
      i:=Random([1..Length(gens)]);
      w:=List([1..i], x-> Random([1..Length(gens)]));
      
      x:=x*EvaluateWord(gens, w);
      
      i:=Random([1..Length(gens)]);
      w:=List([1..i], x-> Random([1..Length(gens)]));
      return EvaluateWord(gens, w)*x;
    fi;
  fi;

  n:=Random([2..Length(data!.orbit)]);
  m:=data[n][2]; o:=data[n][3]; rep:=data[n][4];

  g:=Random(LambdaOrbSchutzGp(o, m));
  i:=Random(OrbSCC(o)[m]);
  return StabilizerAction(s)(rep,g)*LambdaOrbMult(o, m, i)[1];
end);

# different method for regular/inverse, same for ideals

InstallMethod(SchutzenbergerGroup, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local o, m, lambda_schutz, lambda_stab, rho_schutz, rho_stab, schutz, p;
  
  o:=LambdaOrb(d); m:=LambdaOrbSCCIndex(d);
  lambda_schutz:=LambdaOrbSchutzGp(o, m); 
  lambda_stab:=LambdaOrbStabChain(o, m);
  
  o:=RhoOrb(d); m:=RhoOrbSCCIndex(d);
  rho_schutz:=RhoOrbSchutzGp(o, m, infinity);
  rho_stab:=RhoOrbStabChain(o, m);

  if rho_stab=true then
    schutz:=lambda_schutz;
    if lambda_stab=true then 
      SetRhoOrbStabChain(d, true);
      #right transversal required so can use PositionCanonical
      SetRhoCosets(d, RightTransversal(schutz, schutz));
      return lambda_schutz;
    fi;
  elif rho_stab=false then 
    SetRhoOrbStabChain(d, false);
    SetRhoCosets(d, RightTransversal(rho_schutz, rho_schutz));
    return rho_schutz;
  fi;

  p:=LambdaConjugator(Parent(d))(RhoOrbRep(o, m), Representative(d));
  rho_schutz:=rho_schutz^p;

  SetRhoOrbStabChain(d, StabChainImmutable(rho_schutz));
  
  if lambda_stab=false then 
    SetRhoCosets(d, Enumerator(rho_schutz));
    return lambda_schutz;
  elif lambda_stab=true then 
    schutz:=rho_schutz;
  else 
    schutz:=Intersection(lambda_schutz, rho_schutz);
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
 
  lambda_o:=LambdaOrb(h); lambda_m:=LambdaOrbSCCIndex(h);
  lambda_schutz:=LambdaOrbSchutzGp(lambda_o, lambda_m); 
  s:=Parent(h);
  
 lambda_stab:=LambdaOrbStabChain(lambda_o, lambda_m);
  
  if lambda_stab=false then 
    return lambda_schutz;
  fi;

  rho_o:=RhoOrb(h); rho_m:=RhoOrbSCCIndex(h);
  rho_schutz:=RhoOrbSchutzGp(rho_o, rho_m, infinity);
  rho_stab:=RhoOrbStabChain(rho_o, rho_m);

  if rho_stab=false then 
    return rho_schutz;
  fi;

  rep:=Representative(h);
  
  lambda_p:=LambdaOrbMult(lambda_o, lambda_m, Position(lambda_o,
   LambdaFunc(s)(rep)))[2];
   #LambdaConjugator seems to be used for two different things here!
  lambda_p:=LambdaConjugator(s)(rep*lambda_p, rep);
 
  if rho_stab=true then 
    return lambda_schutz^lambda_p;
  fi;

  rho_p:=RhoOrbMult(rho_o, rho_m, Position(rho_o, RhoFunc(s)(rep)))[2];
  rho_p:=LambdaConjugator(s)(RhoOrbRep(rho_o, rho_m), rho_p*rep);

  if lambda_stab=true then 
    return rho_schutz^rho_p;    
  fi;

  return Intersection(lambda_schutz^lambda_p, rho_schutz^rho_p);
end);

# same method for regular, different method for inverse, same for ideals

InstallMethod(SchutzenbergerGroup, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, p;

  o:=RhoOrb(l); m:=RhoOrbSCCIndex(l);
  
  if not IsGreensClassNC(l) then 
    p:=LambdaConjugator(Parent(l))(RhoOrbRep(o, m), Representative(l));
    return RhoOrbSchutzGp(o, m, infinity)^p;
  fi;
  return RhoOrbSchutzGp(o, m, infinity); 
end);

# same method for regular/inverse, same for ideals

InstallMethod(SchutzenbergerGroup, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> LambdaOrbSchutzGp(LambdaOrb(r), LambdaOrbSCCIndex(r)));

# different method for inverse/regular, same for ideals

InstallMethod(Size, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local l, r;
 
  l:=LambdaOrbSchutzGp(LambdaOrb(d), LambdaOrbSCCIndex(d));
  r:=RhoOrbSchutzGp(RhoOrb(d), RhoOrbSCCIndex(d), infinity);
  return Size(r)*Size(l)*Length(LambdaOrbSCC(d))*Length(RhoOrbSCC(d))/
   Size(SchutzenbergerGroup(d));
end);

# same method for inverse/regular, same for ideals

InstallMethod(Size, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
h-> Size(SchutzenbergerGroup(h)));

# same method for inverse/regular, same for ideals

InstallMethod(Size, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> Size(SchutzenbergerGroup(r))*Length(LambdaOrbSCC(r)));

# same method for regular, different method of inverse, same for ideals

InstallMethod(Size, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
l-> Size(SchutzenbergerGroup(l))*Length(RhoOrbSCC(l)));

#same method for inverse/regular, same for ideals

InstallMethod(StructureDescription, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  if not IsGroupHClass(h) then 
    return fail;
  fi;
  return StructureDescription(Range(IsomorphismPermGroup(h)));
end);

# technical

InstallMethod(\=, "for Green's class and class of acting semigroup",
[IsActingSemigroupGreensClass, IsActingSemigroupGreensClass],
function(x, y)
  if (IsGreensRClass(x) and IsGreensRClass(y)) or
   (IsGreensLClass(x) and IsGreensLClass(y)) or
   (IsGreensDClass(x) and IsGreensDClass(y)) or
   (IsGreensHClass(x) and IsGreensHClass(y)) then
    return Parent(x)=Parent(y) and Representative(x) in y;
  fi;
  return Parent(x)=Parent(y) and Representative(x) in y and
   Size(x)=Size(y);
end);

#

InstallMethod(\<, "for Green's class and class of acting semigroup",
[IsActingSemigroupGreensClass, IsActingSemigroupGreensClass],
function(x, y)
  if (IsGreensRClass(x) and IsGreensRClass(y)) or
   (IsGreensLClass(x) and IsGreensLClass(y)) or
   (IsGreensDClass(x) and IsGreensDClass(y)) or
   (IsGreensHClass(x) and IsGreensHClass(y)) then
    return Parent(x)=Parent(y) and Representative(x) <
     Representative(y) and (not Representative(x) in y);
  fi;
  return fail;
end);

# Usage: 
# arg[1] = semigroup; 
# arg[2] = lambda orb scc index;
# arg[3] = lambda orb; 
# arg[4] = rho orb scc index;
# arg[5] = rho orb
# arg[6] = rep; 
# arg[7] = IsGreensClassNc

# NC indicates that the representative is assumed to be in the correct form,
# i.e. RhoFunc(s)(arg[6]) is in the first place of the scc of the rho orb, and
# same for LambdaFunc(s)(arg[6]).

# arg[4] and arg[5] should be fail if we are creating the D-class of an acting
# semigroup with inverse op.

# there is no non-NC version of this as it was too complicated to be useful.

# same method for regular/inverse

InstallGlobalFunction(CreateDClassNC,  
function(arg) 
  local d;
 
  d:=Objectify(DClassType(arg[1]), rec()); 
          
  SetParent(d, arg[1]);
  SetLambdaOrbSCCIndex(d, arg[2]);
  SetLambdaOrb(d, arg[3]);
  if arg[4]<>fail then # for inverse op
    SetRhoOrbSCCIndex(d, arg[4]);
    SetRhoOrb(d, arg[5]);
  fi;
  SetRepresentative(d, arg[6]);
  SetIsGreensClassNC(d, arg[7]);
  SetEquivalenceClassRelation(d, GreensDRelation(arg[1])); 
  return d; 
end); 

# Usage: 
# arg[1] = semigroup; 
# arg[2] = lambda orb scc index;  
# arg[3] = lambda orb; 
# arg[4] = rho orb scc index; 
# arg[5] = rho orb;
# arg[6] = rep;
# arg[7] = IsGreensClassNC. 

# arg[4] and arg[5] should be fail if we are creating the D-class of an acting
# semigroup with inverse op.

# NC version not required since H-class reps are not rectified.

# same method for regular/inverse

InstallGlobalFunction(CreateHClass, 
function(arg)
  local h;
  
  h:=Objectify(HClassType(arg[1]), rec());
  SetParent(h, arg[1]);

  SetLambdaOrbSCCIndex(h, arg[2]);
  SetLambdaOrb(h, arg[3]);
  if arg[4]<>fail then 
    SetRhoOrbSCCIndex(h, arg[4]);
    SetRhoOrb(h, arg[5]);
  fi;
  
  SetRepresentative(h, arg[6]);
  SetEquivalenceClassRelation(h, GreensHRelation(arg[1]));
  SetIsGreensClassNC(h, arg[7]);
  return h;
end);

# Usage: 
# s = semigroup;  
# m = rho orb scc index; 
# o = rho orb;  
# rep = representative;
# nc = IsGreensClassNC. 

# use the NC version for already rectified reps.

# different method for inverse, same method for regular

InstallGlobalFunction(CreateLClass,
function(s, m, o, rep, nc)
  local rectify;
  rectify:=RectifyRho(s, o, rep, fail, m);
  return CreateLClassNC(s, rectify.m, o, rectify.rep, nc);
end);

# Usage: 
# arg[1] = semigroup;  
# arg[2] = rho orb scc index; 
# arg[3] = rho orb;  
# arg[4] = rep;
# arg[5] = IsGreensClassNC. 

# NC indicates that the representative is assumed to be in the correct form,
# i.e. RhoFunc(s)(arg[2]) is in the first place of the scc of the rho orb. 

# different method for inverse, same method for regular

InstallGlobalFunction(CreateLClassNC,
function(s, m, o, rep, nc)
  local l;

  l:=Objectify(LClassType(s), rec());
  SetParent(l, s);
  SetRepresentative(l, rep);
  SetRhoOrb(l, o);
  SetRhoOrbSCCIndex(l, m);
  SetEquivalenceClassRelation(l, GreensLRelation(s));
  SetIsGreensClassNC(l, nc); 
  return l;
end);

# Usage: 
# arg[1] = semigroup; 
# arg[2] = lambda orb scc index;
# arg[3] = lambda orb; 
# arg[4] = rep; 
# arg[5] = Green's class NC

# use the NC version for already rectified reps.

# same method for regular/inverse

InstallGlobalFunction(CreateRClass,
function(s, m, o, rep, nc)
  local rectify;
  rectify:=RectifyLambda(s, o, rep, fail, m);
  return CreateRClassNC(s, rectify.m, o, rectify.rep, nc);
end);

# Usage: 
# arg[1] = semigroup; 
# arg[2] = lambda orb scc index;
# arg[3] = lambda orb; 
# arg[4] = rep; 
# arg[5] = IsRClassNC.
# arg[6] = semigroup data index (optional).

# NC indicates that the representative is assumed to be in the correct form,
# i.e. LambdaFunc(s)(arg[2]) is in the first place of the scc of the lambda
# orb. 

# same method for regular/inverse

InstallGlobalFunction(CreateRClassNC,
function(arg)
  local r;
  
  r:=Objectify(RClassType(arg[1]), rec());

  SetParent(r, arg[1]);
  SetLambdaOrbSCCIndex(r, arg[2]);
  SetLambdaOrb(r, arg[3]);
  SetRepresentative(r, arg[4]);
  SetEquivalenceClassRelation(r, GreensRRelation(arg[1]));
  SetIsGreensClassNC(r, arg[5]);
  if IsBound(arg[6]) then 
    SetSemigroupDataIndex(r, arg[6]);
  fi;

  return r;
end);

#

InstallMethod(DClass, "for an R-class", [IsGreensRClass], DClassOfRClass);
InstallMethod(DClass, "for an L-class", [IsGreensLClass], DClassOfLClass);
InstallMethod(DClass, "for an H-class", [IsGreensHClass], DClassOfHClass);
InstallMethod(LClass, "for an H-class", [IsGreensHClass], LClassOfHClass);
InstallMethod(RClass, "for an H-class", [IsGreensHClass], RClassOfHClass);

# different method for regular/inverse

InstallMethod(DClassType, "for an acting semigroups",
[IsActingSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
          IsEquivalenceClassDefaultRep and IsGreensDClass and
          IsActingSemigroupGreensClass);
end);

# different method for regular/inverse

InstallMethod(HClassType, "for an acting semigroup",
[IsActingSemigroup],
function(s);
 return NewType( FamilyObj( s ), IsEquivalenceClass and
  IsEquivalenceClassDefaultRep and IsGreensHClass and
  IsActingSemigroupGreensClass);
end);

# different method for regular/inverse

InstallMethod(LClassType, "for an acting semigroup",
[IsActingSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensLClass and
         IsActingSemigroupGreensClass);
end);

# different method for regular/inverse

InstallMethod(RClassType, "for an acting semigroup",
[IsActingSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensRClass and
         IsActingSemigroupGreensClass);
end);

# same method for regular/inverse

InstallMethod(GreensJClassOfElement, "for acting semigroup and element.",
[IsActingSemigroup, IsAssociativeElement], 
GreensDClassOfElement);

InstallMethod(IsRegularDClass, "for a D-class of acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass], IsRegularClass);

InstallMethod(IsTransformationSemigroupGreensClass, "for a Green's class",
[IsGreensClass], x-> IsTransformationSemigroup(Parent(x)));

InstallMethod(IsPartialPermSemigroupGreensClass, "for a Green's class",
[IsGreensClass], x-> IsPartialPermSemigroup(Parent(x)));

#EOF
