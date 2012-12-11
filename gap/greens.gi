#############################################################################
##
#W  greens.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# acting...
#############################################################################
#############################################################################

# new for 1.0! - LambdaCosets - "for a D-class of an acting semigp"
##############################################################################

# not required for regular/inverse

InstallMethod(LambdaCosets, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return RightTransversal(LambdaOrbSchutzGp(LambdaOrb(d),
   LambdaOrbSCCIndex(d)), SchutzenbergerGroup(d));
end);

# new for 1.0! - RhoCosets - "for a D-class of an acting semigp"
##############################################################################

# not required for regular/inverse

InstallMethod(RhoCosets, "for a D-class of an acting semigp",
[IsGreensDClass and IsActingSemigroupGreensClass], 
function(d)
  SchutzenbergerGroup(d);
  return RhoCosets(d);
end);

# new for 1.0! - LambdaOrbSCC - "for Green's class of an acting semigroup"
############################################################################

# same method for regular/inverse

InstallOtherMethod(LambdaOrbSCC, "for a Green's class of an acting semi",
[IsActingSemigroupGreensClass and IsGreensClass],
x-> OrbSCC(LambdaOrb(x))[LambdaOrbSCCIndex(x)]);

# new for 1.0! - RhoOrbSCC - "for Green's class of an acting semigroup"
############################################################################

# same method for regular/inverse

InstallOtherMethod(RhoOrbSCC, "for a Green's class of an acting semi",
[IsActingSemigroupGreensClass and IsGreensClass], 
x-> OrbSCC(RhoOrb(x))[RhoOrbSCCIndex(x)]);

# new for 1.0! - RhoOrbStabChain - "for a D-class of an acting semigp"
##############################################################################

# different method for regular/inverse

InstallMethod(RhoOrbStabChain, "for a D-class of an acting semigp",
[IsGreensDClass and IsActingSemigroupGreensClass], 
function(d)
  SchutzenbergerGroup(d);
  return RhoOrbStabChain(d);
end);

# new for 1.0! - RhoOrbStabChain - "for an L-class of an acting semi"
##############################################################################

# same method for regular, not required for inverse. 

InstallMethod(RhoOrbStabChain, "for an L-class of an acting semi",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local g;

  g:=SchutzenbergerGroup(l);

  if IsTrivial(g) then 
    return false;
  elif IsNaturalSymmetricGroup(g) and
   NrMovedPoints(g)=ActionRank(Representative(l)) then 
    return true; 
  fi;
  return StabChainImmutable(g);
end);

# new for 1.0! - SemigroupDataSCC - "for a D-class of an acting semigp"
##############################################################################
# this is useful in PartialOrderOfDClasses...

InstallMethod(SemigroupDataSCC, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local data;
  if not HasSemigroupDataIndex(d) then 
    return fail;
  fi;
  data:=SemigroupData(ParentSemigroup(d));

  # scc of R-reps corresponding to d 
  return OrbSCC(data)[OrbSCCLookup(data)[SemigroupDataIndex(d)]];
end);

# main 
##############################################################################
##############################################################################

# new for 1.0! - \in - "for acting elt and D-class of acting semigp"
#############################################################################
#JDM revise this as per the other version of \in just deleted :)

# different method for regular/inverse 

InstallMethod(\in, "for acting elt and D-class of acting semigp.",
[IsAssociativeElement, IsGreensDClass and IsActingSemigroupGreensClass],
function(f, d)
  local rep, s, g, m, o, scc, l, schutz, cosets, x;
  
  rep:=Representative(d); 
  s:=ParentSemigroup(d);
 
  # ActionRank method selection causes slowdown here.
  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
    ActionRank(f) <> ActionRank(rep) or
    ActionDegree(f)<>ActionDegree(rep) then
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

# new for 1.0! - \in - "for acting elt and H-class of acting semigp"
#############################################################################
 
# same method for regular/inverse 

InstallOtherMethod(\in, "for elt and acting semigroup H-class",
[IsAssociativeElement, IsGreensHClass and IsActingSemigroupGreensClass],
function(f, h)
  local s, rep;

  s:=ParentSemigroup(h);
  rep:=Representative(h);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
    ActionRank(f) <> ActionRank(rep) or
    RhoFunc(s)(f) <> RhoFunc(s)(rep) or 
    LambdaFunc(s)(f) <> LambdaFunc(s)(rep) or
    ActionDegree(rep)<>ActionDegree(f) then 
    return false;
  fi;

  return LambdaPerm(s)(rep, f) in SchutzenbergerGroup(h);
end);

# new for 1.0! - \in - "for acting elt and L-class of acting semigp"
#############################################################################
#JDM this method differs from the one in 0.99. 

# same method for regular, different method for inverse 

InstallMethod(\in, "for acting elt and L-class of acting semigp.",
[IsAssociativeElement, IsGreensLClass and IsActingSemigroupGreensClass],
function(f, l)
  local rep, s, m, o, i, schutz, g, p;

  rep:=Representative(l); 
  s:=ParentSemigroup(l);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or ActionDegree(f) <>
    ActionDegree(rep) or ActionRank(f) <> ActionRank(rep) or LambdaFunc(s)(f)
    <> LambdaFunc(s)(rep) then
    Info(InfoSemigroups, 1, "degree, rank, or lambda value not equal to those of",
    " any of the L-class elements,");
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
  
  #return SiftGroupElement(schutz, LambdaPerm(s)(rep, g)).isone;
  return SiftedPermutation(schutz,  LambdaPerm(s)(rep, g))=();
end);

# new for 1.0! - \in - "for acting elt and R-class of acting semigp"
#############################################################################
# Algorithm E. 

# same method for regular/inverse 

InstallMethod(\in, "for acting elt and R-class of acting semigp.",
[IsAssociativeElement, IsGreensRClass and IsActingSemigroupGreensClass],
function(f, r)
  local rep, s, m, o, l, schutz, g;

  rep:=Representative(r); 
  s:=ParentSemigroup(r);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
    ActionDegree(f) <> ActionDegree(rep) or 
    ActionRank(f) <> ActionRank(rep) or 
    RhoFunc(s)(f) <> RhoFunc(s)(rep) then
    Info(InfoSemigroups, 1, "degree, rank, or rho value not equal to those of",
    " any of the R-class elements,");
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

  #return SiftGroupElement(schutz, LambdaPerm(s)(rep, g)).isone;
  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g))=();
end);

#AAA

# new for 0.1! - AsSSortedList - "for a Green's class of an acting semigp"
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for a Green's class of an acting semigp",
[IsGreensClass and IsActingSemigroupGreensClass], 
function(c)
  return ConstantTimeAccessList(EnumeratorSorted(c));
end);

#DDD

# new for 1.0! - DClassOfLClass - "for a L-class of an acting semigroup"
#############################################################################

# same method for regular/inverse

# only for L-classes not created during GreensLClasses! Those created via
# GreensLClasses should already know DClassOfLClass

InstallMethod(DClassOfLClass, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local s, f, nc, o, i, m;

  s:=ParentSemigroup(l); 
  f:=Representative(l);
  nc:=IsGreensClassNC(l);

  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) and not nc then 
    o:=LambdaOrb(s);
    i:=Position(o, LambdaFunc(s)(f));
  else
    o:=GradedLambdaOrb(s, f, nc<>true);
    i:=LambdaPos(o);
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
  if HasIsInverseOpClass(l) and IsInverseOpClass(l) then 
    return CreateDClassNC(s, m, o, fail, fail, f, nc);
  fi;
  return CreateDClassNC(s, m, o, RhoOrbSCCIndex(l), RhoOrb(l), f, nc);
end);

# new for 1.0! - DClassOfRClass - "for a R-class of an acting semigroup"
#############################################################################

# same method for regular, different method for inverse.

InstallMethod(DClassOfRClass, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, f, nc, o, i, m;

  s:=ParentSemigroup(r); 
  f:=Representative(r);
  nc:=IsGreensClassNC(r);

  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) and not nc then 
    o:=RhoOrb(s);
    i:=Position(o, RhoFunc(s)(f));
  else
    o:=GradedRhoOrb(s, f, nc<>true);
    i:=RhoPos(o);
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

# new for 1.0! - DClassOfHClass - "for a H-class of an acting semigroup"
#############################################################################

# same method for regular, different method for inverse semigroups. 

InstallMethod(DClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local s, lambda_o, lambda_m, rho_o, rho_m, rectify;
  
  s:=ParentSemigroup(h);
  lambda_o:=LambdaOrb(h);
  lambda_m:=LambdaOrbSCCIndex(h);
  rho_o:=RhoOrb(h);
  rho_m:=RhoOrbSCCIndex(h);

  rectify:=RectifyRho(s, rho_o, Representative(h), fail, rho_m);
  rectify:=RectifyLambda(s, lambda_o, rectify.rep, fail, lambda_m);

  return CreateDClassNC(s, lambda_m, lambda_o, rho_m, rho_o, rectify.rep, IsGreensClassNC(h));
end);

# new for 1.0! - LClassOfHClass - "for a H-class of an acting semigroup"
#############################################################################
# same method for regular, different method for inverse

InstallMethod(LClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
# use non-NC so that rho value of f is rectified
h-> CreateLClass(ParentSemigroup(h), RhoOrbSCCIndex(h), RhoOrb(h),
 Representative(h), IsGreensClassNC(h)));

# new for 1.0! - RClassOfHClass - "for a H-class of an acting semigroup"
#############################################################################

# same method for regular/inverse semigroups. 

InstallMethod(RClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
h-> CreateRClass(ParentSemigroup(h), LambdaOrbSCCIndex(h), LambdaOrb(h),
 Representative(h), IsGreensClassNC(h)));

#GGG

# mod for 1.0! - GreensDClasses - "for an acting semigroup"
##############################################################################

# Notes: this could be written in a more compact way but it is not for
# performance reasons. 

# different method for regular/inverse

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
    SetParentSemigroup(d, s);
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

# mod for 1.0! - GreensHClasses - "for an acting semigroup"
##############################################################################

# different method for regular/inverse

InstallMethod(GreensHClasses, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  return Concatenation(List(GreensDClasses(s), GreensHClasses));
end);

# new for 0.1! - GreensHClasses - "for a D-class of an acting semigroup"
#############################################################################
# JDM could this be better/more efficient?!

# different method for regular/inverse. 

InstallOtherMethod(GreensHClasses, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return Concatenation(List(GreensRClasses(d), GreensHClasses));
end);

# new for 1.0! - GreensHClasses - "for an L-class of an acting semigroup"
##############################################################################

# different method for regular/inverse

InstallOtherMethod(GreensHClasses, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local rho_o, rho_m, s, scc, mults, d, lambda_o, lambda_m, cosets, f, nc, out,
  k, i, j;

  rho_o:=RhoOrb(l); 
  rho_m:=RhoOrbSCCIndex(l);
  s:=ParentSemigroup(l);

  scc:=OrbSCC(rho_o)[rho_m];
  mults:=RhoOrbMults(rho_o, rho_m);
  
  d:=DClassOfLClass(l);
  lambda_o:=LambdaOrb(d); 
  lambda_m:=LambdaOrbSCCIndex(d);

  cosets:=RhoCosets(d);
  f:=Representative(l);
  nc:=IsGreensClassNC(l);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;

  for i in scc do 
    i:=mults[i][1]*f;
    for j in cosets do 
      k:=k+1;
      # JDM maybe a bad idea to use CreateHClass here, perhaps expand?
      out[k]:=CreateHClass(s, lambda_m, lambda_o, rho_m, rho_o, i*j, nc);
      SetLClassOfHClass(out[k], l);
      SetDClassOfHClass(out[k], d);
    od;
  od;
  return out;
end);

# new for 1.0! - GreensHClasses - "for an R-class of an acting semigroup"
##############################################################################

# different method for regular/inverse

InstallOtherMethod(GreensHClasses, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local lambda_o, lambda_m, s, scc, mults, d, rho_o, rho_m, cosets, f, nc, out,
  k, i, j;

  lambda_o:=LambdaOrb(r); 
  lambda_m:=LambdaOrbSCCIndex(r);
  s:=ParentSemigroup(r);

  scc:=OrbSCC(lambda_o)[lambda_m];
  mults:=LambdaOrbMults(lambda_o, lambda_m);
  
  d:=DClassOfRClass(r);
  rho_o:=RhoOrb(d); 
  rho_m:=RhoOrbSCCIndex(d);

  cosets:=LambdaCosets(d);
  f:=Representative(r);
  nc:=IsGreensClassNC(r);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;

  for i in cosets do 
    i:=f*i;
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

# mod for 1.0! - GreensLClasses - "for an acting semigroup"
##############################################################################

# different method for regular/inverse

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

# mod for 1.0! - GreensLClasses - "for a D-class of an acting semigroup"
##############################################################################

# different method for regular/inverse

InstallMethod(GreensLClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass], 
function(d)
  local mults, scc, cosets, f, s, o, m, lrel, nc, out, k, g, l, j, i;
 
  mults:=LambdaOrbMults(LambdaOrb(d), LambdaOrbSCCIndex(d));
  scc:=LambdaOrbSCC(d);
  cosets:=LambdaCosets(d);
  f:=Representative(d);
 
  s:=ParentSemigroup(d);
  o:=RhoOrb(d);
  m:=RhoOrbSCCIndex(d);
  nc:=IsGreensClassNC(d);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;
  for j in cosets do
    g:=f*j;
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

# mod for 1.0! - GreensRClasses - "for an acting semigroup"
##############################################################################

# different method for regular/inverse

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

# new for 1.0! - GreensRClasses - "for a D-class of an acting semigroup"
##############################################################################

# different method for regular/inverse

InstallMethod(GreensRClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass], 
function(d)
  local mults, scc, cosets, f, s, o, m, nc, out, k, g, i, j;
 
  mults:=RhoOrbMults(RhoOrb(d), RhoOrbSCCIndex(d));
  scc:=RhoOrbSCC(d);
  cosets:=RhoCosets(d);
  f:=Representative(d);
 
  s:=ParentSemigroup(d);
  o:=LambdaOrb(d);
  m:=LambdaOrbSCCIndex(d);
  nc:=IsGreensClassNC(d); 

  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;
  for i in scc do
    g:=mults[i][1]*f;
    for j in cosets do
      k:=k+1;
      out[k]:=CreateRClassNC(s, m, o, g/j, nc);
      SetDClassOfRClass(out[k], d);
    od;
  od;

  return out;
end);

# mod for 1.0! - GreensDClassOfElement - "for an acting semigp and elt."
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(GreensDClassOfElement, "for an acting semigp and elt",
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
    i:=RhoPos(rho_o);#Position(o, RhoFunc(s)(f));
  fi;

  rectify:=RectifyRho(s, rho_o, rep, i);

  return CreateDClassNC(s, lambda_m, lambda_o, rectify.m, rho_o, rectify.rep,
   false);
end);

# mod for 1.0! - GreensDClassOfElementNC - "for an acting semigp and elt."
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(GreensDClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  return CreateDClassNC(s, 1, GradedLambdaOrb(s, f, false), 
   1, GradedRhoOrb(s, f, false), f, true);
end);

# mod for 1.0! - GreensHClassOfElement - "for an acting semigp and elt."
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(GreensHClassOfElement, "for an acting semigp and elt",
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
    i:=RhoPos(rho_o);#Position(o, RhoFunc(s)(f));
  fi;

  rho_m:=OrbSCCLookup(rho_o)[i];     

  return CreateHClass(s, lambda_m, lambda_o, rho_m, rho_o, f, false);
end);

# mod for 1.0! - GreensHClassOfElementNC - "for an acting semigp and elt."
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(GreensHClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  return CreateHClass(s, 1, GradedLambdaOrb(s, f, false), 
   1, GradedRhoOrb(s, f, false), f, true);
end);

# new for 1.0! - GreensHClassOfElement - "for D-class and elt."
############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(GreensHClassOfElement, "for D-class and elt",
[IsActingSemigroupGreensClass and IsGreensDClass, IsAssociativeElement],
function(d, f)
  local h;

  if not f in d then
    Error("the element does not belong to the Green's class,");
    return;
  fi;

  h:=CreateHClass(ParentSemigroup(d), LambdaOrbSCCIndex(d), LambdaOrb(d),
   RhoOrbSCCIndex(d), RhoOrb(d), f, IsGreensClassNC(d));
  SetDClassOfHClass(h, d);
  
  return h;
end);

# mod for 1.0! - GreensHClassOfElementNC - "for D-class and elt."
#############################################################################

# same method for regular, different method for inverse.

InstallOtherMethod(GreensHClassOfElementNC, "for a D-class and elt",
[IsActingSemigroupGreensClass and IsGreensDClass, IsAssociativeElement],
function(d, f)
  local h;
 
  h:=CreateHClass(ParentSemigroup(d), LambdaOrbSCCIndex(d), LambdaOrb(d),
   RhoOrbSCCIndex(d), RhoOrb(d), f, true);
  SetDClassOfHClass(h, d);

  return h;
end);

# new for 1.0! - GreensHClassOfElement - "for L-class and elt."
############################################################################

# same method for regular, different method for inverse.

InstallOtherMethod(GreensHClassOfElement, "for L-class and elt",
[IsActingSemigroupGreensClass and IsGreensLClass, IsAssociativeElement],
function(l, f)
  local s, nc, o, i, h;

  if not f in l then
    Error("the element does not belong to the Green's class,");
    return;
  fi;

  s:=ParentSemigroup(l);
  nc:=IsGreensClassNC(l);

  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s);
    i:=Position(o, LambdaFunc(s)(f));
  else
    o:=GradedLambdaOrb(s, f, nc<>true);
    i:=LambdaPos(o);
  fi;

  h:=CreateHClass(s, OrbSCCLookup(o)[i], o, RhoOrbSCCIndex(l), RhoOrb(l), f,
   nc);
  SetLClassOfHClass(h, l);

  return h;
end);

# mod for 1.0! - GreensHClassOfElementNC - "for an L-class and elt."
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(GreensHClassOfElementNC, "for an L-class and elt",
[IsActingSemigroupGreensClass and IsGreensLClass, IsAssociativeElement],
function(l, f)
  local h;
 
  h:=CreateHClass(ParentSemigroup(l), 
   1, GradedLambdaOrb(ParentSemigroup(l), f, false), 
   RhoOrbSCCIndex(l), RhoOrb(l), f, true);
  SetLClassOfHClass(h, l);

  return h;
end);

# new for 1.0! - GreensHClassOfElement - "for R-class and elt."
############################################################################

# same method for regular, different method for inverse.

InstallOtherMethod(GreensHClassOfElement, "for R-class and elt",
[IsActingSemigroupGreensClass and IsGreensRClass, IsAssociativeElement],
function(r, f)
  local s, nc, o, i, h;

  if not f in r then
    Error("the element does not belong to the Green's class,");
    return;
  fi;

  s:=ParentSemigroup(r);
  nc:=IsGreensClassNC(r);

  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then 
    o:=RhoOrb(s);
    i:=Position(o, RhoFunc(s)(f));
  else
    o:=GradedRhoOrb(s, f, nc<>true);
    i:=RhoPos(o);
  fi;

  h:=CreateHClass(s, LambdaOrbSCCIndex(r), LambdaOrb(r), OrbSCCLookup(o)[i], 
   o, f, nc);
  SetRClassOfHClass(h, r);

  return h;
end);

# mod for 1.0! - GreensHClassOfElementNC - "for an R-class and elt."
#############################################################################

# same method for regular, different method for inverse.

InstallOtherMethod(GreensHClassOfElementNC, "for an R-class and elt",
[IsActingSemigroupGreensClass and IsGreensRClass, IsAssociativeElement],
function(r, f)
  local h;
  
  h:=CreateHClass(ParentSemigroup(r), LambdaOrbSCCIndex(r), LambdaOrb(r), 
   1, GradedRhoOrb(ParentSemigroup(r), f, false), f, true);
  SetRClassOfHClass(h, r);

  return h;
end);

# mod for 1.0! - GreensLClassOfElement - "for an acting semigp and elt."
#############################################################################

# same method for regular, different method for inverse.

InstallOtherMethod(GreensLClassOfElement, "for an acting semigp and elt",
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
    o:=GradedRhoOrb(s, f, true);
  fi;
  # use non-NC so that rho value of f is rectified. 
  # not that if o is graded orb, then Position(o, RhoFunc(s)(f)) is 
  # passed to CreateLClass and hence RectifyRho in o!.rho_l.
  return CreateLClass(s, fail, o, f, false);
end);

# mod for 1.0! - GreensLClassOfElementNC - "for an acting semigp and elt."
#############################################################################

# same method for regular, different method for inverse.

InstallOtherMethod(GreensLClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  # use NC since rho value of f has to be in first place of GradedRhoOrb
  # with false as final arg
  return CreateLClassNC(s, 1, GradedRhoOrb(s, f, false), f, true);
end);

# mod for 1.0! - GreensLClassOfElement - "for D-class of acting semi and elt"
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(GreensLClassOfElement, "for D-class of acting semi and elt",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local l;
    
  if not f in d then
    Error("the element does not belong to the D-class,");
    return;
  fi;
 
  # use non-NC so that rho value of f is rectified
  l:=CreateLClass(ParentSemigroup(d), RhoOrbSCCIndex(d), RhoOrb(d), f,
   IsGreensClassNC(d));

  SetDClassOfLClass(l, d);
  return l;
end);

# mod for 1.0! - GreensLClassOfElementNC - "for D-class and acting elt"
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(GreensLClassOfElementNC, "for D-class and acting elt",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local l;

  # use non-NC so taht rho value of f is rectified
  l:=CreateLClass(ParentSemigroup(d), RhoOrb(d), RhoOrbSCCIndex(d), f, true);
  SetDClassOfLClass(l, d);
  return l;
end);

# mod for 1.0! - GreensRClassOfElement - "for an acting semigp and elt."
#############################################################################

# different method for regular/inverse.

InstallOtherMethod(GreensRClassOfElement, "for an acting semigp and elt",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;
  return CallFuncList(CreateRClassNC, 
   SemigroupData(s)[Position(SemigroupData(s), f)]);
end);

# mod for 1.0! - GreensRClassOfElementNC - "for an acting semigp and elt."
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensRClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsAssociativeElement],
function(s, f)
  local pos, r;
 
  if HasSemigroupData(s) and IsClosed(SemigroupData(s)) then 
    pos:=Position(SemigroupData(s), f);
    if pos<>fail then
      return CallFuncList(CreateRClassNC, SemigroupData(s)[pos]);
    fi;  
  fi;
  
  return CreateRClassNC(s, 1, GradedLambdaOrb(s, f, false), f, true);
end);

# mod for 1.0! - GreensRClassOfElement - "for D-class and acting elt"
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensRClassOfElement, "for D-class and acting elt",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local r;
    
  if not f in d then
    Error("the element does not belong to the D-class,");
    return;
  fi;
 
  r:=CreateRClass(ParentSemigroup(d), LambdaOrbSCCIndex(d), LambdaOrb(d), f,
   IsGreensClassNC(d));
  SetDClassOfRClass(r, d);

  return r;
end);

# mod for 1.0! - GreensRClassOfElementNC - "for D-class and acting elt"
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensRClassOfElementNC, "for D-class and acting elt",
[IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local r;

  r:=CreateRClass(ParentSemigroup(d), LambdaOrbSCCIndex(d), LambdaOrb(d), f,
   true);
  SetDClassOfRClass(r, d);
  return r;
end);

# mod for 1.0! - GroupHClass - "for a D-class of an acting semigroup"
############################################################################

# same method for regular, different method for inverse.

InstallOtherMethod(GroupHClass, "for a D-class of an acting semigp.",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local s, rho, o, scc, tester, i;

  if HasIsRegularClass(d) and not IsRegularClass(d) then 
    return fail;
  fi;
  
  s:=ParentSemigroup(d);
  rho:=RhoFunc(s)(Representative(d));
  o:=LambdaOrb(d);
  scc:=OrbSCC(o)[LambdaOrbSCCIndex(d)];
  tester:=IdempotentLambdaRhoTester(s);

  for i in scc do 
    if tester(o[i], rho) then 
      if not HasIsRegularClass(d) then 
        SetIsRegularClass(d, true);
      fi;
      return CreateHClass(s, LambdaOrbSCCIndex(d), o, RhoOrbSCCIndex(d),
       RhoOrb(d), IdempotentLambdaRhoCreator(s)(o[i], rho), IsGreensClassNC(d));
    fi;
  od;

  if not HasIsRegularClass(d) then 
    SetIsRegularClass(d, false);
  fi;
  return fail;
end);

# mod for 0.7! - GroupHClassOfGreensDClass - "for D-class"
############################################################################

# same method for regular/inverse

InstallMethod(GroupHClassOfGreensDClass, "for D-class",
[IsGreensDClass], GroupHClass);

#HHH

# new for 1.0! - HClassReps - "for an acting semigp."
############################################################################

# different method for regular/inverse

InstallMethod(HClassReps, "for an acting semigp.",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
s-> Concatenation(List(GreensRClasses(s), HClassReps)));

# mod for 1.0! - HClassReps - "for an D-class of an acting semigroup"
############################################################################

# different method for regular/inverse

# JDM this method could be better...

InstallOtherMethod(HClassReps, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return Concatenation(List(GreensRClasses(d), HClassReps));
end);

# new for 1.0! - HClassReps - "for an L-class of an acting semigroup"
##############################################################################

# different method for regular/inverse 

InstallOtherMethod(HClassReps, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, scc, mults, f, cosets, out, k, i, j;

  o:=RhoOrb(l); 
  m:=RhoOrbSCCIndex(l);
  scc:=OrbSCC(o)[m];
  mults:=RhoOrbMults(o, m);
  f:=Representative(l); 
 
  cosets:=RhoCosets(DClassOfLClass(l));
  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;

  for i in scc do 
    i:=mults[i][1]*f;
    for j in cosets do 
      k:=k+1;
      out[k]:=i*j;
    od;
  od;
  return out;
end);

# new for 1.0! - HClassReps - "for an R-class of an acting semigroup"
##############################################################################

# different method for regular/inverse

InstallOtherMethod(HClassReps, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, scc, mults, f, cosets, out, k, i, j;

  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
  scc:=OrbSCC(o)[m];
  mults:=LambdaOrbMults(o, m);
  f:=Representative(r);
 
  cosets:=LambdaCosets(DClassOfRClass(r));
  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;

  for i in cosets do 
    i:=f*i;
    for j in scc do 
      k:=k+1;
      out[k]:=i*mults[j][1];
    od;
  od;
  return out;
end);

# mod for 1.0! - DClassReps - "for an acting semigroup"
#############################################################################

# Notes: that these are not rectified!

# separate methods for both regular and inverse semigroups.

InstallMethod(DClassReps, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
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

# new for 1.0! - LClassReps - "for an acting semigp."
#############################################################################

# different method for regular/inverse

InstallOtherMethod(LClassReps, "for an acting semigp.",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local D, out, x;
  D:=GreensDClasses(s);
  out:=[];
  for x in D do 
    Append(out, LClassReps(x));
  od;
  return out;
end);

# new for 1.0! - LClassReps - "for an acting semigroup D-class"
#############################################################################

# different method for regular/inverse

InstallOtherMethod(LClassReps, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local o, m, mults, scc, f, cosets, out, k, g, i, j;
  
  o:=LambdaOrb(d); 
  m:=LambdaOrbSCCIndex(d);
  mults:=LambdaOrbMults(o, m);
  scc:=LambdaOrbSCC(d);
  f:=Representative(d);
 
  cosets:=LambdaCosets(d);
  out:=EmptyPlist(Length(scc)*Length(cosets));

  k:=0;
  for i in cosets do
    g:=f*i;
    for j in scc do
      k:=k+1;
      out[k]:=g*mults[j][1];
    od;
  od;
  return out;
end);

# mod for 1.0! - RClassReps - "for an acting semigroup"
############################################################################

# different method for regular/inverse

InstallMethod(RClassReps, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
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

# mod for 1.0! - RClassReps - "for a D-class of an acting semigroup"
############################################################################

# different method for regular/inverse

InstallOtherMethod(RClassReps, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
function(d)
  local o, m, mults, scc, f, out, k, cosets, g, i, j;

  o:=RhoOrb(d); 
  m:=RhoOrbSCCIndex(d);
  mults:=RhoOrbMults(o, m);
  scc:=RhoOrbSCC(d);
  f:=Representative(d);

  cosets:=RhoCosets(d);
  out:=EmptyPlist(Length(scc)*Length(cosets));
  
  k:=0;

  for i in scc do
    g:=mults[i][1]*f;
    for j in cosets do
      k:=k+1;
      out[k]:=g*j^-1;
    od;
  od;
  return out;
end);


#III

# new for 1.0! - Idempotents@ - "class, lambda/rho value, scc, o, onright"
#############################################################################

InstallGlobalFunction(Idempotents@, 
function(x, value, scc, o, onright)
  local s, out, j, tester, creator, i;

  if HasIsRegularClass(x) and not IsRegularClass(x) then 
    return [];
  fi;
  
  s:=ParentSemigroup(x);

  if ActionRank(Representative(x))=ActionDegree(Representative(x)) then
    return [One(s)];
  fi;

  out:=EmptyPlist(Length(scc)); 
  j:=0;
  tester:=IdempotentLambdaRhoTester(s);
  creator:=IdempotentLambdaRhoCreator(s);

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

# mod for 1.0! - Idempotents - "for an acting semigroup" 
#############################################################################
# Notes: this could be more compacted but it is not for performance reasons.

# same method for regular, different method for inverse

InstallOtherMethod(Idempotents, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local lambda_o, creator, r, l, out, nr, tester, rho_o, scc, gens, rhofunc, lookup, rep, rho, j, i, k;

  if IsRegularSemigroup(s) then 

    out:=[];
    nr:=0;
    tester:=IdempotentLambdaRhoTester(s);
    creator:=IdempotentLambdaRhoCreator(s);
    rho_o:=RhoOrb(s);
    scc:=OrbSCC(rho_o);
    lambda_o:=LambdaOrb(s);
    gens:=lambda_o!.gens;
    rhofunc:=RhoFunc(s);
    lookup:=OrbSCCLookup(rho_o);

    for i in [2..Length(lambda_o)] do
      rep:=EvaluateWord(gens, TraceSchreierTreeForward(lambda_o, i));
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

# new for 1.0! - Idempotents - "for a D-class of an acting semigp."
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(Idempotents, "for a D-class of an acting semigp.",
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

# new for 1.0! - Idempotents - "for an H-class of an acting semigroup"
#############################################################################

# same method for regular/inverse

InstallOtherMethod(Idempotents, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local s, f;

  if not IsGroupHClass(h) then 
    return [];
  fi;

  s:=ParentSemigroup(h);
  f:=Representative(h);
  return [IdempotentLambdaRhoCreator(s)(LambdaFunc(s)(f), RhoFunc(s)(f))];
end);

# mod for 1.0! - Idempotents - "for an L-class of an acting semigp"
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(Idempotents, "for an L-class of an acting semigp.",
[IsGreensLClass and IsActingSemigroupGreensClass],
l-> Idempotents@(l, LambdaFunc(ParentSemigroup(l))(Representative(l)),
RhoOrbSCC(l), RhoOrb(l), false));

# mod for 1.0! - Idempotents - "for an R-class of an acting semigp"
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(Idempotents, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> Idempotents@(r, RhoFunc(ParentSemigroup(r))(Representative(r)),
LambdaOrbSCC(r), LambdaOrb(r), true));

# mod for 1.0! - IsGroupHClass - "for an H-class of an acting semigp."
############################################################################

# same method for regular/inverse

InstallOtherMethod(IsGroupHClass, "for an H-class of an acting semigp.",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local s, f;
  s:=ParentSemigroup(h);
  f:=Representative(h);
  return IdempotentLambdaRhoTester(s)(LambdaFunc(s)(f), RhoFunc(s)(f));
end);

# mod for 1.0! - IsomorphismPermGroup - "for H-class of an acting semi"
###########################################################################

# same method for regular/inverse

InstallOtherMethod(IsomorphismPermGroup, "for H-class of an acting semi",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local g, f;

  if not IsGroupHClass(h) then
    Error("the H-class is not a group,");
    return;
  fi;

  return MappingByFunction(h, SchutzenbergerGroup(h), 
   AsPermutation, x-> One(h)*x);
end);

# new for 1.0! - IsRegularClass@ - "class, value, scc, o"
#############################################################################

InstallGlobalFunction(IsRegularClass@, 
function(x, value, scc, o, onright)
  local s, data, tester, i;

  if HasNrIdempotents(x) then 
    return NrIdempotents(x)<>0;
  fi;

  s:=ParentSemigroup(x);
  
  if HasSemigroupDataIndex(x) then
    data:=SemigroupData(s);
    if data!.repslens[data!.orblookup1[SemigroupDataIndex(x)]]>1 then
      return false;
    fi;
  fi; 
  
  # is x the group of units...
  if ActionRank(Representative(x))=LambdaDegree(s) then
    return true;
  fi;   
 
  tester:=IdempotentLambdaRhoTester(s);
  
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

# new for 1.0! - IsRegularClass - "for an D-class of an acting semi"
#############################################################################

# not required for regular/inverse

InstallMethod(IsRegularClass, "for an D-class of an acting semigp",
[IsGreensDClass and IsActingSemigroupGreensClass],
d-> IsRegularClass@(d, RhoFunc(ParentSemigroup(d))(Representative(d)),
LambdaOrbSCC(d), LambdaOrb(d), true));

# new for 1.0! - IsRegularClass - "for an H-class of an acting semigp."
############################################################################

# no method required for regular/inverse

InstallOtherMethod(IsRegularClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
h-> IsRegularClass(RClassOfHClass(h)));

# new for 1.0! - IsRegularClass - "for an L-class of an acting semi"
#############################################################################

# not required for regular/inverse

InstallMethod(IsRegularClass, "for an L-class of an acting semigp",
[IsGreensLClass and IsActingSemigroupGreensClass],
l-> IsRegularClass@(l, LambdaFunc(ParentSemigroup(l))(Representative(l)),
RhoOrbSCC(l), RhoOrb(l), false));

# new for 1.0! - IsRegularClass - "for an R-class of an acting semi"
#############################################################################

# not required for regular/inverse

InstallMethod(IsRegularClass, "for an R-class of an acting semigp",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> IsRegularClass@(r, RhoFunc(ParentSemigroup(r))(Representative(r)),
LambdaOrbSCC(r), LambdaOrb(r), true));

#OOO

# new for 1.0! - OneMutable - "for an H-class of an acting semigp."
############################################################################

# same method for regular/inverse

InstallOtherMethod(OneMutable, "for a H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass], 
function(h)

  if not IsGroupHClass(h) then
    Error("the H-class is not a group,");
    return;        
  fi;
  return Idempotents(h)[1];
end);

#NNN

# mod for 1.0! - NrDClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(NrDClasses, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
s-> Length(OrbSCC(SemigroupData(s)))-1);

# mod for 1.0! - NrRegularDClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(NrRegularDClasses, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local data, datascc, rhofunc, tester, nr, r, x, o, scc, rho, i, j;
  
  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  datascc:=OrbSCC(data);
  
  rhofunc:=RhoFunc(s);
  tester:=IdempotentLambdaRhoTester(s);
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

# mod for 1.0! - NrHClasses - "for an acting semigroup"
#############################################################################
 
# different method for regular/inverse

InstallMethod(NrHClasses, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  return Sum(List(GreensDClasses(s), NrHClasses));
end);

# mod for 1.0! - NrHClasses - "for a D-class of an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallOtherMethod(NrHClasses, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return NrRClasses(d)*NrLClasses(d);
end);

# new for 0.1! - NrHClasses - "for an L-class of an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallOtherMethod(NrHClasses, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
l-> NrRClasses(DClassOfLClass(l)));

# new for 0.1! - NrHClasses - "for an R-class of an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallOtherMethod(NrHClasses, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> NrLClasses(DClassOfRClass(r)));

# mod for 1.0! - NrLClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

# could do better not to create the D-classes. Maybe not, we must store the
# schutz gp of the D-class somewhere and so it might as well be the D-class.

InstallMethod(NrLClasses, "for an acting semigroup",
[IsActingSemigroup], s-> Sum(List(GreensDClasses(s), NrLClasses)));

# mod for 1.0! - NrLClasses - "for a D-class of an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallOtherMethod(NrLClasses, "for a D-class of an acting semigroup",       
[IsActingSemigroupGreensClass and IsGreensDClass],
function(d)
  return Length(LambdaCosets(d))*Length(LambdaOrbSCC(d));
end);

# mod for 1.0! - NrRClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(NrRClasses, "for an acting semigroup",       
[IsActingSemigroup and HasGeneratorsOfSemigroup],        
function(s)
  local data;
  
  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  return Length(data!.orbit)-1;
end);

# mod for 1.0! - NrRClasses - "for a D-class of an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallOtherMethod(NrRClasses, "for a D-class of an acting semigroup",       
[IsActingSemigroupGreensClass and IsGreensDClass],
d-> Length(RhoCosets(d))*Length(RhoOrbSCC(d)));


# new for 1.0! - NrIdempotents@ - not a user function 
#############################################################################

InstallGlobalFunction(NrIdempotents@, 
function(x, value, scc, o, onright)
  local s, data, nr, tester, i;

  if HasIsRegularClass(x) and not IsRegularClass(x) then 
    return 0;
  fi;

  s:=ParentSemigroup(x);     

  # check if we already know this...
  if HasSemigroupDataIndex(x) and not (HasIsRegularClass(x) and
   IsRegularClass(x)) then
    data:=SemigroupData(s);
    if data!.repslens[data!.orblookup1[SemigroupDataIndex(x)]]>1 then
      return 0;
    fi;
  fi;

  # is r the group of units...
  if ActionRank(Representative(x))=LambdaDegree(s) then
    return 1;
  fi;

  nr:=0;
  tester:=IdempotentLambdaRhoTester(s);

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

# new for 1.0! - NrIdempotents - "for a D-class of an acting semigp"
#############################################################################

# same method for regular/inverse

InstallOtherMethod(NrIdempotents, "for a D-class of an acting semigroup",
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

# new for 1.0! - NrIdempotents - "for a H-class of an acting semigp"
#############################################################################

# same method for regular/inverse

InstallOtherMethod(NrIdempotents, "for a H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  if IsGroupHClass(h) then 
    return 1;
  fi;
  return 0;
end);

# new for 1.0! - NrIdempotents - "for an L-class of an acting semigp."
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(NrIdempotents, "for an L-class of an acting semigp.",
[IsGreensLClass and IsActingSemigroupGreensClass],
l-> NrIdempotents@(l, LambdaFunc(ParentSemigroup(l))(Representative(l)),
RhoOrbSCC(l), RhoOrb(l), false));

# new for 1.0! - NrIdempotents - "for an R-class of an acting semigp."
#############################################################################

# same method for regular, different method inverse

InstallOtherMethod(NrIdempotents, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> NrIdempotents@(r, RhoFunc(ParentSemigroup(r))(Representative(r)), LambdaOrbSCC(r), LambdaOrb(r), true));

# new for 0.1! - NrIdempotents - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(NrIdempotents, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local data, reps, repslookup, lenreps, repslens, rhofunc, tester, nr, f, m,
  o, rho, i, k;

  if HasIdempotents(s) then 
    return Length(Idempotents(s));
  fi;

  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  
  reps:=data!.reps; 
  repslookup:=data!.repslookup;
  lenreps:=data!.lenreps;
  repslens:=data!.repslens;

  rhofunc:=RhoFunc(s);
  tester:=IdempotentLambdaRhoTester(s);

  nr:=0;
  for i in [1..lenreps] do 
    f:=reps[i][1]; 
    m:=data[repslookup[i][1]][2];
    o:=data[repslookup[i][1]][3];
    rho:=rhofunc(f);
    for k in OrbSCC(o)[m] do 
      if tester(o[k], rho) then 
        nr:=nr+repslens[i];
      fi;
    od;
  od;

  return nr;
end);

#PPP

# mod for 1.0! - PartialOrderOfDClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(PartialOrderOfDClasses, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local d, n, out, gens, data, graph, datalookup, o, lambdafunc, rhofunc, scc, lookup, lambdarhoht, lambdaperm, repslookup, schutz, mults, reps, repslens, ht, l, m, val, j, f, i, x, k;

  d:=GreensDClasses(s); 
  n:=Length(d);
  out:=List([1..n], x-> EmptyPlist(n));
  
  data:=SemigroupData(s);
  gens:=data!.gens;
  graph:=data!.graph;
  datalookup:=OrbSCCLookup(data)-1;  
  reps:=data!.reps;
  repslens:=data!.repslens;
  ht:=data!.ht;
  repslookup:=data!.repslookup;
  
  lambdafunc:=LambdaFunc(s);
  rhofunc:=RhoFunc(s);
  lambdarhoht:=LambdaRhoHT(s);
  lambdaperm:=LambdaPerm(s);
  
  o:=LambdaOrb(s);
  scc:=OrbSCC(o); 
  lookup:=OrbSCCLookup(o);
  schutz:=o!.schutzstab;
  mults:=o!.mults;

  for i in [1..n] do
    for x in gens do
      # collect info about left multiplying R-class reps of d[i] by gens
      for j in SemigroupDataSCC(d[i]) do 
        for k in graph[j] do  
          AddSet(out[i], datalookup[k]);
        od;
      od;

      for f in LClassReps(d[i]) do
        # the below is an expanded version of Position(data, f * x)
        f:=f*x;
        l:=Position(o, lambdafunc(f));
        m:=lookup[l];
        val:=HTValue(lambdarhoht, Concatenation([m], rhofunc(f)));
        if not IsBound(schutz[m]) then 
          LambdaOrbSchutzGp(o, m);
        fi;
        if schutz[m]=true then 
          j:=repslookup[val][1];
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
              if SiftedPermutation(schutz[m], lambdaperm(reps[val][n], f))=()
               then 
                j:=repslookup[val][n];
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

# new for 1.0! - Random - "for an acting semigroup"
#############################################################################

# different method for inverse/regular

InstallMethod(Random, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local data, gens, n, i, w, m, o, rep, g;
  
  data:=SemigroupData(s);
  
  if not IsClosed(data) then
    gens:=GeneratorsOfSemigroup(s);
    n:=LambdaDegree(s);
    i:=Random([1..2*Length(gens)]);
    w:=List([1..i], x-> Random([1..Length(gens)]));
    return EvaluateWord(gens, w);
  fi;

  n:=Random([2..Length(data)]);
  m:=data[n][2]; o:=data[n][3]; rep:=data[n][4];

  g:=Random(LambdaOrbSchutzGp(o, m));
  i:=Random(OrbSCC(o)[m]);
  return rep*g*LambdaOrbMult(o, m, i)[1];
end);

#SSS

# new for 1.0! - SchutzenbergerGroup - "for a D-class of an acting semigroup"
#############################################################################

# different method for regular/inverse

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
 
  if LambdaFunc(ParentSemigroup(d))(RhoOrbRep(o,m))
   <>LambdaFunc(ParentSemigroup(d))(Representative(d)) then 
    #JDM not sure that something isn't missing here. If the group element
    #corresponding to RhoOrbRep(o, m) and  Representative(d) are not 
    #equal then this could return the wrong answer.
    #In particular, this ought to work without the if statement here, but it
    #does not.
    p:=LambdaConjugator(ParentSemigroup(d))(RhoOrbRep(o, m), Representative(d));
    rho_schutz:=rho_schutz^p;
  fi;

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

# new for 1.0! - SchutzenbergerGroup - "for a H-class of an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(SchutzenbergerGroup, "for a H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local lambda_o, lambda_m, lambda_schutz, lambda_stab, rho_o, rho_m,
  rho_schutz, rho_stab, rep, s, lambda_p, rho_p;
 
  lambda_o:=LambdaOrb(h); lambda_m:=LambdaOrbSCCIndex(h);
  lambda_schutz:=LambdaOrbSchutzGp(lambda_o, lambda_m); 
  s:=ParentSemigroup(h);
  
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

# new for 1.0! - SchutzenbergerGroup - "for an L-class of an acting semigp."
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(SchutzenbergerGroup, "for an L-class of an acting semigp.",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, p;

  o:=RhoOrb(l); m:=RhoOrbSCCIndex(l);
  
  if not IsGreensClassNC(l) then 
    #JDM maybe no need to do this if we know how l is created from a D-class
    p:=LambdaConjugator(ParentSemigroup(l))(RhoOrbRep(o, m),
     Representative(l));
    return RhoOrbSchutzGp(o, m, infinity)^p;
  fi;
  return RhoOrbSchutzGp(o, m, infinity); 
end);

# new for 1.0! - SchutzenbergerGroup - "for an R-class of an acting semigp."
#############################################################################

# same method for regular/inverse

InstallOtherMethod(SchutzenbergerGroup, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> LambdaOrbSchutzGp(LambdaOrb(r), LambdaOrbSCCIndex(r)));

# new for 1.0! - Size - "for a D-class of an acting semigp."
#############################################################################

# different method for inverse/regular.

InstallOtherMethod(Size, "for a D-class of an acting semigp.",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local l, r;
 
  l:=LambdaOrbSchutzGp(LambdaOrb(d), LambdaOrbSCCIndex(d));
  r:=RhoOrbSchutzGp(RhoOrb(d), RhoOrbSCCIndex(d), infinity);
  return Size(r)*Size(l)*Length(LambdaOrbSCC(d))*Length(RhoOrbSCC(d))/
   Size(SchutzenbergerGroup(d));
end);

# new for 1.0! - Size - "for a H-class of an acting semigp."
#############################################################################

# same method for inverse/regular

InstallOtherMethod(Size, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
h-> Size(SchutzenbergerGroup(h)));

# new for 1.0! - Size - "for an R-class of an acting semigp."
#############################################################################
# Algorithm C. 

# same method for inverse/regular

InstallOtherMethod(Size, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
r-> Size(SchutzenbergerGroup(r))*Length(LambdaOrbSCC(r)));

# new for 1.0! - Size - "for an L-class of an acting semigp."
#############################################################################
# Algorithm C. 

# same method for regular, different method of inverse

InstallOtherMethod(Size, "for an L-class of an acting semigp.",
[IsGreensLClass and IsActingSemigroupGreensClass],
l-> Size(SchutzenbergerGroup(l))*Length(RhoOrbSCC(l)));

# new for 0.1! - StructureDescription - "for group H-class of acting semi"
############################################################################

#same method for inverse/regular

InstallOtherMethod(StructureDescription, "for group H-class of acting semi",
[IsGreensHClass and IsActingSemigroupGreensClass and IsGroupHClass],
h-> StructureDescription(Range(IsomorphismPermGroup(h))));

# technical
##############################################################################
##############################################################################

# new for 1.0! - \= - "for Green's class and Green's class of acting semigp"
#############################################################################

InstallMethod(\=, "for Green's class and class of acting semigp",
[IsActingSemigroupGreensClass, IsActingSemigroupGreensClass],
function(x, y)
  if (IsGreensRClass(x) and IsGreensRClass(y)) or
   (IsGreensLClass(x) and IsGreensLClass(y)) or
   (IsGreensDClass(x) and IsGreensDClass(y)) or
   (IsGreensHClass(x) and IsGreensHClass(y)) then
    return ParentSemigroup(x)=ParentSemigroup(y) and Representative(x) in y;
  fi;
  return ParentSemigroup(x)=ParentSemigroup(y) and Representative(x) in y and
   Size(x)=Size(y);
end);

# new for 1.0! - \< - "for Green's class and Green's class of acting semigp"
#############################################################################

InstallMethod(\<, "for Green's class and class of acting semigp",
[IsActingSemigroupGreensClass, IsActingSemigroupGreensClass],
function(x, y)
  if (IsGreensRClass(x) and IsGreensRClass(y)) or
   (IsGreensLClass(x) and IsGreensLClass(y)) or
   (IsGreensDClass(x) and IsGreensDClass(y)) or
   (IsGreensHClass(x) and IsGreensHClass(y)) then
    return ParentSemigroup(x)=ParentSemigroup(y) and Representative(x) <
     Representative(y) and (not Representative(x) in y);
  fi;
  return fail;
end);

# new for 1.0! - CreateDClassNC - not a user function! 
############################################################################# 
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
          
  SetParentSemigroup(d, arg[1]);
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

# new for 1.0! - CreateHClass - not a user function! 
############################################################################# 
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
  SetParentSemigroup(h, arg[1]);

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

# mod for 1.0! - CreateLClass - not a user function!
#############################################################################
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
  rectify:=RectifyRho(s, o, rep, RhoPos(o), m);
  return CreateLClassNC(s, rectify.m, o, rectify.rep, nc);
end);

# mod for 1.0! - CreateLClassNC - not a user function!
#############################################################################
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
  SetParentSemigroup(l, s);
  SetRepresentative(l, rep);
  SetRhoOrb(l, o);
  SetRhoOrbSCCIndex(l, m);
  SetEquivalenceClassRelation(l, GreensLRelation(s));
  SetIsGreensClassNC(l, nc); 
  return l;
end);

# mod for 1.0! - CreateRClass - not a user function!
#############################################################################
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
  rectify:=RectifyLambda(s, o, rep, LambdaPos(o), m);
  return CreateRClassNC(s, rectify.m, o, rectify.rep, nc);
end);

# mod for 1.0! - CreateRClassNC - not a user function!
#############################################################################
# Usage: 
# arg[1] = semigroup; 
# arg[2] = lambda orb scc index;
# arg[3] = lambda orb; 
# arg[4] = rep; 
# arg[5] = IsRClassNC.
# arg[6] = semigroup data index (optional).

# NC indicates that the representative is assumed to be in the correct form,
# i.e. RhoFunc(s)(arg[2]) is in the first place of the scc of the rho orb. 

# same method for regular/inverse

InstallGlobalFunction(CreateRClassNC,
function(arg)
  local r;
  
  r:=Objectify(RClassType(arg[1]), rec());

  SetParentSemigroup(r, arg[1]);
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

# mod for 1.0! - DClass - "for an acting semi and elt or Green's class"
#############################################################################

InstallGlobalFunction(DClass,
function(arg)

  if Length(arg)=2 and IsActingSemigroup(arg[1]) and IsAssociativeElement(arg[2]) then
    return GreensDClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensRClass(arg[1]) then
    return DClassOfRClass(arg[1]);
  elif Length(arg)=1 and IsGreensLClass(arg[1]) then
    return DClassOfLClass(arg[1]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then
    return DClassOfHClass(arg[1]);
  fi;

  Error("usage: for acting semigroup and acting elt, or a Green's class,");
  return;
end);

# mod for 1.0! - DClassNC - "for an acting semigroup and acting elt"
#############################################################################

InstallGlobalFunction(DClassNC,
function(arg)

  if Length(arg)=2 and IsActingSemigroup(arg[1]) and IsAssociativeElement(arg[2]) then
    return GreensDClassOfElementNC(arg[1], arg[2]);
  fi;

  Error("usage: acting semigroup and acting elt,");
  return;
end);

# new for 0.1! - HClass - "for acting semi or Green's class, and acting elt
#############################################################################

InstallGlobalFunction(HClass,
function(arg)

  if Length(arg)=2 and (IsGreensClass(arg[1]) or IsActingSemigroup(arg[1])) and
   IsAssociativeElement(arg[2]) then 
    return GreensHClassOfElement(arg[1], arg[2]);
  fi;

  Error("usage: acting semigroup or Green's class, and acting element,");
  return;
end);

# new for 0.1! - HClassNC - "for acting semi or Green's class, and acting elt
#############################################################################

InstallGlobalFunction(HClassNC,
function(arg)

  if Length(arg)=2 and (IsGreensClass(arg[1]) or IsActingSemigroup(arg[1])) and
   IsAssociativeElement(arg[2]) then 
    return GreensHClassOfElementNC(arg[1], arg[2]);
  fi;

  Error("usage: acting semigroup or Green's class, and acting element,");
  return;
end);

# mod for 1.0! - LClass 
#############################################################################

InstallGlobalFunction(LClass,
function(arg)

  if Length(arg)=2 and (IsActingSemigroup(arg[1]) or IsGreensDClass(arg[1]))
   and IsAssociativeElement(arg[2]) then  
    return GreensLClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then
    return LClassOfHClass(arg[1]);
  fi;

  Error("usage: (acting semigroup or D-class, and acting element) or H-class,");
  return;
end);

# new for 0.1! - LClassNC
#############################################################################

InstallGlobalFunction(LClassNC,
function(arg)

  if Length(arg)=2 and (IsActingSemigroup(arg[1]) or IsGreensDClass(arg[1]))
   and IsAssociativeElement(arg[2]) then  
    return GreensLClassOfElementNC(arg[1], arg[2]);
  fi;

  Error("usage: acting semigroup or D-class, and acting element,");
  return;
end);

# new for 0.1! - RClass 
#############################################################################

InstallGlobalFunction(RClass,
function(arg)

  if Length(arg)=2 and (IsActingSemigroup(arg[1]) or IsGreensDClass(arg[1]))
   and IsAssociativeElement(arg[2]) then 
    return GreensRClassOfElement(arg[1], arg[2]);
  elif Length(arg)=1 and IsGreensHClass(arg[1]) then
    return RClassOfHClass(arg[1]);
  fi;
  
  Error("usage: (acting semigroup or D-class, and acting element) or H-class,");
  return;
end);

# new for 0.1! - RClassNC
#############################################################################

InstallGlobalFunction(RClassNC,
function(arg)

  if Length(arg)=2 and (IsActingSemigroup(arg[1]) or IsGreensDClass(arg[1]))
   and IsAssociativeElement(arg[2]) then 
    return GreensRClassOfElementNC(arg[1], arg[2]);
  fi;
  
  Error("usage: acting semigroup or D-class, and acting element,");
  return;
end);

# new for 1.0! - DClassType - "for an acting semigroup"
############################################################################# 

# different method for regular/inverse

InstallMethod(DClassType, "for an acting semigroups",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
          IsEquivalenceClassDefaultRep and IsGreensDClass and
          IsActingSemigroupGreensClass);
end);

# mod for 1.0! - HClassType - not a user function!
############################################################################

# different method for regular/inverse

InstallMethod(HClassType, "for a transformation semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s);
 return NewType( FamilyObj( s ), IsEquivalenceClass and
  IsEquivalenceClassDefaultRep and IsGreensHClass and
  IsActingSemigroupGreensClass);
end);

# mod for 1.0! - LClassType - "for an acting semigroup"
############################################################################

# different method for regular/inverse

InstallMethod(LClassType, "for an acting semigroup",
[IsActingSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensLClass and
         IsActingSemigroupGreensClass);
end);

# new for 1.0! - RClassType - "for an acting semigroup"
############################################################################

# different method for regular/inverse

InstallMethod(RClassType, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensRClass and
         IsActingSemigroupGreensClass);
end);

# same method for regular/inverse

InstallOtherMethod(GreensJClassOfElement, "for acting semigroup and elt.",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement], 
GreensDClassOfElement);

InstallMethod(IsRegularDClass, "for a D-class of acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass], IsRegularClass);

InstallOtherMethod(IsActingSemigroup, "for a Green's class",
[IsGreensClass], ReturnFalse);

InstallOtherMethod(IsActingSemigroupWithInverseOp, "for a Green's class",
[IsGreensClass], ReturnFalse);

InstallOtherMethod(IsActingSemigroupWithInverseOp, "for an acting semigroup",
[IsActingSemigroup], ReturnFalse);

InstallOtherMethod(IsGreensClass, "for an object", [IsObject], ReturnFalse);

InstallOtherMethod(IsGreensRClass, "for an object", [IsObject], ReturnFalse);

InstallOtherMethod(IsGreensLClass, "for an object", [IsObject], ReturnFalse);

InstallOtherMethod(IsGreensHClass, "for an object", [IsObject], ReturnFalse);

InstallOtherMethod(IsGreensDClass, "for an object", [IsObject], ReturnFalse);

InstallMethod(IsGreensClassOfTransSemigp, "for a Green's class",
[IsGreensClass], x-> IsTransformationSemigroup(ParentAttr(x)));

InstallMethod(IsGreensClassOfTransSemigp, "for a Green's class",
[IsActingSemigroupGreensClass], x->
IsTransformationSemigroup(ParentSemigroup(x)));

InstallOtherMethod(IsGroupHClass, "for an acting semi Green's class",
[IsActingSemigroupGreensClass], ReturnFalse);

#EOF
