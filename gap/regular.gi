#############################################################################
##
#W  regular.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## Methods for Green's classes of regular acting semigroups

# new for 1.0! - \in - "for a regular acting semigroup and elt"
##############################################################################

InstallMethod(\in, "for an acting elt and regular acting semigroup",
[IsActingElt, IsActingSemigroup and IsRegularSemigroup], 
function(f, s)
  local lambda_o, rho_o, lambda, rho, lambda_l, rho_l, m, schutz, scc, g;
  
  if not ElementsFamily(FamilyObj(s))=FamilyObj(f) then 
    Error("the element and semigroup are not of the same type,");
    return;
  fi;

  if HasAsSSortedList(s) then 
    return f in AsSSortedList(s); 
  fi;

  lambda_o:=LambdaOrb(s);
  rho_o:=RhoOrb(s);
  lambda:=LambdaFunc(s)(f);
  rho:=RhoFunc(s)(f); 
  lambda_l:=Position(lambda_o, lambda);
  rho_l:=Position(rho_o, rho);

  if IsClosed(lambda_o) and (lambda_l=fail or (lambda_l=1 and
   ActingSemigroupModifier(s)=1)) then
      return false;
  elif IsClosed(rho_o) and (rho_l=fail or (rho_l=1 and 
   ActingSemigroupModifier(s)=1)) then
      return false;
  fi;
  
  if lambda_l=fail then
    lambda_o!.looking:=true; 
    lambda_o!.lookingfor:=function(o, x) return x=lambda; end;
    lambda_o!.lookfunc:=lambda_o!.lookingfor;
    Enumerate(lambda_o);
    lambda_l:=PositionOfFound(lambda_o);
    lambda_o!.found:=false; lambda_o!.looking:=false;
    Unbind(lambda_o!.lookingfor); Unbind(lambda_o!.lookfunc);

    if lambda_l=false then
      return false;
    fi;
  fi;

  if lambda_l=1 and ActingSemigroupModifier(s)=1 then
    return false;
  fi;

  if rho_l=fail then
    rho_o!.looking:=true; 
    rho_o!.lookingfor:=function(o, x) return x=rho; end;
    rho_o!.lookfunc:=rho_o!.lookingfor;
    Enumerate(rho_o);
    rho_l:=PositionOfFound(rho_o);
    rho_o!.found:=false; rho_o!.looking:=false;
    Unbind(rho_o!.lookingfor); Unbind(rho_o!.lookfunc);

    if rho_l=false then
      return false;
    fi;
  fi;

  if not IsClosed(lambda_o) then  
    lambda_o:=GradedLambdaOrb(s, f, false);
    Enumerate(lambda_o, infinity);
    lambda_l:=1;
  fi;
 
  m:=OrbSCCLookup(lambda_o)[lambda_l];
  schutz:=LambdaOrbStabChain(lambda_o, m);

  if schutz=true then
    return true;
  fi;

  scc:=OrbSCC(lambda_o)[m];
  g:=f;
  
  if lambda_l<>scc[1] then 
    g:=g*LambdaOrbMults(lambda_o, m)[lambda_l][2];
  fi;

  if IsIdempotent(g) then 
    return true;
  elif schutz=false then
    return false;
  fi;

  return SiftedPermutation(schutz, LambdaPerm(s)(One(g), g))=();
end);

# new for 1.0! - \in - "for acting elt and D-class of regular acting semigp"
#############################################################################
#JDM revise this if revising \in for elt and D-class in greens.gi

InstallMethod(\in, "for acting elt and D-class of acting semigp.",
[IsActingElt, IsGreensDClass and IsRegularActingSemigroupGreensClass],
function(f, d)
  local rep, s, g, m, o, scc, l, schutz;

  rep:=Representative(d);
  s:=ParentSemigroup(d);

  # much much better performance using f[2]<>rep[2] below
  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or f[2] <> rep[2] then
    return false;
  fi;

  g:=f;

  m:=LambdaOrbSCCIndex(d); o:=LambdaOrb(d); scc:=OrbSCC(o);

  l:=Position(o, LambdaFunc(s)(g));

  if l = fail or OrbSCCLookup(o)[l]<>m then
    return false;
  fi;

  if l<>scc[m][1] then
    g:=g*LambdaOrbMults(o, m)[l][2];
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
    g:=RhoOrbMults(o, m)[l][2]*g;
  fi;

  if g=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;

  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g))=();
end);

# new for 1.0! - DClassReps - "for a regular acting semigroup"
##############################################################################

InstallOtherMethod(DClassReps, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  local lambda_o, r, i, out, rho_o, scc, lookup, rhofunc, f, l, m;

  lambda_o:=LambdaOrb(s);
  r:=Length(OrbSCC(lambda_o));
  
  i:=ActingSemigroupModifier(s);
  out:=EmptyPlist(r);
  
  rho_o:=RhoOrb(s);
  scc:=OrbSCC(rho_o);
  lookup:=OrbSCCLookup(rho_o);
  rhofunc:=RhoFunc(s);

  for m in [1..r-i] do 
    f:=LambdaOrbRep(lambda_o, m+i);
    l:=Position(rho_o, rhofunc(f));

    if l<>scc[lookup[l]][1] then 
      f:=RhoOrbMults(rho_o, lookup[l])[l][2]*f;
    fi;
    out[m]:=f;
  od;
  return out;
end);

# new for 1.0! - DClassType - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallOtherMethod(DClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsRegularDClass and IsGreensDClass 
         and IsRegularActingSemigroupGreensClass);
end);

# new for 1.0! - HClassType - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallOtherMethod(HClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGroupHClass and IsGreensHClass and
         IsRegularActingSemigroupGreensClass);
end);

# new for 1.0! - LClassType - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallOtherMethod(LClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsRegularLClass and IsGreensLClass and
         IsRegularActingSemigroupGreensClass);
end);

# new for 1.0! - RClassType - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallOtherMethod(RClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsRegularRClass and IsGreensRClass and
         IsRegularActingSemigroupGreensClass);
end);

# new for 1.0! - NrDClasses - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallMethod(NrDClasses, "for a regular acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup and IsRegularSemigroup],
function(s)
  local o;
  o:=Enumerate(LambdaOrb(s), infinity);
  return Length(OrbSCC(o))-ActingSemigroupModifier(s);
end);

# new for 1.0! - NrHClasses - "for a regular acting semigroup"
############################################################################

# different method for inverse semigroups

InstallMethod(NrHClasses, "for a regular acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup and IsRegularSemigroup],
function(s)
  local lambda_o, rho_o, nr, lambda_scc, rho_scc, r, i, rhofunc, lookup, rho, m;
  
  lambda_o:=LambdaOrb(s);
  if not IsClosed(lambda_o) then 
    Enumerate(lambda_o, infinity);
  fi;
  
  rho_o:=LambdaOrb(s);
  if not IsClosed(rho_o) then 
    Enumerate(rho_o, infinity);
  fi;
  
  nr:=0;
  lambda_scc:=OrbSCC(lambda_o);
  rho_scc:=OrbSCC(rho_o);
  r:=Length(lambda_scc);
  i:=ActingSemigroupModifier(s);
  rhofunc:=RhoFunc(s);
  lookup:=OrbSCCLookup(rho_o);

  for m in [1+i..r] do 
    rho:=rhofunc(LambdaOrbRep(lambda_o, m));
    nr:=nr+Length(lambda_scc[m])*Length(rho_scc[lookup[Position(rho_o, rho)]]);
  od;

  return nr;
end);

# new for 1.0! - NrHClasses - "for a D-class of regular acting semigroup"
############################################################################

# same method for inverse semigroups (although there could be one if we
# have to introduce IsInverseActingSemigroupGreensClass).

InstallOtherMethod(NrHClasses, "for a D-class of regular acting semigroup",
[IsRegularActingSemigroupGreensClass and IsGreensDClass],
d-> Length(LambdaOrbSCC(d))*Length(RhoOrbSCC(d)));

# new for 1.0! - NrHClasses - "for a L-class of regular acting semigroup"
############################################################################

# same method for inverse semigroups 

InstallOtherMethod(NrHClasses, "for a L-class of regular acting semigroup",
[IsRegularActingSemigroupGreensClass and IsGreensLClass],
function(l)
   return Length(Enumerate(RhoOrb(l), infinity));
end);

# new for 1.0! - NrHClasses - "for a R-class of regular acting semigroup"
############################################################################

# same method for inverse semigroups (although there could be one if we
# have to introduce IsInverseActingSemigroupGreensClass).

InstallOtherMethod(NrHClasses, "for a R-class of regular acting semigroup",
[IsRegularActingSemigroupGreensClass and IsGreensRClass],
r-> Length(LambdaOrbSCC(r)));

# new for 1.0! - NrLClasses - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallMethod(NrLClasses, "for a regular acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup and IsRegularSemigroup],
function(s)
  local o;
  o:=Enumerate(LambdaOrb(s), infinity);
  return Length(o)-ActingSemigroupModifier(s);
end);

# new for 1.0! - NrLClasses - "for a D-class of regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallOtherMethod(NrLClasses, "for a D-class of regular acting semigroup",
[IsRegularActingSemigroupGreensClass and IsGreensDClass],
d-> Length(LambdaOrbSCC(d)));

# new for 1.0! - NrRClasses - "for a regular acting semigroup"
############################################################################

# different method for inverse semigroups

InstallMethod(NrRClasses, "for a regular acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup and IsRegularSemigroup],
function(s)
  local o;
  o:=Enumerate(RhoOrb(s), infinity);
  return Length(o)-ActingSemigroupModifier(s);
end);

# new for 1.0! - NrRClasses - "for a D-class of regular acting semigroup"
############################################################################

# same method for inverse semigroups (although there could be one if we
# have to introduce IsInverseActingSemigroupGreensClass).

InstallOtherMethod(NrRClasses, "for a D-class of regular acting semigroup",
[IsRegularActingSemigroupGreensClass and IsGreensDClass],
d-> Length(RhoOrbSCC(d)));

# new for 1.0! - NrIdempotents - for a regular acting semigroup
############################################################################

# different method for inverse

InstallOtherMethod(NrIdempotents, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  local nr, tester, rho_o, scc, lambda_o, gens, rhofunc, lookup, rep, rho, j,
  i, k;

  nr:=0;
  tester:=IdempotentLambdaRhoTester(s);
  rho_o:=RhoOrb(s);
  scc:=OrbSCC(rho_o); 
  lambda_o:=LambdaOrb(s);
  gens:=lambda_o!.gens;
  rhofunc:=RhoFunc(s);
  lookup:=OrbSCCLookup(rho_o);

  for i in [1..Length(lambda_o)] do
    rep:=EvaluateWord(TraceSchreierTreeForward(lambda_o, i), gens);
    rho:=rhofunc(rep);
    j:=lookup[Position(rho_o, rho)];
    for k in scc[j] do
      if tester(lambda_o[i], rho_o[k]) then
        nr:=nr+1;
      fi;
    od;
  od;

  return nr;
end);

# new for 1.0! - NrRegularDClasses - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallMethod(NrRegularDClasses, "for a regular acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup and IsRegularSemigroup],
NrDClasses);

# new for 1.0! - Size - "for a regular acting semigroup"
############################################################################

# different method for inverse semigroups

InstallMethod(Size, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  local lambda_o, rho_o, nr, lambda_scc, rho_scc, r, i, rhofunc, lookup, rho, m;

  lambda_o:=LambdaOrb(s);
  if not IsClosed(lambda_o) then 
    Enumerate(lambda_o, infinity);
  fi;
  
  rho_o:=LambdaOrb(s);
  if not IsClosed(rho_o) then 
    Enumerate(rho_o, infinity);
  fi;
  
  nr:=0;
  lambda_scc:=OrbSCC(lambda_o);
  rho_scc:=OrbSCC(rho_o);
  r:=Length(lambda_scc);
  i:=ActingSemigroupModifier(s);
  rhofunc:=RhoFunc(s);
  lookup:=OrbSCCLookup(rho_o);

  for m in [1+i..r] do 
    rho:=rhofunc(LambdaOrbRep(lambda_o, m));
    nr:=nr+Length(lambda_scc[m])*Size(LambdaOrbSchutzGp(lambda_o,m))*
     Length(rho_scc[lookup[Position(rho_o, rho)]]);
  od;

  return nr;
end);


