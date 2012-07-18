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
  local lambda_o, rho_o, lambda, rho, lambda_l, rho_l, m, schutz, scc, g, rep;

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

  #if not IsClosed(lambda_o) then
  #  lambda_o:=GradedLambdaOrb(s, f, false);
  #  lambda_l:=1;
  #fi;

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

  rep:=CanonicalRep(lambda_o, m);
  m:=OrbSCCLookup(rho_o)[rho_l];
  g:=RhoOrbMults(rho_o, m)[rho_l][2]*g;

  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g))=();
end);

# new for 1.0! - \in - "for acting elt and D-class of regular acting semigp"
#############################################################################
#JDM revise this if revising \in for elt and D-class in greens.gi

InstallMethod(\in, "for acting elt and D-class of regular acting semigp.",
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

# new for 1.0! - CanonicalRep - "for a lambda orb and scc index"
##############################################################################

InstallGlobalFunction(CanonicalRep, 
function(o, m)
  local f, s, rho_o, l;

  if IsBound(o!.reps) then 
    if IsBound(o!.reps[m]) then 
      return o!.reps;
    fi;
  else
    o!.reps:=EmptyPlist(Length(OrbSCC(o)));
  fi;

  f:=LambdaOrbRep(o, m);
  s:=o!.semi;   
  rho_o:=Enumerate(RhoOrb(s), infinity);
  l:=Position(rho_o, RhoFunc(s)(f));
  m:=OrbSCCLookup(rho_o)[l];

  if l<>OrbSCC(rho_o)[m][1] then
    f:=RhoOrbMults(rho_o, m)[l][2]*f;
  fi;
  
  o!.reps[m]:=f;
  return f;
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

# mod for 1.0! - Enumerator - "for D-class of regular acting semigp."
#############################################################################

# same method for inverse

#JDM could write another method if nec.

InstallOtherMethod(Enumerator, "for a D-class of acting semigp.",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
    
    return EnumeratorByFunctions(d, rec(

    schutz:=Enumerator(SchutzenbergerGroup(d)),

    #######################################################################

    ElementNumber:=function(enum, pos)
      local l_scc, r_scc, i, j, k, x, y, z, lmults, rmults;
      if pos>Length(enum) then
        return fail;
      fi;

      if pos<=Length(enum!.schutz) then 
        return Representative(d)*enum!.schutz[pos];
      fi;

      l_scc:=LambdaOrbSCC(d);
      r_scc:=RhoOrbSCC(d);

      pos:=pos-1; 
      
      i:=Length(enum!.schutz); 
      j:=Length(l_scc);
      k:=Length(r_scc);

      x:=QuoInt(pos, i*j); 
      y:=QuoInt(pos-x*i*j, i);
      z:=pos-x*i*j-y*i;

      lmults:=LambdaOrbMults(LambdaOrb(d), LambdaOrbSCCIndex(d));
      rmults:=RhoOrbMults(RhoOrb(d), RhoOrbSCCIndex(d));
      
      return rmults[r_scc[x+1]][1]*enum[z+1]*lmults[l_scc[y+1]][1];
    end,

    #######################################################################

    NumberElement:=function(enum, f)
      local rep, s, o, m, x, y, z, i, j, k, g; 
      
      rep:=Representative(d);

      if f[2]<>rep[2] then
        return fail;
      fi;

      if f=rep then
        return 1;
      fi;

      s:=ParentSemigroup(d);
      
      o:=RhoOrb(d); m:=RhoOrbSCCIndex(d);
      x:=Position(o, RhoFunc(s)(f));

      if x=fail or OrbSCCLookup(o)[x]<>m then
        return fail;
      fi;

      g:=f;
      
      if x<>OrbSCC(o)[m][1] then 
        g:=RhoOrbMults(o, m)[x][2]*g;
      fi;
      
      o:=LambdaOrb(d); m:=LambdaOrbSCCIndex(d);
      y:=Position(o, LambdaFunc(s)(g));

      if y=fail or OrbSCCLookup(o)[y]<>m then
        return fail;
      fi;

      if y<>OrbSCC(o)[m][1] then 
        g:=g*LambdaOrbMults(o, m)[y][2];
      fi;
   
      z:=Position(enum!.schutz, LambdaPerm(s)(rep, g));

      if z=fail then
        return fail;
      fi;
      
      i:=Length(enum!.schutz); 
      j:=Length(LambdaOrbSCC(d));
      k:=Length(RhoOrbSCC(d));

      return i*j*(Position(RhoOrbSCC(d), x)-1)+(Position(LambdaOrbSCC(d),
      y)-1)*i+z;
    end,

    #######################################################################

    Membership:=function(elm, enum)
      return elm in d;
    end,

    Length:=enum -> Size(d),

    PrintObj:=function(enum)
      Print( "<enumerator of D-class>");
    return;
  end));
end);

# new for 1.0! - GreensDClasses - for regular acting semi
############################################################################

# same method for inverse.

InstallOtherMethod(GreensDClasses, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  local o, r, scc, out, i;

  o:=Enumerate(LambdaOrb(s), infinity);

  r:=ActingSemigroupModifier(s);
  scc:=OrbSCC(o);
  out:=EmptyPlist(Length(scc));

  for i in [1+r..Length(scc)] do 
    out[i-r]:=CallFuncList(CreateDClass, [s, i, o, CanonicalRep(o,i)]);
  od;
  return out;
end);

# new for 1.0! - GreensHClasses - for regular acting semi
############################################################################

# different method for inverse.

InstallOtherMethod(GreensHClasses, "for a regular acting semigroup", 
[IsRegularActingSemigroup],
function(s)
  local lambda_o, lambda_scc, rho_o, rho_scc, lambda_len, lookup, rhofunc, out, type, hrel, l, n, lambda_m, lambda_mults, f, rho_m, rho_mults, h, i, j, k;

  lambda_o:=Enumerate(LambdaOrb(s), infinity);
  lambda_scc:=OrbSCC(lambda_o);
  rho_o:=Enumerate(RhoOrb(s), infinity);
  rho_scc:=OrbSCC(rho_o);

  lambda_len:=Length(lambda_scc);
  lookup:=OrbSCCLookup(rho_o);
  rhofunc:=RhoFunc(s);

  out:=EmptyPlist(NrHClasses(s));
  type:=HClassType(s);
  hrel:=GreensHRelation(s);
  l:=ActingSemigroupModifier(s);     
  n:=0;

  for i in [1..lambda_l-l] do
    lambda_m:=i+l;
    lambda_mults:=LambdaOrbMults(lambda_o, lambda_m);
    f:=CanonicalRep(lambda_o, lambda_m);
    rho_m:=lookup[Position(rho_o, rhofunc(f))];
    rho_mults:=RhoOrbMults(rho_o, rho_m);
    for j in lambda_scc[lambda_m] do
      f:=f*lambda_mults[j][1];
      for k in rho_scc[rho_m] do
        n:=n+1;
        h:=Objectify(type, rec());
        SetParentSemigroup(h, s);
        SetLambdaOrb(h, lambda_o);
        SetLambdaOrbSCCIndex(h, lambda_m);
        SetRhoOrb(h, rho_o);
        SetRhoOrbSCCIndex(h, rho_m);
        SetRepresentative(h, rho_mults[k][1]*f);
        SetEquivalenceClassRelation(h, hrel);
        SetIsGreensClassNC(h, false);
        out[n]:=h;
      od;
    od;
  od;
  return out;
end);

# new for 1.0! - GreensHClasses - "for L-class of regular acting semigroup"
##############################################################################

# same method for inverse

InstallOtherMethod(GreensHClasses, "for L-class of regular acting semigroup",
[IsRegularLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, scc, mults, f, out, k, j;

  o:=RhoOrb(l);
  m:=RhoOrbSCCIndex(l);
 
  scc:=OrbSCC(o)[m];
  mults:=RhoOrbMults(o, m);
  f:=Representative(l);

  out:=EmptyPlist(Length(scc));
  k:=0;
 
  if not IsGreensClassNC(l) then 
    for j in scc do
      k:=k+1;
      out[k]:=GreensHClassOfElementNC(l, mults[j][1]*f);
      SetLClassOfHClass(out[k], l);
      ResetFilterObj(out[k], IsGreensClassNC); 
      #JDM also set schutz gp here!?
    od;
  else
    for j in scc do
      k:=k+1;
      out[k]:=GreensHClassOfElementNC(l, mults[j][1]*f);
      SetLClassOfHClass(out[k], r);
    od; 
  fi;
  return out;
end);

# new for 1.0! - GreensHClasses - "for R-class of regular acting semigroup"
##############################################################################

# same method for inverse

InstallOtherMethod(GreensHClasses, "for R-class of regular acting semigroup",
[IsRegularRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, scc, mults, f, out, k, j;

  o:=LambdaOrb(r);
  m:=LambdaOrbSCCIndex(r);
 
  scc:=OrbSCC(o)[m];
  mults:=LambdaOrbMults(o, m);
  f:=Representative(r);

  out:=EmptyPlist(Length(scc));
  k:=0;
 
  if not IsGreensClassNC(r) then 
    for j in scc do
      k:=k+1;
      out[k]:=GreensHClassOfElementNC(r, f*mults[j][1]);
      SetRClassOfHClass(out[k], r);
      ResetFilterObj(out[k], IsGreensClassNC); 
      #JDM also set schutz gp here!?
    od;
  else
    for j in scc do
      k:=k+1;
      out[k]:=GreensHClassOfElementNC(r, f*mults[j][1]);
      SetRClassOfHClass(out[k], r);
    od; 
  fi;
  return out;
end);

# new for 1.0! - GreensRClassOfElement - for regular acting semi and elt"
############################################################################

# same method for inverse.

InstallOtherMethod(GreensRClassOfElement, "for regular acting semi and elt",
[IsRegularSemigroup and IsActingSemigroup, IsActingElt],
function(s, f)
  local r, o, l, m;

  if not f in s then 
    Error("the element does not belong to the semigroup,");
    return;
  fi;

  r:=Objectify(RClassType(s), rec());
  SetParentSemigroup(r, s);
  SetEquivalenceClassRelation(r, GreensRRelation(s));
  SetIsGreensClassNC(r, false);

  if IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s);
    l:=Position(o, LambdaFunc(s)(f));
    m:=OrbSCCLookup(o)[l];
    if l<>OrbSCC(o)[m][1] then 
      f:=f*LambdaOrbMults(o, m)[l][2];
    fi;
  else
    o:=GradedLambdaOrb(s, f, true);
    m:=1;
  fi;
  
  SetLambdaOrb(r, o);
  SetLambdaOrbSCCIndex(r, m);
  SetRepresentative(r, f);

  return r;
end);

#HHH

# new for 1.0! - HClassReps - for regular acting semi
############################################################################

# different method for inverse.

InstallOtherMethod(HClassReps, "for a regular acting semigroup", 
[IsRegularActingSemigroup],
function(s)
  local lambda_o, lambda_scc, rho_o, rho_scc, lambda_len, lookup, rhofunc, out, l, n, lambda_m, lambda_mults, f, rho_m, rho_mults, i, j, k;
  
  lambda_o:=Enumerate(LambdaOrb(s), infinity);
  lambda_scc:=OrbSCC(lambda_o);
  rho_o:=Enumerate(RhoOrb(s), infinity);
  rho_scc:=OrbSCC(rho_o);

  lambda_len:=Length(lambda_scc);
  lookup:=OrbSCCLookup(rho_o);
  rhofunc:=RhoFunc(s);

  out:=EmptyPlist(NrHClasses(s));
  l:=ActingSemigroupModifier(s);     
  n:=0;

  for i in [1..lambda_l-l] do
    lambda_m:=i+l;
    lambda_mults:=LambdaOrbMults(lambda_o, lambda_m);
    f:=CanonicalRep(lambda_o, lambda_m);
    rho_m:=lookup[Position(rho_o, rhofunc(f))];
    rho_mults:=RhoOrbMults(rho_o, rho_m);
    for j in lambda_scc[lambda_m] do
      f:=f*lambda_mults[j][1];
      for k in rho_scc[rho_m] do
        n:=n+1;
        out[n]:=rho_mults[k][1]*f;
      od;
    od;
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
         IsEquivalenceClassDefaultRep and IsGreensHClass and
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
  
  lambda_o:=Enumerate(LambdaOrb(s), infinity);
  rho_o:=Enumerate(RhoOrb(s), infinity);
  
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

#RRR

InstallMethod(RhoOrbStabChain, "for D-class of reg acting semigp",
[IsGreensDClass and IsRegularActingSemigroupGreensClass],
d-> StabChainImmutable(SchutzenbergerGroup(d)));

#SSS

# new for 1.0! - SchutzenbergerGroup - "for D-class of regular acting semigroup"
#############################################################################

# same method for inverse

InstallMethod(SchutzenbergerGroup, "for D-class of regular acting semigroup",
[IsGreensDClass and IsRegularActingSemigroupGreensClass],
d-> LambdaOrbSchutzGp(LambdaOrb(d), LambdaOrbSCCIndex(d)));

# new for 1.0! - SchutzenbergerGroup - "for H-class of regular acting semigroup"
#############################################################################

# same method for inverse

InstallMethod(SchutzenbergerGroup, "for H-class of regular acting semigroup",
[IsGreensHClass and IsRegularActingSemigroupGreensClass],
h-> LambdaOrbSchutzGp(LambdaOrb(h), LambdaOrbSCCIndex(h)));

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



