#############################################################################
##
#W  regular.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# new for 1.0! - DClassType - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallOtherMethod(DClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsRegularDClass and IsGreensDClass 
         and IsActingSemigroupGreensClass);
end);

# new for 1.0! - HClassType - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallOtherMethod(HClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensHClass and
         IsActingSemigroupGreensClass);
end);

# new for 1.0! - LClassType - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallOtherMethod(LClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsRegularLClass and IsGreensLClass and
         IsActingSemigroupGreensClass);
end);

# new for 1.0! - RClassType - "for a regular acting semigroup"
############################################################################

# same method for inverse semigroups

InstallOtherMethod(RClassType, "for a regular acting semigroup",
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsRegularRClass and IsGreensRClass and
         IsActingSemigroupGreensClass);
end);


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

  rep:=RectifyRho(rho_o, LambdaOrbRep(lambda_o, m));
  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g))=();
end);

# new for 1.0! - \in - "for acting elt and D-class of regular acting semigp"
#############################################################################
#JDM revise this if revising \in for elt and D-class in greens.gi

InstallMethod(\in, "for acting elt and D-class of regular acting semigp.",
[IsActingElt, IsRegularDClass and IsActingSemigroupGreensClass],
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
  local o, rho_o, scc, out, r, i;

  o:=LambdaOrb(s);
  rho_o:=RhoOrb(s);
  scc:=OrbSCC(o);
  out:=EmptyPlist(Length(scc));
  r:=ActingSemigroupModifier(s);
  
  for i in [1+r..Length(scc)] do 
    out[i-r]:=CallFuncList(CreateDClass, 
     [s, i, o, RectifyRho(rho_o, LambdaOrbRep(o,i))]);
  od;
  return out;
end);

# new for 1.0! - GreensHClasses - for regular acting semi
############################################################################

# different method for inverse.

InstallOtherMethod(GreensHClasses, "for a regular acting semigroup", 
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local lambda_o, lambda_scc, rho_o, rho_scc, lambda_len, lookup, rhofunc, out, type, hrel, l, n, lambda_m, lambda_mults, f, rho_l, rho_m, rho_mults, h, i, j, k;

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

  for i in [1..lambda_len-l] do
    lambda_m:=i+l;
    lambda_mults:=LambdaOrbMults(lambda_o, lambda_m);
    f:=LambdaOrbRep(lambda_o, lambda_m);
    rho_l:=Position(rho_o, rhofunc(f));
    rho_m:=lookup[rho_l];
    rho_mults:=RhoOrbMults(rho_o, rho_m);
    f:=rho_mults[rho_l][2]*f;
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
      SetLClassOfHClass(out[k], l);
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

# new for 0.7! - GreensLClasses - for regular acting semigroup
##############################################################################

# same method for inverse

InstallOtherMethod(GreensLClasses, "for a regular acting semigroup", 
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local rho_o, rho_scc, lambda_o, lambda_scc, lambda_len, lookup, rhofunc, out, type, lrel, l, n, lambda_m, f, rho_l, rho_m, mults, i, j;
  
  rho_o:=RhoOrb(s);
  rho_scc:=OrbSCC(rho_o);
  lambda_o:=LambdaOrb(s);
  lambda_scc:=OrbSCC(lambda_o);

  lambda_len:=Length(lambda_scc);
  lookup:=OrbSCCLookup(rho_o);
  rhofunc:=RhoFunc(s);

  out:=EmptyPlist(NrLClasses(s));
  type:=LClassType(s);
  lrel:=GreensLRelation(s);
  l:=ActingSemigroupModifier(s);     
  n:=0;

  for i in [1..lambda_len-l] do
    lambda_m:=i+l;
    f:=LambdaOrbRep(lambda_o, lambda_m);
    rho_l:=Position(rho_o, rhofunc(f));
    rho_m:=lookup[rho_l];
    f:=RhoOrbMults(rho_o, rho_m)[rho_l][2]*f;
    mults:=LambdaOrbMults(lambda_o, lambda_m);
    for j in lambda_scc[lambda_m] do
      n:=n+1;
      out[n]:=Objectify(type, rec());
      SetParentSemigroup(out[n], s);
      SetRhoOrb(out[n], rho_o);
      SetRhoOrbSCCIndex(out[n], rho_m);
      SetRepresentative(out[n], f*mults[j][1]);
      SetEquivalenceClassRelation(out[n], lrel);
      SetIsGreensClassNC(out[n], false);
    od;
  od;
  return out;
end);

# new for 0.7! - GreensLClasses - for regular D-class ofacting semigroup
##############################################################################

# same method for inverse.

InstallOtherMethod(GreensLClasses, "for regular D-class of acting semigroup", 
[IsActingSemigroupGreensClass and IsRegularDClass],
function(d)
  local mults, scc, f, s, o, m, lrel, nc, out, k, l, i;

  mults:=LambdaOrbMults(LambdaOrb(d), LambdaOrbSCCIndex(d));
  scc:=LambdaOrbSCC(d);
  f:=Representative(d);

  s:=ParentSemigroup(d);
  o:=RhoOrb(d);
  m:=RhoOrbSCCIndex(d);
  lrel:=GreensLRelation(s);
  nc:=IsGreensClassNC(d);
  out:=EmptyPlist(Length(scc));

  k:=0;
  for i in scc do
    k:=k+1;
    l:=Objectify(LClassType(s), rec());

    SetParentSemigroup(l, s);
    SetRhoOrbSCCIndex(l, m);
    SetRhoOrb(l, o);
    SetRepresentative(l, f*mults[i][1]);
    SetEquivalenceClassRelation(l, lrel);
    SetIsGreensClassNC(l, nc);
    SetDClassOfLClass(l, d);
    out[k]:=l;
  od;

  return out;
end);

# new for 0.7! - GreensRClasses - for regular acting semigroup
##############################################################################

# different method for inverse 

InstallOtherMethod(GreensRClasses, "for a regular acting semigroup", 
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local rho_o, rho_scc, lambda_o, lambda_scc, rho_len, lookup, lambdafunc, out, type, rrel, l, n, rho_m, f, lambda_l, lambda_m, mults, i, j;
  
  rho_o:=RhoOrb(s);
  rho_scc:=OrbSCC(rho_o);
  lambda_o:=LambdaOrb(s);
  lambda_scc:=OrbSCC(lambda_o);

  rho_len:=Length(rho_scc);
  lookup:=OrbSCCLookup(lambda_o);
  lambdafunc:=LambdaFunc(s);

  out:=EmptyPlist(NrRClasses(s));
  type:=RClassType(s);
  rrel:=GreensRRelation(s);
  l:=ActingSemigroupModifier(s);     
  n:=0;

  for i in [1..rho_len-l] do
    rho_m:=i+l;
    f:=RhoOrbRep(rho_o, rho_m);
    lambda_l:=Position(lambda_o, lambdafunc(f));
    lambda_m:=lookup[lambda_l];
    f:=f*LambdaOrbMults(lambda_o, lambda_m)[lambda_l][2];
   
    mults:=RhoOrbMults(rho_o, rho_m);
    for j in rho_scc[rho_m] do
      n:=n+1;
      out[n]:=Objectify(type, rec());
      SetParentSemigroup(out[n], s);
      SetLambdaOrb(out[n], lambda_o);
      SetLambdaOrbSCCIndex(out[n], lambda_m);
      SetRepresentative(out[n], mults[j][1]*f);
      SetEquivalenceClassRelation(out[n], rrel);
      SetIsGreensClassNC(out[n], false);
    od;
  od;
  return out;
end);

# new for 1.0! - GreensRClasses - "for a regular D-class of acting semigroup"
##############################################################################

# same method for inverse

InstallMethod(GreensRClasses, "for a regular D-class of acting semigroup",
[IsActingSemigroupGreensClass and IsRegularDClass],
function(d)
  local mults, scc, f, s, o, m, rrel, nc, out, k, r, i;

  mults:=RhoOrbMults(RhoOrb(d), RhoOrbSCCIndex(d));
  scc:=RhoOrbSCC(d);
  f:=Representative(d);

  s:=ParentSemigroup(d);
  o:=LambdaOrb(d);
  m:=LambdaOrbSCCIndex(d);
  rrel:=GreensRRelation(s);   
  nc:=IsGreensClassNC(d);

  out:=EmptyPlist(Length(scc));

  k:=0;
  for i in scc do
    k:=k+1;
    r:=Objectify(RClassType(s), rec());

    SetParentSemigroup(r, s);
    SetLambdaOrbSCCIndex(r, m);
    SetLambdaOrb(r, o);
    SetRepresentative(r, mults[i][1]*f);
    SetEquivalenceClassRelation(r, rrel);
    SetIsGreensClassNC(r, nc);
    SetDClassOfRClass(r, d);
    out[k]:=r;
  od;

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
[IsRegularSemigroup and IsActingSemigroup],
function(s)
  local lambda_o, lambda_scc, rho_o, rho_scc, lambda_len, lookup, rhofunc, out, l, n, lambda_m, lambda_mults, f, rho_l, rho_m, rho_mults, i, j, k;
  
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

  for i in [1..lambda_len-l] do
    lambda_m:=i+l;
    lambda_mults:=LambdaOrbMults(lambda_o, lambda_m);
    f:=LambdaOrbRep(lambda_o, lambda_m);
    rho_l:=Position(rho_o, rhofunc(f));
    rho_m:=lookup[rho_l];
    rho_mults:=RhoOrbMults(rho_o, rho_m);
    f:=rho_mults[rho_l][2]*f;
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

#III

# new for 1.0! - IteratorOfDClassData - "for regular acting semigroup"
###############################################################################
# JDM review this...

# the first part of this could really be a method for IteratorOfGradedLambdaOrbs

# same method for inverse
# there could be a different method for inverse

InstallMethod(IteratorOfDClassData, "for regular acting semigp",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
local iter;

  if not IsClosed(LambdaOrb(s)) then 
    if Length(LambdaOrb(s))=1 and ActingSemigroupModifier(s)=1 then 
      Enumerate(LambdaOrb(s), 2);
    fi;
    iter:=IteratorByFunctions( rec(

      last_called_by_is_done:=false,

      next_value:=fail,

      seen:=HTCreate([1,1],
       rec(forflatplainlists:=true, hashlen:=s!.opts.hashlen.S)),
      
      o:=GradedLambdaOrb(s, LambdaOrb(s)!.gens[1], true),

      m:=0,

      IsDoneIterator:=function(iter)
        local m, seen, lambda_o, new, val, f, i;  

        if iter!.last_called_by_is_done then 
          return iter!.next_value=fail;
        fi;

        iter!.last_called_by_is_done:=true;
        iter!.next_value:=fail;

        m:=iter!.m;
        
        if m=Length(OrbSCC(iter!.o)) then 
          m:=1;
          seen:=iter!.seen;
          # look for a new lambda value
          lambda_o:=LambdaOrb(s);

          # check existing lambda values
          new:=false;
          for i in [1+ActingSemigroupModifier(s)..Length(lambda_o)] do 
            val:=Position(GradedLambdaOrbs(s), lambda_o[i]);
            if val=fail or HTValue(seen, val{[1,2]})=fail then          
              new:=i;
              break;
            fi;
          od;

          # look for new lambda value
          if new=false then  
            lambda_o!.looking:=true;
            lambda_o!.lookingfor:=
              function(o, x) 
                local val;
                val:=Position(GradedLambdaOrbs(s), x);
                return val=fail or HTValue(seen, val{[1,2]})=fail; 
              end;
            lambda_o!.lookfunc:=lambda_o!.lookingfor;
            Enumerate(lambda_o);
            new:=PositionOfFound(lambda_o);
            lambda_o!.found:=false; lambda_o!.looking:=false;
            Unbind(lambda_o!.lookingfor); Unbind(lambda_o!.lookfunc);
          fi;

          if new=false then 
            return true;
          fi;
          
          val:=Position(GradedLambdaOrbs(s), lambda_o[new]);
          if val<>fail then 
            iter!.o:=GradedLambdaOrbs(s)[val[1]][val[2]];
            HTAdd(seen, val{[1,2]}, true);
          else
            iter!.o:=GradedLambdaOrb(s,
             EvaluateWord(lambda_o!.gens, TraceSchreierTreeForward(lambda_o,
             new)), true);
            HTAdd(seen, iter!.o!.val{[1,2]}, true);
          fi;
        else
          m:=m+1;
        fi;
        iter!.m:=m; 
        
        f:=LambdaOrbRep(iter!.o, m)*LambdaOrbMults(iter!.o,
        m)[iter!.o!.lambda_l][2]; 
        iter!.next_value:=[s, m, iter!.o, f, false];
        return false;
      end,

      NextIterator:=function(iter)
        if not iter!.last_called_by_is_done then
          IsDoneIterator(iter);
        fi;
        iter!.last_called_by_is_done:=false;
        return iter!.next_value;
      end,

      ShallowCopy:=iter-> rec( last_called_by_is_done:=false,
      next_value:=fail,
      seen:=HTCreate([1,1],
       rec(forflatplainlists:=true, hashlen:=s!.opts.hashlen.S)),
      o:=GradedLambdaOrb(s, LambdaOrb(s)!.gens[1], true),
      m:=0)));

    HTAdd(iter!.seen, iter!.o!.val{[1,2]}, true);
  else ####

    iter:=IteratorByFunctions( rec(
                 
      m:=ActingSemigroupModifier(s), 
     
      i:=0,      

      scc_limit:=Length(OrbSCC(LambdaOrb(s))),

      IsDoneIterator:=iter-> iter!.m=iter!.scc_limit,

      NextIterator:=function(iter)
        local m, o, f, scc; 
        m:=iter!.m; 

        if m=iter!.scc_limit then
          return fail; 
        fi;

        o:=LambdaOrb(s); scc:=OrbSCC(o);

        m:=m+1;
        iter!.m:=m;
 
        # f ok here? JDM
        f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, scc[m][1])); 
        return [s, m, LambdaOrb(s), f, false];
      end,

      #JDM fill this in!
      ShallowCopy:=iter-> rec()));
  fi;
  return iter;
end);

# new for 0.7! - IteratorOfDClassReps - "for a regular acting semigroup"
###############################################################################

# same method for inverse

InstallMethod(IteratorOfDClassReps, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  if HasDClassReps(s) then
    return IteratorList(DClassReps(s));
  fi;
  return IteratorByIterator(IteratorOfDClassData(s), x-> x[4],
   [IsIteratorOfDClassReps]);
end);

# new for 0.7! - IteratorOfDClasses - "for a regular acting semigroup"
###############################################################################

# same method for inverse

InstallMethod(IteratorOfDClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  if HasGreensDClasses(s) then
    return IteratorList(GreensDClasses(s));
  fi;
  return IteratorByIterator(IteratorOfDClassData(s), x->
   CallFuncList(CreateDClassNC, x), [IsIteratorOfDClasses]);
end);

# new for 0.7! - IteratorOfLClassData - "for a regular acting semigroup
###############################################################################

# no method required for inverse

InstallMethod(IteratorOfLClassData, "for regular acting semigp",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
local iter, scc;

  if not IsClosed(LambdaOrb(s)) then 
    
    iter:=IteratorByFunctions( rec(

      i:=ActingSemigroupModifier(s),

      IsDoneIterator:=iter-> IsClosed(LambdaOrb(s)) and 
       iter!.i>=Length(LambdaOrb(s)),

      NextIterator:=function(iter)
        local i, o, r, f;
        
        o:=LambdaOrb(s); i:=iter!.i;

        if IsClosed(o) and i>=Length(o) then 
          return fail;  
        fi;
        
        i:=i+1;
        
        if i>Length(o) then 
          if not IsClosed(o) then 
            Enumerate(o, i);
            if i>Length(o) then 
              return fail;
            fi;
          else 
            return fail;
          fi;
        fi;

        iter!.i:=i; 
        
        f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, i)); 
        o:=GradedRhoOrb(s, o[i], true);
        return [s, 1, o, RectifyRho(o, f), false];
      end,

      ShallowCopy:=iter-> rec(i:=ActingSemigroupModifier(s))));
  else ####

    scc:=OrbSCC(LambdaOrb(s));

    iter:=IteratorByFunctions( rec(
                 
      m:=ActingSemigroupModifier(s), 
     
      i:=0,      

      scc_limit:=Length(scc),

      i_limit:=Length(scc[Length(scc)]),

      IsDoneIterator:=iter-> iter!.m=iter!.scc_limit and 
       iter!.i=iter!.i_limit,

      NextIterator:=function(iter)
        local i, o, m, scc, f, r, mults;
        
        i:=iter!.i; 
        m:=iter!.m; 

        if m=iter!.scc_limit and i=iter!.i_limit then
          return fail; 
        fi;

        o:=LambdaOrb(s); scc:=OrbSCC(o);

        if i<Length(scc[m]) then 
          i:=i+1;
        else
          i:=1; m:=m+1;
        fi;

        iter!.i:=i; iter!.m:=m;
 
        # f ok here? JDM
        f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, scc[m][i])); 
        return [s, m, RhoOrb(s), RectifyRho(RhoOrb(s), f), false];
      end,

      ShallowCopy:=iter-> rec(m:=ActingSemigroupModifier(s), i:=0,
      scc_limit:=iter!.scc_limit, i_limit:=iter!.i_limit)));
  fi;
  
  return iter;
end);

# new for 0.7! - IteratorOfRClassData - "for a regular acting semigroup
###############################################################################

# different method for inverse

InstallMethod(IteratorOfRClassData, "for regular acting semigp",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
local iter, scc;

  if not IsClosed(RhoOrb(s)) then 
    
    iter:=IteratorByFunctions( rec(

      i:=ActingSemigroupModifier(s),

      IsDoneIterator:=iter-> IsClosed(RhoOrb(s)) and 
       iter!.i>=Length(RhoOrb(s)),

      NextIterator:=function(iter)
        local i, o, r, f;
        
        o:=RhoOrb(s); i:=iter!.i;

        if IsClosed(o) and i>=Length(o) then 
          return fail;  
        fi;
        
        i:=i+1;
        
        if i>Length(o) then 
          if not IsClosed(o) then 
            Enumerate(o, i);
            if i>Length(o) then 
              return fail;
            fi;
          else 
            return fail;
          fi;
        fi;

        iter!.i:=i; 
        
        f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, i)); 
        o:=GradedLambdaOrb(s, o[i], true);
        return [s, 1, o, RectifyLambda(o, f), false];
      end,

      ShallowCopy:=iter-> rec(i:=ActingSemigroupModifier(s))));
  else ####

    scc:=OrbSCC(RhoOrb(s));

    iter:=IteratorByFunctions( rec(
                 
      m:=ActingSemigroupModifier(s), 
     
      i:=0,      

      scc_limit:=Length(scc),

      i_limit:=Length(scc[Length(scc)]),

      IsDoneIterator:=iter-> iter!.m=iter!.scc_limit and 
       iter!.i=iter!.i_limit,

      NextIterator:=function(iter)
        local i, o, m, scc, f, r, mults;
        
        i:=iter!.i; 
        m:=iter!.m; 

        if m=iter!.scc_limit and i=iter!.i_limit then
          return fail; 
        fi;

        o:=RhoOrb(s); scc:=OrbSCC(o);

        if i<Length(scc[m]) then 
          i:=i+1;
        else
          i:=1; m:=m+1;
        fi;

        iter!.i:=i; iter!.m:=m;
 
        # f ok here? JDM
        f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, scc[m][i])); 
        return [s, m, LambdaOrb(s), RectifyLambda(LambdaOrb(s), f), false];
      end,

      ShallowCopy:=iter-> rec(m:=ActingSemigroupModifier(s), i:=0,
      scc_limit:=iter!.scc_limit, i_limit:=iter!.i_limit)));
  fi;
  
  return iter;
end);

# new for 0.7! - IteratorOfLClassReps - "for a part perm inverse semigroup"
###############################################################################

# different method for inverse

InstallMethod(IteratorOfLClassReps, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
s-> IteratorByIterator(IteratorOfLClassData(s), x-> x[4],
[IsIteratorOfLClassReps]));

# new for 0.7! - IteratorOfLClasses - "for a part perm inverse semigroup"
###############################################################################

# different method for inverse

InstallMethod(IteratorOfLClasses, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
s-> IteratorByIterator(IteratorOfLClassData(s), x->
CallFuncList(CreateLClass, x), [IsIteratorOfLClasses]));

# new for 0.7! - IteratorOfRClasses - "for regular acting semigroup 
###############################################################################

# same method for inverse

InstallMethod(IteratorOfRClasses, "for regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x->
CallFuncList(CreateRClassNC, x), [IsIteratorOfRClasses]));

#LLL

# new for 1.0! - LClassReps - "for regular acting semigroup 
###############################################################################

# same method for inverse

InstallOtherMethod(LClassReps, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local rho_o, lambda_o, scc, nr, out, l, n, f, mults, m, j;
  
  rho_o:=RhoOrb(s);
  lambda_o:=LambdaOrb(s);
  scc:=OrbSCC(lambda_o);

  nr:=Length(scc);
  out:=EmptyPlist(NrLClasses(s));
  l:=ActingSemigroupModifier(s);     
  n:=0;

  for m in [1+l..nr] do
    f:=RectifyRho(rho_o, LambdaOrbRep(lambda_o, m));
    mults:=LambdaOrbMults(lambda_o, m);
    for j in scc[m] do
      n:=n+1;
      out[n]:=f*mults[j][1];
    od;
  od;
  return out;
end);

# new for 1.0! - RClassReps - "for regular acting semigroup 
###############################################################################

# different method for inverse

InstallOtherMethod(RClassReps, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local lambda_o, rho_o, scc, nr, out, l, n, f, mults, m, j;
  
  lambda_o:=LambdaOrb(s);
  rho_o:=RhoOrb(s);
  scc:=OrbSCC(rho_o);

  nr:=Length(scc);
  out:=EmptyPlist(NrRClasses(s));
  l:=ActingSemigroupModifier(s);     
  n:=0;

  for m in [1+l..nr] do
    f:=RectifyLambda(lambda_o, RhoOrbRep(rho_o, m));
    mults:=RhoOrbMults(rho_o, m);
    for j in scc[m] do
      n:=n+1;
      out[n]:=mults[j][1]*f;
    od;
  od;
  return out;
end);

#NNN

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
[IsActingSemigroupGreensClass and IsRegularDClass],
d-> Length(LambdaOrbSCC(d))*Length(RhoOrbSCC(d)));

# new for 1.0! - NrHClasses - "for a L-class of regular acting semigroup"
############################################################################

# same method for inverse semigroups (although there could be one if we
# have to introduce IsInverseActingSemigroupGreensClass).

InstallOtherMethod(NrHClasses, "for a L-class of regular acting semigroup",
[IsActingSemigroupGreensClass and IsRegularLClass],
l-> Length(RhoOrbSCC(l)));

# new for 1.0! - NrHClasses - "for a R-class of regular acting semigroup"
############################################################################

# same method for inverse semigroups 

InstallOtherMethod(NrHClasses, "for a R-class of regular acting semigroup",
[IsActingSemigroupGreensClass and IsRegularRClass],
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
[IsActingSemigroupGreensClass and IsRegularDClass],
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
[IsActingSemigroupGreensClass and IsRegularDClass],
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

#PPP

# new for 0.7! - PartialOrderOfDClasses - "for a regular acting semigroup" 
############################################################################## 

#JDM

InstallMethod(PartialOrderOfDClasses, "for a regular acting semigroup", 
[IsRegularSemigroup and IsActingSemigroup], 
function(s) 
  Error("not yet implemented,");   
end); 

#RRR

# new for 1.0! - Random - "for a regular acting semigroup"
#############################################################################

# different method for inverse

InstallMethod(Random, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup],
function(s)
  local lambda_o, gens, i, w, m, f, rho_o, rho_m;
  
  lambda_o:=LambdaOrb(s);

  if not IsClosed(lambda_o) then
    gens:=GeneratorsOfSemigroup(s);    
    i:=Random([1..Int(Length(gens)/2)]);
    w:=List([1..i], x-> Random([1..Length(gens)]));
    return EvaluateWord(gens, w);
  fi;
  
  i:=Random([1..Length(lambda_o)]);
  m:=OrbSCCLookup(lambda_o)[i];
  f:=LambdaOrbRep(lambda_o, m);
  
  if IsClosed(RhoOrb(s)) then 
    rho_o:=RhoOrb(s);
    rho_m:=OrbSCCLookup(rho_o)[Position(rho_o, RhoFunc(s)(f))];
  fi;
  return
  Random(RhoOrbMults(rho_o, rho_m))[1]*f*
   Random(LambdaOrbSchutzGp(lambda_o, m))*
    LambdaOrbMults(lambda_o, m)[i][1];
end);

# new for 1.0! - RhoOrbStabChain - "for a regular D-class "
#############################################################################

InstallMethod(RhoOrbStabChain, "for a regular D-class",
[IsRegularDClass and IsActingSemigroupGreensClass],
d-> StabChainImmutable(SchutzenbergerGroup(d)));

#SSS

# new for 1.0! - SchutzenbergerGroup - "for D-class of regular acting semigroup"
#############################################################################

# same method for inverse

InstallMethod(SchutzenbergerGroup, "for D-class of regular acting semigroup",
[IsRegularDClass and IsActingSemigroupGreensClass],
d-> LambdaOrbSchutzGp(LambdaOrb(d), LambdaOrbSCCIndex(d)));

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



