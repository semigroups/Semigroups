#############################################################################
##
#W  inverse.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Notes: everything here uses LambdaSomething, so don't use RhoAnything

# the first three main functions should be updated!

## Methods for inverse acting semigroups consisting of acting elements with a
## ^-1 operator. 

InstallMethod(IsInverseOpClass, "for a Green's class",
[IsActingSemigroupGreensClass], ReturnFalse);

#

InstallOtherMethod(DClassType, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsInverseOpClass and IsGreensDClass
         and IsActingSemigroupGreensClass);
end);

#

InstallOtherMethod(HClassType, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsInverseOpClass and IsGreensHClass
         and IsActingSemigroupGreensClass);
end);

#

InstallOtherMethod(LClassType, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsInverseOpClass and IsGreensLClass
         and IsActingSemigroupGreensClass);
end);

#

InstallOtherMethod(RClassType, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp and IsActingSemigroup],
function(s)
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsInverseOpClass and IsGreensRClass
         and IsActingSemigroupGreensClass);
end);

#

InstallMethod(\in, 
"for inverse acting elt and acting semigroup with inversion",
[IsAssociativeElement, IsActingSemigroupWithInverseOp],
function(f, s)
  local dom, o, lambda, lambda_l, rho, rho_l, lookingfor, m, schutz, scc, g,
  rep, n;
  
  if not ElementsFamily(FamilyObj(s))=FamilyObj(f) then 
    Error("the element and semigroup are not of the same type,");
    return;
  fi;

  #if HasAsSSortedList(s) then 
  #  return f in AsSSortedList(s); 
  #fi;

  if ActionDegree(s)=0 then 
    return ActionDegree(f)=0;
  fi;

  o:=LambdaOrb(s);
  lambda:=LambdaFunc(s)(f);
  lambda_l:=EnumeratePosition(o, lambda, false);
  
  if lambda_l=fail then
    return false;
  fi;
  
  rho:=RhoFunc(s)(f);
  rho_l:=EnumeratePosition(o, rho, false);
  
  if rho_l=fail then
    return false;
  fi;
 
  if not IsClosed(o) then 
    o:=GradedLambdaOrb(s, f, false);
    Enumerate(o, infinity);
    rho_l:=Position(o, rho);
    if rho_l=fail then 
      return false;
    fi;
    lambda_l:=1;
  fi;

  m:=OrbSCCLookup(o)[lambda_l];

  if OrbSCCLookup(o)[rho_l]<>m then
    return false;
  fi;

  schutz:=LambdaOrbStabChain(o, m);

  if schutz=true then
    return true;
  fi;

  scc:=OrbSCC(o)[m]; g:=f;

  if lambda_l<>scc[1] then 
    g:=g*LambdaOrbMult(o, m, lambda_l)[2];
  fi;

  if rho_l<>scc[1] then 
    g:=LambdaOrbMult(o, m, rho_l)[1]*g;
  fi;

  if IsIdempotent(g) then 
    return true;
  elif schutz=false then
    return false;
  fi;
  # the D-class rep corresponding to lambda_o and scc.
  rep:=RectifyInverseRho(s, o, LambdaOrbRep(o, m)).rep;
  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g))=(); 
end);

#

InstallMethod(\in, "for inverse op D-class",
[IsPartialPerm , IsInverseOpClass and IsGreensDClass and
IsActingSemigroupGreensClass],
function(f, d)
  local rep, s, o, m, lookup, rho_l, lambda_l, schutz, scc, g;
  
  rep:=Representative(d);
  s:=ParentSemigroup(d);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) 
    or ActionRank(f) <> ActionRank(rep) 
    or ActionDegree(f)<>ActionDegree(rep) then
    return false;
  fi;

  o:=LambdaOrb(d);
  m:=LambdaOrbSCCIndex(d);
  lookup:=OrbSCCLookup(o);

  rho_l:=Position(o, RhoFunc(s)(f)); 
  lambda_l:=Position(o, LambdaFunc(s)(f));
  
  if rho_l=fail or lambda_l=fail or lookup[rho_l]<>m or lookup[lambda_l]<>m
   then 
    return false;
  fi;

  schutz:=LambdaOrbStabChain(o, m); 

  if schutz=true then 
    return true;
  fi;

  scc:=OrbSCC(o)[m];
  g:=f;

  if rho_l<>scc[1] then 
    g:=LambdaOrbMult(o, m, rho_l)[1]*g;
  fi;
  
  if lambda_l<>scc[1] then 
    g:=g*LambdaOrbMult(o, m, lambda_l)[2];
  fi; 

  if g=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;

  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g))=(); 
end);

# new for 1.0! - \in - "for acting elt and inverse op L-class of acting semigp"
#############################################################################

InstallMethod(\in, "for acting elt and inverse op L-class of acting semigp.",
[IsAssociativeElement, IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
function(f, l)
  local rep, s, m, o, i, schutz, g, p;

  rep:=Representative(l);
  s:=ParentSemigroup(l);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) 
    or ActionDegree(f) <> ActionDegree(rep)  
    or ActionRank(f) <> ActionRank(rep) 
    or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) then
    Info(InfoSemigroups, 1, "degree, rank, or lambda value not equal to",
     " those of  any of the L-class elements,");
    return false;
  fi;

  m:=LambdaOrbSCCIndex(l);
  o:=LambdaOrb(l);
  #o is closed since we know LambdaOrbSCCIndex

  i:=Position(o, RhoFunc(s)(f));
  if i = fail or OrbSCCLookup(o)[i]<>m then
    return false;
  fi;

  schutz:=LambdaOrbStabChain(o, m);

  if schutz=true then
    Info(InfoSemigroups, 3, "Schutz. group of L-class is symmetric group");
    return true;
  fi;

  if i<>OrbSCC(o)[m][1] then  
    g:=LambdaOrbMult(o, m, i)[1]*f;
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

#

InstallMethod(\in, "for acting elt and inverse op R-class of acting semigp.",
[IsAssociativeElement, IsInverseOpClass and IsGreensRClass and IsActingSemigroupGreensClass],
function(f, r)
  local rep, s, m, o, i, schutz, g, p;

  rep:=Representative(r);
  s:=ParentSemigroup(r);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
   ActionDegree(f) <> ActionDegree(rep) or 
   ActionRank(f) <> ActionRank(rep) or 
   RhoFunc(s)(f) <> RhoFunc(s)(rep) then
    Info(InfoSemigroups, 1, 
    "degree, rank, or lambda value not equal to those of",
    " any of the L-class elements,");
    return false;
  fi;

  m:=LambdaOrbSCCIndex(r);
  o:=LambdaOrb(r);
  #o is closed since we know LambdaOrbSCCIndex

  i:=Position(o, LambdaFunc(s)(f));

  if i = fail or OrbSCCLookup(o)[i]<>m then
    return false;
  fi;

  schutz:=LambdaOrbStabChain(o, m);

  if schutz=true then
    Info(InfoSemigroups, 3, "Schutz. group of R-class is symmetric group");
    return true;
  fi;

  if i<>OrbSCC(o)[m][1] then  
    g:=f*LambdaOrbMult(o, m, i)[2];
  else
    g:=f;
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
  return SiftedPermutation(schutz,  LambdaPerm(s)(rep, g))=();
end);

#CCC

# mod for 1.0! - CreateInverseOpLClassNC - not a user function!
#############################################################################
# Usage: arg[1] = semigroup;  arg[2] = lambda orb scc index; 
# arg[3] = lambda orb;  arg[4] = rep;
# arg[5] = IsGreensClassNC. 

# NC indicates that the representative is assumed to be in the correct form,
# i.e. RhoFunc(s)(arg[2]) is in the first place of the scc of the lambda orb. 

# used and standardised. 

InstallGlobalFunction(CreateInverseOpLClassNC,
function(s, m, o, rep, nc)
  local l;

  l:=Objectify(LClassType(s), rec());
  SetParentSemigroup(l, s);
  SetRepresentative(l, rep);
  SetLambdaOrb(l, o);
  SetLambdaOrbSCCIndex(l, m);
  SetEquivalenceClassRelation(l, GreensLRelation(s));
  SetIsGreensClassNC(l, nc);
  return l;
end);

# mod for 1.0! - CreateInverseOpLClass - not a user function!
#############################################################################
# Usage: arg[1] = semigroup;  arg[2] = lambda orb scc index; 
# arg[3] = lambda orb;  arg[4] = rep;
# arg[5] = IsGreensClassNC. 

# use the NC version for already rectified reps.

# used and standardised. 

InstallGlobalFunction(CreateInverseOpLClass,
function(s, m, o, rep, nc)
  local rectify;

  rectify:=RectifyInverseRho(s, o, rep, LambdaPos(o), m);
  return CreateInverseOpLClassNC(s, rectify.m, o, rectify.rep, nc);
end);

#DDD

# new for 1.0! - DClassOfRClass - "for a inverse op R-class acting semigroup"
#############################################################################

InstallOtherMethod(DClassOfRClass, "for inverse op R-class", 
[IsInverseOpClass and IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, o, m, f;

  s:=ParentSemigroup(r);
  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
  f:=RectifyLambda(s, o, Representative(r), fail, m).rep;
  return CreateDClassNC(s, m, o, fail, fail, f, IsGreensClassNC(r));
end);

# new for 1.0! - DClassOfHClass - "for a inverse op H-class acting semigroup"
#############################################################################

InstallOtherMethod(DClassOfHClass, "for inverse op H-class", 
[IsInverseOpClass and IsGreensRClass and IsActingSemigroupGreensClass],
function(h)
  local s, o, m, f;

  s:=ParentSemigroup(h);
  o:=LambdaOrb(h); 
  m:=LambdaOrbSCCIndex(h);
  f:=RectifyLambda(s, o, Representative(h), fail, m).rep;
  return CreateDClassNC(s, m, o, fail, fail, f, IsGreensClassNC(h));
end);

# new for 1.0! - LClassOfHClass - "for inverse op H-class
##############################################################################

InstallMethod(LClassOfHClass, "for an inverse op H-class",
[IsInverseOpClass and IsGreensHClass and IsActingSemigroupGreensClass],
# use non-NC so that rho value of f is rectified
h-> CreateInverseOpLClass(ParentSemigroup(h), LambdaOrbSCCIndex(h),
LambdaOrb(h), Representative(h), IsGreensClassNC(h)));

# new for 1.0! - DClassReps - "for an acting semigroup with inversion"
##############################################################################

InstallOtherMethod(DClassReps, "for an acting semigroup with inversion",
[IsActingSemigroupWithInverseOp],
function(s)            
  local o, r, out, f, m;
  
  o:=RhoOrb(s);
  r:=Length(OrbSCC(o));
  out:=EmptyPlist(r);
  
  for m in [2..r] do 
    f:=RhoOrbRep(o, m);
# JDM method for RightOne of inverse acting element required.
    out[m-1]:=RightOne(f);
  od;
  return out;
end);

# JDM why is IsGreensClassOfPartPermSemigroup still used here!?

InstallMethod(Enumerator, "for D-class of part perm inv semigroup",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)

  return EnumeratorByFunctions(d, rec(

    schutz:=Enumerator(SchutzenbergerGroup(d)),

    #########################################################################

    ElementNumber:=function(enum, pos)
      local scc, n, m, r, q, q2, mults;
      if pos>Length(enum) then 
        return fail;
      fi;

      if pos<=Length(enum!.schutz) then 
        return enum!.schutz[pos]*Representative(d);
      fi;

      scc:=LambdaOrbSCC(d);
      mults:=LambdaOrbMults(LambdaOrb(d), LambdaOrbSCCIndex(d));

      n:=pos-1; m:=Length(enum!.schutz); r:=Length(scc);
      q:=QuoInt(n, m); q2:=QuoInt(q, r);
      pos:=[ n-q*m, q2, q  - q2 * r ]+1;
      return mults[scc[pos[2]]]*enum[pos[1]]/mults[scc[pos[3]]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local rep, o, m, lookup, s, i, j, scc, g, k;

      rep:=Representative(d);
      
      if ActionRank(f)<>ActionRank(rep) or ActionDegree(f)<>ActionDegree(rep) then 
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=LambdaOrb(d); m:=LambdaOrbSCCIndex(d);
      lookup:=OrbSCCLookup(o);
      s:=ParentSemigroup(d);

      i:=Position(o, RhoFunc(s)(f)); 
      if i=fail or not lookup[i]<>m then 
        return fail;
      fi;

      j:=Position(o, LambdaFunc(s)(f));
      if j=fail or not lookup[j]<>m then 
        return fail;
      fi;

      scc:=OrbSCC(o)[m]; g:=f;
      
      if i<>scc[1] then 
        g:=LambdaOrbMult(o, m, i)[1]*g;
      fi;
      
      if j<>scc[1] then 
        g:=g*LambdaOrbMult(o, m, j)[2];
      fi;

      k:=Position(enum!.schutz, LambdaPerm(s)(rep, g));
      if j=fail then 
        return fail;
      fi;

      return Length(enum!.schutz)*((Position(scc, i)-1)*Length(scc)
      +(Position(scc, j)-1))+k;
    end,

    #########################################################################

    Membership:=function(elm, enum)
      return elm in d;
    end,

    Length:=enum-> Size(d),

    PrintObj:=function(enum)
      Print("<enumerator of D-class>");
      return;
    end));
end);


# mod for 1.0! - Enumerator - "for L-class of an inverse op acting semigroup"
##############################################################################

InstallMethod(Enumerator, "for L-class of an acting semigroup",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, mults, scc;

  o:=LambdaOrb(l); 
  m:=LambdaOrbSCCIndex(l);
  mults:=LambdaOrbMults(o, m);
  scc:=OrbSCC(o)[m];

  return EnumeratorByFunctions(l, rec(

    schutz:=Enumerator(SchutzenbergerGroup(l)),

    len:=Size(SchutzenbergerGroup(l)),

    #########################################################################

    ElementNumber:=function(enum, pos)
      local n, m, q;

      if pos>Length(enum) then 
        return fail;
      fi;

      if pos<=Length(enum!.schutz) then 
        return Representative(l)*enum!.schutz[pos];
      fi;

      n:=pos-1; m:=enum!.len;
      
      q:=QuoInt(n, m); 
      pos:=[ q, n - q * m]+1;
     
     return mults[scc[pos[1]]][2]*enum[pos[2]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local s, rep, o, m, i, g, j;

      s:=ParentSemigroup(l);
      rep:=Representative(l);
      
      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
       ActionDegree(f) <> ActionDegree(rep) or ActionRank(f) <> ActionRank(rep)
       or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) then 
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=RhoOrb(l); m:=RhoOrbSCCIndex(l);
      i:=Position(o, RhoFunc(s)(f));

      if i = fail or OrbSCCLookup(o)[i]<>m then 
        return fail;
      fi;
     
      j:=Position(enum!.schutz, LambdaPerm(s)(rep, mults[i][1]*f));

      if j=fail then 
        return fail;
      fi;
      return enum!.len*(Position(scc, i)-1)+j;
    end,

    #########################################################################

    Membership:=function(elm, enum)
      return elm in l;
    end,

    Length:=enum-> Size(l),

    PrintObj:=function(enum)
      Print("<enumerator of L-class>");
      return;
    end));
end);

#GGG

# new for 0.7! - GreensDClasses - for an acting semigp with inverse op
##############################################################################

InstallOtherMethod(GreensDClasses, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local o, scc, out, i, f, m;

  o:=LambdaOrb(s); 
  scc:=OrbSCC(o); 
  out:=EmptyPlist(Length(scc)); 

  i:=0;
  for m in [2..Length(scc)] do 
    i:=i+1;
    f:=RightOne(LambdaOrbRep(o, m));
    out[i]:=CreateDClassNC(s, m, o, fail, fail, f, false);
  od;
  return out;
end);


# new for 1.0! - GreensHClasses - for an acting semigroup with inverse op
############################################################################

InstallOtherMethod(GreensHClasses, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local o, scc, len, out, n, mults, f, m, j, k;
  
  o:=LambdaOrb(s);
  scc:=OrbSCC(o);
  len:=Length(scc);
  
  out:=EmptyPlist(NrHClasses(s));
  n:=0; 
    
  for m in [2..len] do
    mults:=LambdaOrbMults(o, m);
    f:=RightOne(LambdaOrbRep(o, m));
    for j in scc[m] do
      f:=f*mults[j][1];
      for k in scc[m] do
        n:=n+1;
        out[n]:=CreateHClass(s, m, o, fail, fail, mults[k][2]*f, false);
      od;
    od;
  od;
  return out;
end);

# new for 1.0! - GreensHClasses - for inverse op  D-class
############################################################################

InstallOtherMethod(GreensHClasses, "for inverse op D-class",
[IsActingSemigroupGreensClass and IsGreensDClass and IsInverseOpClass],
function(d)
  local s, o, m, f, scc, mults, out, n, g, j, k;
 
  s:=ParentSemigroup(d);
  o:=LambdaOrb(d);
  m:=LambdaOrbSCCIndex(d); 
  f:=Representative(d);
  
  scc:=OrbSCC(o);
  mults:=LambdaOrbMults(o, m);
  out:=EmptyPlist(NrHClasses(d));
  n:=0; 
    
  for j in scc[m] do
    g:=f*mults[j][1];
    for k in scc[m] do
      n:=n+1;
      out[n]:=CreateHClass(s, m, o, fail, fail, mults[k][2]*g, false);
    od;
  od;
  return out;
end);

# new for 1.0! - GreensHClasses - "for inverse op L-class of acting semigroup"
##############################################################################

InstallOtherMethod(GreensHClasses, "for inverse op L-class of acting semigroup",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, scc, mults, f, nc, s, out, k, j;
  
  o:=LambdaOrb(l);
  m:=LambdaOrbSCCIndex(l);
  scc:=OrbSCC(o)[m];
  mults:=LambdaOrbMults(o, m);
  
  f:=Representative(l);
  nc:=IsGreensClassNC(l);
  s:=ParentSemigroup(l);
  
  out:=EmptyPlist(Length(scc));
  k:=0;
  
  for j in scc do
    k:=k+1;
    out[k]:=CreateHClass(s, m, o, fail, fail, mults[j][2]*f, nc);
    SetLClassOfHClass(out[k], l);
  od;
  
  return out;
end);

# new for 1.0! - GreensHClasses - "for inverse op R-class of acting semigroup"
##############################################################################

InstallOtherMethod(GreensHClasses, "for inverse op R-class of acting semigroup",
[IsInverseOpClass and IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, scc, mults, f, nc, s, out, k, j;
  
  o:=LambdaOrb(r);
  m:=LambdaOrbSCCIndex(r);
  scc:=OrbSCC(o)[m];
  mults:=LambdaOrbMults(o, m);
  
  f:=Representative(r);
  nc:=IsGreensClassNC(r);
  s:=ParentSemigroup(r);
  
  out:=EmptyPlist(Length(scc));
  k:=0;
  
  for j in scc do
    k:=k+1;
    out[k]:=CreateHClass(s, m, o, fail, fail, f*mults[j][1], nc);
    SetRClassOfHClass(out[k], r);
  od;
  
  return out;
end);

# new for 0.7! - GreensLClasses - for acting semigroup with inverse op
##############################################################################
    
InstallOtherMethod(GreensLClasses, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local o, scc, len, out, n, f, mults, m, j;
  
  o:=LambdaOrb(s);
  scc:=OrbSCC(o);
  len:=Length(scc);
  out:=EmptyPlist(NrLClasses(s));
  n:=0;

  for m in [2..len] do
    f:=RightOne(LambdaOrbRep(o, m));
    mults:=LambdaOrbMults(o, m);
    for j in scc[m] do
      n:=n+1;
      out[n]:=CreateInverseOpLClassNC(s, m, o, f*mults[j][1], false);
    od;
  od;
  return out;
end);

# new for 1.0! - GreensLClasses - for inverse op D-class of acting semigroup
##############################################################################

InstallOtherMethod(GreensLClasses, "for inverse op D-class of acting semigroup",
[IsActingSemigroupGreensClass and IsInverseOpClass and IsGreensDClass],
function(d)
  local s, m, o, f, nc, out, k, mults, scc, i;
  
  s:=ParentSemigroup(d);
  m:=LambdaOrbSCCIndex(d);
  o:=LambdaOrb(d);
  f:=Representative(d);
  nc:=IsGreensClassNC(d);
  
  out:=EmptyPlist(Length(scc));
  k:=0;
  mults:=LambdaOrbMults(LambdaOrb(d), LambdaOrbSCCIndex(d));
  scc:=LambdaOrbSCC(d);
  
  for i in scc do
    k:=k+1;
    #use NC since f has rho value in first place of scc
    #JDM maybe don't use CreateLClassNC here, and rather expand!
    out[k]:=CreateInverseOpLClassNC(s, m, o, f*mults[i][1], nc);
    SetDClassOfLClass(out[k], d);
  od;

  return out;
end);

# new for 1.0! - GreensDClassOfElement - "for acting semi with inv op and elt."
############################################################################

InstallOtherMethod(GreensDClassOfElement, 
"for acting semi with inv op and elt",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(s, f)
  local o, i, m, rep;

  if not f in s then 
    Error("the element does not belong to the semigroup,");
    return;
  fi;

  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s);
    i:=Position(o, RhoFunc(s)(f)); #DomPP easier to find :)
  else
    o:=GradedLambdaOrb(s, f, true);
    i:=LambdaPos(o);
  fi;
  
  m:=OrbSCCLookup(o)[i];
  rep:=RightOne(LambdaOrbRep(o, m));

  return CreateDClassNC(s, m, o, fail, fail, rep, false); 
end);

# new for 1.0! - GreensDClassOfElementNC - "for acting semi  inv op and elt
############################################################################

InstallOtherMethod(GreensDClassOfElementNC, 
"for an acting semigp with inverse op and elt",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(s, f)
  return CreateDClassNC(s, 1, GradedLambdaOrb(s, f, false), 
   fail, fail, f, true);
end);

# new for 1.0! - GreensHClassOfElement - "for acting semigp inverse op and elt."
#############################################################################

InstallOtherMethod(GreensHClassOfElement, 
"for an acting semigp with inverse op and elt",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(s, f)
  local o, m;

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;

  o:=LambdaOrb(s);
  m:=OrbSCCLookup(o)[Position(o, LambdaFunc(s)(f))];

  return CreateHClass(s, m, o, fail, fail, f, false);
end);

# new for 1.0! - GreensHClassOfElementNC - "for acting semigp inverse op & elt."
#############################################################################

InstallOtherMethod(GreensHClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(s, f)
  return CreateHClass(s, 1, GradedLambdaOrb(s, f, false),
   fail, fail, f, true);
end);

# new for 1.0! - GreensHClassOfElement - "for inverse op class and elt."
############################################################################

InstallOtherMethod(GreensHClassOfElement, "for inverse op class and elt",
[IsActingSemigroupGreensClass and IsInverseOpClass, IsAssociativeElement],
function(x, f)
  local h;
  
  if not f in x then
    Error("the element does not belong to the Green's class,");
    return;
  fi;
  
  h:=CreateHClass(ParentSemigroup(x), LambdaOrbSCCIndex(x), LambdaOrb(x), fail,
   fail, f, IsGreensClassNC(x));
  
  if IsGreensLClass(x) then 
    SetLClassOfHClass(h, x);
  elif IsGreensRClass(x) then 
    SetRClassOfHClass(h, x);
  elif IsGreensDClass(x) then 
    SetDClassOfHClass(h, x);
  fi;
  
  return h;
end);

# new for 1.0! - GreensHClassOfElementNC - "for inverse op L-class and elt."
############################################################################

InstallOtherMethod(GreensHClassOfElementNC, "for inverse op class and elt",
[IsActingSemigroupGreensClass and IsInverseOpClass and IsGreensLClass, IsAssociativeElement],
function(x, f)
  local h;
  
  h:=CreateHClass(ParentSemigroup(x), LambdaOrbSCCIndex(x), LambdaOrb(x), fail,
   fail, f, true);

  if IsGreensLClass(x) then 
    SetLClassOfHClass(h, x);
  elif IsGreensRClass(x) then 
    SetRClassOfHClass(h, x);
  elif IsGreensDClass(x) then 
    SetDClassOfHClass(h, x);
  fi;
  
  return h;
end);

# mod for 1.0! - GreensLClassOfElement - "for acting semigp inverse op and elt."
#############################################################################

InstallOtherMethod(GreensLClassOfElement, 
"for acting semigp with inverse op and elt",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(s, f)
  local o;

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;

  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then
    o:=LambdaOrb(s);
  else
    o:=GradedLambdaOrb(s, f, true);
  fi;

  return CreateInverseOpLClass(s, fail, o, f, false);
end);

# mod for 1.0! - GreensLClassOfElementNC - "for an acting semigp and elt."
#############################################################################

InstallOtherMethod(GreensLClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroupWithInverseOp, IsAssociativeElement],
function(s, f)
  # use NC since rho value of f has to be in first place of GradedRhoOrb
  # with false as final arg
  return CreateInverseOpLClassNC(s, 1, GradedLambdaOrb(s, f, false), f, true);
end);

# mod for 1.0! - GreensLClassOfElement - "for inverse op D-class and elt"
#############################################################################

InstallOtherMethod(GreensLClassOfElement, "for inverse op D-class and elt",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local l;

  if not f in d then
    Error("the element does not belong to the D-class,");
    return;
  fi;

  # use non-NC so that rho value of f is rectified
  l:=CreateInverseOpLClass(ParentSemigroup(d), LambdaOrbSCCIndex(d),
   LambdaOrb(d), f, IsGreensClassNC(d));

  SetDClassOfLClass(l, d);
  return l;
end);

# mod for 1.0! - GreensLClassOfElementNC - "for D-class and acting elt"
#############################################################################

InstallOtherMethod(GreensLClassOfElementNC, "for D-class and acting elt",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass, IsAssociativeElement],
function(d, f)
  local l;

  # use non-NC so taht rho value of f is rectified
  l:=CreateInverseOpLClass(ParentSemigroup(d), LambdaOrb(d),
   LambdaOrbSCCIndex(d), f, true);
  SetDClassOfLClass(l, d);
  return l;
end);

# new for 0.7! - GreensRClasses - for acting semigroup with inverse op
##############################################################################
                    
InstallOtherMethod(GreensRClasses, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)         
  local o, scc, len, out, i, f, mults, m, j;
                    
  o:=LambdaOrb(s);
  scc:=OrbSCC(o);   
  len:=Length(scc);
  out:=EmptyPlist(Length(o));

  i:=0;             
                    
  for m in [2..len] do
    f:=RightOne(LambdaOrbRep(o, m));
    mults:=LambdaOrbMults(o, m);
    for j in scc[m] do
      i:=i+1;    
      out[i]:=CreateRClassNC(s, m, o, mults[j][2]*f, false);
    od;             
  od;

  return out;
end);

# new for 0.7! - GreensRClasses - for inverse op D-class of acting semigroup
##############################################################################
                    
InstallOtherMethod(GreensRClasses, "for inverse op D-class",
[IsActingSemigroupGreensClass and IsInverseOpClass and IsGreensDClass],
function(d)         
  local s, o, m, f, scc, mults, out, i, j;
  
  s:=ParentSemigroup(d);
  o:=LambdaOrb(d);
  m:=LambdaOrbSCCIndex(d);
  f:=Representative(d);
  
  scc:=OrbSCC(o)[m];   
  mults:=LambdaOrbMults(o, LambdaOrbSCCIndex(d));
  out:=EmptyPlist(Length(o));
  i:=0;             
  
  for j in scc do
    i:=i+1;    
    out[i]:=CreateRClassNC(s, m, o, mults[j][2]*f, false);
  od;             

  return out;
end);

# mod for 1.0! - GroupHClass - "for an inverse op D-class"
#############################################################################

InstallOtherMethod(GroupHClass, "for an inverse op D-class",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass], 
d-> CreateHClass(ParentSemigroup(d), LambdaOrbSCCIndex(d), LambdaOrb(d), fail, 
fail, Representative(d), IsGreensClassNC(d)));

#III

# new for 1.0! - Idempotents - for D-class of acting semigroup with inverse op
###############################################################################

InstallOtherMethod(Idempotents, "for an inverse op D-class",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass], 
function(d)
  local creator, o;

  creator:=IdempotentCreator(ParentSemigroup(d));
  o:=LambdaOrb(d);
  return List(LambdaOrbSCC(d), x-> creator(o[x], o[x]));
end);

# new for 1.0! - Idempotents - for L-class of acting semigroup with inverse op
###############################################################################

InstallOtherMethod(Idempotents, "for an inverse op L-class",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass], 
l-> [RightOne(Representative(l))]);

# new for 1.0! - Idempotents - for R-class of acting semigroup with inverse op
###############################################################################

InstallOtherMethod(Idempotents, "for an inverse op R-class",
[IsInverseOpClass and IsGreensRClass and IsActingSemigroupGreensClass], 
r-> [LeftOne(Representative(r))]);

# new for 1.0! - Idempotents - for acting semigroup with inverse op
###############################################################################

InstallOtherMethod(Idempotents, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp], 
function(s)
  local o, creator, r, out, i;

  o:=LambdaOrb(s);
  if not IsClosed(o) then      
    Enumerate(o, infinity);
  fi;

  creator:=IdempotentCreator(s);
  r:=Length(o);
  out:=EmptyPlist(r-1);

  for i in [2..r] do
    out[i-1]:=creator(o[i], o[i]);
  od;
  return out;
end);

# new for 1.0! - RClassReps - for an acting semigroup with inverse op
##############################################################################

InstallOtherMethod(RClassReps, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp], s-> List(LClassReps(s), x-> x^-1));

# new for 1.0! - RClassReps - for D-class of acting semigroup with inverse op
##############################################################################

InstallOtherMethod(RClassReps, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsInverseOpClass and IsGreensDClass],
d-> List(LClassReps(d), x-> x^-1));

# can't use LambdaOrb and LambdaOrbMult(s) here since if l=scc[m][1], then 
# LambdaOrbMult(o, m, l)[1] or [2]=One(o!.gens) which is not necessarily in the
# semigroup

InstallMethod(Random, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local gens, i, w;
    
  gens:=GeneratorsOfSemigroup(s);    
  i:=Random([1..Int(Length(gens)/2)]);
  w:=List([1..i], x-> Random([1..Length(gens)]));
  return EvaluateWord(gens, w);
end);

#SSS

# new for 1.0! - SchutzenbergerGroup - for an acting semigroup with inverse op
##############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for an inverse op L-class",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
l-> LambdaOrbSchutzGp(LambdaOrb(l), LambdaOrbSCCIndex(l))); 

# new for 1.0! - Size - "for an acting semigroup with inversion"
##############################################################################

InstallOtherMethod(Size, "for an acting semigroup with inversion",
[IsActingSemigroupWithInverseOp], 10, 
function(s)
  local o, scc, r, nr, m;

  o:=LambdaOrb(s);   
  scc:=OrbSCC(o);
  r:=Length(scc); 
  nr:=0;

  for m in [2..r] do 
    nr:=nr+Length(scc[m])^2*Size(LambdaOrbSchutzGp(o, m));
  od;
  return nr;
end);

# new for 1.0! - Size - "for an inverse op D-class"
##############################################################################

InstallOtherMethod(Size, "for an inverse op D-class",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass],
d-> Size(SchutzenbergerGroup(d))*Length(LambdaOrbSCC(d))^2);

# new for 1.0! - Size - "for an inverse op L-class"
##############################################################################

InstallOtherMethod(Size, "for an inverse op L-class",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
l-> Size(SchutzenbergerGroup(l))*Length(LambdaOrbSCC(l)));

#HHH

# new for 1.0! - HClassReps - for an acting semigroup with inverse op
############################################################################

InstallOtherMethod(HClassReps, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local o, scc, len, out, n, mults, f, m, j, k;
  
  o:=LambdaOrb(s);
  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;
  scc:=OrbSCC(o);
  len:=Length(scc);
  
  out:=EmptyPlist(NrHClasses(s));
  n:=0; 
    
  for m in [2..len] do
    mults:=LambdaOrbMults(o, m);
    f:=RightOne(LambdaOrbRep(o, m));
    for j in scc[m] do
      f:=f*mults[j][1];
      for k in scc[m] do
        n:=n+1;
        out[n]:=mults[k][1]*f;
      od;
    od;
  od;
  return out;
end);

# new for 1.0! - HClassReps - "for a inverse op D-class of an acting semigroup"
##############################################################################

InstallOtherMethod(HClassReps, "for a inverse op D-class of acting semigroup",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local o, m, scc, mults, f, out, k, g, i, j;
  
  o:=LambdaOrb(d); 
  m:=LambdaOrbSCCIndex(d);
  scc:=OrbSCC(o)[m];
  mults:=LambdaOrbMults(o, m);
  
  f:=Representative(d);
  
  out:=EmptyPlist(Length(scc)^2);
  k:=0;
  
  for i in scc do
    g:=f*mults[i][1];
    for j in scc do
      k:=k+1;
      out[k]:=mults[j][1]*g;
    od;
  od;
  return out;
end);

# new for 1.0! - HClassReps - "for an inverse op L-class"
##############################################################################

InstallOtherMethod(HClassReps, "for an inverse op L-class",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, scc, mults, f, out, k, i;
  
  o:=LambdaOrb(l); 
  m:=LambdaOrbSCCIndex(l);
  scc:=OrbSCC(o)[m];
  mults:=LambdaOrbMults(o, m);
  f:=Representative(l);
  
  out:=EmptyPlist(Length(scc));
  k:=0;
  
  for i in scc do
    k:=k+1;
    out[k]:=mults[i][2]*f;
  od;
  return out;
end);

#

InstallMethod(IteratorOfRClassData, "for acting semigp with inverse op",
[IsActingSemigroupWithInverseOp], 
function(s)
local iter, scc;
  
  if not IsClosed(LambdaOrb(s)) then 
    
    iter:=IteratorByNextIterator( rec(

      i:=1,

      NextIterator:=function(iter)
        local o, i, f;

        o:=LambdaOrb(s); i:=iter!.i;
        
        if IsClosed(o) and i>=Length(o) then 
          return fail;
        fi;

        i:=i+1;
        
        if i>Length(o) then 
          Enumerate(o, i); 
          if i>Length(o) then 
            return fail;
          fi;
        fi;

        iter!.i:=i; 
        f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, i))^-1; 
        return [s, fail, GradedLambdaOrb(s, f, true), f, false];
      end,

      ShallowCopy:=iter-> rec(i:=1)));
  else ####

    scc:=OrbSCC(LambdaOrb(s));

    iter:=IteratorByFunctions( rec(
                 
      m:=2, 
     
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
 
        f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, scc[m][i]))^-1; 
        return [s, m, LambdaOrb(s), f, false];
      end,

      ShallowCopy:=iter-> rec(m:=1, i:=0,
      scc_limit:=iter!.scc_limit, i_limit:=iter!.i_limit)));
  fi;
  
  return iter;
end);

# new for 0.7! - IteratorOfLClassReps - "for acting semigroup with inverse op"
###############################################################################

InstallMethod(IteratorOfLClassReps, "for acting semigp with inverse op",
[IsActingSemigroupWithInverseOp],
s-> IteratorByIterator(IteratorOfRClassData(s), x-> x[4]^-1,
[IsIteratorOfLClassReps]));

# new for 0.7! - IteratorOfLClasses - "for acting semigroup with inverse op" 
###############################################################################

InstallMethod(IteratorOfLClasses, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
s-> IteratorByIterator(IteratorOfRClassData(s), 
function(x)
  x[4]:=x[4]^-1;
  return CallFuncList(CreateInverseOpLClass, x);
end, [IsIteratorOfLClasses]));

#NNN

# new for 0.7! - NaturalPartialOrder - "for an inverse semigroup"
##############################################################################
# C function for me! JDM

InstallMethod(NaturalPartialOrder, "for an inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  local elts, n, out, i, j;

  elts:=Elements(s);  n:=Length(elts);
  out:=List([1..n], x-> EmptyPlist(n));
  for i in [n, n-1..1] do
    for j in [i-1,i-2 ..1] do
      if NaturalLeqPP(elts[j], elts[i]) then
        AddSet(out[i], j);
      fi;
    od;
  od;
  Perform(out, ShrinkAllocationPlist);
  return out;
end);

# new for 1.0! - NrIdempotents - for an acting semigroup with inverse op
##############################################################################

InstallOtherMethod(NrIdempotents, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp], 
s-> Length(Enumerate(LambdaOrb(s), infinity))-1);     

# new for 1.0! - NrIdempotents - for an inverse op D-class
##############################################################################

InstallOtherMethod(NrIdempotents, "for an inverse op D-class",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass], NrLClasses);   

# new for 1.0! - NrIdempotents - for an inverse op L-class
##############################################################################

InstallOtherMethod(NrIdempotents, "for an inverse op L-class",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass], l-> 1);   

# new for 1.0! - NrIdempotents - for an inverse op R-class
##############################################################################

InstallOtherMethod(NrIdempotents, "for an inverse op R-class",
[IsInverseOpClass and IsGreensRClass and IsActingSemigroupGreensClass], r-> 1);   

# mod for 1.0! - NrRClasses - for an acting semigroup with inverse op
##############################################################################

InstallOtherMethod(NrRClasses, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp], NrLClasses);

# mod for 1.0! - NrRClasses - for inverse op D-class 
##############################################################################

InstallOtherMethod(NrRClasses, "for inverse op D-class",
[IsInverseOpClass and IsGreensDClass and IsActingSemigroupGreensClass], NrLClasses);

# new for 1.0! - NrHClasses - "for an inverse op D-class of acting semigroup"
############################################################################

InstallOtherMethod(NrHClasses, "for an inverse op D-class of acting semigroup",
[IsActingSemigroupGreensClass and IsInverseOpClass and IsGreensDClass],
l-> Length(LambdaOrbSCC(l))^2);

# new for 1.0! - NrHClasses - "for an inverse op L-class of acting semigroup"
############################################################################

InstallOtherMethod(NrHClasses, "for an inverse op L-class of acting semigroup",
[IsActingSemigroupGreensClass and IsInverseOpClass and IsGreensLClass],
l-> Length(LambdaOrbSCC(l)));

# mod for 1.0! - NrHClasses - for an acting semigroup with inverse op
##############################################################################

InstallOtherMethod(NrHClasses, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local o, scc;
  o:=Enumerate(LambdaOrb(s), infinity);
  scc:=OrbSCC(o);

  return Sum(List(scc, m-> Length(m)^2))-1;
end);

# new for 0.7! - PartialOrderOfDClasses - "for acting semigp with inverse op
############################################################################## 
 
InstallMethod(PartialOrderOfDClasses, "for acting semigp with inverse op",
[IsActingSemigroupWithInverseOp],      
function(s)            
  local d, n, out, o, gens, lookup, l, lambdafunc, i, x, f;
                       
  d:=GreensDClasses(s);
  n:=Length(d);
  out:=List([1..n], x-> EmptyPlist(n)); 
  o:=LambdaOrb(s);        
  gens:=o!.gens;
  lookup:=OrbSCCLookup(o);
  lambdafunc:=LambdaFunc(s);
 
  for i in [1..n] do  
    for x in gens do  
      for f in RClassReps(d[i]) do
        AddSet(out[i], lookup[Position(o, lambdafunc(x*f))]-1);      
        AddSet(out[i], lookup[Position(o, lambdafunc(f^-1*x))]-1);     
      od; 
    od;
  od; 
 
  Perform(out, ShrinkAllocationPlist);
  return out; 
end);

