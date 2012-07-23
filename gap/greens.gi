#############################################################################
##
#W  greens.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# for convenience...

# new for 1.0! - RhoOrbStabChain - "for an L-class of an acting semi"
##############################################################################

# same method for regular, not required for inverse. 

InstallMethod(RhoOrbStabChain, "for an L-class of an acting semi",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  #return StabilizerChain(SchutzenbergerGroup(l));
  return StabChainImmutable(SchutzenbergerGroup(l));
end);

# new for 1.0! - RhoCosets - "for a D-class of an acting semigp"
##############################################################################

InstallMethod(RhoCosets, "for a D-class of an acting semigp",
[IsGreensDClass and IsActingSemigroupGreensClass], 
function(d)
  SchutzenbergerGroup(d);
  return RhoCosets(d);
end);

# new for 1.0! - RhoOrbStabChain - "for a D-class of an acting semigp"
##############################################################################

InstallMethod(RhoOrbStabChain, "for a D-class of an acting semigp",
[IsGreensDClass and IsActingSemigroupGreensClass], 
function(d)
  SchutzenbergerGroup(d);
  return RhoOrbStabChain(d);
end);

# new for 1.0! - SemigroupDataSCC - "for a D-class of an acting semigp"
##############################################################################
# JDM this is useful in PartialOrderOfDClasses...

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

# new for 1.0! - LambdaCosets - "for a D-class of an acting semigp"
##############################################################################

InstallMethod(LambdaCosets, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return RightTransversal(LambdaOrbSchutzGp(LambdaOrb(d),
   LambdaOrbSCCIndex(d)), SchutzenbergerGroup(d));
end);

# new for 1.0! - LambdaOrbSCC - "for Green's class of an acting semigroup"
############################################################################

InstallOtherMethod(LambdaOrbSCC, "for a Green's class of an acting semi",
[IsActingSemigroupGreensClass and IsGreensClass],
x-> OrbSCC(LambdaOrb(x))[LambdaOrbSCCIndex(x)]);

InstallOtherMethod(RhoOrbSCC, "for a Green's class of an acting semi",
[IsActingSemigroupGreensClass and IsGreensClass], 
x-> OrbSCC(RhoOrb(x))[RhoOrbSCCIndex(x)]);

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

  p:=LambdaConjugator(ParentSemigroup(d))(RhoOrbRep(o, m),
   Representative(d));
  rho_schutz:=rho_schutz^p;

  SetRhoOrbStabChain(d, StabChainImmutable(rho_schutz));
  #SetRhoOrbStabChain(d, StabilizerChain(rho_schutz));
  
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

# same method for regular/inverse

InstallMethod(SchutzenbergerGroup, "for a H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local lambda_o, lambda_m, lambda_schutz, lambda_stab, rho_o, rho_m, rho_schutz, rho_stab, rep, s, lambda_p, rho_p;
 
  lambda_o:=LambdaOrb(h); lambda_m:=LambdaOrbSCCIndex(h);
  lambda_schutz:=LambdaOrbSchutzGp(lambda_o, lambda_m); 
  s:=ParentSemigroup(h);
  
  if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
    return lambda_schutz;
  fi;
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
  lambda_p:=LambdaConjugator(s)(rep*lambda_p, rep);
 
  if rho_stab=true then 
    return lambda_schutz^lambda_p;
  fi;

  rho_p:=RhoOrbMults(rho_o, rho_m)[Position(rho_o, RhoFunc(s)(rep))][2];
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

#############################################################################
#############################################################################

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

# new for 1.0! - \in - "for acting elt and D-class of acting semigp"
#############################################################################
#JDM revise this as per the other version of \in just deleted :)

# different method for regular/inverse 

InstallMethod(\in, "for acting elt and D-class of acting semigp.",
[IsActingElt, IsGreensDClass and IsActingSemigroupGreensClass],
function(f, d)
  local rep, s, g, m, o, scc, l, schutz, cosets, x;
  
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
    g:=RhoOrbMults(o, m)[l][2]*g;
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
[IsActingElt, IsGreensHClass and IsActingSemigroupGreensClass],
function(f, h)
  local s, rep;

  s:=ParentSemigroup(h);
  rep:=Representative(h);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or f[2] <> rep[2] or
   RhoFunc(s)(f) <> RhoFunc(s)(rep) or LambdaFunc(s)(f) <> LambdaFunc(s)(rep)
   # degree causes problems here again JDM
   then 
    return false;
  fi;

  return LambdaPerm(s)(rep, f) in SchutzenbergerGroup(h);
end);

# new for 1.0! - \in - "for acting elt and L-class of acting semigp"
#############################################################################
#JDM this method differs from the one in 0.99. 

# same method for regular, different method for inverse 

InstallMethod(\in, "for acting elt and L-class of acting semigp.",
[IsActingElt, IsGreensLClass and IsActingSemigroupGreensClass],
function(f, l)
  local rep, s, m, o, i, schutz, g, p;

  rep:=Representative(l); 
  s:=ParentSemigroup(l);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or Degree(f) <> Degree(rep)
   or Rank(f) <> Rank(rep) or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) then
    Info(InfoCitrus, 1, "degree, rank, or lambda value not equal to those of",
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
    Info(InfoCitrus, 3, "Schutz. group of L-class is symmetric group");
    return true;
  fi;

  if i<>OrbSCC(o)[m][1] then 
    g:=RhoOrbMult(o, m, i)[2]*f;
  else 
    g:=f;
  fi;

  if g=rep then
    Info(InfoCitrus, 3, "element with rectified rho value equals ",
    "L-class representative");
    return true;
  elif schutz=false then
    Info(InfoCitrus, 3, "Schutz. group of L-class is trivial");
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
[IsActingElt, IsGreensRClass and IsActingSemigroupGreensClass],
function(f, r)
  local rep, s, m, o, l, schutz, g;

  rep:=Representative(r); 
  s:=ParentSemigroup(r);

  #JDM degree causes problems for partial perms below...
  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) #or Degree(f) <> Degree(rep)
   or Rank(f) <> Rank(rep) or RhoFunc(s)(f) <> RhoFunc(s)(rep) then
    Info(InfoCitrus, 1, "degree, rank, or rho value not equal to those of",
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
    Info(InfoCitrus, 3, "Schutz. group of R-class is symmetric group");
    return true;
  fi;

  g:=f;
  
  if l<>OrbSCC(o)[m][1] then 
    g:=f*LambdaOrbMult(o, m, l)[2];
  fi;

  if g=rep then
    Info(InfoCitrus, 3, "element with rectified lambda value equals ",
    "R-class representative");
    return true;
  elif schutz=false then
    Info(InfoCitrus, 3, "Schutz. group of R-class is trivial");
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

#CCC

# new for 1.0! - CreateDClass - not a user function! 
############################################################################# 
# Usage: arg[1] = semigroup; arg[2] = lambda orb scc index;
# arg[3] = lambda orb; arg[4] = rep; arg[5] = position in SemigroupData of rep.

# not used anywhere!

InstallGlobalFunction(CreateDClass,  
function(arg) 
  local d, rectify;
 
  d:=Objectify(DClassType(arg[1]), rec()); 
          
  SetParentSemigroup(d, arg[1]);
  SetLambdaOrb(d, arg[3]);
  SetLambdaOrbSCCIndex(d, arg[2]);
  SetSemigroupDataIndex(d, arg[5]);

  rectify:=RectifyRho(arg[1], RhoOrb(arg[1]), arg[4]);
  
  SetRepresentative(d, rectify.rep);
  SetRhoOrb(d, RhoOrb(arg[1]));
  SetRhoOrbSCCIndex(d, rectify.m);
  SetEquivalenceClassRelation(d, GreensDRelation(arg[1])); 
  SetIsGreensClassNC(d, false);

  return d; 
end); 

# new for 1.0! - CreateHClass - not a user function! 
############################################################################# 
# Usage: arg[1] = semigroup; arg[2] = rep; 
# arg[3] = lambda orb;  arg[4] = lambda orb scc index; 
# arg[5] = rho orb; arg[6] = rho orb scc index
# arg[7] = IsGreensClassNC. 

InstallGlobalFunction(CreateHClass, 
function(arg)
  local h;
  
  h:=Objectify(HClassType(arg[1]), rec());
  SetParentSemigroup(h, arg[1]);

  SetRepresentative(h, arg[2]);
  SetLambdaOrb(h, arg[3]);
  SetLambdaOrbSCCIndex(h, arg[4]);
  if arg[5]<>fail then 
    SetRhoOrb(h, arg[5]);
    SetRhoOrbSCCIndex(h, arg[6]);
  fi;
  
  SetEquivalenceClassRelation(h, GreensHRelation(arg[1]));
  SetIsGreensClassNC(h, arg[7]);
  return h;
end);

# new for 1.0! - CreateDClassNC - not a user function! 
############################################################################# 
# Usage: arg[1] = semigroup; arg[2] = lambda orb scc index;
# arg[3] = lambda orb; arg[4] = rep; 
# arg[5] = IsGreensClassNc

InstallGlobalFunction(CreateDClassNC,  
function(arg) 
  local d, rep, o, l, m;
 
  d:=Objectify(DClassType(arg[1]), rec()); 
          
  SetParentSemigroup(d, arg[1]);
  SetLambdaOrb(d, arg[3]);
  SetLambdaOrbSCCIndex(d, arg[2]);
  SetRepresentative(d, RectifyLambda(arg[1], arg[3], arg[4]).rep);
  
  SetRhoOrb(d, GradedLambdaOrb(arg[1], arg[4], false));
  SetRhoOrbSCCIndex(d, 1);
 
  SetEquivalenceClassRelation(d, GreensDRelation(arg[1])); 
  SetIsGreensClassNC(d, arg[5]);

  return d; 
end); 

# mod for 1.0! - CreateLClass - not a user function!
#############################################################################

# this is the analogue of Create.ClassNC

InstallGlobalFunction(CreateLClass,
function(arg)
  local l;

  l:=Objectify(LClassType(arg[1]), rec());

  SetParentSemigroup(l, arg[1]);
  SetRhoOrbSCCIndex(l, arg[2]);
  SetRhoOrb(l, arg[3]);
  SetRepresentative(l, arg[4]);
  
  SetEquivalenceClassRelation(l, GreensLRelation(arg[1]));
  SetIsGreensClassNC(l, arg[5]); 
  return l;
end);

# mod for 1.0! - CreateRClass - not a user function!
#############################################################################
# Usage: arg[1] = semigroup; arg[2] = lambda orb scc index;
# arg[3] = lambda orb; arg[4] = rep; arg[5] = position in SemigroupData of rep.

# only use for R-classes created from SemigroupData. 

InstallGlobalFunction(CreateRClass,
function(arg)
  local r;
  
  r:=Objectify(RClassType(arg[1]), rec());

  SetParentSemigroup(r, arg[1]);
  SetLambdaOrbSCCIndex(r, arg[2]);
  SetLambdaOrb(r, arg[3]);
  SetRepresentative(r, arg[4]);
  SetSemigroupDataIndex(r, arg[5]);

  SetEquivalenceClassRelation(r, GreensRRelation(arg[1]));
  SetIsGreensClassNC(r, false);
  return r;
end);

# mod for 1.0! - CreateRClass - not a user function!
#############################################################################
# Usage: arg[1] = semigroup; arg[2] = lambda orb scc index;
# arg[3] = lambda orb; arg[4] = rep; arg[5] = IsRClassNC.

# only use for R-classes created from SemigroupData. 

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
  return r;
end);

#EEE

# mod for 1.0! - Enumerator - "for a D-class of acting semigp."
#############################################################################

# different method for inverse/regular

InstallOtherMethod(Enumerator, "for a D-class of acting semigp.",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
    
    Enumerate(LambdaOrb(d), infinity);
    Enumerate(RhoOrb(d), infinity);

    return EnumeratorByFunctions(d, rec(

    m:=Length(LambdaOrbSCC(d))*Size(LambdaOrbSchutzGp(LambdaOrb(d),
     LambdaOrbSCCIndex(d))),
    # size of any R-class in d.

    #######################################################################

    ElementNumber:=function(enum, pos)
    local q, n, m, R;

      if pos>Length(enum) then
        return fail;
      fi;

      R:=GreensRClasses(d);
      n:=pos-1;
      m:=enum!.m;

      q := QuoInt(n, m);
      pos:= [ q, n - q * m ]+1;

      return Enumerator(R[pos[1]])[pos[2]];
    end,

    #######################################################################

    NumberElement:=function(enum, f)
      local s, rep, g, lm, lo, lscc, ll, lschutz, rm, ro, rscc, rl, schutz,
      cosets, j, r; 

      s:=ParentSemigroup(d);
      rep:=Representative(d);

      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or f[2] <> rep[2] then
        return fail;
      fi;

      lm:=LambdaOrbSCCIndex(d); lo:=LambdaOrb(d); lscc:=OrbSCC(lo);
      ll:=Position(lo, LambdaFunc(s)(f));

      if ll = fail or OrbSCCLookup(lo)[ll]<>lm then
        return fail;
      fi;
     
      if ll<>lscc[lm][1] then
        f:=f*LambdaOrbMult(lo, lm, ll)[2];
      fi;
      g:=f;
      lschutz:=Enumerator(LambdaOrbSchutzGp(lo, lm));

      rm:=RhoOrbSCCIndex(d); ro:=RhoOrb(d); rscc:=OrbSCC(ro);
      rl:=Position(ro, RhoFunc(s)(g));

      if rl = fail or OrbSCCLookup(ro)[rl]<>rm then
        return fail;
      fi;
      
      if rl<>rscc[rm][1] then
        g:=RhoOrbMults(ro, rm)[rl][2]*f;
      fi;

      schutz:=RhoOrbStabChain(d);
      cosets:=RhoCosets(d);
      g:=LambdaPerm(s)(rep, g);
     
      # couldn't the following just be replaced with PositionCanonical?
      if schutz=true or schutz=false then
        j:=PositionCanonical(cosets, g);
      else
        for j in [1..Length(cosets)] do
          #if SiftGroupElement(schutz, g*cosets[j]).isone then 
          if SiftedPermutation(schutz, g*cosets[j])=() then 
            break;
          else
            j:=fail;
          fi;
        od;
      fi;
      
      if j=fail then 
        return fail;
      fi;
      #JDM better to avoid the following line (which is essential)
      r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
      #return enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
      return enum!.m*r+Length(lschutz)*(Position(lscc[lm], ll)-1)+
      Position(lschutz, LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1]*
       rep/cosets[j], f));
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

# mod for 1.0! - Enumerator - "for H-class of acting semigp."
#############################################################################

# same method for inverse/regular

InstallOtherMethod(Enumerator, "for H-class of acting semigp.",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)

  return EnumeratorByFunctions(h, rec(

    schutz:=Enumerator(SchutzenbergerGroup(h)),

    #########################################################################

    ElementNumber:=function(enum, pos)
      if pos>Length(enum) then
        return fail;
      fi;

      return Representative(h)*enum!.schutz[pos];
    end,

    #########################################################################

    NumberElement:=function(enum, f)
      local s, rep;
      s:=ParentSemigroup(h);
      rep:=Representative(h);

      if Rank(f) <> Rank(rep) or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) or
       RhoFunc(s)(f) <> RhoFunc(s)(rep) then
        return fail;
      fi;

      return Position(enum!.schutz, LambdaPerm(s)(rep, f));
    end,

    ###########################################################################

    Membership:=function(elm, enum)
      return elm in h; #the H-class itself!
    end,

    Length:=enum -> Size(h),

    PrintObj:=function(enum)
      Print( "<enumerator of H-class>");
      return;

  end));
end);

# mod for 1.0! - Enumerator - "for R-class of an acting semigroup"
##############################################################################

# same method for regular/inverse

InstallMethod(Enumerator, "for R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, mults, scc;

  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
  mults:=LambdaOrbMults(o, m);
  scc:=OrbSCC(o)[m];

  return EnumeratorByFunctions(r, rec(

    schutz:=Enumerator(SchutzenbergerGroup(r)),

    len:=Size(SchutzenbergerGroup(r)),

    #########################################################################

    ElementNumber:=function(enum, pos)
      local n, m, q;

      if pos>Length(enum) then 
        return fail;
      fi;

      if pos<=Length(enum!.schutz) then 
        return Representative(r)*enum!.schutz[pos];
      fi;

      n:=pos-1; m:=enum!.len;
      
      q:=QuoInt(n, m); 
      pos:=[ q, n - q * m]+1;
     
     return enum[pos[2]]*mults[scc[pos[1]]][1];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local s, rep, o, m, l, g, j;

      s:=ParentSemigroup(r);
      rep:=Representative(r);
      
      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
       #JDM degree causes problems for partial perms
       #Degree(f) <> Degree(rep) or 
       Rank(f) <> Rank(rep) or RhoFunc(s)(f) <> RhoFunc(s)(rep) then 
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=LambdaOrb(r); m:=LambdaOrbSCCIndex(r);
      l:=Position(o, LambdaFunc(s)(f));

      if l = fail or OrbSCCLookup(o)[l]<>m then 
        return fail;
      fi;
     
      g:=f*mults[l][2];

      j:=Position(enum!.schutz, LambdaPerm(s)(rep, g));

      if j=fail then 
        return fail;
      fi;
      return enum!.len*(Position(scc, l)-1)+j;
    end,

    #########################################################################

    Membership:=function(elm, enum)
      return elm in r;
    end,

    Length:=enum-> Size(r),

    PrintObj:=function(enum)
      Print("<enumerator of R-class>");
      return;
    end));
end);

# mod for 1.0! - Enumerator - "for L-class of an acting semigroup"
##############################################################################

# same method for regular, different method for inverse

InstallMethod(Enumerator, "for L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, mults, scc;

  o:=RhoOrb(l); 
  m:=RhoOrbSCCIndex(l);
  mults:=RhoOrbMults(o, m);
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
     
     return mults[scc[pos[1]]][1]*enum[pos[2]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local s, rep, o, m, i, g, j;

      s:=ParentSemigroup(l);
      rep:=Representative(l);
      
      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
       Degree(f) <> Degree(rep) or Rank(f) <> Rank(rep) or 
       LambdaFunc(s)(f) <> LambdaFunc(s)(rep) then 
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
     
      j:=Position(enum!.schutz, LambdaPerm(s)(rep, mults[i][2]*f));

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

# mod for 1.0! - Enumerator - "for an acting semigroup"
#############################################################################
# Notes: this is not an enumerator as I could not get an enumerator to perform 
# well here. 

# same method for regular/inverse

InstallMethod(Enumerator, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local out, iter, j, i;

  out:=EmptyPlist(Size(s)); 

  iter:=Iterator(s);
  j:=0;

  for i in iter do 
    j:=j+1;
    out[j]:=i;
  od;

  return Immutable(out);
end);

#GGG

# mod for 1.0! - GreensDClasses - "for an acting semigroup"
##############################################################################

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
    SetSemigroupDataIndex(d, arg[5]);

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

# new for 1.0! - GreensHClasses - "for an R-class of an acting semigroup"
##############################################################################

# JDM change other GreensHClasses etc to do the same as this one w.r.t.
# IsGreensClassNC

# different method for regular/inverse

InstallOtherMethod(GreensHClasses, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, scc, mults, d, cosets, f, out, k, i, j;

  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
  
  scc:=OrbSCC(o)[m];
  mults:=LambdaOrbMults(o, m);
  d:=DClassOfRClass(r);
  cosets:=LambdaCosets(d);
  f:=Representative(r);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  k:=0;
 
  if not IsGreensClassNC(r) then 
    for i in cosets do 
      i:=f*i;
      for j in scc do 
        k:=k+1; 
        out[k]:=GreensHClassOfElementNC(d, i*mults[j][1]);
        SetRClassOfHClass(out[k], r);
        ResetFilterObj(out[k], IsGreensClassNC);
        #JDM also set schutz gp here!
      od;
    od;
  else
    for i in cosets do 
      i:=f*i;
      for j in scc do 
        k:=k+1; 
        out[k]:=GreensHClassOfElementNC(d, i*mults[j][1]);
        SetRClassOfHClass(out[k], r);
        #JDM also set schutz gp here!
      od;
    od;
  fi;
  return out;
end);

# new for 1.0! - GreensHClasses - "for an L-class of an acting semigroup"
##############################################################################

# different method for regular/inverse

InstallOtherMethod(GreensHClasses, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local rho_o, rho_m, s, scc, mults, d, lambda_o, lambda_m, cosets, f, nc, out, k, i, j;

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
      out[k]:=CreateHClass(s, i*j, lambda_o, lambda_m, rho_o, rho_m, nc);
      SetLClassOfHClass(out[k], l);
      SetDClassOfHClass(out[k], d);
    od;
  od;
  return out;
end);

# new for 0.1! - GreensHClasses - "for a D-class of an acting semigroup"
#############################################################################
# JDM could this be better/more efficient?!

# same method for regular/inverse
# JDM could do a different method if needed

InstallOtherMethod(GreensHClasses, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return Concatenation(List(GreensRClasses(d), GreensHClasses));
end);

# mod for 1.0! - GreensHClasses - "for an acting semigroup"
##############################################################################

# different method for regular/inverse
# JDM could write another method for regular/inverse if needed.

InstallMethod(GreensHClasses, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  return Concatenation(List(GreensDClasses(s), GreensHClasses));
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
  lrel:=GreensLRelation(s); 
  nc:=IsGreensClassNC(d);

  out:=EmptyPlist(Length(scc)*Length(cosets));

  k:=0;
  for j in cosets do
    g:=f*j;
    for i in scc do
      k:=k+1;
      l:=Objectify(LClassType(s), rec());

      SetParentSemigroup(l, s);
      SetRhoOrbSCCIndex(l, m);
      SetRhoOrb(l, o);
      SetRepresentative(l, g*mults[i][1]);
      SetEquivalenceClassRelation(l, lrel);
      SetIsGreensClassNC(l, nc); 
      out[k]:=l;
    od;
  od;

  return out;
end);

# new for 1.0! - GreensRClasses - "for a D-class of an acting semigroup"
##############################################################################

# different method for regular/inverse

InstallMethod(GreensRClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass], 
function(d)
  local mults, scc, cosets, f, s, o, m, rrel, nc, out, k, g, r, i, j;
 
  mults:=RhoOrbMults(RhoOrb(d), RhoOrbSCCIndex(d));
  scc:=RhoOrbSCC(d);
  cosets:=RhoCosets(d);
  f:=Representative(d);
 
  s:=ParentSemigroup(d);
  o:=LambdaOrb(d);
  m:=LambdaOrbSCCIndex(d);
  rrel:=GreensRRelation(s);    
  nc:=IsGreensClassNC(d); 

  out:=EmptyPlist(Length(scc)*Length(cosets));

  k:=0;
  for i in scc do
    g:=mults[i][1]*f;
    for j in cosets do
      k:=k+1;
      r:=Objectify(RClassType(s), rec());

      SetParentSemigroup(r, s);
      SetLambdaOrbSCCIndex(r, m);
      SetLambdaOrb(r, o);
      SetRepresentative(r, g/j);
      SetEquivalenceClassRelation(r, rrel);
      SetIsGreensClassNC(r, nc);
      SetDClassOfRClass(r, d);
      out[k]:=r;
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
  local data, orbit, r, out, i;

  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  
  orbit:=data!.orbit;
  out:=EmptyPlist(Length(orbit));

  for i in [2..Length(orbit)] do 
    out[i-1]:=CallFuncList(CreateRClass, orbit[i]);
  od;
  return out;
end);

# mod for 1.0! - GreensDClassOfElement - "for an acting semigp and elt."
#############################################################################

# same method for regular, should be a different method for inverse

InstallOtherMethod(GreensDClassOfElement, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local d, o, rectify, m, l;

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;

  d:=Objectify(DClassType(s), rec());
  SetParentSemigroup(d, s);

  o:=LambdaOrb(s);
  rectify:=RectifyLambda(s, o, f);
  SetLambdaOrb(d, o);
  SetLambdaOrbSCCIndex(d, rectify.m); 
  f:=rectify.rep;   
  
  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then
    o:=RhoOrb(s);
    rectify:=RectifyRho(s, o, rectify.rep);
    m:=rectify.m;
  else
    o:=GradedRhoOrb(s, f, true);
    l:=o!.rho_l; #Position(o, RhoFunc(s)(f));
    m:=OrbSCCLookup(o)[l];

    if l<>OrbSCC(o)[m][1] then 
      f:=RhoOrbMult(o, m, l)[2]*f;
    fi;
  fi;

  SetRhoOrb(d, o);
  SetRhoOrbSCCIndex(d, m);

  SetRepresentative(d, f);
  SetEquivalenceClassRelation(d, GreensDRelation(s));
  SetIsGreensClassNC(d, false);
  return d;
end);

# mod for 1.0! - GreensDClassOfElementNC - "for an acting semigp and elt."
#############################################################################

# same method for inverse/regular

InstallOtherMethod(GreensDClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local d;

  d:=Objectify(DClassType(s), rec());
  SetParentSemigroup(d, s);

  SetLambdaOrb(d, GradedLambdaOrb(s, f, false));
  SetLambdaOrbSCCIndex(d, 1);
 
  SetRhoOrb(d, GradedRhoOrb(s, f, false));
  SetRhoOrbSCCIndex(d, 1);

  SetRepresentative(d, f);
  SetEquivalenceClassRelation(d, GreensDRelation(s));
  SetIsGreensClassNC(d, true);
  return d;
end);

# mod for 1.0! - GreensHClassOfElement - "for an acting semigp and elt."
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensHClassOfElement, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local h, o;

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;

  h:=Objectify(HClassType(s), rec());
  SetParentSemigroup(h, s);

  o:=LambdaOrb(s);
  SetLambdaOrb(h, o);
  SetLambdaOrbSCCIndex(h, OrbSCCLookup(o)[Position(o, LambdaFunc(s)(f))]); 

  #JDM why not add if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then .. 
  o:=GradedRhoOrb(s, f, true);
  SetRhoOrb(h, o);
  SetRhoOrbSCCIndex(h, OrbSCCLookup(o)[o!.rho_l]);

  SetRepresentative(h, f);
  SetEquivalenceClassRelation(h, GreensHRelation(s));
  SetIsGreensClassNC(h, false);
  return h;
end);

# mod for 1.0! - GreensHClassOfElementNC - "for an acting semigp and elt."
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensHClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local h;

  h:=Objectify(HClassType(s), rec());
  SetParentSemigroup(h, s);

  SetLambdaOrb(h, GradedLambdaOrb(s, f, false));
  SetLambdaOrbSCCIndex(h, 1);
 
  SetRhoOrb(h, GradedRhoOrb(s, f, false));
  SetRhoOrbSCCIndex(h, 1);

  SetRepresentative(h, f);
  SetEquivalenceClassRelation(h, GreensHRelation(s));
  SetIsGreensClassNC(h, true);
  return h;
end);

# new for 1.0! - GreensHClassOfElement - "for D-class and elt."
############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensHClassOfElement, "for D-class and elt",
[IsActingSemigroupGreensClass and IsGreensDClass, IsActingElt],
function(d, f)
  local s, h;

  if not f in d then
    Error("the element does not belong to the Green's class,");
    return;
  fi;

  s:=ParentSemigroup(d);
  h:=Objectify(HClassType(s), rec());
  SetParentSemigroup(h, s);

  SetLambdaOrb(h, LambdaOrb(d));
  SetLambdaOrbSCCIndex(h, LambdaOrbSCCIndex(d));
  SetRhoOrb(h, RhoOrb(d));
  SetRhoOrbSCCIndex(h, RhoOrbSCCIndex(d));
  
  SetRepresentative(h, f);
  SetEquivalenceClassRelation(h, GreensHRelation(s));
  SetDClassOfHClass(h, d);
  SetIsGreensClassNC(h, IsGreensClassNC(d));
  
  return h;
end);

# mod for 1.0! - GreensHClassOfElementNC - "for D-class and elt."
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensHClassOfElementNC, "for a D-class and elt",
[IsActingSemigroupGreensClass and IsGreensDClass, IsActingElt],
function(d, f)
  local s, h;
 
  s:=ParentSemigroup(d);
  h:=Objectify(HClassType(s), rec());
  SetParentSemigroup(h, s);

  SetLambdaOrb(h, LambdaOrb(d));
  SetLambdaOrbSCCIndex(h, LambdaOrbSCCIndex(d));
  SetRhoOrb(h, RhoOrb(d));
  SetRhoOrbSCCIndex(h, RhoOrbSCCIndex(d));
  
  SetRepresentative(h, f);
  SetEquivalenceClassRelation(h, GreensHRelation(s));
  SetDClassOfHClass(h, d);
  SetIsGreensClassNC(h, true);

  return h;
end);

# new for 1.0! - GreensHClassOfElement - "for L-class and elt."
############################################################################

# same method for regular, different method for inverse.

InstallOtherMethod(GreensHClassOfElement, "for L-class and elt",
[IsActingSemigroupGreensClass and IsGreensLClass, IsActingElt],
function(l, f)
  local s, h, o, i;

  if not f in l then
    Error("the element does not belong to the Green's class,");
    return;
  fi;

  s:=ParentSemigroup(l);
  h:=Objectify(HClassType(s), rec());
  SetParentSemigroup(h, s);

  SetRhoOrb(h, RhoOrb(l));
  SetRhoOrbSCCIndex(h, RhoOrbSCCIndex(l));
 
  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s);
    i:=Position(o, LambdaFunc(s)(f));
  else
    o:=GradedLambdaOrb(s, f, IsGreensClassNC(l)<>true);
    i:=o!.lambda_l;
  fi;
  
  SetLambdaOrb(h, o);

  if IsGreensClassNC(l) then 
    SetLambdaOrbSCCIndex(h, 1);
  else
    SetLambdaOrbSCCIndex(h, OrbSCCLookup(o)[i]);
  fi;
  
  SetRepresentative(h, f);
  SetEquivalenceClassRelation(h, GreensHRelation(s));
  SetIsGreensClassNC(h, IsGreensClassNC(l));
  SetLClassOfHClass(h, l);

  return h;
end);

# mod for 1.0! - GreensHClassOfElementNC - "for an L-class and elt."
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensHClassOfElementNC, "for an L-class and elt",
[IsActingSemigroupGreensClass and IsGreensLClass, IsActingElt],
function(l, f)
  local s, h, o;
  
  s:=ParentSemigroup(l);
  h:=Objectify(HClassType(s), rec());
  SetParentSemigroup(h, s);

  SetRhoOrb(h, RhoOrb(l));
  SetRhoOrbSCCIndex(h, RhoOrbSCCIndex(l));
  SetLambdaOrb(h, GradedLambdaOrb(s, f, false));
  SetLambdaOrbSCCIndex(h, 1);
  
  SetRepresentative(h, f);
  SetEquivalenceClassRelation(h, GreensHRelation(s));
  SetIsGreensClassNC(h, true);
  SetLClassOfHClass(h, l);

  return h;
end);

# new for 1.0! - GreensHClassOfElement - "for R-class and elt."
############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensHClassOfElement, "for R-class and elt",
[IsActingSemigroupGreensClass and IsGreensRClass, IsActingElt],
function(r, f)
  local s, h, o;

  if not f in r then
    Error("the element does not belong to the Green's class,");
    return;
  fi;

  s:=ParentSemigroup(r);
  h:=Objectify(HClassType(s), rec());
  SetParentSemigroup(h, s);

  SetLambdaOrb(h, LambdaOrb(r));
  SetLambdaOrbSCCIndex(h, LambdaOrbSCCIndex(r));
 
  #JDM why not add if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then .. 
  o:=GradedRhoOrb(s, f, IsGreensClassNC(r)<>true);
  SetRhoOrb(h, o);

  if IsGreensClassNC(r) then 
    SetRhoOrbSCCIndex(h, 1);
  else
    SetRhoOrbSCCIndex(h, OrbSCCLookup(o)[Position(o, RhoFunc(s)(f))]);
  fi;
  
  SetRepresentative(h, f);
  SetEquivalenceClassRelation(h, GreensHRelation(s));
  SetRClassOfHClass(h, r);
  SetIsGreensClassNC(h, IsGreensClassNC(r));

  return h;
end);

# mod for 1.0! - GreensHClassOfElementNC - "for an R-class and elt."
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensHClassOfElementNC, "for an R-class and elt",
[IsActingSemigroupGreensClass and IsGreensRClass, IsActingElt],
function(r, f)
  local s, h, o;
  
  s:=ParentSemigroup(r);
  h:=Objectify(HClassType(s), rec());
  SetParentSemigroup(h, s);

  SetLambdaOrb(h, LambdaOrb(r));
  SetLambdaOrbSCCIndex(h, LambdaOrbSCCIndex(r));
  SetRhoOrb(h, GradedRhoOrb(s, f, false));
  SetRhoOrbSCCIndex(h, 1);
  
  SetRepresentative(h, f);
  SetEquivalenceClassRelation(h, GreensHRelation(s));
  SetRClassOfHClass(h, r);
  SetIsGreensClassNC(h, true);

  return h;
end);

# mod for 1.0! - GreensLClassOfElement - "for an acting semigp and elt."
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensLClassOfElement, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local l, o, i, m, scc;

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;
  
  l:=Objectify(LClassType(s), rec());
  SetParentSemigroup(l, s);
  
  #JDM why not add if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then .. 
  o:=GradedRhoOrb(s, f, true);
  SetRhoOrb(l, o);
  
  i:=Position(o, RhoFunc(s)(f));
  m:=OrbSCCLookup(o)[i];
  
  SetRhoOrbSCCIndex(l, m);
  
  if i<>OrbSCC(o)[m][1] then 
    f:=RhoOrbMults(o, m)[i][2]*f;
  fi;

  SetRepresentative(l, f);
  SetEquivalenceClassRelation(l, GreensLRelation(s));
  SetIsGreensClassNC(l, false);

  return l;
end);

# mod for 1.0! - GreensLClassOfElementNC - "for an acting semigp and elt."
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensLClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local l;

  l:=Objectify(LClassType(s), rec());
  
  SetParentSemigroup(l, s);
  SetRhoOrbSCCIndex(l, 1);
  SetRhoOrb(l, GradedRhoOrb(s, f, false));
  SetRepresentative(l, f);
  SetEquivalenceClassRelation(l, GreensLRelation(s));
  SetIsGreensClassNC(l, true);
  return l;
end);

# mod for 1.0! - GreensLClassOfElement - "for D-class of acting semi and elt"
#############################################################################
# Notes: can't call GreensLClassOfElementNC since we don't have a way to pass
# IsGreensClassNC(d) to it.

# same method for regular/inverse.

InstallOtherMethod(GreensLClassOfElement, "for D-class of acting semi and elt",
[IsGreensDClass and IsActingSemigroupGreensClass, IsActingElt],
function(d, f)
  local s, l, o, i, m;
    
  if not f in d then
    Error("the element does not belong to the D-class,");
    return;
  fi;
  
  s:=ParentSemigroup(d);
  l:=Objectify(LClassType(s), rec());

  SetParentSemigroup(l, s);
  SetRhoOrbSCCIndex(l, RhoOrbSCCIndex(d));
  
  o:=RhoOrb(d); 
  SetRhoOrb(l, o);
  i:=Position(o, RhoFunc(s)(f));
  m:=OrbSCCLookup(o)[i];

  if i<>OrbSCC(o)[m][1] then 
    f:=RhoOrbMults(o, m)[i][2]*f;
  fi;
  
  SetRepresentative(l, f);
  SetEquivalenceClassRelation(l, GreensLRelation(s));
  SetIsGreensClassNC(l, IsGreensClassNC(d));
  SetDClassOfLClass(l, d);
  return l;
end);

# mod for 1.0! - GreensLClassOfElementNC - "for D-class and acting elt"
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensLClassOfElementNC, "for D-class and acting elt",
[IsGreensDClass and IsActingSemigroupGreensClass, IsActingElt],
function(d, f)
  local s, l, o, i, m;

  s:=ParentSemigroup(d);
  l:=Objectify(LClassType(s), rec());

  SetParentSemigroup(l, s);
  SetRhoOrbSCCIndex(l, LambdaOrbSCCIndex(d));
  
  o:=RhoOrb(d); 
  SetRhoOrb(l, o);
  i:=Position(o, RhoFunc(s)(f));
  m:=OrbSCCLookup(o)[i];

  if i<>OrbSCC(o)[m][1] then 
    f:=RhoOrbMults(o, m)[i][2]*f;
  fi;
  
  SetRepresentative(l, f);
  SetEquivalenceClassRelation(l, GreensRRelation(s));
  SetIsGreensClassNC(l, true);
  SetDClassOfLClass(l, d);
  return l;
end);

# mod for 1.0! - GreensRClassOfElement - "for D-class and acting elt"
#############################################################################
# Notes: can't call GreensRClassOfElementNC since we don't have a way to pass
# IsGreensClassNC(d) to it.

# same method for regular/inverse.

InstallOtherMethod(GreensRClassOfElement, "for D-class and acting elt",
[IsGreensDClass and IsActingSemigroupGreensClass, IsActingElt],
function(d, f)
  local s, r;
    
  if not f in d then
    Error("the element does not belong to the D-class,");
    return;
  fi;
  
  s:=ParentSemigroup(d);
  r:=Objectify(RClassType(s), rec());

  SetParentSemigroup(r, s);
  SetLambdaOrb(r, LambdaOrb(d));
  SetLambdaOrbSCCIndex(r, LambdaOrbSCCIndex(d));
  SetRepresentative(r, RectifyLambda(s, LambdaOrb(d), f).rep);
  
  SetEquivalenceClassRelation(r, GreensRRelation(s));
  SetIsGreensClassNC(r, IsGreensClassNC(d));
  SetDClassOfRClass(r, d);

  return r;
end);

# mod for 1.0! - GreensRClassOfElementNC - "for D-class and acting elt"
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensRClassOfElementNC, "for D-class and acting elt",
[IsGreensDClass and IsActingSemigroupGreensClass, IsActingElt],
function(d, f)
  local s, r, o, l, m;

  s:=ParentSemigroup(d);
  r:=Objectify(RClassType(s), rec());

  SetParentSemigroup(r, s);
  SetLambdaOrb(r, LambdaOrb(d));
  SetLambdaOrbSCCIndex(r, LambdaOrbSCCIndex(d));
  SetRepresentative(r, RectifyLambda(s, LambdaOrb(d), f));
  
  SetEquivalenceClassRelation(r, GreensRRelation(s));
  SetIsGreensClassNC(r, true);
  SetDClassOfRClass(r, d);
  return r;
end);

# mod for 1.0! - GreensRClassOfElement - "for an acting semigp and elt."
#############################################################################

# different method for regular/inverse.

InstallOtherMethod(GreensRClassOfElement, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local pos;

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;

  pos:=Position(SemigroupData(s), f);
  return CallFuncList(CreateRClass, SemigroupData(s)[pos]);
end);

# mod for 1.0! - GreensRClassOfElementNC - "for an acting semigp and elt."
#############################################################################

# same method for regular/inverse.

InstallOtherMethod(GreensRClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local pos, r;
 
  if HasSemigroupData(s) and IsClosed(SemigroupData(s)) then 
    pos:=Position(SemigroupData(s), f);
    if pos<>fail then 
      return CallFuncList(CreateRClass, SemigroupData(s)[pos]);
    fi;  
  fi;
  
  r:=Objectify(RClassType(s), rec());

  SetParentSemigroup(r, s);
  SetLambdaOrbSCCIndex(r, 1);
  SetLambdaOrb(r, GradedLambdaOrb(s, f, false));
  SetRepresentative(r, f);
  SetEquivalenceClassRelation(r, GreensRRelation(s));
  SetIsGreensClassNC(r, true);
  return r;
end);

# mod for 1.0! - GreensJClassOfElement - for an acting semi and elt."
#############################################################################

# same method for regular/inverse

InstallOtherMethod(GreensJClassOfElement, "for acting semigroup and elt.",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsActingElt], 
GreensDClassOfElement);

# mod for 1.0! - GroupHClass - "for a D-class of an acting semigroup"
############################################################################

# same method for regular, different method for inverse.

InstallOtherMethod(GroupHClass, "for a D-class of an acting semigp.",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local s, rho, o, scc, tester, i;

  if not IsRegularDClass(d) then 
    return fail;
  fi;
  
  s:=ParentSemigroup(d);

  rho:=RhoFunc(s)(Representative(d));
  o:=LambdaOrb(d);
  scc:=OrbSCC(o)[LambdaOrbSCCIndex(d)];
  tester:=IdempotentLambdaRhoTester(s);

  for i in scc do 
    if tester(o[i], rho) then 
      return GreensHClassOfElementNC(d, 
       IdempotentLambdaRhoCreator(s)(o[i], rho));
    fi;
  od;
  return;
end);

# mod for 0.7! - GroupHClassOfGreensDClass - "for D-class"
############################################################################

# same method for regular/inverse

InstallMethod(GroupHClassOfGreensDClass, "for D-class",
[IsGreensDClass], GroupHClass);

#III

# new for 1.0! - Idempotents - "for a D-class of an acting semigp."
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(Idempotents, "for a D-class of an acting semigp.",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local s, out, nr, tester, creator, rho_o, rho_scc, lambda_o, lambda_scc, i, j;

  s:=ParentSemigroup(d);

  if Rank(Representative(d))=Degree(s) then
    return [One(s)];
  fi;

  out:=[]; nr:=0;
  creator:=IdempotentLambdaRhoCreator(s);
  
  lambda_o:=LambdaOrb(d); 
  lambda_scc:=LambdaOrbSCC(d);

  tester:=IdempotentLambdaRhoTester(s);
  rho_o:=RhoOrb(d);
  rho_scc:=RhoOrbSCC(d);
  for i in lambda_scc do
    i:=lambda_o[i];
    for j in rho_scc do 
      j:=rho_o[j];
      if tester(i, j) then
        nr:=nr+1;
        out[nr]:=creator(i, j);
      fi;
    od;
  od;

  if not HasNrIdempotents(d) then 
    SetNrIdempotents(d, nr);   
  fi;
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
  return IdempotentLambdaRhoCreator(LambdaFunc(s)(f), RhoFunc(s)(f));
end);

# mod for 1.0! - Idempotents - "for an L-class of an acting semigp"
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(Idempotents, "for an L-class of an acting semigp.",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local s, out, lambda, o, m, scc, j, tester, creator, i;

  if not IsRegularLClass(l) then
    return [];
  fi;
  
  s:=ParentSemigroup(l);

  if Rank(Representative(l))=Degree(s) then
    return [One(s)];
  fi;

  out:=[]; 
  
  lambda:=LambdaFunc(s)(Representative(l));
  o:=RhoOrb(l); 
  m:=RhoOrbSCCIndex(l);
  scc:=OrbSCC(o)[m];

  j:=0;
  tester:=IdempotentLambdaRhoTester(s);
  creator:=IdempotentLambdaRhoCreator(s);

  for i in scc do
    if tester(lambda, o[i]) then
      j:=j+1;
      out[j]:=creator(lambda, o[i]);
    fi;
  od;

  if not HasNrIdempotents(l) then 
    SetNrIdempotents(l, j);   
  fi;

  return out;
end);

# mod for 1.0! - Idempotents - "for an R-class of an acting semigp"
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(Idempotents, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, out, rho, o, m, scc, j, tester, creator, i;

  if not IsRegularRClass(r) then
    return [];
  fi;
  
  s:=ParentSemigroup(r);

  if Rank(Representative(r))=Degree(s) then
    return [One(s)];
  fi;

  
  rho:=RhoFunc(s)(Representative(r));
  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
  scc:=OrbSCC(o)[m];

  out:=EmptyPlist(Length(scc)); 

  j:=0;
  tester:=IdempotentLambdaRhoTester(s);
  creator:=IdempotentLambdaRhoCreator(s);

  for i in scc do
    if tester(o[i], rho) then
      j:=j+1;
      out[j]:=creator(o[i], rho);
    fi;
  od;

  if not HasNrIdempotents(r) then 
    SetNrIdempotents(r, j);   
  fi;

  ShrinkAllocationPlist(out);
  return out;
end);

# mod for 1.0! - Idempotents - "for an acting semigroup" 
#############################################################################

# same method for regular, different method for inverse

InstallOtherMethod(Idempotents, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local lambda_o, creator, r, l, out, nr, tester, rho_o, scc, gens, rhofunc, lookup, rep, rho, j, i, k;

  if IsRegularSemigroup(s) then 

    if HasNrIdempotents(s) then 
      out:=EmptyPlist(NrIdempotents(s));
    else
      out:=[];
    fi;
    
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
      SetNrIdempotents(s, j);
    fi;
    return out;
  fi;

  return Concatenation(List(GreensRClasses(s), Idempotents));
end);

# new for 0.1! - IsGreensClassOfTransSemigp - "for a Green's class"
#############################################################################

InstallMethod(IsGreensClassOfTransSemigp, "for a Green's class",
[IsGreensClass], x-> IsTransformationSemigroup(ParentAttr(x)));

# new for 1.0! - IsGreensClassOfTransSemigp - "for a Green's class"
#############################################################################

InstallMethod(IsGreensClassOfTransSemigp, "for a Green's class",
[IsActingSemigroupGreensClass], x->
IsTransformationSemigroup(ParentSemigroup(x)));

# new for 1.0! - IsActingSemigroup 
#############################################################################

InstallOtherMethod(IsActingSemigroup, "for a Green's class",
[IsGreensClass], ReturnFalse);

InstallOtherMethod(IsActingSemigroupWithInverseOp, "for a Green's class",
[IsGreensClass], ReturnFalse);

# JDM move to acting.gi

InstallOtherMethod(IsActingSemigroupWithInverseOp, "for an acting semigroup",
[IsActingSemigroup], ReturnFalse);

# new for 0.1! - IsGreensClass - "for a Green's class"
#############################################################################

InstallOtherMethod(IsGreensClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensRClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensLClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensHClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensDClass, "for an object", [IsObject], ReturnFalse);

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

# mod for 1.0! - IsGroupHClass - "for an acting semi Green's class"
############################################################################

# same method for regular/inverse

InstallOtherMethod(IsGroupHClass, "for an acting semi Green's class",
[IsActingSemigroupGreensClass], ReturnFalse);

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

  g:=Group(());

  for f in Enumerator(h) do
    g:=ClosureGroup(g, AsPermutation(f));
  od;

  return MappingByFunction(h, g, AsPermutation, x-> Idempotents(h)[1]*x);
end);

# new for 1.0! - IsRegularDClass - "for an D-class of an acting semi"
#############################################################################

# not required for regular/inverse

InstallMethod(IsRegularDClass, "for an D-class of an acting semigp",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local s, data, rho, scc, o, tester, i;

  if HasNrIdempotents(d) then 
    return NrIdempotents(d)<>0;
  fi;

  s:=ParentSemigroup(d);
  data:=SemigroupData(s);
  
  if HasSemigroupDataIndex(d) then
    if data!.repslens[data!.orblookup1[SemigroupDataIndex(d)]]>1 then
      return false;
    fi;
  fi; 
  
  # is r the group of units...
  if Rank(Representative(d))=Degree(s) then
    return true;
  fi;   
 
  rho:=RhoFunc(s)(Representative(d));
  scc:=LambdaOrbSCC(d);
  o:=LambdaOrb(d); 
  tester:=IdempotentLambdaRhoTester(s);

  for i in scc do
    if tester(o[i], rho) then
      return true; 
    fi;
  od;
  return false;
end);

# new for 1.0! - IsRegularLClass - "for an L-class of an acting semi"
#############################################################################

# not required for regular/inverse

InstallMethod(IsRegularLClass, "for an L-class of an acting semigp",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local s, data, lambda, o, scc, tester, i;

  if HasNrIdempotents(l) then 
    return NrIdempotents(l)<>0;
  fi;

  s:=ParentSemigroup(l);
  data:=SemigroupData(s);
  
  if HasSemigroupDataIndex(l) then
    if data!.repslens[data!.orblookup1[SemigroupDataIndex(l)]]>1 then
      return false;
    fi;
  fi; 
  
  # is r the group of units...
  if Rank(Representative(l))=Degree(s) then
    return true;
  fi;   
 
  lambda:=LambdaFunc(s)(Representative(l));
  o:=RhoOrb(l);
  scc:=RhoOrbSCC(l);
  tester:=IdempotentLambdaRhoTester(s);

  for i in scc do
    if tester(lambda, o[i]) then
      return true; 
    fi;
  od;
  return false;
end);

# new for 1.0! - IsRegularRClass - "for an R-class of an acting semi"
#############################################################################

# not required for regular/inverse

InstallMethod(IsRegularRClass, "for an R-class of an acting semigp",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, data, rho, o, scc, tester, i;

  if HasNrIdempotents(r) then 
    return NrIdempotents(r)<>0;
  fi;

  s:=ParentSemigroup(r);
  data:=SemigroupData(s);
  
  if HasSemigroupDataIndex(r) then
    if data!.repslens[data!.orblookup1[SemigroupDataIndex(r)]]>1 then
      return false;
    fi;
  fi; 
  
  # is r the group of units...
  if Rank(Representative(r))=Degree(s) then
    return true;
  fi;   
 
  rho:=RhoFunc(s)(Representative(r));
  o:=LambdaOrb(r);
  scc:=LambdaOrbSCC(r);
  tester:=IdempotentLambdaRhoTester(s);

  for i in scc do
    if tester(o[i], rho) then
      return true; 
    fi;
  od;
  return false;
end);

# new for 1.0! - Iterator - "for an R-class of an acting semi"
#############################################################################
# this method makes Iterator of a semigroup much better!!

# same method for regular/inverse

InstallMethod(Iterator, "for an R-class of an acting semigp",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, mults, iter, scc;

  if HasAsSSortedList(r) then 
    iter:=IteratorList(AsSSortedList(r));
  else
    o:=LambdaOrb(r); 
    m:=LambdaOrbSCCIndex(r);
    mults:=LambdaOrbMults(o, m);
    scc:=OrbSCC(o)[m];

    iter:=IteratorByFunctions(rec(

      schutz:=List(SchutzenbergerGroup(r), x-> Representative(r)*x), 
      at:=[0,1],
      m:=Length(scc),
      n:=Size(SchutzenbergerGroup(r)), 

      IsDoneIterator:=iter-> iter!.at[1]=iter!.m and iter!.at[2]=iter!.n,

      NextIterator:=function(iter)
        local at;

        at:=iter!.at;
        
        if at[1]=iter!.m and at[2]=iter!.n then 
          return fail;
        fi;

        if at[1]<iter!.m then
          at[1]:=at[1]+1;
        else
          at[1]:=1; at[2]:=at[2]+1;
        fi;
       
        return iter!.schutz[at[2]]*mults[scc[at[1]]][1];
      end,
      
      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, at:=[0,1], 
       m:=iter!.m, n:=iter!.n)));
    fi;
    
    SetIsIteratorOfRClassElements(iter, true);
    return iter;
end);

# mod for 1.0! - Iterator - "for a trivial acting semigroup"
#############################################################################
# Notes: required until Enumerator for a trans. semigp does not call iterator. 
# This works but is maybe not the best!

# same method for regular/inverse

InstallOtherMethod(Iterator, "for a trivial acting semigp", 
[IsActingSemigroup and HasGeneratorsOfSemigroup and IsTrivial], 9999,
function(s)
  return TrivialIterator(Generators(s)[1]);
end);

# mod for 1.0! - Iterator - "for an acting semigroup"
#############################################################################

# same method for regular/inverse (the previous inverse method used D-classes
# instead of R-classes).

InstallMethod(Iterator, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter;

  if HasAsSSortedList(s) then 
    iter:=IteratorList(AsSSortedList(s));
  else
    iter:= IteratorByFunctions( rec(

      R:=IteratorOfRClasses(s),

      r:=fail, s:=s,

      NextIterator:=function(iter)

        if IsDoneIterator(iter) then
          return fail;
        fi;

        if iter!.r=fail or IsDoneIterator(iter!.r) then
          iter!.r:=Iterator(NextIterator(iter!.R));
        fi;

        return NextIterator(iter!.r);
      end,

      IsDoneIterator:= iter -> IsDoneIterator(iter!.R) and
       IsDoneIterator(iter!.r),

      ShallowCopy:= iter -> rec(R:=IteratorOfRClasses(s), r:=fail)));
  fi;

  SetIsIteratorOfSemigroup(iter, true);
  return iter;
end);

# new for 0.5! - Iterator - "for a full transformation semigroup"
#############################################################################

# no method required for inverse/regular

InstallMethod(Iterator, "for a full transformation semigroup",
[IsTransformationSemigroup and IsFullTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter;
  
  Info(InfoCitrus, 4, "Iterator: for a full trans. semigroup");

  iter:= IteratorByFunctions( rec(

    s:=s,

    tups:=IteratorOfTuples([1..Degree(s)], Degree(s)),

    NextIterator:=iter-> TransformationNC(NextIterator(iter!.tups)),
  
    IsDoneIterator:=iter -> IsDoneIterator(iter!.tups),
    
    ShallowCopy:= iter -> rec(tups:=IteratorOfTuples([1..Degree(s)],
    Degree(s)))));

  SetIsIteratorOfSemigroup(iter, true);
  return iter;
end);

# new for 1.0! - IteratorOfDClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(IteratorOfDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local iter;
  
  if IsClosed(SemigroupData(s)) then 
    iter:=IteratorList(GreensDClasses(s));
    SetIsIteratorOfDClasses(iter, true);
    return iter;
  fi;

  iter:=IteratorByFunctions( rec( 

    classes:=[],

    R:=IteratorOfRClassData(s),

    last_called_by_is_done:=false,

    next_value:=fail,

    IsDoneIterator:=function(iter)
      local R, X, x, d; 
      if iter!.last_called_by_is_done then 
        return iter!.next_value=fail;
      fi;
      
      iter!.last_called_by_is_done:=true;
      
      iter!.next_value:=fail;
       
      R:=iter!.R; X:=iter!.classes;
      
      repeat 
        x:=NextIterator(R);
        #JDM is there a better method?
      until x=fail or ForAll(X, d-> not x[4] in d);
      
      if x<>fail then 
        d:=DClassOfRClass(CallFuncList(CreateRClass, x));
        Add(X, d);
        iter!.next_value:=d;
        return false;
      fi;
      return true;
    end,

    NextIterator:=function(iter)
      if not iter!.last_called_by_is_done then 
        IsDoneIterator(iter);
      fi;
      iter!.last_called_by_is_done:=false;
      return iter!.next_value;
    end,
    
    ShallowCopy:=iter-> rec(classes:=[], R:=IteratorOfRClassData(s),
     last_called_by_is_done:=false, next_value:=fail)));
  SetIsIteratorOfDClasses(iter, true);
  return iter;
end);

# new for 1.0! - IteratorOfHClasses - "for an acting semigroup"
#############################################################################
# JDM could use IteratorOfRClasses here instead, not sure which is better...

InstallMethod(IteratorOfHClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local iter;
  
  if HasGreensHClasses(s) then 
    iter:=IteratorList(GreensHClasses(s));
  else
    iter:=IteratorByFunctions( rec( 

      i:=0,

      D:=IteratorOfDClasses(s),

      H:=[],

      last_called_by_is_done:=false,

      next_value:=fail,

      IsDoneIterator:=function(iter)
        
        if iter!.last_called_by_is_done then 
          return iter!.next_value=fail;
        fi;
        
        iter!.last_called_by_is_done:=true;
        iter!.next_value:=fail;
        iter!.i:=iter!.i+1;

        if iter!.i>Length(iter!.H) and not IsDoneIterator(iter!.D) then 
          iter!.i:=1;
          iter!.H:=GreensHClasses(NextIterator(iter!.D));
        fi;
        
        if iter!.i<=Length(iter!.H) then 
          iter!.next_value:=iter!.H[iter!.i];
          return false;
        fi;
          
        return true;
      end,

      NextIterator:=function(iter)
        if not iter!.last_called_by_is_done then 
          IsDoneIterator(iter);
        fi;
        iter!.last_called_by_is_done:=false;
        return iter!.next_value;
      end,
      
      ShallowCopy:=iter-> rec(i:=0, D:=IteratorOfDClasses(s),
       H:=[], last_called_by_is_done:=false, next_value:=fail)));
  fi;

  SetIsIteratorOfHClasses(iter, true);
  return iter;
end);

# new for 1.0! - IteratorOfLClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(IteratorOfLClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local iter;
  
  if HasGreensLClasses(s) then 
    iter:=IteratorList(GreensLClasses(s));
  else
    iter:=IteratorByFunctions( rec( 

      i:=0,

      D:=IteratorOfDClasses(s),

      L:=[],

      last_called_by_is_done:=false,

      next_value:=fail,

      IsDoneIterator:=function(iter)
        
        if iter!.last_called_by_is_done then 
          return iter!.next_value=fail;
        fi;
        
        iter!.last_called_by_is_done:=true;
        iter!.next_value:=fail;
        iter!.i:=iter!.i+1;

        if iter!.i>Length(iter!.L) and not IsDoneIterator(iter!.D) then 
          iter!.i:=1;
          iter!.L:=GreensLClasses(NextIterator(iter!.D));
        fi;
        
        if iter!.i<=Length(iter!.L) then 
          iter!.next_value:=iter!.L[iter!.i];
          return false;
        fi;
          
        return true;
      end,

      NextIterator:=function(iter)
        if not iter!.last_called_by_is_done then 
          IsDoneIterator(iter);
        fi;
        iter!.last_called_by_is_done:=false;
        return iter!.next_value;
      end,
      
      ShallowCopy:=iter-> rec(i:=0, D:=IteratorOfDClasses(s),
       L:=[], last_called_by_is_done:=false, next_value:=fail)));
  fi;

  SetIsIteratorOfLClasses(iter, true);
  return iter;
end);

# new for 1.0! - IteratorOfDClassReps - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(IteratorOfDClassReps, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfDClasses(s), Representative,
[IsIteratorOfDClassReps]));

# new for 1.0! - IteratorOfHClassReps - "for an acting semigroup"
#############################################################################

#?? method for regular/inverse

InstallMethod(IteratorOfHClassReps, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfHClasses(s), Representative,
[IsIteratorOfHClassReps]));

# new for 1.0! - IteratorOfLClassReps - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(IteratorOfLClassReps, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfLClasses(s), Representative,
[IsIteratorOfLClassReps]));

# new for 1.0! - IteratorOfRClassData - "for an acting semigroup"
#############################################################################

#different method for regular/inverse

InstallMethod(IteratorOfRClassData, "for an acting semigroup",
[IsActingSemigroup],
function(s)

  return IteratorByFunctions( rec( 
    
    i:=1,

    IsDoneIterator:=iter-> IsClosed(SemigroupData(s)) and 
     iter!.i>=Length(SemigroupData(s)),

    NextIterator:=function(iter)
      local data;

      iter!.i:=iter!.i+1;
      
      data:=Enumerate(SemigroupData(s), iter!.i, ReturnFalse);

      if iter!.i>Length(data!.orbit) then 
        return fail;
      fi;
      return data[iter!.i];
    end,
    
    ShallowCopy:=iter-> rec(i:=1)));
end);

# new for 1.0! - IteratorOfRClassReps - "for an acting semigroup"
#############################################################################

# same method for inverse/regular.

InstallMethod(IteratorOfRClassReps, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x-> x[4],
[IsIteratorOfRClassReps]));

# new for 1.0! - IteratorOfRClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(IteratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x->
CallFuncList(CreateRClass, x), [IsIteratorOfRClasses]));

#LLL

# mod for 1.0! - LClass 
#############################################################################

InstallGlobalFunction(LClass,
function(arg)

  if Length(arg)=2 and (IsActingSemigroup(arg[1]) or IsGreensDClass(arg[1]))
   and IsActingElt(arg[2]) then  
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
   and IsActingElt(arg[2]) then  
    return GreensLClassOfElementNC(arg[1], arg[2]);
  fi;

  Error("usage: acting semigroup or D-class, and acting element,");
  return;
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

#NNN

# new for 1.0! - NrIdempotents - "for a D-class of an acting semigp"
#############################################################################

# same method for regular/inverse

InstallOtherMethod(NrIdempotents, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local s, nr, tester, rho_o, rho_scc, lambda_o, lambda_scc, i, j;
  
  s:=ParentSemigroup(d);
  if Rank(Representative(d))=Degree(s) then
    return 1;
  fi;
  
  nr:=0;
  tester:=IdempotentLambdaRhoTester(s);
  
  rho_o:=RhoOrb(d);
  rho_scc:=RhoOrbSCC(d);
  lambda_o:=LambdaOrb(d);
  lambda_scc:=LambdaOrbSCC(d);
  
  for i in lambda_scc do
    i:=lambda_o[i];
    for j in rho_scc do   
      j:=rho_o[j];
      if tester(i, j) then
        nr:=nr+1;
      fi;
    od;
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
function(l)
  local s, data, lambda, o, m, scc, nr, tester, i;

  if HasIsRegularLClass(l) and not IsRegularLClass(l) then 
    return 0;
  fi;

  s:=ParentSemigroup(l);     

  # check if we already know this...
  if HasSemigroupDataIndex(l) and not (HasIsRegularLClass(l) and
   IsRegularLClass(l)) then
    data:=SemigroupData(s);
    if data!.repslens[data!.orblookup1[SemigroupDataIndex(l)]]>1 then
      return 0;
    fi;
  fi;

  # is r the group of units...
  if Rank(Representative(l))=Degree(s) then
    return 1;
  fi;

  lambda:=LambdaFunc(s)(Representative(l));
  o:=RhoOrb(l); 
  m:=RhoOrbSCCIndex(l);
  scc:=OrbSCC(o)[m];
  nr:=0;
  tester:=IdempotentLambdaRhoTester(s);

  for i in scc do
    if tester(lambda, o[i]) then
      nr:=nr+1;
    fi;
  od;

  return nr;
end);

# new for 1.0! - NrIdempotents - "for an R-class of an acting semigp."
#############################################################################

# same method for regular, different method inverse

InstallOtherMethod(NrIdempotents, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, data, rho, o, m, scc, nr, tester, i;

  if HasIsRegularRClass(r) and not IsRegularRClass(r) then 
    return 0;
  fi;

  s:=ParentSemigroup(r);     

  # check if we already know this...
  if HasSemigroupDataIndex(r) and not (HasIsRegularRClass(r) and
   IsRegularRClass(r)) then
    data:=SemigroupData(s);
    if data!.repslens[data!.orblookup1[SemigroupDataIndex(r)]]>1 then
      return 0;
    fi;
  fi;

  # is r the group of units...
  if Rank(Representative(r))=Degree(s) then
    return 1;
  fi;

  rho:=RhoFunc(s)(Representative(r));
  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
  scc:=OrbSCC(o)[m];
  nr:=0;
  tester:=IdempotentLambdaRhoTester(s);

  for i in scc do
    if tester(o[i], rho) then
      nr:=nr+1;
    fi;
  od;

  return nr;
end);

# new for 0.1! - NrIdempotents - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(NrIdempotents, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local data, reps, repslookup, lenreps, repslens, rhofunc, tester, nr, f, m, o, rho, i, k;

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

# mod for 1.0! - NrHClasses - "for a D-class of an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallOtherMethod(NrHClasses, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return NrRClasses(d)*NrLClasses(d);
end);

# mod for 1.0! - NrLClasses - "for a D-class of an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallOtherMethod(NrLClasses, "for a D-class of an acting semigroup",       
[IsActingSemigroupGreensClass and IsGreensDClass],
function(d)
  return Length(LambdaCosets(d))*Length(LambdaOrbSCC(d));
end);

# mod for 1.0! - NrLClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

#JDM could do better not to create the D-classes. Maybe not, we must store the
#schutz gp of the D-class somewhere and so it might as well be the D-class.

InstallMethod(NrLClasses, "for an acting semigroup",
[IsActingSemigroup], s-> Sum(List(GreensDClasses(s), NrLClasses)));

# mod for 1.0! - NrRClasses - "for a D-class of an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallOtherMethod(NrRClasses, "for a D-class of an acting semigroup",       
[IsActingSemigroupGreensClass and IsGreensDClass],
d-> Length(RhoCosets(d))*Length(RhoOrbSCC(d)));

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
              #if SiftGroupElement(schutz[m], lambdaperm(reps[val][n], f)).isone
              # then  
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
 
# new for 0.7! - PrintObj - for IsIteratorOfDClassElements
############################################################################
   
InstallMethod(PrintObj, [IsIteratorOfDClassElements],
function(iter)
  Print( "<iterator of D-class>");
  return;
end);

# new for 0.7! - PrintObj - IsIteratorOfHClassElements
############################################################################

InstallMethod(PrintObj, [IsIteratorOfHClassElements],
function(iter)
  Print( "<iterator of H-class>");
  return;
end);

# new for 0.7! - PrintObj - IsIteratorOfLClassElements
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClassElements],
function(iter)
  Print( "<iterator of L-class>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfRClassElements
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassElements],
function(iter)
  Print("<iterator of R-class>");
  return;
end);

# mod for 1.0! - PrintObj - for IsIteratorOfDClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfDClassReps],
function(iter)
  Print("<iterator of D-class reps>");
  return;
end);

# mod for 1.0! - PrintObj - for IsIteratorOfHClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfHClassReps],
function(iter)
  Print("<iterator of H-class reps>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfLClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClassReps], 
function(iter)
  Print( "<iterator of L-class reps>");
  return;
end);

# mod for 1.0! - PrintObj - IsIteratorOfRClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassReps],
function(iter)
  Print("<iterator of R-class reps>");
  return;
end);

# new for 0.1! - PrintObj - "for iterator of D-classes"
############################################################################

InstallMethod(PrintObj, [IsIteratorOfDClasses], 
function(iter)
  Print( "<iterator of D-classes>");
  return;
end);

# new for 0.1! - PrintObj - "for iterator of H-classes"
############################################################################

InstallMethod(PrintObj, [IsIteratorOfHClasses], 
function(iter)
  Print( "<iterator of H-classes>");
  return;
end);
 
# new for 0.1! - PrintObj - for IsIteratorOfLClasses
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClasses],
function(iter)
  Print( "<iterator of L-classes>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfRClasses
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClasses],
function(iter)
  Print( "<iterator of R-classes>");
  return;
end); 

# mod for 0.8! - PrintObj - for IsIteratorOfSemigroup
############################################################################

InstallMethod(PrintObj, [IsIteratorOfSemigroup],
function(iter)
  if IsFullTransformationSemigroup(iter!.s) then
    Print("<iterator of full trans. semigroup>");
  elif IsTransformationSemigroup(iter!.s) then
    Print("<iterator of transformation semigroup>");
  elif IsPartialPermSemigroup(iter!.s) and IsInverseSemigroup(iter!.s) then
    Print("<iterator of inverse semigroup>");
  elif IsPartialPermSemigroup(iter!.s) then 
    Print("<iterator of semigroup of partial perms>");
  elif IsSymmetricInverseSemigroup(iter!.s) then 
    Print("<iterator of symmetric inverse semigroup>");
  fi;
  return;
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
    n:=Degree(s);
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

# new for 0.1! - RClass 
#############################################################################

InstallGlobalFunction(RClass,
function(arg)

  if Length(arg)=2 and (IsActingSemigroup(arg[1]) or IsGreensDClass(arg[1]))
   and IsActingElt(arg[2]) then 
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
   and IsActingElt(arg[2]) then 
    return GreensRClassOfElementNC(arg[1], arg[2]);
  fi;
  
  Error("usage: acting semigroup or D-class, and acting element,");
  return;
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

#SSS

# new for 1.0! - Size - "for a D-class of an acting semigp."
#############################################################################

# same method for inverse/regular.

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

#HHH

# new for 0.1! - HClass - "for acting semi or Green's class, and acting elt
#############################################################################

InstallGlobalFunction(HClass,
function(arg)

  if Length(arg)=2 and (IsGreensClass(arg[1]) or IsActingSemigroup(arg[1])) and
   IsActingElt(arg[2]) then 
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
   IsActingElt(arg[2]) then 
    return GreensHClassOfElementNC(arg[1], arg[2]);
  fi;

  Error("usage: acting semigroup or Green's class, and acting element,");
  return;
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

# mod for 1.0! - HClassReps - "for an D-class of an acting semigroup"
############################################################################

# different method for regular/inverse

# JDM this method could be better...

InstallOtherMethod(HClassReps, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return Concatenation(List(GreensRClasses(d), HClassReps));
end);

# new for 1.0! - HClassReps - "for an acting semigp."
############################################################################

# different method for regular/inverse

InstallMethod(HClassReps, "for an acting semigp.",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
s-> Concatenation(List(GreensRClasses(s), HClassReps)));

#DDD

# mod for 1.0! - DClass - "for an acting semi and elt or Green's class"
#############################################################################

InstallGlobalFunction(DClass,
function(arg)

  if Length(arg)=2 and IsActingSemigroup(arg[1]) and IsActingElt(arg[2]) then
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

  if Length(arg)=2 and IsActingSemigroup(arg[1]) and IsActingElt(arg[2]) then
    return GreensDClassOfElementNC(arg[1], arg[2]);
  fi;

  Error("usage: acting semigroup and acting elt,");
  return;
end);

# new for 1.0! - DClassOfLClass - "for a L-class of an acting semigroup"
#############################################################################

# same method for regular, different method for inverse

# only for L-classes not created during GreensLClasses! 

InstallMethod(DClassOfLClass, "for an L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local s, f, d, o, lambda_l, m;

  s:=ParentSemigroup(l); 
  f:=Representative(l);
  d:=Objectify(DClassType(s), rec());

  SetParentSemigroup(d, s);
  SetRhoOrbSCCIndex(d, RhoOrbSCCIndex(l));
  SetRhoOrb(d, RhoOrb(l));
  
  o:=GradedLambdaOrb(s, f, IsGreensClassNC(l)<>true);
  SetLambdaOrb(d, o);

  if IsGreensClassNC(l) then 
    SetLambdaOrbSCCIndex(d, 1);
    SetRepresentative(d, f);
  else
    lambda_l:=o!.lambda_l; #position of LambdaFunc(s)(f) in o 
    m:=OrbSCCLookup(o)[lambda_l];
    SetLambdaOrbSCCIndex(d, m);
    if lambda_l<>OrbSCC(o)[m][1] then 
      SetRepresentative(d, f*LambdaOrbMult(o, m, lambda_l)[2]);
    else
      SetRepresentative(d, f);
    fi;
  fi;

  SetIsGreensClassNC(d, IsGreensClassNC(l)); 
  return d;
end);

# new for 1.0! - DClassOfRClass - "for a D-class of an acting semigroup"
#############################################################################

# same method for regular/inverse

InstallMethod(DClassOfRClass, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, f, d, o, rho_l, m;

  s:=ParentSemigroup(r); 
  f:=Representative(r);
  d:=Objectify(DClassType(s), rec());

  SetParentSemigroup(d, s);
  SetLambdaOrbSCCIndex(d, LambdaOrbSCCIndex(r));
  SetLambdaOrb(d, LambdaOrb(r));
  
  o:=GradedRhoOrb(s, f, IsGreensClassNC(r)<>true);
  SetRhoOrb(d, o);

  if IsGreensClassNC(r) then 
    SetLambdaOrbSCCIndex(d, 1);
    SetRepresentative(d, f);
  else
    rho_l:=o!.rho_l; #position of RhoFunc(s)(f) in o 
    m:=OrbSCCLookup(o)[rho_l];
    SetRhoOrbSCCIndex(d, m);
    if rho_l<>OrbSCC(o)[m][1] then 
      SetRepresentative(d, RhoOrbMults(o, m)[rho_l][2]*f);
    else
      SetRepresentative(d, f);
    fi;
  fi;

  SetIsGreensClassNC(d, IsGreensClassNC(r)); 
  return d;
end);

# new for 1.0! - DClassOfHClass - "for a H-class of an acting semigroup"
#############################################################################

# no new method required for regular/inverse semigroups. 

InstallMethod(DClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local s, f, d, o, m, l;

  s:=ParentSemigroup(h); 
  f:=Representative(h);
  d:=Objectify(DClassType(s), rec());
  SetParentSemigroup(d, s);
  
  o:=LambdaOrb(h);
  m:=LambdaOrbSCCIndex(h);
  
  SetLambdaOrbSCCIndex(d, m);
  SetLambdaOrb(d, o);
  
  l:=Position(o, LambdaFunc(s)(f));

  if l<>OrbSCC(o)[m][1] then 
    f:=f*LambdaOrbMult(o, m, l)[2];
  fi;

  o:=RhoOrb(h);
  m:=RhoOrbSCCIndex(h);

  SetRhoOrbSCCIndex(d, m);
  SetRhoOrb(d, o);
  
  l:=Position(o, RhoFunc(s)(f));

  if l<>OrbSCC(o)[m][1] then 
    f:=RhoOrbMult(o, m, l)[2]*f;
  fi;

  SetRepresentative(d, f);
  SetEquivalenceClassRelation(d, GreensDRelation(s));
  SetIsGreensClassNC(d, IsGreensClassNC(h)); 
  return d;
end);

# new for 1.0! - LClassOfHClass - "for a H-class of an acting semigroup"
#############################################################################

# no new method required for regular/inverse semigroups. 

InstallMethod(LClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local s, f, o, m, l, i;

  s:=ParentSemigroup(h); 
  f:=Representative(h);
  o:=RhoOrb(h);
  m:=RhoOrbSCCIndex(h);

  l:=Objectify(LClassType(s), rec());

  SetParentSemigroup(l, s);
  SetRhoOrbSCCIndex(l, m);
  SetRhoOrb(l, o);
  
  i:=Position(o, RhoFunc(s)(f));

  if i<>OrbSCC(o)[m][1] then 
    SetRepresentative(l, RhoOrbMults(o, m)[i][2]*f);
  else
    SetRepresentative(l, f);
  fi;

  SetEquivalenceClassRelation(l, GreensLRelation(s));
  SetIsGreensClassNC(l, IsGreensClassNC(h)); 
  return l;
end);

# new for 1.0! - RClassOfHClass - "for a H-class of an acting semigroup"
#############################################################################

# no new method required for regular/inverse semigroups. 

InstallMethod(RClassOfHClass, "for an H-class of an acting semigroup",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)
  local s, f, o, m, r, l;

  s:=ParentSemigroup(h); 
  f:=Representative(h);
  o:=LambdaOrb(h);
  m:=LambdaOrbSCCIndex(h);

  r:=Objectify(RClassType(s), rec());

  SetParentSemigroup(r, s);
  SetLambdaOrbSCCIndex(r, m);
  SetLambdaOrb(r, o);
  
  l:=Position(o, LambdaFunc(s)(f));

  if l<>OrbSCC(o)[m][1] then 
    SetRepresentative(r, f*LambdaOrbMult(o, m, l)[2]);
  else
    SetRepresentative(r, f);
  fi;

  SetEquivalenceClassRelation(r, GreensRRelation(s));
  SetIsGreensClassNC(r, IsGreensClassNC(h)); 
  return r;
end);


# mod for 1.0! - DClassReps - "for an acting semigroup"
#############################################################################

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

# mod for 1.0! - EnumeratorOfRClasses - "for an acting semigroup"
#############################################################################
# Notes: the only purpose for this is the method for NumberElement.  Otherwise
# use (if nothing much is known) IteratorOfRClasses or if everything is know
# just use RClasses.

# no method for regular/inverse semigroup just yet

InstallMethod(EnumeratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local enum;

  return EnumeratorByFunctions(s, rec(
    
    ElementNumber:=function(enum, pos)
      return GreensRClasses(s)[pos];
    end,

    NumberElement:=function(enum, r)
      return Position(SemigroupData(s), Representative(r))-1;
    end,

    Membership:=function(r, enum)
      return Position(enum, r)<>fail;
    end,
    
    Length:=enum -> NrRClasses(s),

    PrintObj:=function(enum)
      Print( "<enumerator of R-classes>");
      return;
    end));

  return enum;
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

# mod for 1.0! - NrHClasses - "for an acting semigroup"
#############################################################################
 
# different method for regular/inverse

InstallMethod(NrHClasses, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  return Sum(List(GreensDClasses(s), NrHClasses));
end);

#EOF
