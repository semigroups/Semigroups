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

# the first three functions should be updated!

# methods for inverse acting semigroups consisting of acting elements with a
# ^-1 operator. 

# new for 1.0! - \in - "for an acting semigroup with inversion and elt"
##############################################################################

InstallMethod(\in, "for inverse acting elt and acting semigroup with inversion",
[IsInverseActingElt, IsActingSemigroupWithInverseOp],
function(f, s)
  local dom, o, rho, rho_l, lambda_l, lambda, m, schutz, scc, g;
  
  if not ElementsFamily(FamilyObj(s))=FamilyObj(f) then 
    Error("the element and semigroup are not of the same type,");
    return;
  fi;

  if HasAsSSortedList(s) then 
    return f in AsSSortedList(s); 
  fi;

  dom:=RhoDomain(s);

  if dom=[] then 
    return Degree(f)=0;
  # any way of using points here? JDM
  fi;

  o:=RhoOrb(s);
  rho:=RhoFunc(s)(f);

  if IsClosed(o) then
    rho_l:=Position(o, rho);
    if rho_l=fail or (rho_l=1 and not IsMonoidAsSemigroup(s)) then
      return false;
    fi;
    lambda_l:=Position(o, LambdaFunc(s)(f));
    if lambda_l=fail then
      return false;
    fi;
  else

    rho_l:=Position(o, rho);
    if rho_l=fail then
      o!.looking:=true; o!.lookingfor:=function(o, x) return x=rho; end;
      o!.lookfunc:=o!.lookingfor;
      Enumerate(o);
      rho_l:=PositionOfFound(o);
      o!.found:=false; o!.looking:=false;
      Unbind(o!.lookingfor); Unbind(o!.lookfunc);

      if rho_l=false then
        return false;
      fi;
    fi;

    if rho=[] then
      return true;
    elif rho_l=1 and not IsMonoidAsSemigroup(s) then
      return false;
    fi;

    lambda:=LambdaFunc(s)(f);
    
    if IsClosed(o) then
      lambda_l:=Position(o, lambda);
      if lambda_l=fail then
        return false;
      fi;
    else 
      o:=GradedRhoOrb(s, f, false);
      Enumerate(o, infinity);
      rho_l:=1;
      lambda_l:=Position(o, lambda);
      if lambda_l=fail then
        return false;
      fi;
    fi;
  fi;

  m:=OrbSCCLookup(o)[rho_l];

  if OrbSCCLookup(o)[lambda_l]<>m then
    return false;
  fi;

  schutz:=RhoOrbStabChain(o, m);

  if schutz=true then
    return true;
  fi;

  scc:=OrbSCC(o)[m];
  g:=f;
  if lambda_l<>scc[1] then 
    g:=g*RhoOrbMults(o, m)[lambda_l][1];
  fi;

  if rho_l<>scc[1] then 
    g:=RhoOrbMults(o, m)[rho_l][2]*g;
  fi;

  if IsIdempotent(g) then 
    return true;
  elif schutz=false then
    return false;
  fi;

  #JDM really One?
  return SiftedPermutation(schutz, LambdaPerm(s)(One(g), g))=();
end);

# new for 1.0! - DClassReps - "for an acting semigroup with inversion"
##############################################################################

InstallOtherMethod(DClassReps, "for an acting semigroup with inversion",
[IsActingSemigroupWithInverseOp],
function(s)            
  local o, r, i, out, f, m;
  
  o:=RhoOrb(s);
  r:=Length(OrbSCC(o));
  i:=ActingSemigroupModifier(s);
  out:=EmptyPlist(r);
  
  for m in [1..r-i] do 
    f:=RhoOrbRep(o, m+i);
# JDM method for RightOne of inverse acting element required.
    out[m]:=RightOne(f);
  od;
  return out;
end);

# new for 1.0! - Size - "for an acting semigroup with inversion"
##############################################################################

InstallOtherMethod(Size, "for an acting semigroup with inversion",
[IsActingSemigroupWithInverseOp],
function(s)
  local o, scc, r, i, nr, m;

  o:=RhoOrb(s);   scc:=OrbSCC(o);
  r:=Length(scc); i:=ActingSemigroupModifier(s);
  nr:=0;

  for m in [1+i..r] do 
    nr:=nr+Length(scc[m])^2*Size(RhoOrbSchutzGp(o, m, infinity));
  od;
  return nr;
end);

#GGG

# new for 1.0! - GreensHClasses - for an acting semigroup with inverse op
############################################################################

#JDM RhoOrb of h is not defined here, maybe it should be!?

InstallOtherMethod(GreensHClasses, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local lambda_o, lambda_scc, lambda_len, out, type, hrel, l, n, lambda_m, lambda_mults, f, h, i, j, k;
  
  lambda_o:=Enumerate(LambdaOrb(s), infinity);
  lambda_scc:=OrbSCC(lambda_o);
  lambda_len:=Length(lambda_scc);
  
  out:=EmptyPlist(NrHClasses(s));
  type:=HClassType(s);
  hrel:=GreensHRelation(s);
  l:=ActingSemigroupModifier(s);
  n:=0; 
    
  for i in [1..lambda_len-l] do
    lambda_m:=i+l;
    lambda_mults:=LambdaOrbMults(lambda_o, lambda_m);
    f:=RightOne(LambdaOrbRep(lambda_o, lambda_m));
    for j in lambda_scc[lambda_m] do
      f:=f*lambda_mults[j][1];
      for k in lambda_scc[lambda_m] do
        n:=n+1;
        h:=Objectify(type, rec());
        SetParentSemigroup(h, s);
        SetLambdaOrb(h, lambda_o);
        SetLambdaOrbSCCIndex(h, lambda_m);
        SetRepresentative(h, lambda_mults[k][1]*f);
        SetEquivalenceClassRelation(h, hrel);
        SetIsGreensClassNC(h, false);
        out[n]:=h;
      od;
    od;
  od;
  return out;
end);
                    
# new for 0.7! - GreensRClasses - for acting semigroup with inverse op
##############################################################################
                    
InstallOtherMethod(GreensRClasses, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)         
  local l, o, scc, out, type, rrel, i, f, mults, r, m, j;
                    
  l:=ActingSemigroupModifier(s);
                    
  o:=LambdaOrb(s);
  scc:=OrbSCC(o);   
  out:=EmptyPlist(Length(o));
  type:=RClassType(s);
  rrel:=GreensRRelation(s);

  i:=0;             
                    
  for m in [1+l..Length(scc)] do
    f:=RightOne(LambdaOrbRep(o, m));
    mults:=LambdaOrbMults(o, m);
    for j in scc[m] do
      i:=i+1;       
      r:=Objectify(type, rec());
      SetParentSemigroup(r, s);
      SetLambdaOrbSCCIndex(r, m);
      SetLambdaOrb(r, o);
      SetRepresentative(r, mults[j][1]*f);
      SetEquivalenceClassRelation(r, rrel);
      SetIsGreensClassNC(r, false);
      out[i]:=r;
    od;             
  od;

  return out;
end);

# new for 1.0! - RClassReps - for acting semigroup with inverse op
##############################################################################
                    
InstallOtherMethod(RClassReps, "for acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)         
  local o, scc, nr, out, l, n, f, mults, m, j;
                    
  o:=LambdaOrb(s);
  scc:=OrbSCC(o);   
  
  nr:=Length(scc);
  out:=EmptyPlist(Length(o));
  l:=ActingSemigroupModifier(s);
  n:=0;             
                    
  for m in [1+l..nr] do
    f:=RightOne(LambdaOrbRep(o, m));
    mults:=LambdaOrbMults(o, m);
    for j in scc[m] do
      n:=n+1;       
      out[n]:=mults[j][1]*f;
    od;             
  od;
  return out;
end);

# new for 1.0! - RClassReps - for D-class of acting semigroup with inverse op
##############################################################################

InstallOtherMethod(RClassReps, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsInverseOpDClass],
d-> List(LClassReps(d), x-> x^-1));

# new for 0.7! - Random - "for an acting semigroup with inverse op"
#############################################################################

InstallMethod(Random, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local o, gens, i, w, k, m, l, g;

  o:=LambdaOrb(s);

  if not IsClosed(o) then
    gens:=GeneratorsOfSemigroup(s);    
    i:=Random([1..Int(Length(gens)/2)]);
    w:=List([1..i], x-> Random([1..Length(gens)]));
    return EvaluateWord(gens, w);
  fi;
  k:=Random([1..Length(o)]);
  m:=OrbSCCLookup(o)[k];
  l:=Random(OrbSCC(o)[m]);
  g:=Random(LambdaOrbSchutzGp(o, m));
  return o!.mults[k][1]*g*o!.mults[l][1];
end);

#HHH

# new for 1.0! - HClassReps - for an acting semigroup with inverse op
############################################################################

InstallOtherMethod(HClassReps, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local lambda_o, lambda_scc, lambda_len, out, type, hrel, l, n, lambda_m, lambda_mults, f, h, i, j, k;
  
  lambda_o:=Enumerate(LambdaOrb(s), infinity);
  lambda_scc:=OrbSCC(lambda_o);
  lambda_len:=Length(lambda_scc);
  
  out:=EmptyPlist(NrHClasses(s));
  l:=ActingSemigroupModifier(s);
  n:=0; 
    
  for i in [1..lambda_len-l] do
    lambda_m:=i+l;
    lambda_mults:=LambdaOrbMults(lambda_o, lambda_m);
    f:=RightOne(LambdaOrbRep(lambda_o, lambda_m));
    for j in lambda_scc[lambda_m] do
      f:=f*lambda_mults[j][1];
      for k in lambda_scc[lambda_m] do
        n:=n+1;
        out[n]:=lambda_mults[k][1]*f;
      od;
    od;
  od;
  return out;
end);

# new for 0.7! - IteratorOfRClassData - "for acting semigroup with inverse op
###############################################################################

InstallMethod(IteratorOfRClassData, "for acting semigp with inverse op",
[IsActingSemigroupWithInverseOp], 
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
        #JDM is f correct here? Not canonical rep?
        f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, i)); 
        return [s, 1, GradedLambdaOrb(s, o[i], true), f, false];
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
        return [s, m, LambdaOrb(s), f, false];
      end,

      ShallowCopy:=iter-> rec(m:=ActingSemigroupModifier(s), i:=0,
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
s-> IteratorByIterator(IteratorOfRClassData(s), x->
CallFuncList(CreateLClassNC, x), [IsIteratorOfLClasses]));

#NNN

# new for 0.7! - NaturalPartialOrder - "for an inverse semigroup"
##############################################################################
# C function for me!

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
function(s)
  return Length(Enumerate(LambdaOrb(s), infinity))-
   ActingSemigroupModifier(s);     
end);

# mod for 1.0! - NrRClasses - for an acting semigroup with inverse op
##############################################################################

InstallOtherMethod(NrRClasses, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp], NrLClasses);

# mod for 1.0! - NrHClasses - for an acting semigroup with inverse op
##############################################################################

InstallOtherMethod(NrHClasses, "for an acting semigroup with inverse op",
[IsActingSemigroupWithInverseOp],
function(s)
  local o, scc;
  o:=Enumerate(LambdaOrb(s), infinity);
  scc:=OrbSCC(o);

  return Sum(List(scc, m-> Length(m)^2))-ActingSemigroupModifier(s);
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
  l:=ActingSemigroupModifier(s);
  lambdafunc:=LambdaFunc(s);
 
  for i in [1..n] do  
    for x in gens do  
      for f in RClassReps(d[i]) do
        AddSet(out[i], lookup[Position(o, lambdafunc(x*f))]-l);      
        AddSet(out[i], lookup[Position(o, lambdafunc(f^-1*x))]-l);     
      od; 
    od;
  od; 
 
  Perform(out, ShrinkAllocationPlist);
  return out; 
end);

