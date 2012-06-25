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

# new for 1.0! - RhoSchutzGp - "for a D-class of an acting semigroup"
##############################################################################

InstallMethod(RhoSchutzGp, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local o, m, p;

  o:=RhoOrb(d); 
  m:=RhoOrbSCCIndex(d);

  #JDM the line below obviously won't work in the general case
  p:=MappingPermListList(RanT(RhoOrbRep(o, m)), RanT(Representative(d)));
  return RhoOrbSchutzGp(o, m, infinity)^p;
end);

# new for 1.0! - SemigroupDataSCC - "for a D-class of an acting semigp"
##############################################################################

InstallMethod(SemigroupDataSCC, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local data;
  
  data:=SemigroupData(ParentAttr(d));

  # scc of R-reps corresponding to d 
  return OrbSCC(data)[OrbSCCLookup(data)[d!.orbit_pos]];
end);

# new for 1.0! - SemigroupDataSCCIndex - "for a D-class of an acting semigp"
##############################################################################

InstallMethod(SemigroupDataSCCIndex, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return OrbSCCLookup(SemigroupData(ParentAttr(d)))[d!.orbit_pos];
end);

# new for 1.0! - LambdaCosets - "for a D-class of an acting semigp"
##############################################################################

InstallMethod(LambdaCosets, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return RightTransversal(LambdaOrbSchutzGp(LambdaOrb(d),
   LambdaOrbSCCIndex(d)), SchutzenbergerGroup(d));
end);

# new for 1.0! - RhoCosets - "for a D-class of an acting semigp"
##############################################################################

InstallMethod(RhoCosets, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  return RightTransversal(RhoSchutzGp(d), SchutzenbergerGroup(d));
end);

# new for 1.0! - RhoOrbSCCIndex - "for a Green's class of an acting semigp"
##############################################################################

InstallMethod(RhoOrbSCCIndex, "for a Green's class of an acting semigroup", 
[IsActingSemigroupGreensClass and IsGreensDClass], 
function(d)
  local o;

  o:=RhoOrb(d);
  return OrbSCCLookup(o)[Position(o,
   RhoFunc(ParentAttr(d))(Representative(d)))];
end);

# new for 1.0! - LambdaOrbSCC - "for Green's class of an acting semigroup"
############################################################################

InstallOtherMethod(LambdaOrbSCC, "for a D-class of an acting semi",
[IsActingSemigroupGreensClass and IsGreensDClass],
d-> OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)]);

InstallOtherMethod(RhoOrbSCC, "for a Green's class of an acting semi",
[IsActingSemigroupGreensClass and IsGreensDClass], 
d-> OrbSCC(RhoOrb(d))[RhoOrbSCCIndex(d)]);

# new for 1.0! - SchutzenbergerGroup - "for an R-class of an acting semigp."
#############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  return LambdaOrbSchutzGp(LambdaOrb(r), LambdaOrbSCCIndex(r));
end);

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
    return ParentAttr(x)=ParentAttr(y) and Representative(x) in y;
  fi;
  return ParentAttr(x)=ParentAttr(y) and Representative(x) in y and
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
    return ParentAttr(x)=ParentAttr(y) and Representative(x) <
     Representative(y);
  fi;
  return fail;
end);

# new for 1.0! - \in - "for acting elt and D-classNC of acting semigp"
#############################################################################
# Notes: a D-classNC is one created using lambda and rho orbits and not from
# finding strongly connected components of SemigroupData(s).

#JDM this requires testing

InstallMethod(\in, "for acting elt and D-class of acting semigp.",
[IsActingElt, IsGreensDClass and IsActingSemigroupGreensClass and IsGreensClassNC],
function(f, d)
  local rep, s, g, m, o, scc, l, schutz, cosets, x;
  
  rep:=Representative(d); 
  s:=ParentAttr(d);
  
  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or Degree(f) <> Degree(rep) or
   Rank(f) <> Rank(rep) then
    Info(InfoCitrus, 1, "degree or rank not equal to those of",
    " any of the D-class elements,");
    return false;
  fi;

  g:=f;

  m:=LambdaOrbSCCIndex(d); o:=LambdaOrb(d); scc:=OrbSCC(o);

  l:=Position(o, LambdaFunc(s)(g));

  if l = fail or OrbSCCLookup(o)[l]<>m then 
    return false;
  fi;
  
  if l<>scc[m][1] then 
    g:=g*LambdaOrbMults(o, m)[l];
  fi;

  m:=RhoOrbSCCIndex(d); o:=RhoOrb(d); scc:=OrbSCC(o); 

  l:=Position(o, RhoFunc(s)(g));

  if l = fail or OrbSCCLookup(o)[l]<>m then 
    return false;
  fi;

  schutz:=RhoOrbStabChain(o, m);

  if schutz=true then 
    return true;
  fi;

  if l<>scc[m][1] then 
    g:=RhoOrbMults(o, m)[l]*g;
  fi;

  cosets:=LambdaCosets(d);
  g:=LambdaPerm(rep, g);

  if schutz<>false then 
    for x in cosets do 
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

# new for 1.0! - \in - "for acting elt and D-class of acting semigp"
#############################################################################
# Notes: a D-class is one created from finding strongly connected components of
# SemigroupData(s).

InstallMethod(\in, "for acting elt and D-class of acting semigp.",
[IsActingElt, IsGreensDClass and IsActingSemigroupGreensClass],
function(f, d)
  local rep, s, g, m, o, scc, l, rho, val, lookup, lambdaperm, schutz, data,
  reps, cosets, x;
  
  rep:=Representative(d); 
  s:=ParentAttr(d);
 
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
    g:=g*LambdaOrbMults(o, m)[l];
  fi;

  rho:=RhoFunc(s)(g);

  val:=ShallowCopy(o[scc[m][1]]);
  Append(val, rho);
  val:=HTValue(LambdaRhoHT(s), val);

  lookup:=LambdaRhoLookup(d);

  if val=fail or not IsBound(lookup[val]) then 
    return false;
  fi;

  data:=SemigroupData(s);
  reps:=data!.reps;
  lambdaperm:=LambdaPerm(s);
  schutz:=LambdaOrbStabChain(o, m);
  
  if Length(lookup[val])=1 then 
    g:=lambdaperm(g, reps[val][lookup[val][1]]);
    
    if g=() or schutz=true then 
      return true;
    elif schutz=false then 
      return false;
    fi;
    return SiftedPermutation(schutz, g)=();
  fi;

  if Length(lookup[val])<=Index(LambdaOrbSchutzGp(o, m), 
   SchutzenbergerGroup(d)) then 

    if schutz=true then 
      return true;
    elif schutz<>false then 
      for m in lookup[val] do 
        if SiftedPermutation(schutz, lambdaperm(reps[val][m], g))=() then 
          return true;
        fi;
      od;
    else # schutz is false and so g has to be one the R-reps of D-class
      m:=HTValue(data!.ht, g);
      lookup:=OrbSCCLookup(data);
      if m<>fail and lookup[m]=lookup[d!.orbit_pos] then 
        return true;
      fi;
    fi;
    return false;
  fi;
 
  # do the old thing
  o:=RhoOrb(d); m:=RhoOrbSCCIndex(d); 
  schutz:=RhoOrbStabChain(o, m);

  if schutz=true then 
    return true;
  fi;

  l:=Position(o, RhoFunc(s)(g)); scc:=OrbSCC(o); 
  
  if l<>scc[m][1] then 
    g:=RhoOrbMults(o, m)[l]*g;
  fi;

  # schutz<>false as if it is then Length(lookup[val])=1<=Index above.
  g:=LambdaPerm(g, rep);
  cosets:=LambdaCosets(d);

  for x in cosets do 
    if SiftedPermutation(schutz, g/x)=() then 
      return true;
    fi;
  od;

  return false;
end);

# if there is only one value in any set of LambdaRhoLookup (and hence all) then 
# just check if PermLeftQuoTransformationNC=LambdaPerm(f, anyone of those one
# values) in LambdaOrbSchutzGp. If RhoOrbSchutzGp is known, then check if there
# are more cosets of SchutzGp(d)=LambdaOrbSchutzGp\cap RhoOrbSchutzGp in
# LambdaOrbSchutzGp or in RhoOrbSchutzGp and check membership in whichever has
# fewer cosets. Otherwise calculate RhoOrbSchutzGp. Note that the elements in
# any of the sets in LambdaRhoLookup are a transversal of SchutzGp(d) in
# RhoOrbSchutzGp and so if LambdaOrbSchutzGp is trivial, then they are just
# the elements (and hence a generating set) for RhoOrbSchutzGp. 

# JDM the above is currently not used

# new for 1.0! - \in - "for acting elt and R-class of acting semigp"
#############################################################################
# Algorithm E. 

InstallMethod(\in, "for acting elt and R-class of acting semigp.",
[IsActingElt, IsGreensRClass and IsActingSemigroupGreensClass],
function(f, r)
  local rep, s, m, o, l, schutz, g;

  rep:=Representative(r); 
  s:=ParentAttr(r);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or Degree(f) <> Degree(rep) or
   Rank(f) <> Rank(rep) or RhoFunc(s)(f) <> RhoFunc(s)(rep) then
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

  g:=f*LambdaOrbMults(o, m)[l];

  if g=rep then
    Info(InfoCitrus, 3, "element with rectified lambda value equals ",
    "R-class representative");
    return true;
  elif schutz=false then
    Info(InfoCitrus, 3, "Schutz. group of R-class is trivial");
    return false;
  fi;

  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g))=();
end);

#AAA

# new for 1.0! - AsList - "for an R-class of an acting semigp."
#############################################################################
# Algorithm D.

InstallOtherMethod(AsList, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)        
  local f, g, elts, o, m, mults, scc, p, i;
  
  f:=Representative(r); 
  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
 
  g:=List(SchutzenbergerGroup(r), x-> f*x);
  elts:=EmptyPlist(Size(r));
  
  mults:=LambdaOrbMults(o, m);
  scc:=OrbSCC(o)[m];
  
  for i in scc do
    p:=mults[i]; 
    Append(elts, g*p^-1);
  od;
  
  return elts;
end);

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

InstallGlobalFunction(CreateDClass,  
function(arg) 
  local d, val;
 
  d:=Objectify(DClassType(arg[1]), rec(orbit_pos:=arg[5])); 
  
  SetParentAttr(d, arg[1]);
  SetLambdaOrbSCCIndex(d, arg[2]);
  SetLambdaOrb(d, arg[3]);
  SetRepresentative(d, arg[4]); 

  SetRhoOrb(d, RhoOrb(arg[1]));
  SetEquivalenceClassRelation(d, GreensDRelation(arg[1])); 
  SetIsGreensClassNC(d, false);
  return d; 
end); 

# mod for 1.0! - CreateRClass - not a user function!
#############################################################################
# Usage: arg[1] = semigroup; arg[2] = lambda orb scc index;
# arg[3] = lambda orb; arg[4] = rep; arg[5] = position in SemigroupData of rep.

# rep should be with rectified image only!

InstallGlobalFunction(CreateRClass,
function(arg)
  local r;
  
  r:=Objectify(RClassType(arg[1]), rec(orbit_pos:=arg[5]));

  SetParentAttr(r, arg[1]);
  SetLambdaOrbSCCIndex(r, arg[2]);
  SetLambdaOrb(r, arg[3]);
  SetRepresentative(r, arg[4]);

  SetEquivalenceClassRelation(r, GreensRRelation(arg[1]));
  SetIsGreensClassNC(r, false);
  return r;
end);

#HHH



#EEE

# mod for 1.0! - Enumerator - "for a D-class of acting semigp."
#############################################################################

InstallOtherMethod(Enumerator, "for a D-class of acting semigp.",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)

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
      local R, i, j;

      R:=GreensRClasses(d);
      for i in [1..Length(R)] do
        j:=Position(Enumerator(R[i]), f);
        if not j=fail then
          return enum!.m*(i-1)+j;
        fi;
      od;
      return fail;
    end,

    #######################################################################

    Membership:=function(elm, enum)
      return elm in d; #the D-class itself!
    end,

    Length:=enum -> Size(d),

    PrintObj:=function(enum)
      Print( "<enumerator of D-class>");
    return;
  end));
end);

# mod for 1.0! - Enumerator - "for R-class of an acting semigroup"
##############################################################################

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
     
     return enum[pos[2]]/mults[scc[pos[1]]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local s, rep, o, m, l, g, j;

      s:=ParentAttr(r);
      rep:=Representative(r);
      
      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
       Degree(f) <> Degree(rep) or Rank(f) <> Rank(rep) or 
        RhoFunc(s)(f) <> RhoFunc(s)(rep) then 
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
     
      g:=f*mults[l];

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

# mod for 1.0! - Enumerator - "for a transformation semigroup"
#############################################################################
# Notes: this is not an enumerator as I could not get an enumerator to perform 
# well here. 

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

InstallMethod(GreensDClasses, "for an acting semigroup",
[IsActingSemigroup], 
function(s)
  local data, orbit, r, scc, out, x, i;

  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  
  orbit:=data!.orbit;
  r:=data!.modifier;

  scc:=OrbSCC(data);
  out:=EmptyPlist(Length(scc));
  
  for i in [1+r..Length(scc)] do 
    out[i-r]:=CallFuncList(CreateDClass, orbit[i]);
  od;
  return out;
end);

# mod for 1.0! - GreensLClasses - "for a D-class of an acting semigroup"
##############################################################################

InstallMethod(GreensLClasses, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass], 
function(d)
  Error("not yet implemented");
  return;
end);

# new for 1.0! - GreensRClasses - "for a D-class of an acting semigroup"
##############################################################################

InstallMethod(GreensRClasses, "for an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass], 
function(d)
  local scc, out, orbit, j, i;

  scc:=SemigroupDataSCC(d);
  out:=EmptyPlist(Length(scc));
  orbit:=SemigroupData(ParentAttr(d))!.orbit;
  j:=0;

  for i in scc do 
    j:=j+1;
    out[j]:=CallFuncList(CreateRClass, orbit[i]);
  od;
  return out;
end);

# mod for 1.0! - GreensRClasses - "for an acting semigroup"
##############################################################################

InstallMethod(GreensRClasses, "for an acting semigroup",
[IsActingSemigroup], 
function(s)
  local data, orbit, r, out, i;

  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  
  orbit:=data!.orbit;
  r:=data!.modifier;
  
  out:=EmptyPlist(Length(orbit));

  for i in [1+r..Length(orbit)] do 
    out[i-r]:=CallFuncList(CreateRClass, orbit[i]);
  od;
  return out;
end);

# mod for 1.0! - GreensDClassOfElement - "for an acting semigp and elt."
#############################################################################

InstallOtherMethod(GreensDClassOfElement, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local pos, d, o;

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;

  if IsClosed(SemigroupData(s)) then 
    pos:=Position(SemigroupData(s), f);
    return CallFuncList(CreateDClass, SemigroupData(s)[pos]);
  fi;
  
  d:=Objectify(DClassType(s, rec()));
  SetParentAttr(d, s);

  o:=GradedLambdaOrb(s, f, true);     
  SetLambdaOrb(d, o);
  SetLambdaOrbSCCIndex(d, OrbSCCLookup(o)[Position(o, LambdaFunc(s)(f))]);
  
  o:=GradedRhoOrb(s, f, true);
  SetRhoOrb(d, o);
  SetRhoOrbSCCIndex(d, OrbSCCLookup(o)[Position(o, RhoFunc(s)(f))]);

  SetRepresentative(d, f);
  SetEquivalenceClassRelation(d, GreensDRelation(s));
  SetIsGreensClassNC(d, false);
  return d;
end);

# mod for 1.0! - GreensDClassOfElementNC - "for an acting semigp and elt."
#############################################################################

InstallOtherMethod(GreensDClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local d;
  
  d:=Objectify(DClassType(s), rec());
 
  SetParentAttr(d, s);
  SetLambdaOrbSCCIndex(d, 1);
  SetLambdaOrb(d, GradedLambdaOrb(s, f, false));
  SetRhoOrbSCCIndex(d, 1);
  SetRhoOrb(d, GradedRhoOrb(s, f, false));

  SetRepresentative(d, f);
  SetEquivalenceClassRelation(d, GreensDRelation(s));
  SetIsGreensClassNC(d, true);
  return d;
end);

# mod for 1.0! - GreensRClassOfElement - "for D-class and acting elt"
#############################################################################

InstallOtherMethod(GreensRClassOfElement, "for D-class and acting elt",
[IsGreensDClass and IsActingSemigroupGreensClass, IsActingElt],
function(d, f)
    
  if not f in d then
    Error("the element does not belong to the D-class,");
    return;
  fi;
  
  return GreensRClassOfElementNC(d, f);
end);

# mod for 1.0! - GreensRClassOfElementNC - "for D-class and acting elt"
#############################################################################

InstallOtherMethod(GreensRClassOfElementNC, "for D-class and acting elt",
[IsGreensDClass and IsActingSemigroupGreensClass, IsActingElt],
function(d, f)
  local s, r;

  s:=ParentAttr(d);
  r:=Objectify(RClassType(s), rec());

  SetParentAttr(r, s);
  SetLambdaOrbSCCIndex(r, LambdaOrbSCCIndex(d));
  SetLambdaOrb(r, LambdaOrb(d));
  SetRepresentative(r, f);
  SetEquivalenceClassRelation(r, GreensRRelation(s));
  SetIsGreensClassNC(r, true);
  return r;
end);

# mod for 1.0! - GreensRClassOfElement - "for an acting semigp and elt."
#############################################################################

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

InstallOtherMethod(GreensRClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local pos, r;
  
  pos:=Position(SemigroupData(s), f);
  
  if pos<>fail then 
    return CallFuncList(CreateRClass, SemigroupData(s)[pos]);
  fi;  

  r:=Objectify(RClassType(s), rec());

  SetParentAttr(r, s);
  SetLambdaOrbSCCIndex(r, 1);
  SetLambdaOrb(r, GradedLambdaOrb(s, f, false));
  SetRepresentative(r, f);
  SetEquivalenceClassRelation(r, GreensRRelation(s));
  SetIsGreensClassNC(r, true);
  return r;
end);

# mod for 1.0! - GreensJClassOfElement - for an acting semi and elt."
#############################################################################

InstallOtherMethod(GreensJClassOfElement, "for acting semigroup and elt.",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsActingElt], 
GreensDClassOfElement);

#III

# mod for 1.0! - Idempotents - "for a R-class of a trans. semigp."
#############################################################################

InstallOtherMethod(Idempotents, "for a R-class of a trans. semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, out, rho, o, m, scc, j, tester, creator, i;

  if not IsRegularRClass(r) then
    return [];
  fi;
  
  s:=ParentAttr(r);

  if Rank(Representative(r))=Degree(s) then
    return [One(s)];
  fi;

  out:=[]; 
  
  rho:=RhoFunc(s)(Representative(r));
  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
  scc:=OrbSCC(o)[m];
  j:=0;
  tester:=IdempotentLambdaRhoTester(s);
  creator:=IdempotentLambdaRhoCreator(s);

  for i in scc do
    if tester(o[i], rho) then
      j:=j+1;
      out[j]:=creator(o[i], rho);
    fi;
  od;

  if HasNrIdempotents(r) then 
    SetNrIdempotents(r, j);   
  fi;

  return out;
end);

# new for 0.1! - IsGreensClassOfTransSemigp - "for a Green's class"
#############################################################################

InstallMethod(IsGreensClassOfTransSemigp, "for a Green's class",
[IsGreensClass], x-> IsTransformationSemigroup(ParentAttr(x)));

# new for 0.1! - IsGreensClass - "for a Green's class"
#############################################################################
# JDM remove these?

InstallOtherMethod(IsGreensClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensRClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensLClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensHClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensDClass, "for an object", [IsObject], ReturnFalse);

# new for 1.0! - IsRegularRClass - "for an R-class of an acting semi"
#############################################################################

InstallMethod(IsRegularRClass, "for an R-class of an acting semigp",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, data, rho, o, m, scc, tester, i;

  if HasNrIdempotents(r) then 
    return NrIdempotents(r)<>0;
  fi;

  s:=ParentAttr(r);
  data:=SemigroupData(s);
  
  if not IsGreensClassNC(r) then
    if data!.repslens[data!.orblookup[r!.orbit_pos]]>1 then
      return false;
    fi;
  fi; 
  
  # is r the group of units...
  if Rank(Representative(r))=Degree(s) then
    return true;
  fi;   
 
  rho:=RhoFunc(s)(Representative(r));
  o:=LambdaOrb(r);
  m:=LambdaOrbSCCIndex(r);
  scc:=OrbSCC(o)[m];
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

InstallMethod(Iterator, "for an R-class of an acting semigp",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, mults, iter, scc;

  o:=LambdaOrb(r); m:=LambdaOrbSCCIndex(r);
  mults:=LambdaOrbMults(o, m);
  scc:=OrbSCC(o)[m];

  if HasAsSSortedList(r) then 
    iter:=IteratorList(AsSSortedList(r));
  else
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
       
        return iter!.schutz[at[2]]/mults[scc[at[1]]];
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

InstallOtherMethod(Iterator, "for a trivial acting semigp", 
[IsActingSemigroup and HasGeneratorsOfSemigroup and IsTrivial], 9999,
function(s)
  return TrivialIterator(Generators(s)[1]);
end);

# mod for 1.0! - Iterator - "for an acting semigroup"
#############################################################################

InstallMethod(Iterator, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter;

  Info(InfoCitrus, 4, "Iterator: for a trans. semigroup");

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

  SetIsIteratorOfSemigroup(iter, true);

  return iter;
end);

# new for 0.5! - Iterator - "for a full transformation semigroup"
#############################################################################

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

# new for 1.0! - IteratorOfDClassData - "for an acting semigroup"
#############################################################################
# Notes: this should not be used if IsClosed(SemigroupData(s)); 

InstallMethod(IteratorOfDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)

  if IsClosed(SemigroupData(s)) then 
    return IteratorList(GreensRClasses(s));
  fi;

  return IteratorByFunctions( rec( 

    classes:=[],

    R:=IteratorOfRClasses(s),

    IsDoneIterator:=iter-> IsDoneIterator(iter!.R),

    NextIterator:=function(iter)
      local R, classes, r, d; 
      R:=iter!.R; classes:=iter!.classes;
      repeat 
        r:=NextIterator(R);
      until IsDoneIterator(R) or 
       ForAll(classes, d-> not Representative(r) in d);
      d:=DClassOfRClass(r);
      Add(classes, d);
      return d;
    end,
    
    ShallowCopy:=iter-> rec(classes:=[], R:=IteratorOfRClasses(s))));
end);


# new for 1.0! - IteratorOfRClassData - "for an acting semigroup"
#############################################################################

InstallMethod(IteratorOfRClassData, "for an acting semigroup",
[IsActingSemigroup],
function(s)

  return IteratorByFunctions( rec( 
    
    i:=SemigroupData(s)!.modifier,

    IsDoneIterator:=iter-> IsClosed(SemigroupData(s)) and 
     iter!.i>=Length(SemigroupData(s)),

    NextIterator:=function(iter)
      local data;

      iter!.i:=iter!.i+1;
      
      data:=Enumerate(SemigroupData(s), iter!.i, ReturnFalse);

      if iter!.i>Length(data!.orbit) then 
        return fail;
      fi;
      return data!.orbit[iter!.i];
    end,
    
    ShallowCopy:=iter-> rec(i:=0)));
end);

# new for 1.0! - IteratorOfRClassReps - "for an acting semigroup"
#############################################################################

InstallMethod(IteratorOfRClassReps, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x-> x[4],
[IsIteratorOfRClassReps]));

# new for 1.0! - IteratorOfRClasses - "for an acting semigroup"
#############################################################################

InstallMethod(IteratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x->
CallFuncList(CreateRClass, x), [IsIteratorOfRClasses]));

#LLL

# new for 1.0! - LClassReps - "for an acting semigroup D-class"
#############################################################################

InstallOtherMethod(LClassReps, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local o, m, mults, scc, cosets, f, out, k, i, j;
  
  o:=LambdaOrb(d); 
  m:=LambdaOrbSCCIndex(d);
  mults:=LambdaOrbMults(o, m);
  scc:=LambdaOrbSCC(d);
  cosets:=LambdaCosets(d);
  f:=Representative(d);
  
  out:=EmptyPlist(Length(scc)*Length(cosets));

  k:=0;
  for i in scc do
    for j in cosets do
      k:=k+1;
     out[k]:=f*(j/mults[i]);
    od;
  od;

  return out;
end);

#NNN

# new for 1.0! - NrIdempotents - "for an R-class of an acting semigp."
#############################################################################

InstallOtherMethod(NrIdempotents, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, data, rho, o, m, scc, nr, tester, i;

  if HasIsRegularRClass(r) and not IsRegularRClass(r) then 
    return 0;
  fi;

  s:=ParentAttr(r);     

  # check if we already know this...
  if not IsGreensClassNC(r) then
    data:=SemigroupData(s);
    if data!.repslens[data!.orblookup[r!.orbit_pos]]>1 then
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

InstallMethod(NrIdempotents, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local nr, data, reps, repslens, repslookup, len, rhofunc, tester, f, j, o, m, scc, rho, x, i, k;

  if HasIdempotents(s) then 
    return Length(Idempotents(s));
  fi;

  nr:=0;

  if HasGreensDClasses(s) then 
    for x in GreensDClasses(s) do 
      nr:=nr+NrIdempotents(x);
    od;
  else
    data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
    reps:=data!.reps; repslens:=data!.repslens;
    repslookup:=data!.repslookup;

    len:=Length(reps);
    rhofunc:=RhoFunc(s);
    tester:=IdempotentLambdaRhoTester(s);

    for i in [1..len] do 
      f:=reps[i][1]; 
      j:=repslookup[i][1];
      o:=data[j][3];
      m:=data[j][2];
      scc:=OrbSCC(o)[m];
      rho:=rhofunc(f);
      for k in scc do 
        if tester(o[k], rho) then 
          nr:=nr+1;
        fi;
      od;
    od;
  fi;

  return nr;
end);

# mod for 1.0! - NrDClasses - "for an acting semigroup"
#############################################################################

InstallMethod(NrDClasses, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  return Length(OrbSCC(SemigroupData(s)))-SemigroupData(s)!.modifier;
end);
 
# mod for 1.0! - NrRClasses - "for a D-class of an acting semigroup"
#############################################################################

InstallOtherMethod(NrRClasses, "for a D-class of an acting semigroup",       
[IsActingSemigroupGreensClass and IsGreensDClass],
d-> Length(SemigroupDataSCC(d)));

# mod for 1.0! - NrRClasses - "for an acting semigroup"
#############################################################################

InstallMethod(NrRClasses, "for an acting semigroup",       
[IsActingSemigroup and HasGeneratorsOfSemigroup],        
function(s)
  local data;
  
  data:=Enumerate(SemigroupData(s), infinity, ReturnFalse);
  return Length(data!.orbit)-data!.modifier;
end);

#OOO

# new for 0.5! - One - "for a transformation"
#############################################################################
# this should go to transform.gi

InstallMethod(One, "for a transformation",
[IsTransformation], 10, s-> TransformationNC([1..Degree(s)]*1));

#PPP

# mod for 1.0! - PrintObj - IsIteratorOfRClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassReps],
function(iter)
  Print("<iterator of R-class reps>");
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
  fi;
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfRClassElements
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassElements],
function(iter)
  Print("<iterator of R-class>");
  return;
end);

#RRR

# mod for 1.0! - RClassReps - "for a D-class of an acting semigroup"
############################################################################

InstallOtherMethod(RClassReps, "for a D-class of an acting semigroup",
[IsActingSemigroupGreensClass and IsGreensDClass],
function(d)
  local data, o, m, f, mults, scc, cosets, out, k, g, i, j;

  if not IsGreensClassNC(d) then
    data:=SemigroupData(ParentAttr(d));
    return List(SemigroupDataSCC(d), i-> data[i][4]);
  fi;

  o:=RhoOrb(d); 
  m:=RhoOrbSCCIndex(d);
  f:=Representative(d);
  mults:=RhoOrbMults(o, m);
  scc:=RhoOrbSCC(d);
  cosets:=RhoCosets(d);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  SetNrRClasses(d, Length(scc)*Length(cosets));
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

InstallMethod(RClassReps, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local data, orbit, nr, r, out, i;

  data:=Enumerate(SemigroupData(s)); 
  orbit:=data!.orbit;
  nr:=Length(orbit);
  r:=data!.modifier;
  out:=EmptyPlist(nr-r);

  for i in [1+r..nr] do 
    out[i-r]:=orbit[i][4];
  od;
  return out;
end);

# new for 1.0! - DClassType - "for an acting semigroup"
############################################################################# 

InstallMethod(DClassType, "for an acting semigroups",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
          IsEquivalenceClassDefaultRep and IsGreensDClass and
          IsActingSemigroupGreensClass);
end);

# new for 1.0! - RClassType - "for an acting semigroup"
############################################################################

InstallMethod(RClassType, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensRClass and
         IsActingSemigroupGreensClass);
end);

#SSS

# new for 1.0! - SchutzenbergerGroup - "for a D-class of an acting semigroup"
#############################################################################

InstallMethod(SchutzenbergerGroup, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local o, m, lambda_schutz, lambda_stab, rho_schutz, p;
  
  o:=LambdaOrb(d); m:=LambdaOrbSCCIndex(d);
  lambda_schutz:=LambdaOrbSchutzGp(o, m); 
  
  if IsTrivial(lambda_schutz) then 
    return lambda_schutz;
  fi;

  lambda_stab:=LambdaOrbStabChain(o, m);
  o:=RhoOrb(d); m:=RhoOrbSCCIndex(d);
  
  #if not IsBound(o!.schutz) or not IsBound(o!.schutz[m]) then 
    #do something complicated
    # - create the RhoOrbSchutzGp using the elements of LambdaRhoLookup
    #  and lambda_stab, bound on size, sifting to avoid adding gens 
    #  outside the intersection of RhoOrbSchutzGp and LambdaOrbSchutzGp
  #fi;

  if RhoOrbStabChain(o, m)=true then 
    return lambda_schutz;
  fi;
    
  rho_schutz:=RhoSchutzGp(d);
  if IsTrivial(rho_schutz) then 
    return rho_schutz;
  fi;
  
  if lambda_stab=true then 
    return rho_schutz;
  fi;

  return Intersection(lambda_schutz, rho_schutz);
end);

# new for 1.0! - Size - "for a D-class of an acting semigp."
#############################################################################

InstallOtherMethod(Size, "for a D-class of an acting semigp.",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local l, r;
  
  l:=LambdaOrbSchutzGp(LambdaOrb(d), LambdaOrbSCCIndex(d));
  r:=RhoOrbSchutzGp(RhoOrb(d), RhoOrbSCCIndex(d), infinity);
  return Size(r)*Size(l)*Length(LambdaOrbSCC(d))*Length(RhoOrbSCC(d))/
   Size(SchutzenbergerGroup(d));
end);

# new for 1.0! - Size - "for an R-class of an acting semigp."
#############################################################################
# Algorithm C. 

InstallOtherMethod(Size, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m;
 
  o:=LambdaOrb(r); m:=LambdaOrbSCCIndex(r);
  return Size(SchutzenbergerGroup(r))*Length(OrbSCC(o)[m]);
end);

#UUU

# old 

# new for 0.1! - HClassReps - "for a transformation semigp."
############################################################################

InstallMethod(HClassReps, "for a transformation semigp.",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local out, iter, i, f;
  Info(InfoCitrus, 4, "HClassReps");

  out:=EmptyPlist(NrHClasses(s));
  iter:=IteratorOfHClassReps(s);
  i:=0;

  for f in iter do
    i:=i+1;
    out[i]:=f;
  od;

  return out;
end);

#DDD

# new for 1.0! - DClassOfRClass - "for a D-class of an acting semigroup"
#############################################################################

InstallMethod(DClassOfRClass, "for an R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, f, d;

  if not IsGreensClassNC(r) and IsClosed(SemigroupData(ParentAttr(r))) then 
    return CallFuncList(CreateDClass, 
     SemigroupData(ParentAttr(r))[r!.orbit_pos]);
  fi;

  s:=ParentAttr(r); 
  f:=Representative(r);
  d:=Objectify(DClassType(s), rec());

  SetParentAttr(d, s);
  SetLambdaOrbSCCIndex(d, LambdaOrbSCCIndex(r));
  SetLambdaOrb(d, LambdaOrb(r));
  SetRhoOrbSCCIndex(d, 1);
  SetRhoOrb(d, GradedRhoOrb(s, f, false));

  SetRepresentative(d, f);
  SetEquivalenceClassRelation(d, GreensDRelation(s));
  SetIsGreensClassNC(d, true); 
  return d;
end);

# new for 0.1! - DClassReps - "for a trans. semigroup"
#############################################################################

InstallMethod(DClassReps, "for a trans. semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
  function(s)

  ExpandOrbitsOfKernels(s);
  return List(OrbitsOfKernels(s)!.data, x-> DClassRepFromData(s, x));
end);

# new for 0.4! - EnumeratorOfRClasses - "for a trans. semigroup"
#############################################################################
# Notes: NumberElement does not work for RClassNCs, JDM maybe it should!

InstallMethod(EnumeratorOfRClasses, "for a trans. semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local enum;

  if HasGreensRClasses(s) then 
    enum:=EnumeratorByFunctions(s, rec(
      
      ElementNumber:=function(enum, pos)
        return GreensRClasses(s)[pos];
      end,

      NumberElement:=function(enum, r)
        
        if not ParentAttr(r)=s then 
          return fail;
        fi;

        if IsRClassNC(r) then 
          return fail;
        fi;

        return RClassIndexFromData(s, r!.data);
      end,

      Membership:=function(r, enum)
        return not Position(enum, r)=fail;
      end,
      
      Length:=enum -> NrRClasses(s),

      PrintObj:=function(enum)
        Print( "<enumerator of R-classes>");
        return;
      end));

    return enum;
  fi;

  enum:=EnumeratorByFunctions(s, rec(
   
    ElementNumber:=function(enum, pos)
      local data, m, iter, i;

      data:=OrbitsOfImages(s)!.data; m:=Length(data);

      if m>=pos then 
        data:=data[pos];
      elif OrbitsOfImages(s)!.finished then 
        return fail;
      else
        iter:=IteratorOfNewRClassRepsData(s);
        for i in [1..pos-m-1] do 
          NextIterator(iter);
        od;
        data:=NextIterator(iter);
      fi;

      if not data=fail then 
        return CreateRClass(s, data, OrbitsOfImages(s),
          RClassRepFromData(s, data));        
      fi;
      return fail;
    end,

    NumberElement:=function(enum, r)

      if not ParentAttr(r)=s then
        return fail;
      fi;

      if IsRClassNC(r) then
        return fail;
      fi;  

      return RClassIndexFromData(s, r!.data);
    end,

    Length:=enum -> NrRClasses(s),

    PrintObj:=function(enum)
      Print( "<enumerator of R-classes>");
      return;
  end));
      
  return enum;
end);

# new for 0.1! - GreensHClasses - "for a transformation semigroup"
##############################################################################

InstallMethod(GreensHClasses, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter, out, i, h;

  Info(InfoCitrus, 4, "GreensHClasses");

  iter:=IteratorOfHClasses(s);
  out:=EmptyPlist(NrHClasses(s));
  i:=0;

  for h in iter do 
    i:=i+1;
    out[i]:=h;
  od;

  return out;
end);

# mod for 0.4! - Idempotents - "for a transformation semigroup"
#############################################################################

InstallOtherMethod(Idempotents, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local n, out, kers, imgs, j, i, ker, img;

  if IsRegularSemigroup(s) then 
    n:=DegreeOfTransformationSemigroup(s);

    if HasNrIdempotents(s) then 
      out:=EmptyPlist(NrIdempotents(s));
    else
      out:=[];
    fi;

    kers:=GradedKernelsOfTransSemigroup(s); 
    imgs:=GradedImagesOfTransSemigroup(s);

    j:=0;
    
    for i in [1..n] do
      for ker in kers[i] do
        for img in imgs[i] do 
          if IsInjectiveTransOnList(ker, img) then 
            j:=j+1;
            out[j]:=IdempotentNC(ker, img);
          fi;
        od;
      od;
    od;

    if not HasNrIdempotents(s) then 
      SetNrIdempotents(s, j);
    fi;
    return out;
  fi;

  return Concatenation(List(GreensRClasses(s), Idempotents));
end);

# new for 0.1! - Idempotents - "for a trans. semigroup and pos. int."
#############################################################################

InstallOtherMethod(Idempotents, "for a trans. semigroup and pos. int.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt],
function(s, i)
  local out, n, kers, imgs, j, ker, img, r;
  
  n:=DegreeOfTransformationSemigroup(s);
  
  if i>n then 
    return [];
  fi;

  if HasIdempotents(s) then 
    return Filtered(Idempotents(s), x-> RankOfTransformation(x)=i);
  fi; 

  if HasNrIdempotents(s) then
    out:=EmptyPlist(NrIdempotents(s));
  else
    out:=[];
  fi;

  if IsRegularSemigroup(s) then 

    kers:=GradedKernelsOfTransSemigroup(s)[i]; 
    imgs:=GradedImagesOfTransSemigroup(s)[i];
    j:=0;

    for ker in kers do
      for img in imgs do 
        if IsInjectiveTransOnList(ker, img) then 
          j:=j+1;
          out[j]:=IdempotentNC(ker, img);
        fi;
      od;
    od;

    return out;
  fi;

  for r in GreensRClasses(s) do 
    if RankOfTransformation(r!.rep)=i then 
      out:=Concatenation(out, Idempotents(r));
    fi;
  od;
  return out;
end);

# new for 0.1! - NrHClasses - "for a transformation semigroup"
#############################################################################
 
InstallMethod(NrHClasses, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local i, iter, o, d;
  i:=0;

  iter:=IteratorOfDClassRepsData(s);
  o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];

  for d in iter do 
    i:=i+Length(ImageOrbitCosetsFromData(s, d[2], o[2]))*
     Length(ImageOrbitSCCFromData(s, d[1], o[1]))*
      Length(KernelOrbitSCCFromData(s, d[2], o[2]))*
       Length(KernelOrbitCosetsFromData(s, d[2], o[2]));
  od;

  return i;
end);

# new for 0.1! - Size - "for a simple transformation semigroup"
#############################################################################
# JDM check this is actually superior to the above method for Size

InstallOtherMethod(Size, "for a simple transformation semigroup",
[IsSimpleSemigroup and IsTransformationSemigroup],
function(s)
  local gens, ims, kers, H;

  gens:=Generators(s);

  ims:=Size(Set(List(gens, ImageSetOfTransformation)));
  kers:=Size(Set(List(gens, CanonicalTransSameKernel)));
  H:=GreensHClassOfElement(s, gens[1]);

  return Size(H)*ims*kers;
end);




#EOF
