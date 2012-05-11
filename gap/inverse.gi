#############################################################################
##
#W  inverse.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

## functions and methods for inverse semigroups of partial permutations

# D-data: [scc index]
# R-data: [scc index, pos. of dom, pos. of ran=scc[1]]
# L-data: [scc index, pos. of dom=scc[1], pos of ran]
# H-data: [scc index, pos. of dom, pos of ran]

# new for 0.7! - \= - "for Green's class and Green's class of part perm semi"
#############################################################################

InstallMethod(\=, "for Green's class and class of part. perm. semigp.",
[IsGreensClass and IsGreensClassOfPartPermSemigroup, IsGreensClass and
IsGreensClassOfPartPermSemigroup],
function(c1, c2)
  if (IsGreensRClass(c1) and IsGreensRClass(c2)) or 
   (IsGreensLClass(c1) and IsGreensLClass(c2)) or
   (IsGreensDClass(c1) and IsGreensDClass(c2)) or
   (IsGreensHClass(c1) and IsGreensHClass(c2)) then     
    return c1!.parent=c2!.parent and Representative(c1) in c2;
  fi;
  return c1!.parent=c2!.parent and Representative(c1) in c2 and 
   Size(c1)=Size(c2);
end);

# new for 0.7! - \< - "for Green's class and Green's class of part perm semi"
#############################################################################

InstallMethod(\<, "for Green's class and class of part. perm. semigp.",
[IsGreensClass and IsGreensClassOfPartPermSemigroup, IsGreensClass and
IsGreensClassOfPartPermSemigroup],
function(c1, c2)
  if (IsGreensRClass(c1) and IsGreensRClass(c2)) or 
   (IsGreensLClass(c1) and IsGreensLClass(c2)) or
   (IsGreensDClass(c1) and IsGreensDClass(c2)) or
   (IsGreensHClass(c1) and IsGreensHClass(c2)) then     
    return c1!.parent=c2!.parent and Representative(c1) < Representative(c2);
  fi;
  return false;
end);

# new for 0.7! - \in - "for an inverse semigroup"
#############################################################################

InstallMethod(\in, "for an inverse semigroup of part perms",
[IsPartialPerm, IsInverseSemigroup and IsPartialPermSemigroup],
function(f, s)
  local o, k, l, ran, m, schutz, g, dom_g, ran_g;

  if not f[1]=0 and (f[5]<Points(s)[1] or f[6]>Points(s)[Length(Points(s))]) 
   then 
    return false;
  fi;

  o:=LongOrb(s);

  if IsClosed(o) then 
    k:=Position(o, DomPP(f));
    if k=fail or (k=1 and not IsPartialPermMonoid(s)) then 
      return false;
    fi;
    l:=Position(o, RanSetPP(f));
    if l=fail then 
      return false;
    fi;
  else
    k:=Position(o, DomPP(f));
    if k=fail then 
      o!.looking:=true; o!.lookingfor:=function(o, x) return x=DomPP(f); end;
      o!.lookfunc:=o!.lookingfor;
      Enumerate(o);
      k:=PositionOfFound(o);
      o!.found:=false; o!.looking:=false; 
      Unbind(o!.lookingfor); Unbind(o!.lookfunc);
      
      if k=false then
        return false;
      fi;
    fi;

    if DomPP(f)=[] then 
      return true;
    elif k=1 and not IsPartialPermMonoid(s) then 
      return false;
    fi;
    ran:=RanSetPP(f);
       
    if IsClosed(o) and Position(o, ran)=fail then 
      return false;
    fi;
    # JDM could look ahead here...

    k:=1;
    o:=ShortOrb(s, DomPP(f));
    Enumerate(o, infinity);
    
    l:=Position(o, ran);
  
    if l=fail then 
      return false;
    fi;
  fi;

  # JDM should use TraceSchreierTreeForward(o, k) and not f when Schutz gp is
  # created!

  m:=OrbSCCLookup(o)[k];
  
  if not OrbSCCTruthTable(o)[m][l] then 
    return false;
  fi;
  
  schutz:=CreateOrbSCCSchutzGp(o, m)[1];
  
  if schutz=true then 
    return true;
  fi;

  g:=o!.mults[k]^-1*f*o!.mults[l]; #LQuoPP
  dom_g:=DomPP(g);
  ran_g:=RanPP(g);

  if dom_g=ran_g then # g is an idempotent! 
    return true;
  elif schutz=false then
    return false;
  fi;

  return SiftedPermutation(schutz, MappingPermListList(dom_g, ran_g))=();
end);

# new for 0.7! - \in - "for an R-class of inv semi and part perm" 
#############################################################################

InstallMethod(\in, "for an R-class of inv semi and part perm",
[IsPartialPerm , IsGreensRClass and 
IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(f, r)
  local rep, o, l, m, schutz, g;
  
  rep:=Representative(r);

  if f[1]<>rep[1] or f[f[1]+7]<>rep[rep[1]+7] or f[2]<>rep[2] 
   or DomPP(f)<>DomPP(rep) then 
    Info(InfoCitrus, 1, "degree, rank, or domain not equal to those of",
        " any of the R-class elements,");
    return false;
  fi;

  o:=r!.o;
  if not IsClosed(o) then     
    Enumerate(o, infinity);
  fi;
  l:=Position(o, RanSetPP(f));
  m:=r!.data[1];

  if l=fail or not OrbSCCTruthTable(o)[m][l] then 
    Info(InfoCitrus, 1, "range not equal to that of any R-class element,");
    return false;
  fi;

  schutz:=OrbSCCStabChain(r); 

  if schutz=true then 
    return true;
  fi;

  g:=f*o!.mults[l];

  if g=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;
  
  g:=rep^-1*g; #LQuoP #JDM is this necessary?
  return SiftedPermutation(schutz, MappingPermListList(DomPP(g), RanPP(g)))=(); 
end);

# new for 0.7! - \in - "for an L-class of inv semi and part perm" 
#############################################################################

InstallMethod(\in, "for an L-class of inv semi and part perm",
[IsPartialPerm , IsGreensLClass and 
IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(f, r)
  local rep, o, l, m, schutz, g;
  
  rep:=Representative(r);

  if f[3]<>rep[3] or f[4]<>rep[4] or f[2]<>rep[2] or RanSetPP(f)<>RanSetPP(rep) 
   then 
    Info(InfoCitrus, 1, "degree, rank, or range not equal to those of",
        " any of the L-class elements,");
    return false;
  fi;

  o:=r!.o;
  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;
  l:=Position(o, DomPP(f));
  m:=r!.data[1];

  if l=fail or not OrbSCCTruthTable(o)[m][l] then 
    Info(InfoCitrus, 1, "domain not equal to that of any L-class element,");
    return false;
  fi;

  schutz:=OrbSCCStabChain(r); 

  if schutz=true then 
    return true;
  fi;

  g:=o!.mults[l]^-1*f; #LQuoPP

  if g=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;

  return SiftedPermutation(schutz, MappingPermListList(DomPP(g), RanPP(g)))=(); 
end);

# new for 0.7! - \in - "for an D-class of inv semi and part perm" 
#############################################################################

InstallMethod(\in, "for an D-class of inv semi and part perm",
[IsPartialPerm , IsGreensDClass and 
IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(f, r)
  local rep, o, m, l_dom, l_ran, schutz, g;
  
  rep:=Representative(r);

  if f[2]<>rep[2] then
    Info(InfoCitrus, 1, "rank not equal to those of",
        " any of the D-class elements,");
    return false;
  fi;

  o:=r!.o; 
  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;
  m:=r!.data[1];
  
  l_dom:=Position(o, DomPP(f)); l_ran:=Position(o, RanSetPP(f));
  
  if l_dom=fail or l_ran=fail or not OrbSCCTruthTable(o)[m][l_dom] or 
   not OrbSCCTruthTable(o)[m][l_ran] then 
    Info(InfoCitrus, 1, "range or domain not equal to that of any D-class",
     " element,");
    return false;
  fi;

  schutz:=OrbSCCStabChain(r); 

  if schutz=true then 
    return true;
  fi;

  g:=o!.mults[l_dom]^-1*f*o!.mults[l_ran]; #LQuoPP

  if g=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;

  return SiftedPermutation(schutz, MappingPermListList(DomPP(g), RanPP(g)))=(); 
end);

# new for 0.7! - \in - "for an H-class of inv semi and part perm" 
############################################################################

InstallMethod(\in, "for an H-class of inv semi and part perm",
[IsPartialPerm , IsGreensHClass and 
IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(f, h)
  local rep, o, data, mults, schutz, g;
  
  rep:=Representative(h);

  if f[1]<>rep[1] or f[2]<>rep[2] or DomPP(f)<>DomPP(rep) or 
   RanSetPP(f)<>RanSetPP(rep) then
    return false;
  fi;

  schutz:=OrbSCCStabChain(h); 
  
  o:=h!.o; data:=h!.data; mults:=o!.mults;

  if schutz=true then 
    return true;
  fi;

  if f=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;
  g:=mults[data[2]]^-1*f*mults[data[3]]; #LQuoPP
  return SiftedPermutation(schutz, MappingPermListList(DomPP(g), RanPP(g)))=(); 
end);

#AAA

# new for 0.7! - AsList - "for a Green's class of inverse semigp."
############################################################################

InstallOtherMethod(AsList, "for an Green's class of inverse semigp.",
[IsGreensClass and IsGreensClassOfInverseSemigroup and IsGreensClassOfPartPermSemigroup],
C-> ListByIterator(Iterator(C), Size(C)));

# new for 0.7! - AsSSortedList - "for Green's class of partial perm semigroup"
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for Green's class of part. perm. semi.",
[IsGreensClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(r)
  return ConstantTimeAccessList(EnumeratorSorted(r));
end);

#CCC

# new for 0.7! - CreateOrbSCCMultipliers - not a user function 
#############################################################################
# Usage: gens = GeneratorsOfSemigroup; o = LongOrb; 
# m = scc index; scc = o!.scc[m].

#InstallGlobalFunction(CreateOrbSCCMultipliers, 
#function(gens, o, j, scc, mults)
#  local tree, m, out, i;
#  
#  tree:=SchreierTreeOfSCC(o, j);
#  m:=Length(scc); out:=EmptyPlist(m);
#
#  mults[scc[1]]:=PartialPermNC(o[scc[1]], o[scc[1]]);
#
#  for i in [2..m] do
#    i:=scc[i];
#    while i > scc[1] do
#      if not IsBound(mults[i]) then 
#        mults[i]:=mults[tree[2][i]]*gens[tree[1][i]];
#      fi;
#     i:=tree[2][i];
#  od;
# od;
#return mults;

#end);

InstallGlobalFunction(CreateOrbSCCMultipliers, 
function(gens, o, m, scc)
  
  if not IsBound(o!.mults) then 
    if not IsClosed(o) then 
      Enumerate(o);
    fi;
    o!.mults:=EmptyPlist(Length(o));
  fi;

  if not IsBound(o!.mults[scc[1]]) then 
    CreateOrbSCCMultipliersNC(gens, o, m, scc, o!.mults);
  fi;
  return o!.mults;
end);

# new for 0.7! - CreateOrbSCCMultipliersNC - not a user function 
#############################################################################
# Usage: gens = GeneratorsOfSemigroup; o = LongOrb; 
# m = scc index; scc = o!.scc[m]; mults = o!.mults.

InstallGlobalFunction(CreateOrbSCCMultipliersNC, 
function(gens, o, m, scc, mults)
  local w, i;

  for i in scc do 
    w:=TraceSchreierTreeOfSCCForward(o, m, i);
    if w=[] then 
      mults[i]:=PartialPermNC(o[i], o[i]);
    else
      mults[i]:=EvaluateWord(gens, w)^-1;
    fi;    
  od;
  o!.mults:=mults;
  return mults;
end);

# new for 0.7! - CreateOrbSCCSchutzGp - not a user function
#############################################################################
# Usage: o = orbits of images; m = scc index; 
# f = representative (optional)

# if the optional third arg is present it should have its range in position
# scc[1] of o!! Otherwise this will mess everything up!

InstallGlobalFunction(CreateOrbSCCSchutzGp, 
function(arg)
  local o, m, scc, f;
  
  o:=arg[1]; m:=arg[2]; scc:=OrbSCC(o)[m];

  if IsBound(o!.schutz) then 
    if IsBound(o!.schutz[m]) then 
      return o!.schutz[m];
    fi;
  else
    o!.schutz:=EmptyPlist(Length(scc));
  fi;
 
  if IsBound(arg[3]) then
    f:=arg[3];
  else
    #f:=TraceSchreierTreeForward(o, scc[1]);
    # if f=[] then
      f:=PartialPermNC(o[scc[1]], o[scc[1]]);
    #else
    #  f:=EvaluateWord(o!.gens, f);
    #fi;
  fi;  

  CreateOrbSCCMultipliers(o!.gens, o, m, scc);
  o!.schutz[m]:=CreateOrbSCCSchutzGpNC(o!.gens, o, f, o!.scc[m], o!.truth[m], 
   OrbitGraph(o), Length(o!.gens), o!.mults);
  return o!.schutz[m];
end);

# new for 0.7! - CreateOrbSCCSchutzGpNC - not a user function
#############################################################################

InstallGlobalFunction(CreateOrbSCCSchutzGpNC, 
function(gens, o, f, scc, truth, graph, r, mults)
  local bound, g, is_sym, k, i, j;
 
  if Length(o[scc[1]])<1000 then
    bound:=Factorial(Length(o[scc[1]]));
  else
    bound:=infinity;
  fi;

  g:=Group(()); is_sym:=false; 

  for i in scc do
    for j in [1..r] do
      if IsBound(graph[i][j]) and truth[graph[i][j]] then
        k:=(RightOne(f)/mults[i])*(gens[j]*mults[graph[i][j]]);
        g:=ClosureGroup(g, MappingPermListList(DomPP(k), RanPP(k)));
      fi;

      if Size(g)>=bound then
        is_sym:=true;
        break;
      fi;
    od;

    if Size(g)>=bound then
      break;
    fi;

  od;

  if is_sym then
    return [ true, g ];
  elif Size(g)=1 then
    return [ false, g ];
  fi;

  return [StabChainImmutable(g), g];
end);

#DDD

# new for 0.7! - DClassOfHClass - "for an H-class of a inverse semigroup"
##############################################################################

InstallOtherMethod(DClassOfHClass, "for an H-class of a inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup], 
function(h)
  local o, i, scc;

  o:=h!.o; i:=h!.data[1]; scc:=OrbSCC(o);
  return CreateDClass(h!.parent, [i], o, PartialPermNC(o[scc[i][1]],
   o[scc[i][1]]));
end);        

# new for 0.7! - DClassOfLClass - "for an L-class of a inverse semigroup"
##############################################################################

InstallOtherMethod(DClassOfLClass, "for an L-class of a inverse semigroup",
[IsGreensLClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup], 
l-> CreateDClass(l!.parent, [l!.data[1]], l!.o,
LeftOne(Representative(l))));        

# new for 0.7! - DClassOfRClass - "for an R-class of a inverse semigroup"
##############################################################################

InstallOtherMethod(DClassOfRClass, "for an R-class of a inverse semigroup",
[IsGreensRClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup], 
r-> CreateDClass(r!.parent, [r!.data[1]], r!.o,
RightOne(Representative(r))));        

# new for 0.7! - DClassReps - "for an inverse semi of part perms"
##############################################################################

InstallOtherMethod(DClassReps, "for an inverse semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, scc, r, i, out, j;
  
  o:=LongOrb(s);
  scc:=OrbSCC(o); 
  r:=Length(scc);
  
  if IsPartialPermMonoid(s) then 
    i:=0;
  else
    i:=1;
  fi;

  out:=EmptyPlist(r);
  
  for j in [1..r-i] do 
    out[j]:=PartialPermNC(o[scc[j+i][1]], o[scc[j+i][1]]);
  od;
  return out;
end);

# new for 0.7! - DClassType - "for a partial perm inverse semigroup"
############################################################################

InstallOtherMethod(DClassType, "for a partial perm inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensDClass and
         IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup);
end);

#EEE

# new for 0.7! - EnumerateInverseSemiData - "for an inverse semi of part perms"
##############################################################################

InstallGlobalFunction(EnumerateInverseSemiData, 
function(s)
  local o, modifier, scc, r, mults, schutz, graph, truth, gens, n, w, f, i;
  
  o:=LongOrb(s);
  
  if IsPartialPermMonoid(s) then 
    modifier:=0;
  else
    modifier:=1;
  fi;
   
  scc:=OrbSCC(o);
  r:=Length(scc);
  
  if not o!.finished then 
    mults:=EmptyPlist(Length(o)); 
    schutz:=EmptyPlist(r);
    graph:=OrbitGraph(o);;
    truth:=OrbSCCTruthTable(o);;
    gens:=o!.gens;
    n:=Length(gens);
    for i in [1..r] do 
      CreateOrbSCCMultipliersNC(gens, o, i, scc[i], mults);
      f:=PartialPermNC(o[scc[i][1]], o[scc[i][1]]);
      schutz[i]:=CreateOrbSCCSchutzGpNC(gens, o, f, scc[i], 
         truth[i], graph, n, mults);
    od;
    o!.mults:=mults;
    o!.schutz:=schutz;
    o!.finished:=true;
  else
    schutz:=o!.schutz;
  fi;
  
  SetSize(s, Sum(List([1..r], m-> Length(scc[m])^2*Size(schutz[m][2])))-
     modifier);
  return;
end); 

# new for 0.7! - Enumerator - "for an inverse semigroup"
#############################################################################
# Notes: this is not an enumerator as I could not get an enumerator to perform 
# well here. 

InstallOtherMethod(Enumerator, "for an inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup], 
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

# new for 0.7! - Enumerator - "for R-class of part perm inverse semigroup"
##############################################################################

InstallMethod(Enumerator, "for R-class of part perm inv semigroup",
[IsGreensRClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(r)

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
      q:=QuoInt(n, m); pos:=[ q, n - q * m]+1;
      return enum[pos[2]]/OrbMultipliers(r)[OrbSCC(r!.o)[r!.data[1]][pos[1]]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local rep, o, data, i, j, k;

      rep:=Representative(r);
      
      if f[1]<>rep[1] or f[f[1]+7]<>rep[rep[1]+7] or f[2]<>rep[2] 
       or DomPP(f)<>DomPP(rep) then
        Info(InfoCitrus, 1, "degree, rank, or domain not equal to those of",
          " any of the R-class elements,");
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=r!.o; data:=r!.data;
      i:=Position(r!.o, RanSetPP(f));

      if i = fail or not o!.truth[data[1]][i] then 
        return fail;
      fi;
     
      k:=rep^-1*f*o!.mults[i];
      j:=Position(enum!.schutz, MappingPermListList(DomPP(k), RanPP(k)));

      if j=fail then 
        return fail;
      fi;
      return enum!.len*(Position(OrbSCC(r!.o)[r!.data[1]], i)-1)+j;
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

# new for 0.7! - Enumerator - "for L-class of part perm inverse semigroup"
##############################################################################

InstallMethod(Enumerator, "for L-class of part perm inv semigroup",
[IsGreensLClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(r)

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
        return enum!.schutz[pos]*Representative(r);
      fi;

      n:=pos-1; m:=enum!.len;
      q:=QuoInt(n, m); pos:=[ q, n - q * m]+1;
      return OrbMultipliers(r)[OrbSCC(r!.o)[r!.data[1]][pos[1]]]*enum[pos[2]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local rep, o, data, i, j, k;

      rep:=Representative(r);
      
      if f[4]<>rep[4] or f[3]<>rep[3] or f[2]<>rep[2] or
        RanSetPP(f)<>RanSetPP(rep) then
        Info(InfoCitrus, 1, "degree, rank, or range not equal to those of",
          " any of the L-class elements,");
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=r!.o; data:=r!.data;
      i:=Position(r!.o, DomPP(f));

      if i = fail or not o!.truth[data[1]][i] then 
        return fail;
      fi;
      k:=o!.mults[i]^-1*f/rep; #LQuoPP
      j:=Position(enum!.schutz, MappingPermListList(DomPP(k), RanPP(k)));

      if j=fail then 
        return fail;
      fi;
      return enum!.len*(Position(OrbSCC(r!.o)[r!.data[1]], i)-1)+j;
    end,

    #########################################################################

    Membership:=function(elm, enum)
      return elm in r;
    end,

    Length:=enum-> Size(r),

    PrintObj:=function(enum)
      Print("<enumerator of L-class>");
      return;
    end));
end);

# new for 0.7! - Enumerator - "for D-class of part perm inverse semigroup"
##############################################################################

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

      scc:=OrbSCC(d!.o)[d!.data[1]];
      n:=pos-1; m:=Length(enum!.schutz); r:=Length(scc);
      q:=QuoInt(n, m); q2:=QuoInt(q, r);
      pos:=[ n-q*m, q2, q  - q2 * r ]+1;
      mults:=OrbMultipliers(d);
      return mults[scc[pos[2]]]*enum[pos[1]]/mults[scc[pos[3]]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local rep, o, m, k, l, j, scc;
      
      rep:=Representative(d);
      
      if f[2]<>rep[2] then 
        Info(InfoCitrus, 1, "rank not equal to those of",
          " any of the D-class elements,");
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=d!.o; m:=d!.data[1];
      
      k:=Position(o, DomPP(f)); 
      if k=fail or not OrbSCCTruthTable(o)[m][k] then 
        Info(InfoCitrus, 1, "domain not equal to that of any",
        " D-class element,");
        return fail;
      fi;

      l:=Position(o, RanSetPP(f));
      if l=fail or not OrbSCCTruthTable(o)[m][l] then
        Info(InfoCitrus, 1, "range not equal to that of any",
        " D-class element,");
        return fail;
      fi;

      m:=o!.mults[k]^-1*f*o!.mults[l]; #LQuoPP
      j:=Position(enum!.schutz, MappingPermListList(DomPP(m), RanPP(m)));

      if j=fail then 
        return fail;
      fi;
      scc:=OrbSCC(d!.o)[d!.data[1]];

      return Length(enum!.schutz)*((Position(scc, k)-1)*Length(scc)
      +(Position(scc, l)-1))+j;
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

# new for 0.7! - Enumerator - "for H-class of part perm inverse semigroup"
##############################################################################

InstallMethod(Enumerator, "for H-class of part perm inv semigroup",
[IsGreensHClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(h)

  return EnumeratorByFunctions(h, rec(

    schutz:=Enumerator(SchutzenbergerGroup(h)),

    #########################################################################

    ElementNumber:=function(enum, pos)
      local f,g;  
      if pos>Length(enum) then 
        return fail;
      fi;
      f:=h!.o!.mults[h!.data[2]];
      g:=h!.o!.mults[h!.data[3]]; 

      return f*enum!.schutz[pos]/g;
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local rep, g;
      
      rep:=Representative(h);
      
      if f[1]<>rep[1] or f[2]<>rep[2] or DomPP(f)<>DomPP(rep) or
         RanSetPP(f)<>RanSetPP(rep) then 
        return fail;
      fi;
     
      g:=h!.o!.mults[h!.data[2]]^-1*f*h!.o!.mults[h!.data[3]]; #LQuoPP

      return Position(enum!.schutz, MappingPermListList(DomPP(g), RanPP(g)));
    end,

    #########################################################################

    Membership:=function(elm, enum)
      return elm in h;
    end,

    Length:=enum-> Size(h),

    PrintObj:=function(enum)
      Print("<enumerator of H-class>");
      return;
    end));
end);


#GGG

# new for 0.7! - GreensDClassOfElement - for an inv semi and part perm
##############################################################################

InstallOtherMethod(GreensDClassOfElement, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(s, f)
  if not f in s then 
    Error("the partial perm. is not an element of the semigroup,");
    return;
  fi;
  return GreensDClassOfElementNC(s, f);
end);

# new for 0.7! - GreensDClassOfElementNC - for an inv semi and part perm
##############################################################################
# Notes: data is: [scc index, scc[1], pos of dom, pos of ran]

InstallOtherMethod(GreensDClassOfElementNC, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(s, f)
  local o, k, m, rep, d;

  if IsClosed(LongOrb(s)) then 
    o:=LongOrb(s);
    k:=Position(o, DomPP(f));
    if k=fail then 
      Info(InfoCitrus, 1, "the partial perm. is not an element of the semigroup");
      return fail;
    fi;
    m:=OrbSCCLookup(o)[k];
    k:=Position(o, RanSetPP(f));
    if k=fail or not OrbSCCTruthTable(o)[m][k] then 
      Info(InfoCitrus, 1, "the partial perm. is not an element of the           semigroup");
      return fail;
    fi;
    k:=OrbSCC(o)[m][1];
    rep:=PartialPermNC(o[k], o[k]);
  else
    o:=ShortOrb(s, DomPP(f));
    rep:=LeftOne(f);
    m:=1; 
  fi;

  return CreateDClass(s, [m], o, rep); 
end);

# new for 0.7! - GreensHClasses - for an inv semi of partial perms
############################################################################

InstallOtherMethod(GreensHClasses, "for an inv semi of partial perms",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  local o, scc, r, out, l, reps, m, mults, f, i, j, k;
  
  o:=LongOrb(s);
  EnumerateInverseSemiData(s);
  scc:=OrbSCC(o);
  r:=Length(scc);
  out:=EmptyPlist(NrHClasses(s));
 
  if IsPartialPermMonoid(s) then 
    l:=0;
  else;
    l:=1;
  fi;

  if HasHClassReps(s) then 
    reps:=HClassReps(s);
    m:=0;
    for i in [1..r-l] do 
      for j in scc[i+l] do
        for k in scc[i+l] do
          m:=m+1;
          out[m]:=CreateHClass(s, [i+l,j,k], o, reps[m]);
        od;
      od; 
    od;
  else
    reps:=EmptyPlist(NrHClasses(s));
    mults:=o!.mults;
    m:=0;

    for i in [1..r-l] do 
      f:=PartialPermNC(o[scc[i+l][1]], o[scc[i+l][1]]);
      for j in scc[i+l] do
        for k in scc[i+l] do
          m:=m+1;
          reps[m]:=mults[j]*f/mults[k];
          out[m]:=CreateHClass(s, [i+l,j,k], o, reps[m]);
        od;
      od;
    od;
    SetHClassReps(s, reps); 
  fi;

  return out;
end);

# new for 0.7! - GreensHClasses - for an D-class of inv semi of partial perms
############################################################################

InstallOtherMethod(GreensHClasses, "for D-class of inv semi of partial perms",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local m, o, scc, s, out, reps, k, mults, f, i, j;
  
  m:=d!.data[1]; o:=d!.o; scc:=OrbSCC(o)[m]; s:=d!.parent;
  out:=EmptyPlist(Length(scc)^2);
  
  if HasHClassReps(d) then 
    reps:=HClassReps(d);
    k:=0;
    for i in scc do
      for j in scc do
        k:=k+1;
        out[k]:=CreateHClass(s, [m,i,j], o, reps[k]);
      od;
    od; 
  else
    reps:=EmptyPlist(Length(scc)^2);
    mults:=OrbMultipliers(d);
    f:=Representative(d);
    
    k:=0; 
    for i in scc do
      for j in scc do
        k:=k+1;
        reps[k]:=mults[i]*f/mults[j];
        out[k]:=CreateHClass(s, [m,i,j], o, reps[k]);
      od;
    od;
    SetHClassReps(d, reps); 
  fi;

  return out;
end);

# new for 0.7! - GreensHClasses - for an L-class of inv semi of partial perms
############################################################################

InstallOtherMethod(GreensHClasses, "for L-class of inv semi of partial perms",
[IsGreensLClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local m, j, o, scc, s, out, reps, mults, f, i;
  
  m:=d!.data[1]; j:=d!.data[3]; o:=d!.o; scc:=OrbSCC(d!.o)[m];
  s:=d!.parent; out:=EmptyPlist(Length(scc));
  
  if HasHClassReps(d) then 
    reps:=HClassReps(d);
    for i in [1..Length(scc)] do
      out[i]:=CreateHClass(s, [m,scc[i],j], o, reps[i]);
    od;
  else
    reps:=EmptyPlist(Length(scc));
    mults:=OrbMultipliers(d);
    f:=Representative(d);
  
    for i in [1..Length(scc)] do
      reps[i]:=mults[scc[i]]*f;
      out[i]:=CreateHClass(s, [m,scc[i],j], o, reps[i]);
    od;
    SetHClassReps(d, reps); 
  fi;

  return out;
end);

# new for 0.7! - GreensHClasses - for an R-class of inv semi of partial perms
############################################################################

InstallOtherMethod(GreensHClasses, "for R-class of inv semi of partial perms",
[IsGreensRClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(r)
  local m, k, l, o, scc, s, out, reps, mults, f, i;
  
  m:=r!.data[1]; k:=r!.data[2]; l:=r!.data[3]; o:=r!.o; 
  scc:=OrbSCC(o)[m]; s:=r!.parent; 
  out:=EmptyPlist(Length(scc));
  
  if HasHClassReps(r) then 
    reps:=HClassReps(r);
    for i in [1..Length(scc)] do
      out[i]:=CreateHClass(s, [m,k,scc[i]], o, reps[i]);
    od; 
  else
    reps:=EmptyPlist(Length(scc));
    mults:=OrbMultipliers(r);
    f:=Representative(r);
  
    for i in [1..Length(scc)] do
      reps[i]:=f/mults[scc[i]];
      out[i]:=CreateHClass(s, [m,k,scc[i]], o, reps[i]);
    od;
    SetHClassReps(r, reps); 
  fi;

  return out;
end);

# new for 0.7! - GroupHClass - for a D-class of inv semi
############################################################################

InstallOtherMethod(GroupHClass, "for a D-class of inverse semi",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local h;

  h:=GreensHClassOfElementNC(d, Representative(d));
  SetIsGroupHClass(h, true);
  return h;
end);

# new for 0.7! - GreensLClasses - for a D-class of inv semi of part perms
##############################################################################

InstallOtherMethod(GreensLClasses, "for D-class of inv semi of part perms",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local m, o, scc, s, out, reps, mults, f, i;

  m:=d!.data[1]; o:=d!.o; scc:=OrbSCC(d!.o)[m]; s:=d!.parent; 
  out:=EmptyPlist(Length(scc));

  if HasLClassReps(d) then 
    reps:=LClassReps(d);
    for i in [1..Length(scc)] do 
      out[i]:=CreateLClass(s, [m,scc[1],scc[i]], o, reps[i]);
    od;
  else
    reps:=EmptyPlist(Length(scc));
    mults:=OrbMultipliers(d);
    f:=Representative(d);
    for i in [1..Length(scc)] do 
      reps[i]:=f/mults[scc[i]];
      out[i]:=CreateLClass(s, [m,scc[1],scc[i]], o, reps[i]);
    od;
    SetLClassReps(d, reps);
  fi;
  return out;
end);

# new for 0.7! - GreensLClasses - for an inv semi of part perms
##############################################################################

InstallOtherMethod(GreensLClasses, "for an inv semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local l, o, scc, out, type, mults, i, f, rep, m, j;

  if IsPartialPermMonoid(s) then
    l:=0;
  else
    l:=1;
  fi;
  
  o:=LongOrb(s); EnumerateInverseSemiData(s);
  scc:=OrbSCC(o); out:=EmptyPlist(Length(o));
  type:=LClassType(s); mults:=o!.mults;

  i:=0; 
  
  for m in [1..Length(scc)-l] do
    f:=PartialPermNC(o[scc[m+l][1]], o[scc[m+l][1]]);
    for j in scc[m+l] do 
      i:=i+1;
      rep:=f/mults[j];
      out[i]:=CreateLClass(s, [m+l,scc[m+l][1],j], o, rep);
   od;
  od;

  return out;
end);

# new for 0.7! - GreensRClasses - for a D-class of inv semi of part perms
##############################################################################

InstallOtherMethod(GreensRClasses, "for D-class of inv semi of part perms",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local m, o, scc, s, out, reps, mults, f, i;

  m:=d!.data[1]; o:=d!.o; scc:=OrbSCC(o)[m]; s:=d!.parent; 
  
  out:=EmptyPlist(Length(scc));

  if HasRClassReps(d) then 
    reps:=RClassReps(d);
    for i in [1..Length(scc)] do 
      out[i]:=CreateRClass(s, [m,scc[i],scc[1]], o, reps[i]);
    od;
  else
    reps:=EmptyPlist(Length(scc));
    mults:=OrbMultipliers(d);
    f:=Representative(d);
    for i in [1..Length(scc)] do 
      reps[i]:=mults[scc[i]]*f;
      out[i]:=CreateRClass(s, [m,scc[i],scc[1]], o, reps[i]);
    od;
    SetRClassReps(d, reps);
  fi;
  return out;
end);

# new for 0.7! - GreensRClasses - for an inv semi of part perms
##############################################################################

InstallOtherMethod(GreensRClasses, "for an inv semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local l, o, scc, out, type, mults, rrel, i, f, rep, m, j;

  if IsPartialPermMonoid(s) then
    l:=0;
  else
    l:=1;
  fi;
  
  o:=LongOrb(s); EnumerateInverseSemiData(s); scc:=OrbSCC(o);
  out:=EmptyPlist(Length(o)); 

  type:=RClassType(s); mults:=o!.mults; rrel:=GreensRRelation(s);

  i:=0; 
  
  for m in [1..Length(scc)-l] do
    f:=PartialPermNC(o[scc[m+l][1]], o[scc[m+l][1]]);
    for j in scc[m+l] do 
      i:=i+1;
      rep:=mults[j]*f;
      out[i]:=CreateRClass(s, [m+l, j, scc[m+l][1]], o, rep);
   od;
  od;

  return out;
end);

# new for 0.7! - GreensDClasses - for an inv semi of part perms
##############################################################################

InstallOtherMethod(GreensDClasses, "for an inv semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local l, o, scc, out, i, f, m;

  if IsPartialPermMonoid(s) then 
    l:=0;
  else
    l:=1;
  fi;

  o:=LongOrb(s); EnumerateInverseSemiData(s); 
  scc:=OrbSCC(o); out:=EmptyPlist(Length(scc)); 

  i:=0;
  for m in [1..Length(scc)-l] do 
    i:=i+1;
    f:=PartialPermNC(o[scc[m+l][1]], o[scc[m+l][1]]);
    out[i]:=CreateDClass(s,[m+l], o, f);
  od;
  return out;
end);

# new for 0.7! - GreensHClassOfElement - for an inv semi and part perm
##############################################################################

InstallOtherMethod(GreensHClassOfElement, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(s, f)
  if not f in s then 
    Error("the partial perm. is not an element of the semigroup,");
    return;
  fi;
  return GreensHClassOfElementNC(s, f);
end);

# new for 0.7! - GreensHClassOfElement - for Green's class  and part perm
##############################################################################

InstallOtherMethod(GreensHClassOfElement, "for Green's class and part perm",
[IsGreensClass and IsGreensClassOfPartPermSemigroup and 
 IsGreensClassOfInverseSemigroup, IsPartialPerm],
function(class, f)
  if not f in class then 
    Error("the partial perm. is not an element of the Green's class,");
    return;
  fi;
  return GreensHClassOfElementNC(class, f);
end);

# new for 0.7! - GreensHClassOfElementNC - for an inv semi and part perm
##############################################################################
# Notes: data is: [scc index, scc[1], pos of DomPP, pos of ran]

InstallOtherMethod(GreensHClassOfElementNC, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(s, f)
  local o, k, m, l;

  if IsClosed(LongOrb(s)) then 
    o:=LongOrb(s);
    k:=Position(o, DomPP(f));
    if k=fail then 
      Info(InfoCitrus, 1, "the partial perm. is not an element of the semigroup");
      return fail;
    fi;
    m:=OrbSCCLookup(o)[k];
  else
    o:=ShortOrb(s, DomPP(f));
    Enumerate(o);
    k:=1; m:=1; 
  fi;
  
  l:=Position(o, RanSetPP(f));
  if l=fail or not OrbSCCTruthTable(o)[m][l] then 
    Info(InfoCitrus, 1, "the partial perm. is not an element of the           semigroup");
    return fail;
  fi;
  
  return CreateHClass(s, [m,k,l], o, f); 
end);

# new for 0.7! - GreensHClassOfElementNC - for an R-class and part perm
##############################################################################

InstallOtherMethod(GreensHClassOfElementNC, "for an R-class and part perm",
[IsGreensRClass and IsGreensClassOfPartPermSemigroup and 
IsGreensClassOfInverseSemigroup, IsPartialPerm],
function(r, f)
  local o, m, k, l;

  o:=r!.o; m:=r!.data[1]; k:=r!.data[2];
  
  l:=Position(o, RanSetPP(f));
  if l=fail or not OrbSCCTruthTable(o)[m][l] then 
    return fail;
  fi;

  return CreateHClass(r!.parent, [m, k, l], o, f);
end);

# new for 0.7! - GreensHClassOfElementNC - for an L-class and part perm
##############################################################################

InstallOtherMethod(GreensHClassOfElementNC, "for an L-class and part perm",
[IsGreensLClass and IsGreensClassOfPartPermSemigroup and 
IsGreensClassOfPartPermSemigroup, IsPartialPerm ],
function(r, f)
  local o, m, k, l, h;

  o:=r!.o; m:=r!.data[1]; l:=r!.data[3];
  
  k:=Position(o, DomPP(f));
  if k=fail or not OrbSCCTruthTable(o)[m][k] then 
    return fail;
  fi;

  return CreateHClass(r!.parent, [m, k, l], o, f);
end);

# new for 0.7! - GreensHClassOfElementNC - for an D-class and part perm
##############################################################################

InstallOtherMethod(GreensHClassOfElementNC, "for an D-class and part perm",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and 
IsGreensClassOfPartPermSemigroup, IsPartialPerm ],
function(r, f)
  local o, m, k, l, h;

  o:=r!.o; m:=r!.data[1];
  
  k:=Position(o, DomPP(f));
  if k=fail or not OrbSCCTruthTable(o)[m][k] then 
    return fail;
  fi;

  l:=Position(o, RanSetPP(f));
  if l=fail or not OrbSCCTruthTable(o)[m][l] then 
    return fail;
  fi;
  
  return CreateHClass(r!.parent, [m, k, l], o, f);
end);

# new for 0.7! - GreensJClassOfElement - for an inverse semi  and partial perm"
#############################################################################
  
InstallOtherMethod(GreensJClassOfElement, "for inverse semi and partial perm",
[IsInverseSemigroup and IsPartialPermSemigroup and HasGeneratorsOfSemigroup, IsPartialPerm], GreensDClassOfElement);

# new for 0.7! - GreensLClassOfElement - for an inv semi and part perm
##############################################################################

InstallOtherMethod(GreensLClassOfElement, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(s, f)
  if not f in s then 
    Error("the partial perm. is not an element of the semigroup,");
    return;
  fi;
  return GreensLClassOfElementNC(s, f);
end);

# new for 0.7! - GreensLClassOfElement - for a D-class and part perm
##############################################################################

InstallOtherMethod(GreensLClassOfElement, "for a D-class and part perm",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup, IsPartialPerm],
function(d, f)
  if not f in d then 
    Error("the partial perm. is not an element of the D-class,");
    return;
  fi;
  return GreensLClassOfElementNC(d, f);
end);

# new for 0.7! - GreensLClassOfElementNC - for an inv semi and part perm
##############################################################################
# Notes: data is: [scc index, scc[1], pos of dom, pos of ran]

InstallOtherMethod(GreensLClassOfElementNC, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(s, f)
  local o, k, m, rep, l;

  if IsClosed(LongOrb(s)) then 
    o:=LongOrb(s);
    k:=Position(o, DomPP(f));
    if k=fail then 
      Info(InfoCitrus, 1, "the partial perm. is not an element of the semigroup");
      return fail;
    fi;
    m:=OrbSCCLookup(o)[k];
    CreateOrbSCCSchutzGp(o, m); #JDM replace this with something more sensible
    rep:=o!.mults[k]^-1*f; #LQuoPP
  else
    o:=ShortOrb(s, DomPP(f));
    Enumerate(o);
    k:=1; m:=1; rep:=f;
  fi;

  l:=Position(o, RanSetPP(f));
  if l=fail or not OrbSCCTruthTable(o)[m][l] then 
    return fail;
  fi;

  return CreateLClass(s, [m, k, l], o, rep);
end);

# new for 0.7! - GreensLClassOfElementNC - for D-class and part perm
##############################################################################
# Notes: data is: [scc index, scc[1], pos of dom, pos of ran]

InstallOtherMethod(GreensLClassOfElementNC, "for a D-class and part perm",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup, IsPartialPerm],
function(d, f)
  local o, m, k, rep, l;

  o:=d!.o; m:=d!.data[1];
  k:=Position(o, DomPP(f));
  
  if k=fail then 
    return fail;
  fi;
  
  CreateOrbSCCSchutzGp(o, m); #JDM replace this with something more sensible
  rep:=o!.mults[k]^-1*f;
  
  l:=Position(o, RanSetPP(f));

  if l=fail or not OrbSCCTruthTable(o)[m][l] then 
    return fail;
  fi;

  return CreateLClass(d!.parent, [m, o!.scc[m][1], l], o, rep);
end);

# new for 0.7! - GreensRClassOfElement - for an inv semi and part perm
##############################################################################

InstallOtherMethod(GreensRClassOfElement, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(s, f)
  if not f in s then 
    Error("the partial perm. is not an element of the semigroup,");
    return;
  fi;
  return GreensRClassOfElementNC(s, f);
end);

# new for 0.7! - GreensRClassOfElement - for a D-class and part perm
##############################################################################

InstallOtherMethod(GreensRClassOfElement, "for a D-class and part perm",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup, IsPartialPerm],
function(d, f)
  if not f in d then 
    Error("the partial perm. is not an element of the D-class,");
    return;
  fi;
  return GreensRClassOfElementNC(d, f);
end);

# new for 0.7! - GreensRClassOfElementNC - for an inv semi and part perm
##############################################################################
# Notes: data is: [scc index, scc[1], pos of dom, pos of ran]

InstallOtherMethod(GreensRClassOfElementNC, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(s, f)
  local o, l, m, rep, k;

  if IsClosed(LongOrb(s)) then 
    o:=LongOrb(s);
    l:=Position(o, RanSetPP(f));
    if l=fail then 
      Info(InfoCitrus, 1, "the partial perm. is not an element of the semigroup");
      return fail;
    fi;
    m:=OrbSCCLookup(o)[l];
    
    CreateOrbSCCSchutzGp(o, m); #JDM replace this with something more sensible
    rep:=f*o!.mults[l]; l:=o!.scc[m][1];
    k:=Position(o, DomPP(f));
    if k=fail or not OrbSCCTruthTable(o)[m][k] then 
      return fail;
    fi;
  else
    rep:=LeftOne(f);
    o:=ShortOrb(s, RanSetPP(rep));
    m:=1; k:=1; l:=1;
  fi;
 
  return CreateRClass(s, [m,k,l], o, rep);
end);

# new for 0.7! - GreensRClassOfElementNC - for D-class and part perm
##############################################################################
# Notes: data is: [scc index, scc[1], pos of dom, pos of ran]

InstallOtherMethod(GreensRClassOfElementNC, "for a D-class and part perm",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup, IsPartialPerm],
function(d, f)
  local o, m, l, rep, k;

  o:=d!.o; m:=d!.data[1];
  l:=Position(o, RanSetPP(f));
  
  if l=fail then 
    return fail;
  fi;

  CreateOrbSCCSchutzGp(o, m); #JDM replace this with something more sensible
  rep:=f*o!.mults[l];
  k:=Position(o, DomPP(f));

  if k=fail or not OrbSCCTruthTable(o)[m][k] then 
    return fail;
  fi;

  return CreateRClass(d!.parent, [m,k,o!.scc[m][1]], o, rep);
end);

#HHH

# new for 0.7! - HClassReps - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(HClassReps, "for an inverse semi of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, scc, mults, r, out, l, m, f, i, j, k;

  o:=LongOrb(s);
  EnumerateInverseSemiData(s);
  scc:=OrbSCC(o);
  mults:=o!.mults;
  r:=Length(scc);
  out:=EmptyPlist(NrHClasses(s));

  if IsPartialPermMonoid(s) then 
    l:=0;
  else
    l:=1;
  fi;

  m:=0;
  for i in [1..r-l] do 
    f:=PartialPermNC(o[scc[i+l][1]], o[scc[i+l][1]]);
    for j in scc[i+l] do 
      for k in scc[i+l] do 
        m:=m+1;
        out[m]:=mults[j]*f/mults[k];  
      od;
    od;
  od;
  return out;
end);

# new for 0.7! - HClassReps - for an D-class of inv semi of partial perms
############################################################################

InstallOtherMethod(HClassReps, "for an D-class of inv semi of partial perms",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local scc, mults, f, out, k, i, j;

  scc:=OrbSCC(d!.o)[d!.data[1]]; 
  mults:=OrbMultipliers(d);
  f:=Representative(d);
  out:=EmptyPlist(Length(scc)^2); 

  k:=0;
  for i in scc do 
    for j in scc do 
      k:=k+1;
      out[k]:=mults[i]*f/mults[j];
    od;
  od;
  return out;
end);

# new for 0.7! - HClassReps - for an L-class of inv semi of partial perms
############################################################################

InstallOtherMethod(HClassReps, "for an L-class of inv semi of partial perms",
[IsGreensLClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(l)
  local scc, mults, f, out, j, i;

  scc:=OrbSCC(l!.o)[l!.data[1]]; 
  mults:=OrbMultipliers(l);
  f:=Representative(l);
  out:=EmptyPlist(Length(scc)); 

  j:=0;
  for i in scc do 
    j:=j+1;
    out[j]:=mults[i]*f;
  od;
  return out;
end);

# new for 0.7! - HClassReps - for an R-class of inv semi of partial perms
############################################################################

InstallOtherMethod(HClassReps, "for an R-class of inv semi of partial perms",
[IsGreensRClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(r)
  local scc, mults, f, out, j, i;

  scc:=OrbSCC(r!.o)[r!.data[1]]; 
  mults:=OrbMultipliers(r);
  f:=Representative(r);
  out:=EmptyPlist(Length(scc)); 

  j:=0;
  for i in scc do 
    j:=j+1;
    out[j]:=f/mults[i];
  od;
  return out;
end);

# new for 0.7! - HClassType - "for a partial perm inverse semigroup"
############################################################################

InstallOtherMethod(HClassType, "for a partial perm inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s);

  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensHClass and
         IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup);
end);

#III

# new for 0.7! - Idempotents - "for a class of a part perm inv semigroup"
##############################################################################

InstallOtherMethod(Idempotents, "for a class of a part perm inv semigroup",
[IsGreensClass and IsGreensClassOfInverseSemigroup and IsGreensClassOfPartPermSemigroup],
function(class)

  if IsGreensRClass(class) then 
    return [LeftOne(Representative(class))];
  elif IsGreensLClass(class) then 
    return [RightOne(Representative(class))];
  elif IsGreensDClass(class) then 
    return List(OrbSCC(class!.o)[class!.data[1]], x-> 
     PartialPermNC(class!.o[x],class!.o[x]));
  elif IsGreensHClass(class) and IsGroupHClass(class) then 
    return [LeftOne(Representative(class))];
  fi;
  return [];
end);

# new for 0.7! - Idempotents - "for a part perm inv semigroup"
##############################################################################

InstallOtherMethod(Idempotents, "for a part perm inv semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, r, l, out, i;
  
  o:=LongOrb(s);
  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;
  r:=Length(o);
  
  if IsPartialPermMonoid(s) then 
    l:=0;
  else
    l:=1;
  fi;

 out:=EmptyPlist(r-l);
 
  for i in [1..r-l] do
    out[i]:=PartialPermNC(o[i+l], o[i+l]);
  od;
  return out;
end);

# new for 0.7! - Idempotents - "for a part perm inv semigroup and pos int"
##############################################################################

InstallOtherMethod(Idempotents, "for a part perm inv semigroup and pos int",
[IsInverseSemigroup and IsPartialPermSemigroup, IsInt],
function(s, i)
  local o, j, out, k;
  
  if i<0 then 
    Error("the argument should be a non-negative integer,");
  fi;

  if i>MaximumList(List(Generators(s), Rank)) then 
    return [];
  fi;

  o:=LongOrb(s); 
  k:=0;

  if IsClosed(o) then 
    out:=EmptyPlist(Length(o));
    for j in o do 
      if Length(j)=i then 
        k:=k+1;
        out[k]:=PartialPermNC(j, j);
      fi;
    od;
    ShrinkAllocationPlist(out);
    return out;
  fi;

  o:=Orb(s, Points(s), OnIntegerSetsWithPP,
        rec(forflatplainlists:=true, hashlen:=CitrusOptionsRec.hashlen.M,
        gradingfunc:=function(o, x) return Length(x); end,
        onlygrades:=function(x, y) return x>=i; end));

  Enumerate(o, infinity);
  out:=EmptyPlist(Length(o));
  for j in [1..Length(o)] do 
    if Grades(o)[j]=i then 
      k:=k+1;
      out[k]:=PartialPermNC(o[j], o[j]);
    fi;
  od;
  ShrinkAllocationPlist(out);
  return out;
end);

# new for 0.7! - IsGroupHClass - "for a part perm inv semigroup"
##############################################################################

InstallOtherMethod(IsGroupHClass, "for H-class of part perm inv semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and 
IsGreensClassOfInverseSemigroup], h-> 
 DomPP(Representative(h))=RanSetPP(Representative(h)));

# new for 0.7! - IsRegularDClass - "for D-class of inv semigroup"
##############################################################################

InstallTrueMethod(IsRegularDClass, IsGreensDClass and 
IsGreensClassOfInverseSemigroup); 

# new for 0.7! - IsRegularLClass - "for L-class of inv semigroup"
##############################################################################

InstallTrueMethod(IsRegularLClass, IsGreensLClass and 
IsGreensClassOfInverseSemigroup); 

# new for 0.7! - IsRegularRClass - "for R-class of inv semigroup"
##############################################################################

InstallTrueMethod(IsRegularRClass, IsGreensRClass and 
IsGreensClassOfInverseSemigroup); 

# new for 0.2! - Iterator - "for a trivial trans. semigp."
#############################################################################
# Notes: required until Enumerator does not call iterator. 

InstallOtherMethod(Iterator, "for a trivial partial perm semigroup",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup and IsTrivial], 9999,
function(s)
  return TrivialIterator(Generators(s)[1]);
end);

# new for 0.1! - Iterator - "for an inverse semigroup"
#############################################################################

InstallMethod(Iterator, "for an inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup], 
function(s)
  local iter;

  iter:= IteratorByFunctions( rec(

    R:=IteratorOfDClasses(s),

    r:=fail, s:=s,

    NextIterator:=function(iter)

      if IsDoneIterator(iter!.R) and IsDoneIterator(iter!.r) then
        return fail;
      fi;

      if iter!.r=fail or IsDoneIterator(iter!.r) then
        iter!.r:=Iterator(NextIterator(iter!.R));
      fi;

      return NextIterator(iter!.r);
    end,

    IsDoneIterator:= iter -> IsDoneIterator(iter!.R) and
     IsDoneIterator(iter!.r),

    ShallowCopy:= iter -> rec(R:=IteratorOfDClasses(s), r:=fail)));

  SetIsIteratorOfSemigroup(iter, true);
  SetIsCitrusPkgIterator(iter, true);

  return iter;
end);

# new for 0.7! - Iterator - "for D-class of inv semigroup"
##############################################################################

InstallMethod(Iterator, "for D-class of inv semigroup",
[IsGreensDClass and IsGreensClassOfInverseSemigroup
and IsGreensClassOfPartPermSemigroup],
function(d)
  local iter;
  if HasAsSSortedList(d) then 
    iter:=IteratorList(AsSSortedList(d));
  else
    iter:=IteratorByFunctions(rec(

      schutz:=List(SchutzenbergerGroup(d), x-> Representative(d)*x),

      at:=[0,1,1],

      m:=Length(OrbSCC(d!.o)[d!.data[1]]), n:=Size(SchutzenbergerGroup(d)),

      IsDoneIterator:=iter-> 
       iter!.at[1]=iter!.m and iter!.at[2]=iter!.n and iter!.at[3]=iter!.m,

      NextIterator:=function(iter)
        local at, scc;

        at:=iter!.at;
        if IsDoneIterator(iter) then 
          return fail;
        fi;

        if at[1]<iter!.m then 
          at[1]:=at[1]+1;
        elif at[3]<iter!.m then 
          at[1]:=1; at[3]:=at[3]+1;
        elif at[2]<iter!.n then 
          at[1]:=1; at[3]:=1; at[2]:=at[2]+1;
        fi;

        scc:=OrbSCC(d!.o)[d!.data[1]];
        return OrbMultipliers(d)[scc[at[1]]]*iter!.schutz[at[2]]/
         OrbMultipliers(d)[scc[at[3]]];
      end,

      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, m:=iter!.m, n:=iter!.n, 
      at:=[0,1,1])));
  fi;

  SetIsIteratorOfDClassElements(iter, true);
  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

# new for 0.7! - Iterator - "for H-class of inv semigroup"
##############################################################################

InstallMethod(Iterator, "for H-class of inv semigroup",
[IsGreensHClass and IsGreensClassOfInverseSemigroup
and IsGreensClassOfPartPermSemigroup],
function(h)
  local iter;
  if HasAsSSortedList(h) then 
    iter:=IteratorList(AsSSortedList(h));
  else
    iter:=IteratorByFunctions(rec(

      schutz:=Enumerator(SchutzenbergerGroup(h)),
      
      pre:=Representative(h)*OrbMultipliers(h)[h!.data[3]],
  
      post:=OrbMultipliers(h)[h!.data[3]]^-1,

      at:=0,

      IsDoneIterator:=iter-> iter!.at=Length(iter!.schutz),
       
      NextIterator:=function(iter)

        if IsDoneIterator(iter) then 
          return fail;
        fi;

        iter!.at:=iter!.at+1;

        return iter!.pre*iter!.schutz[iter!.at]*iter!.post;
      end,

      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, pre:=iter!.pre, 
       post:=iter!.post, at:=0)));
  fi;

  SetIsIteratorOfHClassElements(iter, true);
  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

# new for 0.7! - Iterator - "for L-class of inv semigroup"
##############################################################################

InstallMethod(Iterator, "for L-class of inv semigroup",
[IsGreensLClass and IsGreensClassOfInverseSemigroup
and IsGreensClassOfPartPermSemigroup],
function(d)
  local iter;
  if HasAsSSortedList(d) then 
    iter:=IteratorList(AsSSortedList(d));
  else
    iter:=IteratorByFunctions(rec(

      schutz:=List(SchutzenbergerGroup(d), x-> x*Representative(d)),

      at:=[0,1],

      m:=Length(OrbSCC(d!.o)[d!.data[1]]), n:=Size(SchutzenbergerGroup(d)),

      IsDoneIterator:=iter-> 
       iter!.at[1]=iter!.m and iter!.at[2]=iter!.n,

      NextIterator:=function(iter)
        local at;

        at:=iter!.at;
        if IsDoneIterator(iter) then 
          return fail;
        fi;

        if at[1]<iter!.m then 
          at[1]:=at[1]+1;
        else
          at[1]:=1; at[2]:=at[2]+1;
        fi;

        return OrbMultipliers(d)[OrbSCC(d!.o)[d!.data[1]][at[1]]]
         *iter!.schutz[at[2]];
      end,

      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, m:=iter!.m, n:=iter!.n, 
      at:=[0,1])));
  fi;

  SetIsIteratorOfLClassElements(iter, true);
  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

# new for 0.7! - Iterator - "for R-class of inv semigroup"
##############################################################################

InstallMethod(Iterator, "for R-class of inv semigroup",
[IsGreensRClass and IsGreensClassOfInverseSemigroup
and IsGreensClassOfPartPermSemigroup],
function(d)
  local iter;
  if HasAsSSortedList(d) then 
    iter:=IteratorList(AsSSortedList(d));
  else
    iter:=IteratorByFunctions(rec(

      schutz:=List(SchutzenbergerGroup(d), x-> Representative(d)*x),

      at:=[0,1],

      m:=Length(OrbSCC(d!.o)[d!.data[1]]), n:=Size(SchutzenbergerGroup(d)),

      IsDoneIterator:=iter-> 
       iter!.at[1]=iter!.m and iter!.at[2]=iter!.n,

      NextIterator:=function(iter)
        local at;

        at:=iter!.at;
        if IsDoneIterator(iter) then 
          return fail;
        fi;

        if at[1]<iter!.m then 
          at[1]:=at[1]+1;
        else
          at[1]:=1; at[2]:=at[2]+1;
        fi;

        return iter!.schutz[at[2]]/OrbMultipliers(d)[OrbSCC(d!.o)[d!.data[1]][at[1]]];
      end,

      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, m:=iter!.m, n:=iter!.n, 
      at:=[0,1])));
  fi;

  SetIsIteratorOfRClassElements(iter, true);
  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

# new for 0.7! - IteratorOfDClassRepsData - "for part perm inverse semigroup""
###############################################################################
# JDM this should have a method like IteratorOfRClassRepsData and
# IteratorOfLClassRepsData since the scc:=OrbSCC line makes this work not so
# well!


InstallMethod(IteratorOfDClassRepsData, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup], 
function(s)
  local offset, o, scc;

  if IsPartialPermMonoid(s) then
    offset:=0;
  else
    offset:=1;
  fi; 

  o:=LongOrb(s); scc:=OrbSCC(o);
  
  return IteratorByFunctions( rec(
                 
    o:=o, m:=offset, scc_limit:=Length(scc),

    IsDoneIterator:=iter-> iter!.m=iter!.scc_limit,

    NextIterator:=function(iter)
      local m, o, scc, mults;
      m:=iter!.m; 

      if m=iter!.scc_limit then 
        return fail;
      fi;
     
      o:=iter!.o;   scc:=OrbSCC(o); 
      m:=m+1;       iter!.m:=m;

      mults:=CreateOrbSCCMultipliers(o!.gens, o, m, scc[m]); 
      
      return [s, [m], LongOrb(s),
       PartialPermNC(o[scc[m][1]], o[scc[m][1]])];
    end,

    ShallowCopy:=iter-> rec(o:=iter!.o, m:=iter!.m, at:=[0,1],
    scc_limit:=iter!.scc_limit, at_limit:=iter!.at_limit)));
end);

# new for 0.7! - IteratorOfDClassReps - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfDClassReps, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  if HasDClassReps(s) then 
    return IteratorList(DClassReps(s));
  fi;
  return IteratorByIterator(IteratorOfDClassRepsData(s), x-> x[4],
   [IsIteratorOfDClassReps]);
end);

# new for 0.7! - IteratorOfHClasses - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfDClasses, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  if HasGreensDClasses(s) then 
    return IteratorList(GreensDClasses(s));
  fi;
  return IteratorByIterator(IteratorOfDClassRepsData(s), x->
   CallFuncList(CreateDClass, x), [IsIteratorOfDClasses]);
end);


# new for 0.7! - IteratorOfHClassRepsData - "for part perm inverse semigroup""
###############################################################################
# JDM this should have a method like IteratorOfRClassRepsData and
# IteratorOfLClassRepsData since the scc:=OrbSCC line makes this work not so
# well!

InstallMethod(IteratorOfHClassRepsData, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup], 
function(s)
  local offset, o, scc;

  if IsPartialPermMonoid(s) then
    offset:=0;
  else
    offset:=1;
  fi; 

  o:=LongOrb(s); scc:=OrbSCC(o);
  
  return IteratorByFunctions( rec(
                 
    o:=o, m:=offset+1, at:=[1,0], scc_limit:=Length(scc),
    at_limit:=[Length(scc[Length(scc)]), Length(scc[Length(scc)])],

    IsDoneIterator:=iter-> iter!.m=iter!.scc_limit and 
     iter!.at=iter!.at_limit,

    NextIterator:=function(iter)
      local o, scc, at, m, mults;

      m:=iter!.m; at:=iter!.at;

      if m=iter!.scc_limit and at=iter!.at_limit then 
        return fail;
      fi;
     
      o:=iter!.o; scc:=OrbSCC(o); 
      if at[2]<Length(scc[m]) then 
        at[2]:=at[2]+1;
      elif at[1]<Length(scc[m]) then 
        at[1]:=at[1]+1;
        at[2]:=1;
      else
        at:=[1,1];
        m:=m+1;
      fi;

      iter!.at:=at; iter!.m:=m;

      mults:=CreateOrbSCCMultipliers(o!.gens, o, m, scc[m]); 
      
      return [s, [m, scc[m][at[1]], scc[m][at[2]]], LongOrb(s),
       mults[scc[m][at[1]]]*PartialPermNC(o[scc[m][1]],
       o[scc[m][1]])*mults[scc[m][at[2]]]^-1];
    end,

    ShallowCopy:=iter-> rec(o:=iter!.o, m:=iter!.m, at:=[0,1],
    scc_limit:=iter!.scc_limit, at_limit:=iter!.at_limit)));
end);

# new for 0.7! - IteratorOfHClassReps - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfHClassReps, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  if HasHClassReps(s) then 
    return IteratorList(HClassReps(s));
  fi;
  return IteratorByIterator(IteratorOfHClassRepsData(s), x-> x[4],
   [IsIteratorOfHClassReps]);
end);

# new for 0.7! - IteratorOfHClasses - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfHClasses, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  if HasGreensHClasses(s) then 
    return IteratorList(GreensHClasses(s));
  fi;
  return IteratorByIterator(IteratorOfHClassRepsData(s), x->
   CallFuncList(CreateHClass, x), [IsIteratorOfHClasses]);
end);

# new for 0.7! - IteratorOfLClassRepsData - "for part perm inverse semigroup""
###############################################################################

InstallMethod(IteratorOfLClassRepsData, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup], 
function(s)
  local offset, iter, o, scc;

  if IsPartialPermMonoid(s) then
    offset:=0;
  else
    offset:=1;
  fi; 

  if not IsClosed(LongOrb(s)) then 
    
    iter:=IteratorByFunctions( rec(
      
      o:=LongOrb(s), 

      i:=offset,

      IsDoneIterator:=iter-> IsClosed(iter!.o) and iter!.i>=Length(iter!.o),

      NextIterator:=function(iter)
        local i, o, r;
        
        o:=iter!.o; i:=iter!.i;

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
  
        return [s, [1,1,1], ShortOrb(s, o[i]), PartialPermNC(o[i], o[i])];
      end,

      ShallowCopy:=iter-> rec(o:=iter!.o, i:=iter!.i)));
  else ####

    o:=LongOrb(s); scc:=OrbSCC(o);

    iter:=IteratorByFunctions( rec(
                 
      o:=o,

      m:=offset+1, i:=0,      

      scc_limit:=Length(scc),
      i_limit:=Length(scc[Length(scc)]),

      IsDoneIterator:=iter-> iter!.m=iter!.scc_limit and 
       iter!.i=iter!.i_limit,

      NextIterator:=function(iter)
        local i, m, o, scc, mults;

        i:=iter!.i; m:=iter!.m; 
        if i=iter!.i_limit and m=iter!.scc_limit then
          return fail; 
        fi;

        o:=iter!.o; scc:=OrbSCC(o);
        if i<Length(scc[m]) then 
          i:=i+1;
        else
          i:=1; m:=m+1;
        fi;
        iter!.i:=i; iter!.m:=m;
  
        mults:=CreateOrbSCCMultipliers(o!.gens, o, m, scc[m]); 
        return [s, [m, scc[m][1], scc[m][i]], LongOrb(s),
         RestrictedPP(mults[scc[m][i]]^-1, o[scc[m][1]])];
      end,

      ShallowCopy:=iter-> rec(o:=iter!.o, i:=iter!.i, m:=iter!.m,
       scc_limit:=iter!.scc_limit, i_limit:=iter!.i_limit)));
  fi;
  
  return iter;
end);

# new for 0.7! - IteratorOfLClassReps - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfLClassReps, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
s-> IteratorByIterator(IteratorOfLClassRepsData(s), x-> x[4],
[IsIteratorOfLClassReps]));

# new for 0.7! - IteratorOfLClasses - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfLClasses, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
s-> IteratorByIterator(IteratorOfLClassRepsData(s), x->
CallFuncList(CreateLClass, x), [IsIteratorOfLClasses]));

# new for 0.7! - IteratorOfRClassRepsData - "for part perm inverse semigroup""
###############################################################################

InstallMethod(IteratorOfRClassRepsData, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup], 
function(s)
  local offset, iter, o, scc;

  if IsPartialPermMonoid(s) then
    offset:=0;
  else
    offset:=1;
  fi; 

  if not IsClosed(LongOrb(s)) then 
    
    iter:=IteratorByFunctions( rec(
      
      o:=LongOrb(s), 

      i:=offset,

      IsDoneIterator:=iter-> IsClosed(iter!.o) and iter!.i>=Length(iter!.o),

      NextIterator:=function(iter)
        local i, o, r;
        
        o:=iter!.o; i:=iter!.i;

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
  
        return [s, [1,1,1], ShortOrb(s, o[i]), PartialPermNC(o[i], o[i])];
      end,

      ShallowCopy:=iter-> rec(o:=iter!.o, i:=iter!.i)));
  else ####

    o:=LongOrb(s); scc:=OrbSCC(o);

    iter:=IteratorByFunctions( rec(
                 
      o:=o,

      m:=offset+1, i:=0,      

      scc_limit:=Length(scc),
      i_limit:=Length(scc[Length(scc)]),

      IsDoneIterator:=iter-> iter!.m=iter!.scc_limit and 
       iter!.i=iter!.i_limit,

      NextIterator:=function(iter)
        local i, o, m, scc, f, r, mults;
        
        i:=iter!.i; m:=iter!.m; 
        if i=iter!.i_limit and m=iter!.scc_limit then
          return fail; 
        fi;

        o:=iter!.o; scc:=OrbSCC(o);
        if i<Length(scc[m]) then 
          i:=i+1;
        else
          i:=1; m:=m+1;
        fi;
        iter!.i:=i; iter!.m:=m;
  
        mults:=CreateOrbSCCMultipliers(o!.gens, o, m, scc[m]); 
        return [s, [m, scc[m][i], scc[m][1]], LongOrb(s),
         RestrictedPP(mults[scc[m][i]], o[scc[m][i]])];
      end,

      ShallowCopy:=iter-> rec(o:=iter!.o, i:=iter!.i, m:=iter!.m,
       scc_limit:=iter!.scc_limit, i_limit:=iter!.i_limit)));
  fi;
  
  return iter;
end);

# new for 0.7! - IteratorOfRClassReps - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfRClassReps, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
s-> IteratorByIterator(IteratorOfRClassRepsData(s), x-> x[4],
[IsIteratorOfRClassReps]));

# new for 0.7! - IteratorOfRClasses - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfRClasses, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
s-> IteratorByIterator(IteratorOfRClassRepsData(s), x->
CallFuncList(CreateRClass, x), [IsIteratorOfRClasses]));

#LLL

# new for 0.7! - LClassOfHClass - "for an H-class of an inverse semigrp"
#############################################################################

InstallMethod(LClassOfHClass, "for an H-class of an inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(h)
  local o, scc, data, mults;

  o:=h!.o; scc:=OrbSCC(o); data:=h!.data;
  mults:=OrbMultipliers(h);

  return CreateLClass(h!.parent, [data[1], scc[data[1]][1], data[3]], h!.o, 
   mults[data[2]]^-1*Representative(h));
end);

# new for 0.7! - LClassReps - for an inv semi of part perms
##############################################################################

InstallOtherMethod(LClassReps, "for an inv semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, scc, out, gens, i, l, mults, f, j, k;

  o:=LongOrb(s);
  scc:=OrbSCC(o);
  out:=EmptyPlist(Length(o));
  gens:=o!.gens;

  if IsPartialPermMonoid(s) then
    i:=0;
  else
    i:=1;
  fi;

  l:=0;
  for j in [1+i..Length(scc)] do
    mults:=CreateOrbSCCMultipliers(gens, o, j, scc[j]);
    f:=PartialPermNC(o[scc[j][1]], o[scc[j][1]]);
    for k in scc[j] do
      l:=l+1;
      out[l]:=f/mults[k];
    od;
  od;
  return out;
end);

# new for 0.7! - LClassReps - for D-class of part perm inv semi
##############################################################################

InstallOtherMethod(LClassReps, "for D-class of part perm inv semi",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(d)
  local scc, out, rep, mults, i, j;

  scc:=OrbSCC(d!.o)[d!.data[1]];
  out:=EmptyPlist(Length(scc));
  rep:=Representative(d);
  mults:=OrbMultipliers(d);

  i:=0;
  for j in scc do 
    i:=i+1;
    out[i]:=rep/mults[j];
  od;
  return out;
end);

# new for 0.7! - LClassType - "for a partial perm inverse semigroup"
############################################################################

InstallOtherMethod(LClassType, "for a partial perm inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s);

  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensLClass and
         IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup);
end);

# new for 0.7! - LongOrb - "for an inverse semi of part. perms"
##############################################################################
# seed is required for finding a small generating set. If the seed is not
# specified and the second generator acts on some point that the first does
# not, then the resulting LongOrb does not properly relate to the semigroup. 

InstallMethod(LongOrb, "for an inverse semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)

  return Orb(s, Points(s), OnIntegerSetsWithPP, 
        rec(forflatplainlists:=true, schreier:=true, orbitgraph:=true, 
        storenumbers:=true, log:=true, hashlen:=CitrusOptionsRec.hashlen.M, 
        finished:=false));
end);

#NNN

# new for 0.7! - NaturalPartialOrder - "for an inverse semigroup"
##############################################################################
# C function for me!

InstallMethod(NaturalPartialOrder, "for an inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  local elts, n, out, i, j;
    
  elts:=Elements(s);
  n:=Length(elts);
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

# new for 0.7! - NrRClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrRClasses, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  if not IsClosed(LongOrb(s)) then 
    Enumerate(LongOrb(s));
  fi;

  if IsPartialPermMonoid(s) then 
    return Length(LongOrb(s));
  fi;

  return Length(LongOrb(s))-1;
end); 

# new for 0.7! - NrRClasses - for D-class of inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrRClasses, "for D-class of semigp of partial perms",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(d)
  return Length(OrbSCC(d!.o)[d!.data[1]]);
end); 

# new for 0.7! - NrLClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrLClasses, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup], NrRClasses);

# new for 0.7! - NrLClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrLClasses, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup], NrRClasses);

# new for 0.7! - NrLClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrLClasses, "for D-class of semigp of partial perms",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and                        IsGreensClassOfInverseSemigroup], NrRClasses);

# new for 0.7! - NrDClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrDClasses, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  if not IsClosed(LongOrb(s)) then 
    Enumerate(LongOrb(s));
  fi;
  if IsPartialPermMonoid(s) then 
    return Length(OrbSCC(LongOrb(s)));
  fi;

  return Length(OrbSCC(LongOrb(s)))-1;
end); 

# new for 0.7! - NrRegularDClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrRegularDClasses, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup], NrDClasses);

# new for 0.7! - NrHClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrHClasses, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local scc;
  if not IsClosed(LongOrb(s)) then 
    Enumerate(LongOrb(s));
  fi;
  
  scc:=OrbSCC(LongOrb(s));

  if IsPartialPermMonoid(s) then 
    return Sum(List(scc, m-> Length(m)^2));
  fi;

  return Sum(List(scc, m-> Length(m)^2))-1;
end); 

# new for 0.7! - NrHClasses - for a D-class of inv semigroup of partial perms
##############################################################################

InstallOtherMethod(NrHClasses, "for a D-class of inv semi. of partial perm",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup], d-> Length(OrbSCC(d!.o)[d!.data[1]])^2);

# new for 0.7! - NrHClasses - for a L-class of inv semigroup of partial perms
##############################################################################

InstallOtherMethod(NrHClasses, "for a L-class of inv semi. of partial perm",
[IsGreensLClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup], l-> Length(OrbSCC(l!.o)[l!.data[1]]));

# new for 0.7! - NrHClasses - for a R-class of inv semigroup of partial perms
##############################################################################

InstallOtherMethod(NrHClasses, "for a R-class of inv semi. of partial perm",
[IsGreensRClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup], r-> Length(OrbSCC(r!.o)[r!.data[1]]));

# new for 0.7! - NrIdempotents - for an inverse semigroup of partial perms
##############################################################################

InstallMethod(NrIdempotents, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  if not IsClosed(LongOrb(s)) then 
    Enumerate(LongOrb(s), infinity);
  fi;
  
  if IsPartialPermMonoid(s) then 
    return Length(LongOrb(s));
  fi;

  return Length(LongOrb(s))-1;
end); 

# new for 0.7! - NrIdempotents - for an H-class of an inv semi
##############################################################################

InstallOtherMethod(NrIdempotents, "for an H-class of an inverse semi",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup],
function(h)
  local f;
  
  f:=Representative(h);
  if DomPP(f)=RanSetPP(f) then 
    return 1;
  fi;

  return 0;
end);

# new for 0.7! - NrIdempotents - for a D- class of an inv semi
##############################################################################

InstallOtherMethod(NrIdempotents, "for an D-class of an inverse semi",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup], NrRClasses);

# new for 0.7! - NrIdempotents - for an L-class of an inv semi
##############################################################################

InstallOtherMethod(NrIdempotents, "for an L-class of an inverse semi",
[IsGreensLClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup], x-> 1);

# new for 0.7! - NrIdempotents - for an R-class of an inv semis
##############################################################################

InstallOtherMethod(NrIdempotents, "for an R-class of an inverse semi",
[IsGreensRClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup], x-> 1);

#OOO

# new for 0.7! - One - for a partial perm semigroup
##############################################################################

InstallMethod(OneImmutable, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> PartialPermNC(Points(s), Points(s)));

# new for 0.7! - OrbMultipliers - for a Green's class of a part perm inv semi
##############################################################################
# partial perm that maps o[scc[i]] to o[scc[1]] 

InstallMethod(OrbMultipliers, "for a class of a part perm inv semi",
[IsGreensClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup],
function(C)
  return CreateOrbSCCMultipliers(C!.o!.gens, C!.o, C!.data[1], 
   OrbSCC(C!.o)[C!.data[1]]);
end);

# new for 0.7! - OrbSCCStabChain 
###############################################################################

InstallMethod(OrbSCCStabChain, "for a class of a part perm inv semi",
[IsGreensClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup],
function(C)
  return CreateOrbSCCSchutzGp(C!.o, C!.data[1], Representative(C))[1];
end);

#PPP

# new for 0.7! - PartialOrderOfDClasses - "for a partial perm inv semigroup"
##############################################################################

InstallMethod(PartialOrderOfDClasses, "for an inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local d, n, out, gens, o, lookup, offset, i, x, f;
  
  d:=GreensDClasses(s);
  n:=Length(d);
  out:=List([1..n], x-> EmptyPlist(n));
  o:=LongOrb(s);
  gens:=o!.gens;
  lookup:=OrbSCCLookup(o);

  if IsPartialPermMonoid(s) then 
    offset:=0;
  else
    offset:=1;
  fi;

  for i in [1..n] do 
    for x in gens do 
      for f in RClassReps(d[i]) do 
        AddSet(out[i], lookup[Position(o, DomPP(x*f))]-offset);
        AddSet(out[i], lookup[Position(o, RanSetPP(f^-1*x))]-offset);
      od;
    od;
  od;

  Perform(out, ShrinkAllocationPlist);
  return out;
end);

# new for 0.7! - ParentAttr - "for a Green's class of a part perm semigroup
##############################################################################

InstallMethod(ParentAttr, "for a R-class of inverse semigroup",
[IsGreensClass and IsGreensClassOfPartPermSemigroup], x-> x!.parent);

# new for 0.7! - Points - "for a partial perm semigroup"
##############################################################################

InstallMethod(Points, "for a partial perm semigroup",
[IsPartialPermSemigroup], s-> Union(List(GeneratorsOfSemigroup(s), DomPP)));

# new for 0.7! - Points - "for a partial perm collection"
##############################################################################

InstallOtherMethod(Points, "for a partial perm coll",
[IsPartialPermCollection], coll-> Union(List(coll, DomPP)));

#RRR

# new for 0.7! - Random - "for a part. perm. inv. semigroup (citrus pkg)"
#############################################################################

InstallMethod(Random, "for a part perm inv semigroup (citrus pkg)",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  local o, gens, i, w, k, m, l, g;

  o:=LongOrb(s);

  if not IsClosed(o) then  
    gens:=GeneratorsOfSemigroup(s);
    i:=Random([1..Int(Length(gens)/2)]);
    w:=List([1..i], x-> Random([1..Length(gens)]));
    return EvaluateWord(gens, w);
  else
    k:=Random([1..Length(o)]);
    m:=OrbSCCLookup(o)[k];
    l:=Random(OrbSCC(o)[m]);
    g:=Random(CreateOrbSCCSchutzGp(o,m)[2]);
    return o!.mults[k]*g/o!.mults[l];
  fi;
end);

# new for 0.7! - RClassOfHClass - "for an H-class of an inverse semigrp"
#############################################################################

InstallMethod(RClassOfHClass, "for an H-class of an inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(h)
  local o, scc, data, mults;

  o:=h!.o; scc:=OrbSCC(o); data:=h!.data;
  mults:=OrbMultipliers(h);

  return CreateRClass(h!.parent, [data[1], data[2], scc[data[1]][1]], h!.o, 
   Representative(h)*mults[data[3]]);
end);

# new for 0.7! - RClassReps - for an inv semi of part perms
##############################################################################

InstallOtherMethod(RClassReps, "for an inv semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, scc, out, gens, i, l, mults, f, j, k;

  o:=LongOrb(s);
  scc:=OrbSCC(o);
  out:=EmptyPlist(Length(o));
  gens:=o!.gens;

  if IsPartialPermMonoid(s) then 
    i:=0;
  else
    i:=1;
  fi;

  l:=0;
  for j in [1+i..Length(scc)] do
    mults:=CreateOrbSCCMultipliers(gens, o, j, scc[j]);
    f:=PartialPermNC(o[scc[j][1]], o[scc[j][1]]);
    for k in scc[j] do 
      l:=l+1; 
      out[l]:=mults[k]*f;
    od;
  od;
  return out;
end);

# new for 0.7! - RClassReps - for D-class of part perm inv semi
##############################################################################

InstallOtherMethod(RClassReps, "for D-class of part perm inv semi",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(d)
  local scc, out, rep, mults, i, j;

  scc:=OrbSCC(d!.o)[d!.data[1]];
  out:=EmptyPlist(Length(scc));
  rep:=Representative(d);
  mults:=OrbMultipliers(d);

  i:=0;
  for j in scc do 
    i:=i+1;
    out[i]:=mults[j]*rep;
  od;
  return out;
end);

# new for 0.7! - RClassType - "for a partial perm inverse semigroup"
############################################################################

InstallOtherMethod(RClassType, "for a partial perm inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensRClass and
         IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup);
end);

#SSS

# new for 0.7! - SchutzenbergerGroup - "for Green's class of inverse semigroup"
##############################################################################

InstallMethod(SchutzenbergerGroup, "for Green's class of inverse semigroup",
[IsGreensClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup], 
function(C)
  return CreateOrbSCCSchutzGp(C!.o, C!.data[1], Representative(C))[2];
end);

# new for 0.7! - SchutzenbergerGroup - "for Green's L-class of inv semigroup"
##############################################################################
# JDM why is this and the next method necessary?

InstallMethod(SchutzenbergerGroup, "for Green's class of inverse semigroup",
[IsGreensLClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup], 
function(C)
  return CreateOrbSCCSchutzGp(C!.o, C!.data[1], Representative(C)^-1)[2];
end);

# new for 0.7! - SchutzenbergerGroup - "for an H-class of inverse semigroup"
##############################################################################

InstallMethod(SchutzenbergerGroup, "for an H-class of an inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup], 
function(h)
  local o, m, scc, s, rep;
 
  o:=h!.o;
  m:=h!.data[1];
  scc:=OrbSCC(o)[m];
  s:=h!.parent;

  CreateOrbSCCMultipliers(o!.gens, o, m, scc);

  if not IsBound(o!.schutz) then
    o!.schutz:=EmptyPlist(Length(scc));
  fi;
 
  if not IsBound(o!.schutz[m]) then
    rep:=o!.mults[h!.data[2]]^-1*Representative(h)*o!.mults[h!.data[3]]; #LQuoPP
    o!.schutz[m]:=CreateOrbSCCSchutzGpNC(o!.gens, o, rep, scc, o!.truth[m],
     OrbitGraph(o), Length(o!.gens), o!.mults);
  fi;

  return o!.schutz[m][2]; 
end);

# new for 0.7! - ShortOrb - "for an inverse semigp of partial perms"
##############################################################################

InstallGlobalFunction(ShortOrb, 
function(s, set)

  return Orb(s, set, OnIntegerSetsWithPP, 
      rec(forflatplainlists:=true,
        hashlen:=CitrusOptionsRec.hashlen.M,
        schreier:=true,
        gradingfunc := function(o,x) return Length(x); end,
        orbitgraph := true,
        onlygrades:= function(x,y) return x=Length(set); end,
        storenumbers:=true,
        log:=true)); 
end);

# new for 0.7! - Size - for an D-class of an inverse semigroup
##############################################################################

InstallMethod(Size, "for an D-class of an inverse semigroup",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(d)
  return Length(OrbSCC(d!.o)[d!.data[1]])^2*Size(SchutzenbergerGroup(d));
end);

# new for 0.7! - Size - for an H-class of an inverse semigroup
##############################################################################

InstallMethod(Size, "for an H-class of an inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(h)
  return Size(SchutzenbergerGroup(h));
end);

# new for 0.7! - Size - for an L-class of an inverse semigroup
##############################################################################

InstallMethod(Size, "for an L-class of an inverse semigroup",
[IsGreensLClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(l)
  return Length(OrbSCC(l!.o)[l!.data[1]])*Size(SchutzenbergerGroup(l));
end);

# new for 0.7! - Size - for an R-class of an inverse semigroup
##############################################################################

InstallMethod(Size, "for an R-class of an inverse semigroup",
[IsGreensRClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(r)
  return Length(OrbSCC(r!.o)[r!.data[1]])*Size(SchutzenbergerGroup(r));
end);

# new for 0.7! - Size - for an inverse semigroup of partial perms
##############################################################################

InstallMethod(Size, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  EnumerateInverseSemiData(s);
  return Size(s);
end); 

# new for 0.7! - StructureDescription - for a group H-class of inv semi
##############################################################################

InstallOtherMethod(StructureDescription, "for group H-class of inv semi",
[IsGroupHClass and IsGreensClassOfPartPermSemigroup], 
h-> StructureDescription(Range(IsomorphismPermGroup(h))));

#EOF



