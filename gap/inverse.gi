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
[IsPartialPerm and IsPartialPermRep, IsInverseSemigroup and IsPartialPermSemigroup],
function(f, s)
  local o, k, l, ran, m, schutz, g;

  if not IsEmptyPartialPerm(f) and (MinDomainRange(f)<SmallestMovedPoint(s) or
   MaxDomainRange(f)>LargestMovedPoint(s)) then 
    return false;
  fi;

  o:=RangesOrb(s);

  if IsClosed(o) then 
    k:=Position(o, Dom(f));
    if k=fail then 
      return false;
    fi;
    l:=Position(o, RangeSetOfPartialPerm(f));
    if l=fail then 
      return false;
    fi;
  else
    k:=Position(o, Dom(f));
    if k=fail then 
      o!.looking:=true; o!.lookingfor:=function(o, x) return x=Dom(f); end;
      o!.lookfunc:=o!.lookingfor;

      Enumerate(o);
      k:=PositionOfFound(o);
      o!.found:=false; o!.looking:=false; 
      Unbind(o!.lookingfor); Unbind(o!.lookfunc);
      
      if k=false then
        return false;
      fi;
    fi;

    if Dom(f)=[] then 
      return true;
    fi;
    ran:=RangeSetOfPartialPerm(f);
       
    if IsClosed(o) and Position(o, ran)=fail then 
      return false;
    fi;
    # JDM could look ahead here...

    k:=1;
    o:=ShortOrb(s, Dom(f));
    Enumerate(o);
    
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
  
  schutz:=RangeOrbStabChain(o, m);
  
  if schutz=true then 
    return true;
  fi;

  g:=o!.mults[k]^-1*f*o!.mults[l];

  if Dom(g)=Ran(g) then # g is an idempotent! 
    return true;
  elif schutz=false then
    return false;
  fi;

  return SiftedPermutation(schutz, g);
end);

# new for 0.7! - \in - "for an R-class of inv semi and part perm" 
#############################################################################

InstallMethod(\in, "for an R-class of inv semi and part perm",
[IsPartialPerm and IsPartialPermRep, IsGreensRClass and 
IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(f, r)
  local rep, o, l, m, schutz, g;
  
  rep:=Representative(r);

  if Degree(f)<>Degree(rep) or MinDomain(f)<>MinDomain(rep) or 
   Rank(f)<>Rank(rep) or Dom(f)<>Dom(rep) then 
    Info(InfoCitrus, 1, "degree, rank, or domain not equal to those of",
        " any of the R-class elements,");
    return false;
  fi;

  o:=r!.o;
  Enumerate(o);
  l:=Position(o, RangeSetOfPartialPerm(f));
  m:=r!.data[1];

  if l=fail or not OrbSCCTruthTable(o)[m][l] then 
    Info(InfoCitrus, 1, "range not equal to that of any R-class element,");
    return false;
  fi;

  schutz:=RangeOrbStabChain(o, m, rep); 

  if schutz=true then 
    return true;
  fi;

  g:=f*o!.mults[l];

  if g=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;

  return SiftedPermutation(schutz, AsPermutation(g))=(); 
end);

# new for 0.7! - \in - "for an L-class of inv semi and part perm" 
#############################################################################

InstallMethod(\in, "for an L-class of inv semi and part perm",
[IsPartialPerm and IsPartialPermRep, IsGreensLClass and 
IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(f, r)
  local rep, o, l, m, schutz, g;
  
  rep:=Representative(r);

  if MinRange(f)<>MinRange(rep) or MaxRange(f)<>MaxRange(rep) or 
   Rank(f)<>Rank(rep) or RangeSetOfPartialPerm(f)<>RangeSetOfPartialPerm(rep) 
   then 
    Info(InfoCitrus, 1, "degree, rank, or range not equal to those of",
        " any of the L-class elements,");
    return false;
  fi;

  o:=r!.o;
  Enumerate(o);
  l:=Position(o, Dom(f));
  m:=r!.data[1];

  if l=fail or not OrbSCCTruthTable(o)[m][l] then 
    Info(InfoCitrus, 1, "range not equal to that of any L-class element,");
    return false;
  fi;

  schutz:=RangeOrbStabChain(o, m); 

  if schutz=true then 
    return true;
  fi;

  g:=o!.mults[l]^-1*f;

  if g=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;

  return SiftedPermutation(schutz, AsPermutation(g))=(); 
end);

# new for 0.7! - \in - "for an D-class of inv semi and part perm" 
#############################################################################

InstallMethod(\in, "for an D-class of inv semi and part perm",
[IsPartialPerm and IsPartialPermRep, IsGreensDClass and 
IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(f, r)
  local rep, o, m, l_dom, l_ran, schutz, g;
  
  rep:=Representative(r);

  if Rank(f)<>Rank(rep) then
    Info(InfoCitrus, 1, "rank not equal to those of",
        " any of the D-class elements,");
    return false;
  fi;

  o:=r!.o; 
  Enumerate(o);
  m:=r!.data[1];
  
  l_dom:=Position(o, Dom(f)); l_ran:=Position(o, RangeSetOfPartialPerm(f));
  
  if l_dom=fail or l_ran=fail or not OrbSCCTruthTable(o)[m][l_dom] or 
   not OrbSCCTruthTable(o)[m][l_ran] then 
    Info(InfoCitrus, 1, "range or domain not equal to that of any D-class",
     " element,");
    return false;
  fi;

  schutz:=RangeOrbStabChain(o, m, rep); 

  if schutz=true then 
    return true;
  fi;

  g:=o!.mults[l_dom]^-1*f*o!.mults[l_ran];

  if g=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;

  return SiftedPermutation(schutz, AsPermutation(g))=(); 
end);

# new for 0.7! - \in - "for an H-class of inv semi and part perm" 
############################################################################

InstallMethod(\in, "for an H-class of inv semi and part perm",
[IsPartialPerm and IsPartialPermRep, IsGreensHClass and 
IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(f, r)
  local rep, o, data, mults, schutz, g;
  
  rep:=Representative(r);

  if Degree(f)<>Degree(rep) or Rank(f)<>Rank(rep) or Dom(f)<>Dom(rep) or 
   RangeSetOfPartialPerm(f)<>RangeSetOfPartialPerm(rep) then
    return false;
  fi;

  o:=r!.o; data:=r!.data; mults:=o!.mults;
  
  schutz:=RangeOrbStabChain(o, data[1]); 

  if schutz=true then 
    return true;
  fi;

  g:=mults[data[3]]^-1*f*mults[data[4]];

  if g=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;

  return SiftedPermutation(schutz, AsPermutation(g))=(); 
end);

#AAA

# new for 0.7! - AsList - not a user function 
############################################################################

InstallOtherMethod(AsList, "for an R-class of trans. semigp.",
[IsGreensRClass and IsGreensClassOfInverseSemigroup and IsGreensClassOfPartPermSemigroup],
function(r)
  local f, g, elts, mults, scc, i;

  f:=Representative(r); 
  g:=List(SchutzenbergerGroup(r), x-> f*x);
  elts:=EmptyPlist(Size(r));

  mults:=r!.o!.mults; scc:=r!.o!.scc[r!.data[1]];

  for i in scc do
    Append(elts, g*mults[i]^-1);
  od;
  return elts;
end);

# new for 0.7! - AsList - not a user function 
############################################################################

InstallOtherMethod(AsList, "for an L-class of trans. semigp.",
[IsGreensLClass and IsGreensClassOfInverseSemigroup and IsGreensClassOfPartPermSemigroup],
function(r)
  local f, g, elts, mults, scc, i;

  f:=Representative(r); 
  g:=List(SchutzenbergerGroup(r), x-> x*f);
  elts:=EmptyPlist(Size(r));

  mults:=r!.o!.mults; scc:=r!.o!.scc[r!.data[1]];

  for i in scc do
    Append(elts, mults[i]*g);
  od;
  return elts;
end);

# new for 0.7! - AsList - not a user function 
############################################################################

InstallOtherMethod(AsList, "for an H-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfInverseSemigroup and IsGreensClassOfPartPermSemigroup],
function(h)
  local schutz, mults, data, f, g;
  
  schutz:=SchutzenbergerGroup(h);
  mults:=h!.o!.mults; data:=h!.data;
  f:=mults[data[3]]; g:=mults[data[4]]^-1; 

  return List(schutz, x-> f*x*g);
end);

# new for 0.7! - AsList - not a user function 
############################################################################

InstallOtherMethod(AsList, "for an D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and IsGreensClassOfPartPermSemigroup],
function(r)
  local f, g, elts, mults, scc, i, j, k;

  f:=Representative(r); 
  g:=List(SchutzenbergerGroup(r), x-> f*x);
  elts:=EmptyPlist(Size(r));

  mults:=r!.o!.mults; scc:=r!.o!.scc[r!.data[1]];

  k:=0;
  for i in scc do
    for j in scc do 
      k:=k+1;
      elts[k]:=mults[j]*g*mults[i]^-1;
    od;
  od;
  return elts;
end);

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

# new for 0.7! - CreateSCCMultipliers - not a user function 
#############################################################################

InstallGlobalFunction(CreateSCCMultipliers, 
function(gens, o, j, scc, mults)
  local w, i;
  
  for i in scc do 
    w:=TraceSchreierTreeOfSCCForward(o, j, i);
    if w=[] then 
      mults[i]:=PartialPermNC(o[i], o[i]);
    else
      mults[i]:=EvaluateWord(gens, w)^-1;
    fi;    
  od;
  return mults;
end);

# new for 0.7! - CreateSchutzGp - not a user function
#############################################################################
# Usage: o = orbits of images; k = scc index; scc = scc; 
# truth = o!.truth[k]; graph:=OrbitGraph(o); gens = GeneratorsOfSemigroup;
# r = Length(gens);

InstallGlobalFunction(CreateSchutzGp, 
function(gens, o, f, scc, truth, graph, r, p)
  local bound, g, is_sym, i, j;
 
  if Length(o[scc[1]])<1000 then
    bound:=Factorial(Length(o[scc[1]]));
  else
    bound:=infinity;
  fi;

  g:=Group(()); is_sym:=false; 

  for i in scc do
    for j in [1..r] do
      if IsBound(graph[i][j]) and truth[graph[i][j]] then
        g:=ClosureGroup(g, AsPermutation(f^-1*f/p[i] * 
         (gens[j]*p[graph[i][j]])));
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
    return [true, g ];
  elif Size(g)=1 then
    return [false, g ];
  fi;

  return [StabChainImmutable(g), g];
end);

#DDD

# new for 0.7! - DClassReps - "for an inverse semi of part perms"
##############################################################################

InstallOtherMethod(DClassReps, "for an inverse semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, scc, r, i, gens, out, j;
  
  o:=RangesOrb(s);
  scc:=OrbSCC(o); 
  r:=Length(scc);
  
  if IsPartialPermMonoid(s) then 
    i:=0;
  else
    i:=1;
  fi;

  gens:=GeneratorsOfSemigroup(s);
  out:=EmptyPlist(r);
  
  for j in [1..r-i] do 
    out[j]:=EvaluateWord(gens, TraceSchreierTreeForward(o, scc[j+i][1]));
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

# new for 0.7! - EnumerateRangesOrb - "for an inverse semi of part perms"
##############################################################################

InstallGlobalFunction(EnumerateRangesOrb, 
function(s)
  local o, scc, r, mults, schutz, graph, truth, gens, w, f, modifier, i, j;
  
  o:=RangesOrb(s);
  
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
    graph:=OrbitGraph(o);
    truth:=OrbSCCTruthTable(o);
    gens:=GeneratorsOfSemigroup(s);

    for i in [1..r] do 
      for j in scc[i] do
        w:=TraceSchreierTreeOfSCCForward(o, i, j);
        if w=[] then 
          mults[j]:=PartialPermNC(o[j], o[j]);
        else
          mults[j]:=EvaluateWord(gens, w)^-1;
        fi;
      od;
      w:=TraceSchreierTreeForward(o, scc[i][1]);
      if w=[] then 
        f:=PartialPermNC(o[scc[i][1]], o[scc[i][1]]);
      else
        f:=EvaluateWord(gens, w);
      fi;
      schutz[i]:=CreateSchutzGp(gens, o, f, scc[i], 
         truth[i], graph, r, mults);
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
      return enum[pos[2]]*RangeOrbMults(r)[RangeOrbSCC(r)[pos[1]]]^-1;
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local rep, o, data, i, j;

      rep:=Representative(r);
      
      if Degree(f)<>Degree(rep) or MinDomain(f)<>MinDomain(rep) or 
       Rank(f)<>Rank(rep) or Dom(f)<>Dom(rep) then
        Info(InfoCitrus, 1, "degree, rank, or domain not equal to those of",
          " any of the R-class elements,");
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=r!.o; data:=r!.data;
      i:=Position(r!.o, RangeSetOfPartialPerm(f));

      if i = fail or not o!.truth[data[1]][i] then 
        return fail;
      fi;
      
      j:=Position(enum!.schutz, AsPermutation(rep^-1*f*o!.mults[i]));

      if j=fail then 
        return fail;
      fi;
      return enum!.len*(Position(RangeOrbSCC(r), i)-1)+j;
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
      return RangeOrbMults(r)[RangeOrbSCC(r)[pos[1]]]*enum[pos[2]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local rep, o, data, i, j;

      rep:=Representative(r);
      
      if MaxRange(f)<>MaxRange(rep) or MinRange(f)<>MinRange(rep) or 
       Rank(f)<>Rank(rep) or
        RangeSetOfPartialPerm(f)<>RangeSetOfPartialPerm(rep) then
        Info(InfoCitrus, 1, "degree, rank, or range not equal to those of",
          " any of the L-class elements,");
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=r!.o; data:=r!.data;
      i:=Position(r!.o, Dom(f));

      if i = fail or not o!.truth[data[1]][i] then 
        return fail;
      fi;
      j:=Position(enum!.schutz, AsPermutation(o!.mults[i]^-1*f*rep^-1));

      if j=fail then 
        return fail;
      fi;
      return enum!.len*(Position(RangeOrbSCC(r), i)-1)+j;
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

#GGG

# new for 0.7! - GreensDClassOfElement - for an inv semi and part perm
##############################################################################

InstallOtherMethod(GreensDClassOfElement, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm and   
 IsPartialPermRep],
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
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm and   
 IsPartialPermRep],
function(s, f)
  local o, l, m, t, k, rep, d;

  if IsClosed(RangesOrb(s)) then 
    o:=RangesOrb(s);
    l:=Position(o, Dom(f));
    if l=fail then 
      Info(InfoCitrus, 1, "the partial perm. is not an element of the semigroup");
      return fail;
    fi;
    m:=OrbSCCLookup(o)[l];
    t:=OrbSCC(o)[m][1];
    k:=Position(o, RangeSetOfPartialPerm(f));
    if k=fail or not OrbSCCTruthTable(o)[m][k] then 
      Info(InfoCitrus, 1, "the partial perm. is not an element of the           semigroup");
      return fail;
    fi;
    rep:=o!.mults[l]^-1*f*o!.mults[k];
  else
    o:=ShortOrb(s, Dom(f));
    rep:=f*f^-1;
    l:=1; m:=1; t:=1;
  fi;

  d:=Objectify(DClassType(s), rec(parent:=s, data:=[m,t,l,k], o:=o)); 

  SetRepresentative(d, rep);
  SetEquivalenceClassRelation(d, GreensDRelation(s));
  return d; 
end);

# new for 0.7! - GreensHClassOfElement - for an inv semi and part perm
##############################################################################

InstallOtherMethod(GreensHClassOfElement, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm and   
 IsPartialPermRep],
function(s, f)
  if not f in s then 
    Error("the partial perm. is not an element of the semigroup,");
    return;
  fi;
  return GreensHClassOfElementNC(s, f);
end);

# new for 0.7! - GreensHClassOfElementNC - for an inv semi and part perm
##############################################################################
# Notes: data is: [scc index, scc[1], pos of dom, pos of ran]

InstallOtherMethod(GreensHClassOfElementNC, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm and   
 IsPartialPermRep],
function(s, f)
  local o, l, m, t, k, d;

  if IsClosed(RangesOrb(s)) then 
    o:=RangesOrb(s);
    l:=Position(o, Dom(f));
    if l=fail then 
      Info(InfoCitrus, 1, "the partial perm. is not an element of the semigroup");
      return fail;
    fi;
    m:=OrbSCCLookup(o)[l];
    t:=OrbSCC(o)[m][1];
  else
    o:=ShortOrb(s, Dom(f));
    Enumerate(o);
    l:=1; m:=1; t:=1;
  fi;
  
  k:=Position(o, RangeSetOfPartialPerm(f));
  if k=fail or not OrbSCCTruthTable(o)[m][k] then 
    Info(InfoCitrus, 1, "the partial perm. is not an element of the           semigroup");
    return fail;
  fi;
  
  d:=Objectify(HClassType(s), rec(parent:=s, data:=[m,t,l,k], o:=o)); 

  SetRepresentative(d, f);
  SetEquivalenceClassRelation(d, GreensHRelation(s));
  return d; 
end);

# new for 0.7! - GreensLClassOfElement - for an inv semi and part perm
##############################################################################

InstallOtherMethod(GreensLClassOfElement, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm and   
 IsPartialPermRep],
function(s, f)
  if not f in s then 
    Error("the partial perm. is not an element of the semigroup,");
    return;
  fi;
  return GreensLClassOfElementNC(s, f);
end);

# new for 0.7! - GreensLClassOfElementNC - for an inv semi and part perm
##############################################################################
# Notes: data is: [scc index, scc[1], pos of ran]

InstallOtherMethod(GreensLClassOfElementNC, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm and   
 IsPartialPermRep],
function(s, f)
  local o, l, m, t, rep, r;

  if IsClosed(RangesOrb(s)) then 
    o:=RangesOrb(s);
    l:=Position(o, Dom(f));
    if l=fail then 
      Info(InfoCitrus, 1, "the partial perm. is not an element of the semigroup");
      return fail;
    fi;
    m:=OrbSCCLookup(o)[l];
    t:=OrbSCC(o)[m][1];
    rep:=o!.mults[l]^-1*f;
  else
    o:=ShortOrb(s, Dom(f));
    l:=1; m:=1; t:=1;
    rep:=f;
  fi;

  r:=Objectify(LClassType(s), rec(parent:=s, data:=[m,t,l], o:=o)); 

  SetRepresentative(r, rep);
  SetEquivalenceClassRelation(r, GreensLRelation(s));
  return r; 
end);

# new for 0.7! - GreensRClassOfElement - for an inv semi and part perm
##############################################################################

InstallOtherMethod(GreensRClassOfElement, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm and   
 IsPartialPermRep],
function(s, f)
  if not f in s then 
    Error("the partial perm. is not an element of the semigroup,");
    return;
  fi;
  return GreensRClassOfElementNC(s, f);
end);

# new for 0.7! - GreensRClassOfElementNC - for an inv semi and part perm
##############################################################################
# Notes: data is: [scc index, scc[1], pos of dom]

InstallOtherMethod(GreensRClassOfElementNC, "for an inv semi and part perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm and   
 IsPartialPermRep],
function(s, f)
  local o, l, m, t, rep, r;

  if IsClosed(RangesOrb(s)) then 
    o:=RangesOrb(s);
    l:=Position(o, RangeSetOfPartialPerm(f));
    if l=fail then 
      Info(InfoCitrus, 1, "the partial perm. is not an element of the semigroup");
      return fail;
    fi;
    m:=OrbSCCLookup(o)[l];
    t:=OrbSCC(o)[m][1];
    rep:=f*o!.mults[l];
  else
    o:=ShortOrb(s, RangeSetOfPartialPerm(f));
    l:=1; m:=1; t:=1; rep:=f;
  fi;
  
  r:=Objectify(RClassType(s), rec(parent:=s, data:=[m,t,l], o:=o)); 

  SetRepresentative(r, rep);
  SetEquivalenceClassRelation(r, GreensRRelation(s));
  return r; 
end);

#HHH

# new for 0.7! - HClassReps - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(HClassReps, "for an inverse semi of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, scc, mults, r, gens, out, l, m, w, f, i, j, k;

  o:=RangesOrb(s);
  EnumerateRangesOrb(s);
  scc:=OrbSCC(o);
  mults:=o!.mults;
  r:=Length(scc);
  gens:=GeneratorsOfSemigroup(s);
  out:=EmptyPlist(NrHClasses(s));

  if IsPartialPermMonoid(s) then 
    l:=0;
  else
    l:=1;
  fi;

  m:=0;
  for i in [1..r-l] do 
    w:=TraceSchreierTreeForward(o, i+l);
    if w=[] then 
      f:=PartialPermNC(o[scc[i+l][1]], o[scc[i+l][1]]);
    else
      f:=EvaluateWord(gens, TraceSchreierTreeForward(o, scc[i+l][1]));
    fi;
    for j in [1..Length(scc[i+l])] do 
      for k in [1..Length(scc[i+l])] do 
        m:=m+1;
        out[m]:=mults[scc[i+l][j]]*f^-1*f*mults[scc[i+l][k]]^-1;  
      od;
    od;
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

#LLL

# new for 0.7! - LClassReps - for an inv semi of part perms
##############################################################################

InstallOtherMethod(LClassReps, "for an inv semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, out, gens, i, j;

  o:=RangesOrb(s);
  EnumerateRangesOrb(s);
  out:=EmptyPlist(Length(o));
  gens:=GeneratorsOfSemigroup(s);

  if IsPartialPermMonoid(s) then 
    i:=0;
  else
    i:=1;
  fi;
  
  for j in [1..Length(o)-i] do 
    out[j]:=EvaluateWord(gens, TraceSchreierTreeForward(o, j+i))^-1;
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

#NNN

# new for 0.7! - NrRClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrRClasses, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  Enumerate(RangesOrb(s));

  if IsPartialPermMonoid(s) then 
    return Length(RangesOrb(s));
  fi;

  return Length(RangesOrb(s))-1;
end); 

# new for 0.7! - NrLClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrLClasses, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local i;
  Enumerate(RangesOrb(s));
  
  if IsPartialPermMonoid(s) then 
    return Length(RangesOrb(s));
  fi;

  return Length(RangesOrb(s))-1;
end); 

# new for 0.7! - NrDClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrDClasses, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  Enumerate(RangesOrb(s));
  if IsPartialPermMonoid(s) then 
    return Length(OrbSCC(RangesOrb(s)));
  fi;

  return Length(OrbSCC(RangesOrb(s)))-1;
end); 

# new for 0.7! - NrHClasses - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrHClasses, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local scc;
  
  Enumerate(RangesOrb(s));
  scc:=OrbSCC(RangesOrb(s));

  if IsPartialPermMonoid(s) then 
    return Sum(List(scc, m-> Length(m)^2));
  fi;

  return Sum(List(scc, m-> Length(m)^2))-1;
end); 

# new for 0.7! - NrIdempotents - for an inverse semigroup of partial perms
##############################################################################

InstallOtherMethod(NrIdempotents, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  Enumerate(RangesOrb(s));
  if IsPartialPermMonoid(s) then 
    return Length(RangesOrb(s));
  fi;

  return Length(RangesOrb(s))-1;
end); 

# new for 0.7! - ParentAttr - "for a Green's class of a part perm semigroup
##############################################################################

InstallMethod(ParentAttr, "for a R-class of a trans. semigroup",
[IsGreensClass and IsGreensClassOfPartPermSemigroup], x-> x!.parent);

#RRR

# new for 0.1! - Random - "for a part. perm. inv. semigroup (citrus pkg)"
#############################################################################
# move to greens.gi

InstallMethod(Random, "for a part perm inv semigroup (citrus pkg)",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  local o, gens, i, w, k, m, l, g;

  o:=RangesOrb(s);

  if not IsClosed(o) then  
    gens:=GeneratorsOfSemigroup(s);
    i:=Random([1..Length(gens)]);
    w:=List([1..i], x-> Random([1..Length(gens)]));
    return EvaluateWord(gens, w);
  else
    k:=Random([1..Length(o)]);
    m:=OrbSCCLookup(o)[k];
    l:=Random(OrbSCC(o)[m]);
    g:=Random(o!.schutz[m][2]);
    return o!.mults[k]*g*o!.mults[l]^-1;
  fi;
end);


# new for 0.7! - RangeOrbMults - for a Green's class of a part perm inv semi
##############################################################################

InstallMethod(RangeOrbMults, "for a Green's class of a part perm inv semi",
[IsGreensClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup],
function(class)
  return class!.o!.mults;
end);

# new for 0.7! - RangeOrbSCC - for a Green's class of a part perm inv semi
##############################################################################

InstallMethod(RangeOrbSCC, "for a Green's class of a part perm inv semi",
[IsGreensClass and IsGreensClassOfPartPermSemigroup and
IsGreensClassOfInverseSemigroup],
function(class)
  return OrbSCC(class!.o)[class!.data[1]];
end);

# new for 0.7! - RangeOrbStabChain 
##############################################################################
# if the optional third arg is present it should have its range in position
# scc[1] of o!! Otherwise this will mess everything up!

InstallGlobalFunction(RangeOrbStabChain,
function(arg)
  local o, m, scc, f;

  o:=arg[1]; m:=arg[2];
  scc:=OrbSCC(o)[m];
  
  if IsBound(arg[3]) then 
    f:=arg[3];
  else
    f:=TraceSchreierTreeForward(o, scc[1]);
    if f=[] then 
      f:=PartialPermNC(o[scc[1]], o[scc[1]]);
    else
      f:=EvaluateWord(o!.gens, f);
    fi;
  fi;

  if not IsBound(o!.mults) then
    o!.mults:=EmptyPlist(Length(o));
  fi;

  if not IsBound(o!.mults[scc[1]]) then
    CreateSCCMultipliers(o!.gens, o, m, scc, o!.mults);
  fi;

  if not IsBound(o!.schutz) then
    o!.schutz:=EmptyPlist(Length(scc));
  fi;

  if not IsBound(o!.schutz[m]) then
    o!.schutz[m]:=CreateSchutzGp(o!.gens, o, f, scc, 
     o!.truth[m], OrbitGraph(o), Length(o!.gens), o!.mults);
  fi;

  return o!.schutz[m][1];
end);

# new for 0.7! - RangesOrb - "for an inverse semi of part. perms"
##############################################################################

InstallMethod(RangesOrb, "for an inverse semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local n;

  n:=LargestMovedPoint(s);
  return Orb(s, [1..n]*1, OnIntegerSetsWithPartialPerm, 
        rec(schreier:=true, orbitgraph:=true, storenumbers:=true, 
        log:=true, hashlen:=CitrusOptionsRec.hashlen.M, finished:=false));
        #JDM orb bug prevents us from using onflatplainlist above
end);

# new for 0.7! - RClassReps - for an inv semi of part perms
##############################################################################

InstallOtherMethod(RClassReps, "for an inv semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, out, gens, i, j;

  o:=RangesOrb(s);
  EnumerateRangesOrb(s);
  out:=EmptyPlist(Length(o));
  gens:=GeneratorsOfSemigroup(s);

  if IsPartialPermMonoid(s) then 
    i:=0;
  else
    i:=1;
  fi;

  for j in [1..Length(o)-i] do 
    out[j]:=EvaluateWord(gens, TraceSchreierTreeForward(o, j+i));
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
function(r)
  local o, m, scc, s, rep;
  
  o:=r!.o; 
  m:=r!.data[1];
  scc:=OrbSCC(o)[m];
  s:=r!.parent;

  if not IsBound(o!.mults) then 
    o!.mults:=EmptyPlist(Length(o));
  fi;

  if not IsBound(o!.mults[scc[1]]) then 
    CreateSCCMultipliers(GeneratorsOfSemigroup(s), o, m, scc, o!.mults);    
  fi;

  if not IsBound(o!.schutz) then 
    o!.schutz:=EmptyPlist(Length(scc));
  fi;
  
  if not IsBound(o!.schutz[m]) then 
    rep:=Representative(r);
    if IsGreensLClass(r) then 
      rep:=rep^-1;
    fi;
    o!.schutz[m]:=CreateSchutzGp(GeneratorsOfSemigroup(s), o,
     rep, scc, o!.truth[m], OrbitGraph(o), 
      Length(GeneratorsOfSemigroup(s)), o!.mults);
  fi;

  return o!.schutz[m][2];
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

  if not IsBound(o!.mults) then
    o!.mults:=EmptyPlist(Length(o));
  fi;

  if not IsBound(o!.mults[scc[1]]) then
    CreateSCCMultipliers(GeneratorsOfSemigroup(s), o, m, scc, o!.mults);
  fi;

  if not IsBound(o!.schutz) then
    o!.schutz:=EmptyPlist(Length(scc));
  fi;
 
  if not IsBound(o!.schutz[m]) then
    rep:=o!.mults[h!.data[3]]^-1*Representative(h)*o!.mults[h!.data[4]];
    o!.schutz[m]:=CreateSchutzGp(GeneratorsOfSemigroup(s), o,
     rep, scc, o!.truth[m], OrbitGraph(o),
      Length(GeneratorsOfSemigroup(s)), o!.mults);
  fi;

  return o!.schutz[m][2]; 
end);

# new for 0.7! - ShortOrb - "for an inverse semigp of partial perms"
##############################################################################

InstallGlobalFunction(ShortOrb, 
function(s, set)

  return Orb(s, set, OnIntegerSetsWithPartialPerm, 
      rec(onflatplainlist:=true,
        treehashsize:=CitrusOptionsRec.hashlen.M,
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
  EnumerateRangesOrb(s);
  return Size(s);
end); 

#EOF



