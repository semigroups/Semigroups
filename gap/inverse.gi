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

# new for 0.7! - CreateSCCMultipliers - not a user function 
#############################################################################

InstallGlobalFunction(CreateSCCMultipliers, 
function(gens, o, j, scc, mults)
  local i;
  
  for i in scc do 
    mults[i]:=EvaluateWord(gens, TraceSchreierTreeOfSCCForward(o, j, i))^-1;
    #could use TraceSchreierTreeOfSCCBack here too..
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
    o!.finished:=true;
  else
    schutz:=o!.schutz;
  fi;
  
  SetSize(s, Sum(List([1..r], m-> Length(scc[m])^2*Size(schutz[m][2])))-
     modifier);
  return;
end); 

#GGG

#LLL

# new for 0.7! - LClassReps - for an inv semi of part perms
##############################################################################

InstallOtherMethod(LClassReps, "for an inv semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, out, gens, i;

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
    return Length(OrbSCC(RangesOrb(s)));
  fi;

  return Length(OrbSCC(RangesOrb(s)))-1;
end); 

#RRR

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
  local o, out, gens, i;

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

#SSS

# new for 0.7! - Size - for an inverse semigroup of partial perms
##############################################################################

InstallMethod(Size, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  EnumerateRangesOrb(s);
  return Size(s);
end); 

#EOF



