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

# new for 0.7! - EnumerateInvSemigpData - "for an inverse semi of part perms"
##############################################################################

InstallGlobalFunction(EnumerateInvSemigpData, 
function(s)
  local o, scc, r, mults, schutz, graph, truth, gens, modifier, i;
  
  o:=InvSemigpData(s);

  if not IsClosed(o) then 
    scc:=OrbSCC(o);
    r:=Length(scc);
    mults:=EmptyPlist(Length(o)); 
    schutz:=EmptyPlist(r);
    graph:=OrbitGraph(o);
    truth:=OrbSCCTruthTable(o);
    gens:=GeneratorsOfSemigroup(s);

    for i in [1..r] do 
      CreateSCCMultipliers(gens, o, i, scc[i], mults);
      schutz[i]:=CreateSchutzGp(gens, o, EvaluateWord(gens,   
       TraceSchreierTreeForward(o, scc[i][1])), scc[i], truth[i], graph, r, 
       mults);
    od;

    if IsPartialPermMonoid(s) then 
      modifier:=0;
    else
      modifier:=1;
    fi;

    SetSize(s, Sum(List([1..r], m-> Length(scc[m])^2*Size(schutz[m][2])))-
     modifier);
    SetNrDClasses(s, r-modifier);
    SetNrRClasses(s, Length(o)-modifier);
    SetNrLClasses(s, Length(o)-modifier);
    SetNrHClasses(s, Sum(List([1..r], m-> Length(scc[m])^2))-modifier);
    SetNrIdempotents(s, Length(o)-modifier);
  fi;
  
  return;
end); 

# new for 0.7! - InvSemigpData - "for an inverse semi of part. perms"
##############################################################################

InstallMethod(InvSemigpData, "for an inverse semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local n;

  n:=LargestMovedPoint(s);
  return Orb(s, [1..n]*1, OnIntegerSetsWithPartialPerm, 
        rec(schreier:=true, orbitgraph:=true, storenumbers:=true, 
        log:=true, hashlen:=6257));
        #JDM orb bug prevents us from using onflatplainlist above
end);

# new for 0.7! - Size - for an inverse semigroup of partial perms
##############################################################################

InstallMethod(Size, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  EnumerateInvSemigpData(s);
  return Size(s);
end); 
  







