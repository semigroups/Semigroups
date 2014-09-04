#############################################################################
##
#W  semitrans.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# different method required (but not yet given!!) for ideals

InstallMethod(IsTransitive, 
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(S)
  return IsTransitive(S, DegreeOfTransformationSemigroup(S));
end);

InstallMethod(IsTransitive, 
"for a transformation semigroup with generators and a positive int",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt], 
function(S, n)
  return IsTransitive(GeneratorsOfSemigroup(S), n);
end);

InstallMethod(IsTransitive, 
"for a transformation semigroup with generators and a set of pos ints",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsList], 
function(S, set)
  return IsTransitive(GeneratorsOfSemigroup(S), set);
end);

# JDM this could be done without creating the graph first and then running
# IsStronglyConnectedDirectedGraph, but just using the method of
# IsStronglyConnectedDirectedGraph with the generators themselves.

InstallMethod(IsTransitive, 
"for a transformation collection and a positive int",
[IsTransformationCollection, IsPosInt], 
function(coll, n)
  local gens, nrgens, graph, i, x;

  nrgens:=Length(coll);
  graph:=EmptyPlist(n);

  for i in [1..n] do 
    graph[i]:=EmptyPlist(nrgens);;
    for x in coll do 
      Add(graph[i], i^x);
    od;
  od;

  return IsStronglyConnectedDirectedGraph(graph);
end);

InstallMethod(IsTransitive, 
"for a transformation collection and set of positive integers",
[IsTransformationCollection, IsList], 
function(coll, set)
  local n, nrgens, graph, lookup, j, i, x;
  
  if not (IsSSortedList(set) and IsHomogeneousList(set) and IsPosInt(set[1]))
   then 
    Error("usage: the second argument <set> must be a set of positive",
    " integers");
    return;
  fi;

  n:=Length(set);
  nrgens:=Length(coll);
  graph:=EmptyPlist(n);
  lookup:=[];

  for i in [1..n] do 
    lookup[set[i]]:=i;
  od;

  for i in [1..n] do 
    graph[i]:=EmptyPlist(nrgens);;
    for x in coll do 
      j:=set[i]^x;
      if IsBound(lookup[j]) then # <j> is in <set>!
        Add(graph[i], lookup[set[i]^x]);
      fi;
    od;
  od;

  return IsStronglyConnectedDirectedGraph(graph);
end);

# same method for ideals

InstallMethod(IsSynchronizingSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(S) 
  return IsSynchronizingSemigroup(S, DegreeOfTransformationSemigroup(S));
end);

# same method for ideals

InstallMethod(IsSynchronizingSemigroup,
"for a transformation semigroup and positive integer", 
[IsTransformationSemigroup, IsPosInt],
function(S, n)
  local gens;
 
  if HasGeneratorsOfSemigroup(S) then 
    gens:=GeneratorsOfSemigroup(S);
  else
    gens:=GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  fi;

  return IsSynchronizingTransformationCollection(gens, n);
end);

# this method comes from PJC's slides from the Lisbon Workshop in July 2014
# not applicable to ideals

InstallMethod(IsSynchronizingTransformationCollection, 
"for a transformation collection and positive integer", 
[IsTransformationCollection, IsPosInt],
function(gens, n)
  local NumberPair, PairNumber, genstoapply, act, graph, constants, x, adj, y, num, marked, squashed, i, j;
  
  NumberPair:=function(x)
    if x[2]>x[1] then 
      return n*(x[1]-1)+x[2]-x[1];
    else 
      return (n-1)*(x[1]-1)+x[2];
    fi;
  end;

  PairNumber:=function(x)
    local q, r;
    q:=QuoInt(x-1, n-1);
    r:=(x-1)-q*(n-1);
    if q>r then
      return [q+1, r+1];
    else
      return [q+1, r+2];
    fi;
  end;

  genstoapply:=[1..Length(gens)];
  
  act:=function(set, f) 
    return OnPosIntSetsTrans(set, f, n);
  end;

  graph:=List([1..n^2], x->[]); 
  constants:=false;
  
  # add edges for pairs
  for i in [1..n^2-n] do 
    x:=PairNumber(i);
    adj:=[];
    for j in genstoapply do
      y:=act(x, gens[j]);
      if Length(y)=2 then 
        num:=NumberPair(act(x, gens[j]));
        AddSet(graph[num], i);
        AddSet(adj, num);
      else
        AddSet(graph[n^2-n+y[1]], i);
        AddSet(adj, n^2-n+y[1]);
        constants:=true;
      fi;
      
    od;
    if Length(adj)=1 and adj[1]=i then 
      # can't get anywhere by applying things to this pair
      return false;
    fi;
  od;
   
  if not constants then 
    return false;
  fi;

  marked:=BlistList([1..n^2], [n^2-n+1..n^2]);
  squashed:=[n^2-n+1..n^2];
  for i in squashed do 
    for j in graph[i] do 
      if not marked[j] then 
        marked[j]:=true;
        squashed[Length(squashed)+1]:=j;
      fi;
    od;
  od;
  
  return Length(squashed)=n^2;
end);

#

InstallMethod(AsTransformationSemigroup, "for a semigroup",
[IsSemigroup], 
function(S)
  return Range(IsomorphismTransformationSemigroup(S));
end);

#

InstallMethod(ViewString, 
"for a transformation semigroup ideal with ideal generators",
[IsTransformationSemigroup and IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal], 
function(I)
  local str, nrgens;
  
  str:="\><";

  if HasIsTrivial(I) and IsTrivial(I) then 
    Append(str, "\>trivial\< ");
  else 
    if HasIsCommutative(I) and IsCommutative(I) then 
      Append(str, "\>commutative\< ");
    fi;
  fi;

  if HasIsTrivial(I) and IsTrivial(I) then 
  elif HasIsZeroSimpleSemigroup(I) and IsZeroSimpleSemigroup(I) then 
    Append(str, "\>0-simple\< ");
  elif HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I) then 
    Append(str, "\>simple\< ");
  fi;

  if HasIsInverseSemigroup(I) and IsInverseSemigroup(I) then 
    Append(str, "\>inverse\< ");
  elif HasIsRegularSemigroup(I) 
   and not (HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I)) then 
    if IsRegularSemigroup(I) then 
      Append(str, "\>regular\< ");
    else
      Append(str, "\>non-regular\< ");
    fi;
  fi;
  
  Append(str, "\>transformation\< \>semigroup\< \>ideal\< ");
  Append(str, "\>on \>");
  Append(str, ViewString(DegreeOfTransformationSemigroup(I)));
  Append(str, "\< pts with\> ");
  
  nrgens:=Length(GeneratorsOfSemigroupIdeal(I));
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens>1 or nrgens=0 then 
    Append(str, "s\<");
  else 
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

#

InstallMethod(DegreeOfTransformationSemigroup, 
"for a transformation semigroup ideal",
[IsTransformationSemigroup and IsSemigroupIdeal],
function(I)
  return DegreeOfTransformationSemigroup(SupersemigroupOfIdeal(I));
end);

#

InstallMethod(ComponentRepsOfTransformationSemigroup, 
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  local pts, reps, next, opts, gens, o, out, i;

  pts:=[1..DegreeOfTransformationSemigroup(S)];
  reps:=BlistList(pts, []);
  # true=its a rep, false=not seen it, fail=its not a rep
  next:=1;
  opts:=rec(lookingfor:=function(o, x) 
    return reps[x]=true or reps[x]=fail;
  end);

  if IsSemigroupIdeal(S) then 
    gens:=GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  else
    gens:=GeneratorsOfSemigroup(S);
  fi;
  
  repeat
    o:=Orb(gens, next, OnPoints, opts);  
    Enumerate(o);
    if PositionOfFound(o)<>false and reps[o[PositionOfFound(o)]]=true then 
      reps[o[PositionOfFound(o)]]:=fail;
    fi;
    reps[next]:=true;
    for i in [2..Length(o)] do 
      reps[o[i]]:=fail;
    od;
    next:=Position(reps, false, next);
  until next=fail;

  out:=[];
  for i in pts do 
    if reps[i]=true then 
      Add(out, i);
    fi;
  od;

  return out;
end);

#

InstallMethod(ComponentsOfTransformationSemigroup, 
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  local pts, comp, next, nr, opts, gens, o, out, i;

  pts:=[1..DegreeOfTransformationSemigroup(S)];
  comp:=BlistList(pts, []);
  # integer=its component index, false=not seen it
  next:=1;  nr:=0;
  opts:=rec(lookingfor:=function(o, x) 
    return IsPosInt(comp[x]);
  end);

  if IsSemigroupIdeal(S) then 
    gens:=GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  else
    gens:=GeneratorsOfSemigroup(S);
  fi;
  
  repeat
    o:=Orb(gens, next, OnPoints, opts);  
    Enumerate(o);
    if PositionOfFound(o)<>false then 
      for i in o do 
        comp[i]:=comp[o[PositionOfFound(o)]];
      od;
    else
      nr:=nr+1;
      for i in o do 
        comp[i]:=nr;
      od;
    fi;
    next:=Position(comp, false, next);
  until next=fail;

  out:=[];
  for i in pts do
    if not IsBound(out[comp[i]]) then 
      out[comp[i]]:=[];
    fi;
    Add(out[comp[i]], i);
  od;

  return out;
end);

#

InstallMethod(CyclesOfTransformationSemigroup, 
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  local pts, comp, next, nr, cycles, opts, gens, o, scc, i;

  pts:=[1..DegreeOfTransformationSemigroup(S)];
  comp:=BlistList(pts, []);
  # integer=its component index, false=not seen it
  next:=1;  nr:=0; cycles:=[];
  opts:=rec(lookingfor:=function(o, x) 
    return IsPosInt(comp[x]);
  end);
  
  if IsSemigroupIdeal(S) then 
    gens:=GeneratorsOfSemigroup(SupersemigroupOfIdeal(S));
  else
    gens:=GeneratorsOfSemigroup(S);
  fi;
  
  repeat
    o:=Orb(gens, next, OnPoints, opts);  
    Enumerate(o);
    if PositionOfFound(o)<>false then 
      for i in o do 
        comp[i]:=comp[o[PositionOfFound(o)]];
      od;
    else
      nr:=nr+1;
      for i in o do 
        comp[i]:=nr;
      od;
      scc:=First(OrbSCC(o), x-> Length(x)>1);
      if scc=fail then 
        Add(cycles, [o[Length(o)]]);
      else
        Add(cycles, o{scc});
      fi;
    fi;
    next:=Position(comp, false, next);
  until next=fail;

  return cycles;
end);

#EOF
