#############################################################################
##
#W  semitrans.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


if not IsBound(IS_STRONGLY_CONNECTED_DIGRAPH) then 
  # non-recursive version below...
  BindGlobal("IS_STRONGLY_CONNECTED_DIGRAPH", 
  function(digraph)   
    local n, stack1, len1, stack2, len2, id, count, fptr, level, l, w, v;
    
    n:=Length(digraph);
    
    if n=0 then 
      return true;
    fi;

    stack1:=EmptyPlist(n); len1:=0;
    stack2:=EmptyPlist(n); len2:=0;
    id:=[1..n]*0;
    count:=Length(digraph);
    fptr:=[];

    for v in [1..Length(digraph)] do
      if id[v]=0 then 
        level:=1;
        fptr[1] := v; #fptr[0], vertex
        fptr[2] := 1; #fptr[2], index
        len1:=len1+1;
        stack1[len1]:=v;   
        len2:=len2+1;
        stack2[len2]:=len1; 
        id[v]:=len1;

        while level>0 do
          if fptr[2*level] > Length(digraph[fptr[2*level-1]]) then 
            if stack2[len2]=id[fptr[2*level-1]] then
              if count=Length(digraph)+1 then 
                return false;
              fi;
              len2:=len2-1;
              count:=count+1;
              l:=0;
              repeat
                w:=stack1[len1];
                id[w]:=count;
                len1:=len1-1; #pop from stack1
                l:=l+1;
              until w=fptr[2*level-1];
            fi;
            level:=level-1;
          else
            w:=digraph[fptr[2*level-1]][fptr[2*level]];
            fptr[2*level]:=fptr[2*level]+1;

            if id[w]=0 then 
              level:=level+1;
              fptr[2*level-1]:=w; #fptr[0], vertex
              fptr[2*level]:=1;   #fptr[2], index
              len1:=len1+1;
              stack1[len1]:=w;   
              len2:=len2+1;
              stack2[len2]:=len1; 
              id[w]:=len1;

            else # we saw <w> earlier in this run
              while stack2[len2] > id[w] do
                len2:=len2-1; # pop from stack2
              od;
            fi;
          fi;
        od;
      fi;
    od;

    return true;
  end);
fi;

# different method required (but not yet given!!) for ideals

# JDM this could be done without creating the graph first and then running
# IS_STRONGLY_CONNECTED_DIGRAPH, but just using the method of
# IS_STRONGLY_CONNECTED_DIGRAPH with the generators themselves.

InstallMethod(IsTransitive, 
"for a transformation semigroup with generators and a positive int",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt], 
function(S)
  return IsTransitive(S, DegreeOfTransformationSemigroup(S));
end);

InstallMethod(IsTransitive, 
"for a transformation semigroup with generators and a positive int",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt], 
function(S, n)
  local gens, nrgens, graph, i, x;

  gens:=GeneratorsOfSemigroup(S);
  nrgens:=Length(gens);
  graph:=EmptyPlist(n);

  for i in [1..n] do 
    graph[i]:=EmptyPlist(nrgens);;
    for x in gens do 
      Add(graph[i], i^x);
    od;
  od;

  return IS_STRONGLY_CONNECTED_DIGRAPH(graph);
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
