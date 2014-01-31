#############################################################################
##
#W  semipperm.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(CyclesOfPartialPerm, "for a partial perm", [IsPartialPerm], 
function(f)
  local n, seen, out, i, j, cycle;

  n:=Maximum(DegreeOfPartialPerm(f), CoDegreeOfPartialPerm(f));
  seen:=BlistList([1..n], ImageSetOfPartialPerm(f));
  out:=[];
  
  #find chains
  for i in DomainOfPartialPerm(f) do
    if not seen[i] then
      i:=i^f;
      while i<>0 do 
        seen[i]:=false;
        i:=i^f;
      od;
    fi;
  od;

  #find cycles
  for i in DomainOfPartialPerm(f) do 
    if seen[i] then 
      j:=i^f;
      cycle:=[j];
      while j<>i do 
        j:=j^f;
        Add(cycle, j);
      od;
      Add(out, cycle);
    fi;
  od;
  return out;
end);

#

InstallMethod(ComponentRepsOfPartialPermSemigroup, 
"for a partial perm semigroup", [IsPartialPermSemigroup],
function(S)
  local pts, reps, next, opts, o, out, i;

  pts:=[1..DegreeOfPartialPermSemigroup(S)];
  reps:=BlistList(pts, []);
  # true=its a rep, false=not seen it, fail=its not a rep
  next:=1;
  opts:=rec(lookingfor:=function(o, x) 
    if not IsEmpty(x) then 
      return reps[x[1]]=true or reps[x[1]]=fail;
    else
      return false;
    fi;
  end);

  repeat
    o:=Orb(S, [next], OnSets, opts);  
    Enumerate(o);
    if PositionOfFound(o)<>false and reps[o[PositionOfFound(o)][1]]=true then 
      if not IsEmpty(o[PositionOfFound(o)]) then 
        reps[o[PositionOfFound(o)][1]]:=fail;
      fi;
    fi;
    reps[next]:=true;
    for i in [2..Length(o)] do 
      if not IsEmpty(o[i]) then 
        reps[o[i][1]]:=fail;
      fi;
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

InstallMethod(ComponentsOfPartialPermSemigroup, 
"for a partial perm semigroup", [IsPartialPermSemigroup],
function(S)
  local pts, comp, next, nr, opts, o, out, i;

  pts:=[1..DegreeOfPartialPermSemigroup(S)];
  comp:=BlistList(pts, []);
  # integer=its component index, false=not seen it
  next:=1;  nr:=0;
  opts:=rec(lookingfor:=function(o, x) 
    if not IsEmpty(x) then 
      return IsPosInt(comp[x[1]]);
    else
      return false;
    fi;
  end);

  repeat
    o:=Orb(S, [next], OnSets, opts);  
    Enumerate(o);
    if PositionOfFound(o)<>false then 
      for i in o do 
        if not IsEmpty(i) then 
          comp[i[1]]:=comp[o[PositionOfFound(o)][1]];
        fi;
      od;
    else
      nr:=nr+1;
      for i in o do 
        if not IsEmpty(i) then
          comp[i[1]]:=nr;
        fi;
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

InstallMethod(CyclesOfPartialPermSemigroup, 
"for a partial perm semigroup", [IsPartialPermSemigroup],
function(S)
  local pts, comp, next, nr, cycles, opts, o, scc, i;

  pts:=[1..DegreeOfPartialPermSemigroup(S)];
  comp:=BlistList(pts, []);
  # integer=its component index, false=not seen it
  next:=1;  nr:=0; cycles:=[];
  opts:=rec(lookingfor:=function(o, x) 
    if not IsEmpty(x) then 
      return IsPosInt(comp[x[1]]);
    else
      return false;
    fi;
  end);

  repeat
    #JDM the next line doesn't work if OnPoints is used...
    o:=Orb(S, [next], OnSets, opts);  
    Enumerate(o);
    if PositionOfFound(o)<>false then 
      for i in o do 
        if not IsEmpty(i) then
          comp[i[1]]:=comp[o[PositionOfFound(o)][1]];
        fi;
      od;
    else
      nr:=nr+1;
      for i in o do 
        if not IsEmpty(i) then
          comp[i[1]]:=nr;
        fi;
      od;
      scc:=First(OrbSCC(o), x-> Length(x)>1);
      if scc<>fail then 
        Add(cycles, List(o{scc}, x-> x[1]));
      fi;
    fi;
    next:=Position(comp, false, next);
  until next=fail;

  return cycles;
end);

#EOF
