#############################################################################
##
#W  orbits.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#JDM this should work for the RhoOrb too!

InstallMethod( Enumerate, "for a lambda orbit and a limit (Semigroups)", 
[IsLambdaOrb and IsHashOrbitRep, IsCyclotomic],
function( o, limit )
  local orb, i, nr, looking, lookfunc, found, stopper, op, gens, ht, genstoapply, schreiergen, schreierpos, log, logind, logpos, depth, depthmarks, grades, gradingfunc, onlygrades, onlygradesdata, orbitgraph, nrgens, htadd, htvalue, suc, yy, pos, grade, j;

  # Set a few local variables for faster access:
  orb := o!.orbit;
  i := o!.pos;  # we go on here
  nr := Length(orb);

  # We copy a few things to local variables to speed up access:
  looking := o!.looking;
  if looking then 
    lookfunc := o!.lookfunc; 
    found:=o!.found;
    if found<>false then 
      for j in [found+1..nr] do 
        if lookfunc(o,orb[j]) then
          o!.found := j;
          return o;
        fi;
      od;
    fi;
  fi;
  
  stopper := o!.stopper;
  op := o!.op;
  gens := o!.gens;
  ht := o!.ht;
  genstoapply := o!.genstoapply;
  schreiergen := o!.schreiergen;
  schreierpos := o!.schreierpos;
  
  log := o!.log;
  logind := o!.logind;
  logpos := o!.logpos;
  depth := o!.depth;
  depthmarks := o!.depthmarks;
  
  grades := o!.grades;
  gradingfunc := o!.gradingfunc;
  onlygrades := o!.onlygrades;
  onlygradesdata := o!.onlygradesdata;
  orbitgraph := o!.orbitgraph;
  nrgens:=Length(gens);
  
  if IsBoundGlobal("ORBC") then 
    htadd:=HTAdd_TreeHash_C;
    htvalue:=HTValue_TreeHash_C;
  else
    htadd:=HTAdd;
    htvalue:=HTValue;
  fi;
  
  # Maybe we are looking for something and it is the start point:
  while nr <= limit and i <= nr and i <> stopper do
    if i >= depthmarks[depth+1] then
      depth := depth + 1;
      depthmarks[depth+1] := nr+1;
    fi;
   
    logind[i]:=logpos;
    suc:=false;

    # Now apply generators:
    for j in genstoapply do
      yy := op(orb[i],gens[j]);
      pos := htvalue(ht,yy);
      if gradingfunc <> false then
        grade := gradingfunc(o,yy);
        if onlygrades <> false and 
          not(onlygrades(grade,onlygradesdata)) then
          pos := false;
        fi;
      fi;     

      if pos = fail then
        nr := nr + 1;
        orb[nr] := yy;
        if grades <> false then
          grades[nr] := grade;
        fi;

        htadd(ht,yy,nr);
          
        orbitgraph[nr] := EmptyPlist(nrgens);
        orbitgraph[i][j] := nr;

        # Handle Schreier tree:
        schreiergen[nr] := j;
        schreierpos[nr] := i;
                
        suc := true;
        log[logpos] := j;
        log[logpos+1] := nr;
        logpos := logpos+2;
        o!.logpos := logpos;    # write back to preserve
                
        # Are we looking for something?
        if looking and not found then
          if lookfunc(o,yy) then
            found:=true;
            o!.found := nr;
          fi;
        fi;
      elif pos <> false then    # false if point was rejected by grade
        orbitgraph[i][j]:=pos;
      fi;
    od;    
    # Now close the log for this point:
    if suc then
      log[logpos-2] := -log[logpos-2];
      if looking and found then i:=i+1; break; fi;
    else
      logind[i]:=0;
    fi;
    i := i + 1;
  od;
  o!.pos := i;
  o!.depth := depth;
  if i > nr then
    SetFilterObj(o, IsClosed); 
    o!.orbind := [1..nr];
  fi;
  return o;
end );

#

InstallMethod(EvaluateWord, "for bipartition coll and list of integers", 
[IsBipartitionCollection, IsList],
function ( gens, w )
    local  i, res, pts;
    if Length( w ) = 0  then
        return One(gens);
    fi;
    res := gens[AbsInt(w[1])]^SignInt(w[1]);
    for i  in [ 2 .. Length( w ) ]  do
        res := res * gens[AbsInt(w[i])]^SignInt(w[i]);
    od;
    return res;
end);

#

InstallMethod(EvaluateWord, "for partial perm coll and list of integers", 
[IsPartialPermCollection, IsList],
function ( gens, w )
    local  i, res, pts;
    if Length( w ) = 0  then
        return One(gens);
    fi;
    res := gens[AbsInt(w[1])]^SignInt(w[1]);
    for i  in [ 2 .. Length( w ) ]  do
        res := res * gens[AbsInt(w[i])]^SignInt(w[i]);
    od;
    return res;
end);

#

InstallMethod(EvaluateWord, 
"for Rees 0-matrix semigroup element collection and a list of positive integers", 
[IsReesZeroMatrixSemigroupElementCollection, IsList],
function ( gens, w )
    local  i, res, pts;
    if Length( w ) = 0  then
        return UniversalFakeOne;
    fi;
    res := gens[w[1]];
    for i  in [ 2 .. Length( w ) ]  do
        res := res * gens[w[i]];
    od;
    return res;
end);

#

InstallGlobalFunction(EnumeratePosition, 
"for an orbit, value, and boolean",
function(arg)
  local o, val, onlynew, pos;
  
  o:=arg[1]; val:=arg[2];
  if Length(arg)=3 then 
    onlynew:=arg[3];
  else
    onlynew:=false;
  fi;

  if not onlynew then 
    pos:=Position(o, val);
    if pos<>fail or IsClosed(o) then 
      return pos;
    fi;
  fi;
 
  if IsClosed(o) then 
    return fail;
  fi;
  o!.looking:=true;
  o!.lookingfor:=function(o, x) return x=val; end;
  o!.lookfunc:=o!.lookingfor;
  Enumerate(o);
  pos:=PositionOfFound(o);
  o!.found:=false;
  o!.looking:=false;
  Unbind(o!.lookingfor);
  Unbind(o!.lookfunc);
  if pos<>false then 
    return pos;
  fi;
  return fail;
end);

#

InstallGlobalFunction(LookForInOrb, 
"for an orbit, a function, and positive integer",
function(o, func, start)
  local pos, i;
 
  # not including this line means that when considering LambdaOrb(s) 
  # the first point is considered which it shouldn't be. Whatever is broken
  # when this line is not included should be fixed as at present this is not
  # consistent. JDM
  Enumerate(o, Length(o)+1);
  
  if start<=Length(o) then 
    for i in [start..Length(o)] do 
      if func(o, o[i]) then 
        return i;
      fi;
    od;
  fi;

  if IsClosed(o) then 
    return false;
  fi;
  
  o!.looking:=true;
  o!.lookingfor:=func;
  o!.lookfunc:=o!.lookingfor;
  Enumerate(o);
  pos:=PositionOfFound(o);
  o!.found:=false;
  o!.looking:=false;
  Unbind(o!.lookingfor);
  Unbind(o!.lookfunc);
  return pos;
end);

#

InstallGlobalFunction(OrbSCC,
function(o)
  local scc, r, i;

  if IsBound(o!.scc) then 
    return o!.scc;
  fi;

  if not IsClosed(o) or not IsClosedData(o) then 
    Enumerate(o, infinity);
  fi;

  scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(OrbitGraphAsSets(o)),
     Set));;
  r:=Length(scc);

  o!.scc:=scc;
  o!.scc_lookup:=ListWithIdenticalEntries(Length(o), 1);

  if r>1 then
    for i in [2..r] do
      o!.scc_lookup{scc[i]}:=ListWithIdenticalEntries(Length(scc[i]), i);
    od;
  fi;

  return scc;
end); 

#

InstallGlobalFunction(OrbSCCLookup, 
function(o)

  if IsBound(o!.scc_lookup) then 
    return o!.scc_lookup;
  fi;

  OrbSCC(o);
  return o!.scc_lookup;
end);

#

InstallGlobalFunction(OrbSCCTruthTable, 
function(o)
  local scc, r;

  if IsBound(o!.truth) then
    return o!.truth;
  fi;

  scc:=OrbSCC(o); r:=Length(scc);
  o!.truth:=List([1..r], i-> BlistList([1..Length(o)], scc[i]));
  return o!.truth;
end);

#

InstallGlobalFunction(ReverseSchreierTreeOfSCC,
function(o, i)
  local r, rev, graph, j, len, nrgens, genstoapply, scc, gen, pos, seen, lookup, oo, nroo, nrscc, k, l, m;

  r:=Length(OrbSCC(o));

  if i>r then 
    Error("the orbit only has ", r, " strongly connected components,");
    return;
  fi;

  if not IsBound(o!.reverse) then
    o!.reverse:=EmptyPlist(r);
  fi;
 
  if IsBound(o!.reverse[i]) then
    return o!.reverse[i];
  fi;
  
  if not IsBound(o!.rev) then
    o!.rev:=[];
  fi;
 
  # update o!.rev if necessary
  rev:=o!.rev;        graph:=OrbitGraph(o);
  j:=Length(rev);     len:=Length(graph);
 
  nrgens:=Length(o!.gens);
  genstoapply:=[1..nrgens];
  
  Append(rev, List([j+1..len], x-> List(genstoapply, x-> [])));

  while j<len do 
    j:=j+1; 
    for k in genstoapply do
      if IsBound(graph[j][k]) then
        Add(rev[graph[j][k]][k], j);
        #starting at position j and applying gens[k] we obtain graph[j][k];
      fi;
    od;
  od;

  #rev[i][j][k]:=l implies that o[l]^gens[j]=o[i]

  scc:=o!.scc[i]; 
  gen:=EmptyPlist(Length(o));
  pos:=EmptyPlist(Length(o));

  gen[scc[1]]:=fail; pos[scc[1]]:=fail;

  seen:=BlistList([1..Length(o)], [scc[1]]);
 
  lookup:=OrbSCCLookup(o);
  oo:=EmptyPlist(Length(scc));
  oo[1]:=scc[1]; j:=0; nroo:=1;
  nrscc:=Length(scc);  

  while nroo<nrscc do
    j:=j+1;
    k:=oo[j];
    l:=0;
    while l<nrgens and nroo<nrscc do
      l:=l+1;
      m:=0;
      len:=Length(rev[k][l]);
      while m<len and nroo<nrscc do
        m:=m+1;
        if not seen[rev[k][l][m]] and lookup[rev[k][l][m]]=i then
          Add(oo, rev[k][l][m]); nroo:=nroo+1;
          seen[rev[k][l][m]]:=true; gen[rev[k][l][m]]:=l; pos[rev[k][l][m]]:=k;
        fi;
      od;
    od;
  od;
  
  o!.reverse[i]:=[gen, pos];
  return [gen, pos];
end);

#

InstallGlobalFunction(SchreierTreeOfSCC,
function(o, i)
  local scc, len, gen, pos, seen, lookup, oo, m, graph, j, k, l, len_k;

  if not IsBound(o!.scc) then 
    OrbSCC(o);
  fi;

  if not IsBound(o!.trees) then
    o!.trees:=EmptyPlist(Length(o));
  fi;

  if IsBound(o!.trees[i]) then 
    return o!.trees[i];
  fi;

  if i=1 then
    o!.trees[i]:=[o!.schreiergen, o!.schreierpos];#JDM remove this line
    return o!.trees[i];
  fi;

  scc:=o!.scc[i]; len:=Length(o);

  #gen:=ListWithIdenticalEntries(len, fail);
  #pos:=ListWithIdenticalEntries(len, fail);
  gen:=EmptyPlist(len);
  pos:=EmptyPlist(len);
  gen[scc[1]]:=fail; pos[scc[1]]:=fail;

  seen:=BlistList([1..len], [scc[1]]);
  #JDM remove the use of truth table here
  #t:=OrbSCCTruthTable(o)[i];
  lookup:=OrbSCCLookup(o);
  oo:=[scc[1]]; m:=1;
  graph:=OrbitGraph(o);
  j:=0;
  len:=Length(scc);

  while m<len do
    j:=j+1; k:=oo[j]; l:=0; len_k:=Length(graph[k]);
    while l<len_k and m<len do
      l:=l+1;
      if IsBound(graph[k][l]) and not seen[graph[k][l]] 
       and lookup[graph[k][l]]=i then
        m:=m+1;
        oo[m]:=graph[k][l]; seen[graph[k][l]]:=true;
        gen[graph[k][l]]:=l; pos[graph[k][l]]:=k;
      fi;
    od;
  od;
  o!.trees[i]:=[gen, pos];

  return [gen, pos];
end);

# Usage: o = orbit of images; i = index of scc; j = element of scc[i].

# Notes: returns a word in the generators that takes o[j] to o!.scc[i][1]  
# assuming that j in scc[i]

InstallMethod(TraceSchreierTreeOfSCCBack,
"for an orbit and two positive integers",
[IsOrbit, IsPosInt, IsPosInt],
function(o, i, j)
  local tree, mult, scc, word;
  
  if not IsInverseOrb(o) then 
    tree:=ReverseSchreierTreeOfSCC(o, i);
    mult:=1;
  else 
    tree:=SchreierTreeOfSCC(o, i);
    mult:=-1;
  fi;
  
  scc:=OrbSCC(o)[i];

  word := [];
  while j > scc[1] do
    Add(word, tree[1][j]);
    j := tree[2][j];
  od;
  
  return word*mult;
end);

# Usage: o = orbit of images; i = index of scc; j = element of scc[i].

# Notes: returns a word in the generators that takes o!.scc[i][1] to o[j] 
# assuming that j in scc[i]

InstallMethod(TraceSchreierTreeOfSCCForward,
"for an orbit and two positive integers",
[IsOrbit, IsPosInt, IsPosInt],
function(o, i, j)
  local tree, scc, word;

  tree:=SchreierTreeOfSCC(o, i);
  scc:=OrbSCC(o)[i];

  word := [];
  while j > scc[1] do
    Add(word, tree[1][j]);
    j :=tree[2][j];
  od;
  return Reversed(word);
end);

#EOF
