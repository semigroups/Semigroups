############################################################################
##
#W  ideals-lambda-rho.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#InstallMethod(Enumerate, "for an ideal lambda orb", 
#[IsIdealLambdaOrb], 
#function(o, limit, lookfunc)
#end);

#

InstallMethod(Length, "for a ideal orb", 
[IsIdealOrb],
function(o)
  return Sum(o!.lens);
end);

#

InstallMethod(IsBound\[\], "for an ideal orb and positive integer",
[IsIdealOrb, IsPosInt], 
function(o, i)
  local nr;

  nr:=1;
  while IsBound(o!.orbits[nr]) and i>Length(o!.orbits[nr]) do 
    i:=i-Length(o!.orbits[nr]);
    nr:=nr+1;
  od;
  return IsBound(o!.orbits[nr]) and IsBound(o!.orbits[nr][i]);
end);

#

InstallMethod(ELM_LIST, "for an ideal orb and positive integer",
[IsIdealOrb, IsPosInt], 
function(o, i)
  local nr;

  nr:=1;
  while i>Length(o!.orbits[nr]) do 
    i:=i-Length(o!.orbits[nr]);
    nr:=nr+1;
  od;
  return o!.orbits[nr][i];
end);

#

InstallMethod(\in, "for an object and ideal orb",
[IsObject, IsIdealOrb],
function(obj, o)
  return HTValue(o!.ht, obj)<>fail;
end);

#

InstallMethod(Position, "for an ideal orb, object, zero cyc",
[IsIdealOrb, IsObject, IsZeroCyc],
function(o, obj, n)
  return HTValue(o!.ht, obj);
end);

#

InstallMethod(OrbitGraph, "for an ideal orb",
[IsIdealOrb],
function(o)
  return o!.orbitgraph;
end);

#

InstallMethod(ViewObj, "for a ideal orb", 
[IsIdealOrb],
function(o)
  Print("<");
  if IsClosed(o) then 
    Print("closed ");
  else
    Print("open ");
  fi;
  Print("ideal ");
  if IsIdealLambdaOrb(o) then 
    Print("lambda ");
  else
    Print("rho ");
  fi;
  
  Print("orbit with ", Length(o)-1, " points in ", Length(o!.orbits)-1, 
  " components>");
  return;
end);

#

InstallMethod(LambdaOrb, "for an acting semigroup ideal", 
[IsActingSemigroup and IsSemigroupIdeal],
function(I)
  local record, htopts, fam;
  
  record:=rec();
  record.orbits:=[[fail]];      record.lens:=[1];    
  record.parent:=I;             record.scc:=[[1]];       
  record.scc_reps:=[fail,];     record.scc_lookup:=[1];
  record.schreiergen:=[fail];   record.schreierpos:=[fail];
  record.orbitgraph:=[[]];      record.gens:=GeneratorsOfSemigroup(Parent(I));
  record.orbschreierpos := [];
  record.orbschreiergen := [];
  record.orbtogen := [];
  
  htopts:=ShallowCopy(LambdaOrbOpts(I)); 
  htopts.treehashsize:=I!.opts.hashlen.M;
  record.ht:=HTCreate(LambdaFunc(I)(Representative(I)), htopts);
  
  fam:=CollectionsFamily(FamilyObj(LambdaFunc(I)(Representative(I))));
  return Objectify(NewType(fam, IsIdealLambdaOrb), record);
end);

# assumes that <pt> is not in <o> already...

InstallGlobalFunction(UpdateIdealLambdaOrb, 
function(o, pt, x, pos, gen, ind)
  local I, record, len, new, ht, i, nrorb;

  I:=o!.parent; 
  record:=ShallowCopy(LambdaOrbOpts(I));
  
  record.schreier:=true;        record.orbitgraph:=true;
  record.storenumbers:=true;    record.log:=true;
  record.parent:=I;             record.treehashsize:=I!.opts.hashlen.M;
 
  len:=Length(o);

  if len<>0 then 
    record.gradingfunc:=function(new, x)
      return x in o;
    end;
    record.onlygrades:=function(x, data);
      return not x;
    end;
    record.onlygradesdata:=fail;
  fi;

  new:=Orb(GeneratorsOfSemigroup(Parent(I)), pt, LambdaAct(I), record);
  Enumerate(new);
  
  ht:=o!.ht;
  for i in [1..Length(new)] do 
    HTAdd(ht, new[i], i+len);
  od;
  
  o!.scc_reps[Length(o!.scc)+1]:=x;
  
  # JDM probably don't store these things in <o> since they are already in <new>
  # or remove them from the individual orbits...
  Append(o!.scc_lookup, OrbSCCLookup(new)+Length(o!.scc));
  Append(o!.scc, OrbSCC(new)+len);  
  Append(o!.schreiergen, new!.schreiergen);
  Add(o!.schreierpos, fail);
  for i in [2..Length(new)] do 
    Add(o!.schreierpos, new!.schreierpos[i]+len);
  od;
  Append(o!.orbitgraph, new!.orbitgraph+len);

  o!.orbits[Length(o!.orbits)+1]:=new;
  o!.lens[Length(o!.orbits)]:=Length(new);

  nrorb := Length(o!.orbits);  
  o!.orbschreierpos[nrorb] := pos;
  o!.orbschreiergen[nrorb] := gen;

  # jj assume that if a generator is passed into UpadateIdealLambdaOrb then
  # pos = the index of the generator and ind <> fail.
  if ind <> fail then
    o!.orbtogen[nrorb] := ind;
  fi;
  return len+1;
end);

#

InstallMethod(RhoOrb, "for an acting semigroup ideal", 
[IsActingSemigroup and IsSemigroupIdeal],
function(I)
  local record, htopts, fam;
 
  record:=rec();
  record.orbits:=[[fail]];      record.lens:=[1];    
  record.parent:=I;             record.scc:=[[1]];       
  record.scc_reps:=[fail,];     record.scc_lookup:=[1];
  record.schreiergen:=[fail];   record.schreierpos:=[fail];
  record.orbitgraph:=[[]];      record.gens:=GeneratorsOfSemigroup(Parent(I));
  record.orbschreierpos := [];
  record.orbschreiergen := [];
  record.orbtogen := [];

  htopts:=ShallowCopy(RhoOrbOpts(I)); 
  htopts.treehashsize:=I!.opts.hashlen.M;
  record.ht:=HTCreate(RhoFunc(I)(Representative(I)), htopts);
  
  fam:=CollectionsFamily(FamilyObj(RhoFunc(I)(Representative(I))));
  return Objectify(NewType(fam, IsIdealRhoOrb), record);
end);

# assumes that <pt> is not in <o> already...

InstallGlobalFunction(UpdateIdealRhoOrb, 
function(o, pt, x, pos, gen, ind)
  local I, record, len, new, ht, i, nrorb;

  I:=o!.parent; 
  record:=ShallowCopy(RhoOrbOpts(I));
  
  record.schreier:=true;        record.orbitgraph:=true;
  record.storenumbers:=true;    record.log:=true;
  record.parent:=I;             record.treehashsize:=I!.opts.hashlen.M;
 
  len:=Length(o);

  if len<>0 then 
    record.gradingfunc:=function(new, x)
      return x in o;
    end;
    record.onlygrades:=function(x, data);
      return not x;
    end;
    record.onlygradesdata:=fail;
  fi;

  new:=Orb(GeneratorsOfSemigroup(Parent(I)), pt, RhoAct(I), record);
  Enumerate(new);
  
  ht:=o!.ht;
  for i in [1..Length(new)] do 
    HTAdd(ht, new[i], i+len);
  od;
  
  o!.scc_reps[Length(o!.scc)+1]:=x;
  
  # JDM probably don't store these things in <o> since they are already in <new>
  # or remove them from the individual orbits...
  Append(o!.scc_lookup, OrbSCCLookup(new)+Length(o!.scc));
  Append(o!.scc, OrbSCC(new)+len);  
  Append(o!.schreiergen, new!.schreiergen);
  Add(o!.schreierpos, fail);
  for i in [2..Length(new)] do 
    Add(o!.schreierpos, new!.schreierpos[i]+len);
  od;
  Append(o!.orbitgraph, new!.orbitgraph+len);

  o!.orbits[Length(o!.orbits)+1]:=new;
  o!.lens[Length(o!.orbits)]:=Length(new);

  nrorb := Length(o!.orbits);  
  o!.orbschreierpos[nrorb] := pos;
  o!.orbschreiergen[nrorb] := gen;

  # jj assume that if a generator is passed into UpadateIdealRhoOrb then
  # pos = the index of the generator and ind<>fail.
  if ind<>fail then
    o!.orbtogen[nrorb] := ind;
  fi;
  return len+1;
end);

#

InstallMethod(EvaluateWord, 
"for a semigroup ideal and a triple of words (Semigroups)",
[IsSemigroupIdeal, IsList],
function(I, w)
    local res, gens, i;

    gens:=GeneratorsOfSemigroup(Parent(I));
    res:=GeneratorsOfSemigroupIdeal(I)[w[2]];
    
    for i in [1..Length(w[1])] do
      res:=gens[w[1][i]]*res;
    od;
    for i in [1..Length(w[3])] do
      res:=res*gens[w[3][i]];
    od;
    return res;
  end );

#

InstallMethod(EvaluateWord, 
"for a semigroup and a words (Semigroups)",
[IsSemigroup, IsList], 1, # to beat the methods for IsXCollection
function(S, w)
  return EvaluateWord(GeneratorsOfSemigroup(S), w);
end);

#

InstallMethod(TraceSchreierTreeForward,
"for an ideal orbit and positive integer",
[IsIdealOrb, IsPosInt],
function(o, i)
  local orbschreierpos, orbschreiergen, schreiergen, schreierpos,
   leftword, rightword, nr, j;

  orbschreierpos := o!.orbschreierpos;
  orbschreiergen := o!.orbschreiergen;
  
  schreierpos := o!.schreierpos;
  schreiergen := o!.schreiergen;
   
  leftword := [];
  rightword := [];
  nr:=1;
  j:=i;
  while j>Length(o!.orbits[nr]) do 
    j:=j-Length(o!.orbits[nr]);
    nr:=nr+1;
  od;

  repeat 

    while schreierpos[i] <> fail do
      Add(rightword, schreiergen[i]);
      i := schreierpos[i];
    od;

    if orbschreiergen[nr] = fail then
      break;
    fi;

    i := orbschreierpos[nr];
    Add(leftword, orbschreiergen[nr]);

    while j>Length(o!.orbits[nr]) do 
      j:=j-Length(o!.orbits[nr]);
      nr:=nr+1;
    od;

  until orbschreiergen[nr]=fail;

  return [Reversed(leftword), o!.orbtogen[nr], Reversed(rightword)];
end);

# the first position of the returned word refers to the generators of the ideal
# corresponding to the position in the orbit of the point from which the <o[pos]>
# is obtained. For example, [1,2,3] means I.1*S.2*S.3.

#InstallMethod( TraceSchreierTreeForward, 
#"for an ideal orb and a position (Semigroups)",
#  [ IsIdealOrb, IsPosInt ],
#  function( o, pos )
#    local word;
#    word := [];
#    while o!.schreierpos[pos] <> fail do
#        Add(word,o!.schreiergen[pos]);
#        pos := o!.schreierpos[pos];
#    od;
#    Add(word, o!.genslookup[pos]);
#    return Reversed(word);
#  end );
#
##
#
#
##Usage: o = orbit of images; i = index of scc; j = element of scc[i].
#
## Notes: returns a word in the generators that takes o!.scc[i][1] to o[j] 
## assuming that j in scc[i]
#
#InstallMethod(TraceSchreierTreeOfSCCForward,
#"for an ideal orbit and two positive integers",
#[IsIdealOrb, IsPosInt, IsPosInt],
#function(o, i, j)
#  local tree, scc, word, parent;
#
#  tree:=SchreierTreeOfSCC(o, i);
#  scc:=OrbSCC(o)[i];
#
#  word := [];
#  parent := tree[2][j];
#  while parent  <> fail do
#    Add(word, tree[1][j]);
#    j := parent;
#    parent := tree[2][j];
#  od;
#  
#  Add(word, o!.genslookup[j]);
#  return Reversed(word);
#end);
#
#
#InstallMethod(TraceSchreierTreeOfSCCBack,
#"for an ideal orbit and two positive integers",
#[IsIdealOrb, IsPosInt, IsPosInt],
#function(o, i, j)
#  local tree, mult, scc, word, parent;
#
#  if not IsInvLambdaOrb(o) then
#    tree:=ReverseSchreierTreeOfSCC(o, i);
#    mult:=1;
#  else
#    tree:=SchreierTreeOfSCC(o, i);
#    mult:=-1;
#  fi;
#
#  scc:=OrbSCC(o)[i];
#
#  word := [];
#  parent := tree[2][j];
#  while parent <> fail do
#    Add(word, tree[1][j]);
#    j := parent;
#    parent := tree[2][j];
#  od;
#
#  return word*mult;
#end);
#
#
#
