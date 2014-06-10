#############################################################################
##
#W  semitrans.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains methods for every operation/attribute/property that is
# specific to transformation semigroups.

# 

InstallMethod(IsomorphismTransformationMonoid, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local id, dom, gens, inv;

  if IsMonoid(s) then 
    return MappingByFunction(s, s, IdFunc, IdFunc);
  fi;

  if MultiplicativeNeutralElement(s)=fail then 
    Error( "usage: <s> must have a multiplicative neutral element,");
    return;
  fi;
  
  id:=MultiplicativeNeutralElement(s);
  dom:=ImageSetOfTransformation(id);
  
  gens:=List(Generators(s), x-> TransformationOp(x, dom));

  inv:=function(f)
    local out, i;

    out:=[1..DegreeOfTransformationSemigroup(s)];
    for i in [1..Length(dom)] do 
      out[dom[i]]:=dom[i^f];
    od;
    return id*Transformation(out);
  end;

  return MappingByFunction(s, Monoid(gens), f-> TransformationOp(f, dom), 
   inv);
end);

# same method for ideals

InstallMethod(IsomorphismPermGroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
 
  if not IsGroupAsSemigroup(s)  then
   Error( "usage: a transformation semigroup satisfying IsGroupAsSemigroup,");
   return; 
  fi;

  return MagmaIsomorphismByFunctionsNC(s, 
   Group(List(GeneratorsOfSemigroup(s), PermutationOfImage)), 
    PermutationOfImage, 
    x-> AsTransformation(x, DegreeOfTransformationSemigroup(s)));
end);

# same method for ideals

InstallMethod(GroupOfUnits, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
  local r, g, deg, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g:=SchutzenbergerGroup(r);
  deg:=DegreeOfTransformationSemigroup(s);   
 
  u:=Monoid(List(GeneratorsOfGroup(g), x-> AsTransformation(x, deg)));
  
  SetIsomorphismPermGroup(u, MappingByFunction(u, g, PermutationOfImage, 
   x-> AsTransformation(x, deg)));
   
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

#

InstallMethod(IsTransformationSemigroupGreensClass, "for a Green's class",
[IsGreensClass], x-> IsTransformationSemigroup(Parent(x)));

#

ViewStringForGroupOfTransformations@:=function(s)
local str, nrgens;
  str:="\><";
  if HasIsTrivial(s) and IsTrivial(s) then
    Append(str, "\>trivial\< ");
  fi;

  Append(str, "\>transformation\< \>group\<");
  if HasIsTrivial(s) and not IsTrivial(s) and HasSize(s) 
   and Size(s)<2^64 then
    Append(str, " \>of size \>");
    Append(str, String(Size(s)));
    Append(str, ",\<\<");
  fi;

  nrgens:=Length(Generators(s));
  if DegreeOfTransformationSemigroup(s)>0 then  
    Append(str, " \>on \>");
    Append(str, ViewString(DegreeOfTransformationSemigroup(s)));
    Append(str, "\< pts");
  fi;
  if nrgens>0 then 
    Append(str, " with\> ");
    Append(str, ViewString(nrgens));
    Append(str, "\< generator");
    if nrgens>1 or nrgens=0 then
      Append(str, "s\<");
    else
      Append(str, "\<");
    fi;
  fi;
  Append(str, ">\<");

  return str;
end;
  
InstallMethod(ViewString, "for a group of transformations",
[IsTransformationSemigroup and IsGroupAsSemigroup],
ViewStringForGroupOfTransformations@);

InstallMethod(ViewString, "for a group of transformations",
[IsTransformationSemigroup and IsGroup],
ViewStringForGroupOfTransformations@);

Unbind(ViewStringForGroupOfTransformations@);

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
