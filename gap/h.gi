#############################################################################
##
#W  h.gi
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

# Notes
# 
# - H-class data is [unrectified image data, unrectified kernel data, image
# orbit coset rep, kernel orbit coset rep]

# TODO
# - Make sure all the description strings make sense and that they are uniform...

# - GroupHClassOfGreensDClass...


#############################################################################
# other equalities of Green's classes handled by generic method in greens.gi!


# new for 0.1! - \= - "for H-class and H-class of trans. semigp."
############################################################################

InstallMethod(\=, "for H-class and H-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp, IsGreensHClass and 
IsGreensClassOfTransSemigp],
function(h1, h2)
return h1!.parent=h2!.parent and h1!.rep in h2;
end);

# new for 0.1! - \< - "for H-class and H-class of trans. semigp."
############################################################################

InstallMethod(\<, "for H-class and H-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp, IsGreensHClass and 
IsGreensClassOfTransSemigp],
function(h1, h2)
return h1!.parent=h2!.parent and h1!.rep < h2!.rep;
end);

# new for 0.1! - \in - "for trans. and H-class of trans. semigp."
############################################################################

InstallOtherMethod(\in, "for trans. and H-class of trans. semigp.",
[IsTransformation, IsGreensHClass and IsGreensClassOfTransSemigp], 
function(f, h)
  local rep;

  rep:= h!.rep; 

  if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or 
   RankOfTransformation(f) <> RankOfTransformation(rep) or 
   CanonicalTransSameKernel(f) <> CanonicalTransSameKernel(rep) or 
   ImageSetOfTransformation(f) <> ImageSetOfTransformation(rep) then
    return false;
  fi;

  return PermLeftQuoTransformationNC(rep, f) in SchutzenbergerGroup(h);  
end);

#AAA

# new for 0.1! - AsSSortedList - "for H-class of trans. semigp."
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for H-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  Info(InfoCitrusGreens, 4, "AsSSortedList: for an H-class");
  return ConstantTimeAccessList(EnumeratorSorted(h));
end);

#CCC

# new for 0.1! - CreateHClass - not a user function
#############################################################################
# Usage: s = semigroup; data = [unrectified img data, unrectified ker data, 
# img coset rep, ker coset rep]; orbit = [OrbitsOfImages, OrbitsOfKernels];
# rep = H-class representative.

# Notes: data[4]=ker coset rep, should be () unless we are creating the H-class
# from an L-class, in which case data[3] should be the img coset rep used in the
# L-class data. 

InstallGlobalFunction(CreateHClass, 
function(s, data, orbit, rep)
  local d, h;

  data:=[data[1]{[1..6]}, data[2]{[1..6]}, data[3], data[4]];

  h:=Objectify(HClassType(s), rec(parent:=s, data:=data, 
  o:=orbit, rep:=rep));
  SetRepresentative(h, rep);
  SetEquivalenceClassRelation(h, GreensHRelation(s));
  return h;
end);

#DDD

# new for 0.1! - DClassOfHClass - for an H-class of a trans. semigroup
#############################################################################

InstallOtherMethod(DClassOfHClass, "for an H-class of a trans. semigroup",
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  local s, d, o, rep;

  s:=h!.parent; d:=h!.data; o:=h!.o;  
  d[1][3]:=ImageOrbitSCCFromData(s, d[1], o[1])[1]; 
  d[2][3]:=KernelOrbitSCCFromData(s, d[2], o[2])[1];
  rep:=DClassRepFromData(s, d, o);

  return CreateDClass(s, d, o, rep);
end);

#EEE

# new for 0.1! - Enumerator - "for H-class of trans. semigp."
#############################################################################

InstallOtherMethod(Enumerator, "for H-class of trans. semigp.", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  local enum;

  Info(InfoCitrusGreens, 4, "Enumerator: for an H-class");

  enum:=EnumeratorByFunctions(h, rec(
          
    schutz:=Enumerator(SchutzenbergerGroup(h)),
          
    #########################################################################
          
    ElementNumber:=function(enum, pos)
      if pos>Length(enum) then 
        return fail;
      fi;
      
      return h!.rep*enum!.schutz[pos];
    end, 
          
    #########################################################################
          
    NumberElement:=function(enum, f)
      local rep;
      rep:=h!.rep;

      if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
       RankOfTransformation(f) <> RankOfTransformation(rep) or
       ImageSetOfTransformation(f) <> ImageSetOfTransformation(rep) or
       CanonicalTransSameKernel(f) <> CanonicalTransSameKernel(rep) then
        return fail;
      fi;
      
      return Position(enum!.schutz, PermLeftQuoTransformationNC(rep, f));
    end, 

    ###########################################################################
    
    Membership:=function(elm, enum) 
      return elm in h; #the H-class itself!
    end,
    
    Length:=enum -> Size(h),

    PrintObj:=function(enum)
      Print( "<enumerator of H-class>");
      return;
    end));

  return enum;
end);

#GGG

# new for 0.1! - GreensHClasses - "for a transformation semigroup"
##############################################################################
# JDM move to greens.gi

InstallMethod(GreensHClasses, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter, out, i, h;

  Info(InfoCitrusGreens, 4, "GreensHClasses");

  iter:=IteratorOfGreensHClasses(s);
  out:=EmptyPlist(NrGreensHClasses(s));
  i:=0;

  for h in iter do 
    i:=i+1;
    out[i]:=h;
  od;

  return out;
end);

# new for 0.1! - GreensHClassOfElement - "for a trans. semigp. and trans."
############################################################################

InstallMethod(GreensHClassOfElement, "for a trans. semigp. and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation],
function(s, f)
  local d, l_img, l_ker;

  Info(InfoCitrusGreens, 4, "GreensHClassOfElement");

  if not f in s then 
    Info(InfoWarning, 1, "transformation is not an element of the semigroup");
    return fail;
  fi;

  d:=PreInOrbitsOfKernels(s, f, false); l_img:=d[1][2][3]; l_ker:=d[2][2][3];

  if not d[2][1] then #D-class containing f not previously calculated
    d:=[d[1][2], d[2][2]];
    d[1][3]:=ImageOrbitSCCFromData(s, d[1])[1];
    if not l_ker=fail then
      d[2][3]:=KernelOrbitSCCFromData(s, d[2])[1];
    fi;
    d:=StructuralCopy(AddToOrbitsOfKernels(s, d[1][7], d)); 
    d[1][3]:=l_img; 
    if not l_ker=fail then 
      d[2][3]:=l_ker; 
    fi;  
    d[3]:=(); d[4]:=(); 
  else
    d:=[d[1][2], d[2][2], ImageOrbitCosetsFromData(s, d[2][2])[d[2][2][8]],
          ()];
  fi;

  return CreateHClass(s, d, [OrbitsOfImages(s), OrbitsOfKernels(s)], 
   HClassRepFromData(s, d));
end);

# new for 0.1! - GreensHClassOfElementNC - "for a trans. semigp and trans."
#############################################################################

InstallOtherMethod(GreensHClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation],
function(s, f)
  local n, d, j, img_o, ker_o, kernels;

  Info(InfoCitrusGreens, 4, "GreensHClassOfElementNC");

  n:=DegreeOfTransformationSemigroup(s);

  if not DegreeOfTransformation(f)=n then
    Info(InfoWarning, 1, "Usage: trans. semigroup and trans. of equal degree");
    return fail;
  fi;

  d:=PreInOrbitsOfKernels(s, f, false);

  if d[1][1] then 
    Info(InfoCitrusGreens, 2, "transformation is an element of the semigroup");
    #JDM somewhat inefficient as we run PreInOrbitsOfKernels twice!
    return GreensHClassOfElement(s, f);
  elif OrbitsOfImages(s)!.finished then #f not in s!
    Info(InfoCitrusGreens, 2, "transformation is not an element of the ",
     "semigroup");
    return fail;
  fi;

  Info(InfoCitrusGreens, 2, "transformation may not be an element of the ",
  "semigroup");

  # JDM not really sure what the point of doing the following is!

  j:=Length(ImageSetOfTransformation(f));

  Info(InfoCitrusGreens, 2, "finding orbit of image...");
  img_o:=[]; img_o[j]:=[ForwardOrbitOfImage(s, f)[1]];
  #JDM see comments in GreensDClassOfElementNC
  img_o:=rec( finished:=false, orbits:=img_o, gens:=Generators(s), s:=s,
   deg := n, data:=[[j,1,1,1,1,1]], images:=fail, lens:=List([1..n],
   function(x) if x=j then return 1; else return 0; fi; end),
   data_ht:=HTCreate([1,1,1,1,1,1]));

  Info(InfoCitrusGreens, 2, "finding orbit of kernel...");
  ker_o:=[]; ker_o[j]:=[ForwardOrbitOfKernel(s, f)];
  ker_o:=rec( orbits:=ker_o, gens:=Generators(s), data:=[[[j,1,1,1,1,1],
   [j,1,1,1,1,1]]], kernels:=fail, data_ht:=HTCreate([1,1,1,1,1,1]));
 
  Info(InfoCitrusGreens, 2, "finding the kernel orbit schutz. group");
  Add(ker_o!.orbits[j][1]!.d_schutz[1], [CreateSchutzGpOfDClass(s,
   [[j,1,1,1,1,1],[j,1,1,1,1,1]], [img_o, ker_o])]);
  HTAdd(ker_o!.data_ht, [j,1,1,1,1,1], 1);

  return CreateHClass(s, [[j,1,1,1,1,1], [j,1,1,1,1,1], (), ()], 
   [img_o, ker_o], f);
end);

# new for 0.1! - GroupHClass - "for a D-class of a trans. semigp."
############################################################################
# JDM move to d.gi!

InstallMethod(GroupHClass, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local s, data, o, n, m, ker, scc, lookup, f, h, i, j;

  if HasIsRegularDClass(d) and not IsRegularDClass(d) then 
    return fail;
  fi;
  
  s:=d!.parent; data:=d!.data; o:=d!.o;

  if NrIdempotentsRClassFromData(s, data[1], o[1])=0 then
    return fail;
  fi;

  n:=DegreeOfTransformationSemigroup(s);
  m:=RankOfTransformation(d!.rep);       

  if m=n then
    return GreensHClassOfElementNC(s, [TransformationNC([1..n])]);
  fi;

  ker:=KernelOrbit(d)[1]; o:=ImageOrbit(d); scc:=ImageOrbitSCC(d);

  for i in scc do
    if IsInjectiveTransOnList(ker, o[i]) then 
      lookup:=EmptyPlist(n);
      for j in [1..m] do
        lookup[ker[o[i][j]]]:=o[i][j];
      od;

      f:=TransformationNC(List(ker, x-> lookup[x]));
      h:=GreensHClassOfElementNC(s, f);
      SetIsGroupHClass(h, true);
      SetIdempotents(h, [f]);
      return h;
    fi;
  od;

  return fail;
end);

# new for 0.1! - GroupHClassOfGreensDClass - "for D-class of trans. semigp."
############################################################################
# move to d.gi! JDM

InstallMethod(GroupHClassOfGreensDClass, "for D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], d-> GroupHClass(d));

#III 

# new for 0.1! - Idempotents - "for an H-class of a trans. semigp."
############################################################################

InstallOtherMethod(Idempotents, "for an H-class of a trans. semigp.", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  local f, img, ker, lookup, i;

  if IsGroupHClass(h) then 
    f:=h!.rep; img:=ImageSetOfTransformation(f);
    ker:=CanonicalTransSameKernel(f);
    lookup:=EmptyPlist(Length(ker)); #degree

    for i in [1..Length(img)] do #rank
      lookup[ker[img[i]]]:=img[i];
    od;

    return [TransformationNC(List(ker, x-> lookup[x]))];
  fi;

  return [];
end);

# new for 0.1! - IsGroupHClass - "for an H-class of a trans. semigp."
############################################################################
# install methods for other types of Green's classes...

InstallOtherMethod(IsGroupHClass, "for an H-class of a trans. semigp.", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  local f;
  f:=h!.rep;
  return IsInjectiveTransOnList(f, ImageSetOfTransformation(f));
end);

# new for 0.1! - IsomorphismPermGroup - "for H-class of trans. semigp."
###########################################################################

InstallOtherMethod(IsomorphismPermGroup, "for H-class of trans. semigp.", 
[IsGreensHClass and IsGreensClassOfTransSemigp],
function(h)
  local g, f;

  if not IsGroupHClass(h) then
    Info(InfoWarning, 1, "H-class is not a group");
    return fail;
  fi;
  
  g:=Group(());

  for f in Enumerator(h) do 
    g:=ClosureGroup(g, AsPermutation(f));
  od;

  return MappingByFunction(h, g, AsPermutation);
end);

# new for 0.1! - IteratorOfGreensHClasses - "for a transformation semigroup"
############################################################################
# move to greens.gi

InstallMethod(IteratorOfGreensHClasses, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
local iter;

  if not IsTransformationSemigroup(s) then
    Info(InfoWarning, 1, "Usage: arg. should be a transformation semigroup.");
    return fail;
  fi;

  Info(InfoCitrusGreens, 4, "IteratorOfGreensHClasses");

  iter:=IteratorByFunctions( rec(

    data:=IteratorOfHClassRepsData(s), s:=s, 
    
    IsDoneIterator := iter -> IsDoneIterator(iter!.data), 
    
    NextIterator:= function(iter)
      local d;
    
      d:=NextIterator(iter!.data);
    
      if d=fail then 
        return fail;
      fi;
    
      return CreateHClass(s, d, [OrbitsOfImages(s), OrbitsOfKernels(s)], 
       HClassRepFromData(s, d));;
    end,

    ShallowCopy:=iter-> rec(data:=IteratorOfHClassRepsData(s))));

  SetIsIteratorOfGreensHClasses(iter, true);
  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

# new for 0.1! - IteratorOfHClassReps - user function!
############################################################################

InstallGlobalFunction(IteratorOfHClassReps,
function(s)
  local iter;

  Info(InfoCitrusGreens, 4, "IteratorOfHClassReps");
  if not IsTransformationSemigroup(s) then
    Info(InfoWarning, 1, "Usage: argument should be a transformation",
     " semigroup");
    return fail;
  fi;

  iter:=IteratorByFunctions( rec(

    s:=s, data:=IteratorOfHClassRepsData(s),
	
    IsDoneIterator := iter-> IsDoneIterator(iter!.data),
	
    NextIterator := function(iter)
      if not IsDoneIterator(iter!.data) then 
	return HClassRepFromData(iter!.s, NextIterator(iter!.data));
      fi;
      return fail; 
    end,
	
    ShallowCopy := iter -> rec( data:=IteratorOfHClassRepsData(iter!.s))));

  SetIsIteratorOfHClassReps(iter, true);
  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

# new for 0.1! - IteratorOfHClassRepsData - not a user function
############################################################################

InstallGlobalFunction(IteratorOfHClassRepsData, 
function(s)
local iter;

Info(InfoCitrusGreens, 4, "IteratorOfHClassRepsData");

iter:=IteratorByFunctions( rec(
	
    i:=0, s:=s, 
	
    r:=IteratorOfRClassRepsData(s),
	
    data:=[],
    
    IsDoneIterator := iter -> IsDoneIterator(iter!.r) and 
     iter!.i>=Length(iter!.data), 
      
    NextIterator:= function(iter)
    local i;
    
      if IsDoneIterator(iter) then 
              return fail;
      fi;
      
      iter!.i:=iter!.i+1;
      i:=iter!.i;
      
      if i<=Length(iter!.data) then 
              return iter!.data[i];
      fi;
      
      iter!.data:=GreensHClassRepsDataFromData(s, NextIterator(iter!.r),
       OrbitsOfImages(s));
      iter!.i:=1;
      
      return iter!.data[1];
    end,

    ShallowCopy:=iter-> rec(i:=0, r:=IteratorOfGreensRClasses(s), 
     data:=[])));

  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

#HHH

# new for 0.1! - HClassRepFromData - not a user function!
############################################################################
# Usage: s = semigroup; 
# d = [image data, kernel data, image cosets rep, kernel cosets rep];
# o = [OrbitsOfImages, OrbitsOfKernels] (optional).
  
InstallGlobalFunction(HClassRepFromData,
function(arg)
  local s, d, o, f, perms, rels;
  
  s:=arg[1]; d:=arg[2];
  if Length(arg)=3 then 
    o:=arg[3];
   else
     o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
  fi;
 
  perms:=ImageOrbitPermsFromData(s, d[1], o[1]);
  
  if not d[4]=fail then #created from an L-class
    f:=LClassRepFromData(s, d, o);
    rels:=KernelOrbitRelsFromData(s, d[2], o[2]);
    return rels[d[2][3]][1]*f*d[4];
  fi;
  
  f:=RClassRepFromData(s, d[1], o[1]);
  return f*(d[3]/perms[d[1][3]]);
end);

# new for 0.1! - HClassClassType - not a user function!
############################################################################

InstallMethod(HClassType, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s);
 
 return NewType( FamilyObj( s ), IsEquivalenceClass and 
  IsEquivalenceClassDefaultRep and IsGreensHClass and 
  IsGreensClassOfTransSemigp);
end);

#LLL

# new for 0.1! - LClassOfHClass - "for an H-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(LClassOfHClass, "for an H-class of a trans. semigroup", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  local s, d, o, rep;

  s:=h!.parent; d:=h!.data; o:=h!.o;
  d[2][3]:=KernelOrbitSCCFromData(s, d[2], o[2])[1];
  rep:=LClassRepFromData(s, d, o);

  return CreateLClass(s, d, o, rep);
end);

#RRR

# new for 0.1! - RClassOfHClass - "for an H-class of a trans. semigroup"
#############################################################################
# JDM this is not correct! the R-class of the H-class corresponds to d[1] and a
# kernel orbit coset. The kernel orbit coset is not represented here. This
# should require a change in d[1][6]! 

InstallOtherMethod(RClassOfHClass, "for an H-class of a trans. semigroup", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  local s, d, o, rep;

  s:=h!.parent; d:=h!.data; o:=h!.o;
  d[1][3]:=ImageOrbitSCCFromData(s, d[1], o[1])[1];
  rep:=RClassRepFromData(s, d[1], o[1]);

  return CreateRClass(s, d[1], o[1], rep);
end);

#PPP

# new for 0.1! - ParentAttr - "for H-class of a trans. semigroup"
############################################################################

InstallMethod(ParentAttr, "for H-class of a trans. semigroup", 
[IsGreensHClass and IsGreensClassOfTransSemigp], x-> x!.parent);


# new for 0.1! - PrintObj - IsIteratorOfHClassReps
############################################################################
# JDM move to greens.gi?
InstallMethod(PrintObj, [IsIteratorOfHClassReps], 
function(iter)

Print( "<iterator of H-class reps>");
return;
end);

# new for 0.1! - PrintObj - IsIteratorOfGreensHClasses
############################################################################
# JDM move to greens.gi.

InstallMethod(PrintObj, [IsIteratorOfGreensHClasses], 
function(iter)
  Print( "<iterator of H-classes>");
  return;
end);

#SSS

# new for 0.1! - SchutzenbergerGroup - "for an H-class of a trans. semigp."
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for an H-class of a trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  local s, d, o, perms;

  s:=h!.parent; d:=h!.data; o:=h!.o;
  perms:=ImageOrbitPermsFromData(s, d[1], o[1]);

  return DClassSchutzGpFromData(s, d[2], o[2])^(d[3]/perms[d[1][3]]);
end);

# new for 0.1! - Size - "for an H-class of a trans. semigp."
############################################################################

InstallMethod(Size, "for an H-class of a trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp],
  h-> Size(DClassSchutzGpFromData(h!.parent, h!.data[2], h!.o[2])));

# new for 0.1! - StructureDescription - "for group H-class of trans. semigp."
############################################################################

InstallOtherMethod(StructureDescription, "for group H-class of trans. semigp.",
[IsGreensHClass and IsGreensClassOfTransSemigp and IsGroupHClass],
h-> StructureDescription(Range(IsomorphismPermGroup(h))));

#EOF
