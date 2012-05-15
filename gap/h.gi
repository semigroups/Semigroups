#############################################################################
##
#W  h.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Notes
# 
# - H-class data is [unrectified image data, unrectified kernel data, image
# orbit coset rep, kernel orbit coset rep]

# TODO
# - Make sure all the description strings make sense and that they are uniform...

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
    Info(InfoCitrus, 1, "degree, rank, kernel or image not equal to those of",
        " any of the H-class elements,");
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
  Info(InfoCitrus, 4, "AsSSortedList: for an H-class");
  return ConstantTimeAccessList(EnumeratorSorted(h));
end);

#CCC

# new for 0.1! - CreateHClass - not a user function
#############################################################################
# Usage: s = semigroup; data = [unrectified img data, unrectified ker data, 
# img coset rep, ker coset rep]; orbit = [OrbitsOfImages, OrbitsOfKernels];
# rep = H-class representative.

# Notes: data[4]=ker coset rep, should be fail unless we are creating the
# H-class from an L-class, in which case data[3] should be the img coset rep
# used in the L-class data. Also note that an H-class created from an L-class
# doesn't know anything a priori about the R-class containing it. 

InstallGlobalFunction(CreateHClass, 
function(s, data, orbit, rep)
  local d, h;

  #data:=[data[1]{[1..6]}, data[2]{[1..6]}, data[3], data[4]];

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

  Info(InfoCitrus, 4, "Enumerator: for an H-class");

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

# new for 0.1! - GreensHClassOfElement - "for a trans. semigp. and trans."
############################################################################

InstallMethod(GreensHClassOfElement, "for a trans. semigp. and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation],
function(s, f)
  local d, l_img, l_ker;

  Info(InfoCitrus, 4, "GreensHClassOfElement");

  if not f in s then 
    Error("transformation is not an element of the semigroup,");
    return;
  fi;

  d:=PreInOrbitsOfKernels(s, f, false); l_img:=d[1][2][3]; l_ker:=d[2][2][3];

  if not d[2][1] then #D-class containing f not previously calculated
    d:=[d[1][2], d[2][2]];
    d[1][3]:=ImageOrbitSCCFromData(s, d[1])[1];
    if not l_ker=fail then
      d[2][3]:=KernelOrbitSCCFromData(s, d[2])[1];
    fi;
    d:=StructuralCopy(AddToOrbitsOfKernels(s, TransformationNC(d[1][7]), d)); 
    d[1][3]:=l_img; 
    if not l_ker=fail then #JDM_hack!
      d[2][3]:=l_ker; 
    fi;  
    d[3]:=(); d[4]:=fail; 
  else
    d:=[d[1][2], d[2][2], ImageOrbitCosetsFromData(s, d[2][2])[d[2][2][8]],
          fail];
  fi;

  return CreateHClass(s, [d[1]{[1..6]}, d[2]{[1..6]}, d[3], d[4]],
  [OrbitsOfImages(s), OrbitsOfKernels(s)], HClassRepFromData(s, d));
end);

# new for 0.5! - GreensHClassOfElement - "for a Green's class and trans."
############################################################################

InstallOtherMethod(GreensHClassOfElement, "for Green's class and trans.", 
[IsGreensClassOfTransSemigp, IsTransformation],
function(class, f)
  
  if not f in class then 
    Error("the transformation is not an element of the Green's class,");
    return;
  fi;

  return GreensHClassOfElementNC(class, f);
end);

# new for 0.5! - GreensHClassOfElementNC - "for an H-class and trans."
############################################################################

InstallOtherMethod(GreensHClassOfElementNC, "for an H-class and trans.", 
[IsGreensHClass and IsGreensClassOfTransSemigp, IsTransformation],
function(h, f)
  return h;
end);

# new for 0.1! - GreensHClassOfElementNC - "for a trans. semigp and trans."
#############################################################################

InstallOtherMethod(GreensHClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation],
function(s, f)
  local n, d, j, img_o, ker_o, kernels;

  Info(InfoCitrus, 4, "GreensHClassOfElementNC");

  n:=DegreeOfTransformationSemigroup(s);

  if not DegreeOfTransformation(f)=n then
    Error("Usage: trans. semigroup and trans. of equal degree,");
    return;
  fi;

  d:=PreInOrbitsOfKernels(s, f, false);

  if d[1][1] then 
    Info(InfoCitrus, 2, "transformation is an element of the semigroup");
    #JDM somewhat inefficient as we run PreInOrbitsOfKernels twice!
    return GreensHClassOfElement(s, f);
  elif OrbitsOfImages(s)!.finished then #f not in s!
    Error("transformation is not an element of the semigroup,");
    return;
  fi;

  Info(InfoCitrus, 2, "transformation may not be an element of the ",
  "semigroup");

  # JDM not really sure what the point of doing the following is!

  j:=Length(ImageSetOfTransformation(f));

  Info(InfoCitrus, 3, "finding orbit of image...");
  img_o:=[]; img_o[j]:=[ForwardOrbitOfImage(s, f![1])];
  #JDM see comments in GreensDClassOfElementNC
  img_o:=rec( finished:=false, orbits:=img_o, gens:=Generators(s), s:=s,
   deg := n, data:=[[j,1,1,1,1,1]], images:=fail, lens:=List([1..n],
   function(x) if x=j then return 1; else return 0; fi; end),
   data_ht:=HTCreate([1,1,1,1,1,1], rec(hashlen:=s!.opts!.hashlen!.M)));

  Info(InfoCitrus, 3, "finding orbit of kernel...");
  ker_o:=[]; ker_o[j]:=[ForwardOrbitOfKernel(s, f)];
  ker_o:=rec( orbits:=ker_o, gens:=Generators(s), data:=[[[j,1,1,1,1,1],
   [j,1,1,1,1,1]]], kernels:=fail, data_ht:=HTCreate([1,1,1,1,1,1],
   rec(hashlen:=s!.opts!.hashlen!.S)));
 
  Info(InfoCitrus, 2, "finding the kernel orbit schutz. group");
  Add(ker_o!.orbits[j][1]!.d_schutz[1], [CreateSchutzGpOfDClass(s,
   [[j,1,1,1,1,1],[j,1,1,1,1,1]], [img_o, ker_o])]);
  HTAdd(ker_o!.data_ht, [j,1,1,1,1,1], 1);

  return CreateHClass(s, [[j,1,1,1,1,1], [j,1,1,1,1,1], (), ()], 
   [img_o, ker_o], f);
end);


# new for 0.1! - roupHClass - "for a D-class of a trans. semigp."
############################################################################
# JDM move to d.gi!

InstallMethod(GroupHClass, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local s, data, o, n, m, ker, scc, lookup, f, h, i, j;

  if HasIsRegularDClass(d) and not IsRegularDClass(d) then 
    Info(InfoCitrus, 2, "the D-class is not regular,");
    return fail;
  fi;
  
  s:=d!.parent; data:=d!.data; o:=d!.o;

  if NrIdempotentsRClassFromData(s, data[1], o[1])=0 then
    Error("the D-class is not regular,");
    return;
  fi;

  n:=DegreeOfTransformationSemigroup(s);
  m:=RankOfTransformation(d!.rep);       

  if m=n then
    Info(InfoCitrus, 2, "the D-class is the group of units");
    f:=TransformationNC([1..n]*1);
    h:=GreensHClassOfElementNC(s, f);
    SetIsGroupHClass(h, true); SetIdempotents(h, [f]);
    return h;
  fi;

  ker:=KernelOrbit(d)[KernelOrbitSCC(d)[1]]; 
  o:=ImageOrbit(d); scc:=ImageOrbitSCC(d);

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

  Error("the D-class is not regular,");
  return;
end);

# mod for 0.7! - GroupHClassOfGreensDClass - "for D-class"
############################################################################
# move to d.gi! JDM

InstallMethod(GroupHClassOfGreensDClass, "for D-class",
[IsGreensDClass], GroupHClass);

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

InstallOtherMethod(IsGroupHClass, "for an H-class of a trans. semigp.", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  local f;
  f:=h!.rep;
  return IsInjectiveTransOnList(f, ImageSetOfTransformation(f));
end);

# new for 0.5! - IsGroupHClass - "for a non H-Class of a trans. semigp."
############################################################################

InstallOtherMethod(IsGroupHClass, "for a non H-Class of a trans. semigp.", 
[IsGreensClassOfTransSemigp], ReturnFalse);

# new for 0.1! - IsomorphismPermGroup - "for H-class of trans. semigp."
###########################################################################

InstallOtherMethod(IsomorphismPermGroup, "for H-class of trans. semigp.", 
[IsGreensHClass and IsGreensClassOfTransSemigp],
function(h)
  local g, f;

  if not IsGroupHClass(h) then
    Error("the H-class is not a group,");
    return;
  fi;
  
  g:=Group(());

  for f in Enumerator(h) do 
    g:=ClosureGroup(g, AsPermutation(f));
  od;

  return MappingByFunction(h, g, AsPermutation);
end);

# new for 0.1! - IteratorOfHClasses - "for a transformation semigroup"
############################################################################
# move to greens.gi

InstallMethod(IteratorOfHClasses, "for a trans. semigroup",
[IsTransformationSemigroup],
function(s)
local iter;

  Info(InfoCitrus, 4, "IteratorOfHClasses");

  iter:=IteratorByFunctions( rec(

    data:=IteratorOfHClassRepsData(s), s:=s, 
    
    IsDoneIterator := iter -> IsDoneIterator(iter!.data), 
    
    NextIterator:= function(iter)
      local d;
    
      d:=NextIterator(iter!.data);
    
      if d=fail then 
        return fail;
      fi;
    
      return CreateHClass(s, [d[1]{[1..6]}, d[2]{[1..6]}, d[3], d[4]],
      [OrbitsOfImages(s), OrbitsOfKernels(s)], HClassRepFromData(s, d));;
    end,

    ShallowCopy:=iter-> rec(data:=IteratorOfHClassRepsData(s))));

  SetIsIteratorOfHClasses(iter, true);
  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

# new for 0.1! - IteratorOfHClassReps - user function!
############################################################################

InstallMethod(IteratorOfHClassReps, "for a trans. semigroup",
[IsTransformationSemigroup], 
function(s)
  local iter;

  Info(InfoCitrus, 4, "IteratorOfHClassReps");

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

InstallMethod(IteratorOfHClassRepsData, "for a trans. semigroup",
[IsTransformationSemigroup], 
function(s)
  local iter;

  Info(InfoCitrus, 4, "IteratorOfHClassRepsData");

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
      
      iter!.data:=HClassRepsDataFromData(s, NextIterator(iter!.r),
       OrbitsOfImages(s));
      iter!.i:=1;
      
      return iter!.data[1];
    end,

    ShallowCopy:=iter-> rec(i:=0, r:=IteratorOfRClasses(s), 
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

# new for 0.1! - HClassType - not a user function!
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

#NNN 

# new for 0.1! - NrIdempotents - "for an H-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrIdempotents, "for an H-class of a trans. semigroup",
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  local f;

  f:=h!.rep;
  if IsInjectiveTransOnList(f, ImageSetOfTransformation(f)) then 
    return 1;
  fi;
  return 0;
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

# new for 0.1! - PrintObj - IsIteratorOfHClasses
############################################################################
# JDM move to greens.gi.

InstallMethod(PrintObj, [IsIteratorOfHClasses], 
function(iter)
  Print( "<iterator of H-classes>");
  return;
end);

# new for 0.7! - PrintObj - IsIteratorOfHClassElements
############################################################################

InstallMethod(PrintObj, [IsIteratorOfHClassElements], 
function(iter)
  Print( "<iterator of H-class>");
  return;
end);

#RRR

# new for 0.1! - RClassOfHClass - "for an H-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(RClassOfHClass, "for an H-class of a trans. semigroup", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
  local s, d, o, rep;

  if h!.data[4]=fail then #created from R-class or D-class 
    s:=h!.parent; d:=h!.data; o:=h!.o;
    d[1][3]:=ImageOrbitSCCFromData(s, d[1], o[1])[1];
    rep:=RClassRepFromData(s, d[1], o[1]);

    return CreateRClass(s, d[1], o[1], rep);
  fi;
  # the below is probably best possible since no info about the R-class
  # of an H-class created from an L-class is known. 
  #JDM is the above right?
  return RClass(ParentAttr(s), Representative(h));
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
