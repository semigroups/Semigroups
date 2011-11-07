#############################################################################
##
#W  l.gi
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################
# Notes

# - L-class data should be [unrectified image data, rectified kernel data,
# image orbit coset rep].

# - this file is alphabetized, keep it that way!

# - install method for ImageOrbit of L-class!

#############################################################################
# other equalities of Green's classes handled by generic method in greens.gi!

# new for 0.1! - \= - "for L-class and L-class of trans. semigp."
#############################################################################

InstallMethod(\=, "for L-class and L-class of trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp, IsGreensLClass and 
IsGreensClassOfTransSemigp],
function(l1, l2)
  return l1!.parent=l2!.parent and l1!.rep in l2;
end);

# new for 0.1! - \< - "for L-class and L-class of trans. semigp."
############################################################################

InstallMethod(\<, "for L-class and L-class of trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp, IsGreensLClass and 
IsGreensClassOfTransSemigp],
function(h1, h2)
  return h1!.parent=h2!.parent and h1!.rep < h2!.rep;
end);

# new for 0.1! - \in - "for trans. and L-class of trans. semigp."
#############################################################################

InstallOtherMethod(\in, "for trans. and L-class of trans. semigp.",
[IsTransformation, IsGreensLClass and IsGreensClassOfTransSemigp], 
function(f, l)
  local rep, s, d, o, o_ker, i, schutz, perms, cosets, g, p;

  rep:=l!.rep;

  if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or 
   RankOfTransformation(f) <> RankOfTransformation(rep) or 
    ImageSetOfTransformation(f) <> ImageSetOfTransformation(rep) then
    return false;
  fi;

  s:=l!.parent; d:=l!.data; o:=l!.o; 
  o_ker:=o[2]!.orbits[d[2][1]][d[2][2]];

  i:=Position(o_ker, CanonicalTransSameKernel(f));

  if i = fail or not o_ker!.truth[d[2][4]][i] then 
    return false;
  fi;

  schutz:=KernelOrbitStabChain(l);

  if schutz=true then 
    return true;
  fi;

  perms:=ImageOrbitPerms(l); cosets:=ImageOrbitCosets(l);

  g:=o_ker!.rels[i][2]*f*(d[3]/perms[d[1][3]])^-1;

  if g=rep then 
    return true;
  elif schutz=false then 
    return false;
  fi;

  rep:=DClassRepFromData(s, d, o);
  p:=KerRightToImgLeftFromData(s, d[2], o[2])^-1;

  return SiftedPermutation(schutz, PermLeftQuoTransformationNC(rep, g)^p)=();
end);

# new for 0.1! - AsSSortedList - "for L-class of trans. semigp."
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for L-class of trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
  Info(InfoCitrusGreens, 4, "AsSSortedList: for an L-class");
  return ConstantTimeAccessList(EnumeratorSorted(l));
end);

#CCC

# new for 0.1! - CreateLClass - not a user function!
#############################################################################
# Usage: s = semigroup; data = [image data, kernel data, coset index] (image
# data, kernel data any lengths); orbit = [OrbitsOfImages, OrbitsOfKernels];
# rep = representative. 

# Notes: image data should be unrectified (i.e. data[1][3] is the position of
# the image of the rep in image orbit), kernel data should be rectified (so that
# it corresponds to the D-class containing this L-class), and coset index is 
# InOrbitsOfKernels(%)[2][2][8].

InstallGlobalFunction(CreateLClass, 
function(s, data, orbit, rep)
  local l;

  data:=[data[1]{[1..6]}, data[2]{[1..6]}, data[3]];

  l:=Objectify(LClassType(s), rec(parent:=s, data:=data, 
   o:=orbit, rep:=rep));
  
  SetRepresentative(l, rep);
  SetEquivalenceClassRelation(l, GreensLRelation(s));
  return l;
end);

#DDD

# new for 0.1! - DClassOfLClass - "for an L-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(DClassOfLClass, "for an L-class of a trans. semigroup",
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)
  local s, o, data, rep;
  
  s:=l!.parent; o:=l!.o; data:=o[2]!.data[HTValue(o[2]!.data_ht, l!.data[2])];
  rep:=DClassRepFromData(s, data, o);

  return CreateDClass(s, data, o, rep);
end);

#EEE

# new for 0.1! - Enumerator - "for L-class of trans. semigp."
##############################################################################

InstallOtherMethod(Enumerator, "for L-class of trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
  local enum;

  Info(InfoCitrusGreens, 4, "Enumerator: for an L-class");

  enum:=EnumeratorByFunctions(l, rec(
    
    rep:=l!.rep, len:=Size(SchutzenbergerGroup(l)),
    
    schutz:=Enumerator(SchutzenbergerGroup(l)),

    ###########################################################################
    
    ElementNumber:=function(enum, pos)
      local q, n, m;
      
      if pos>Length(enum) then 
        return fail;
      fi;
      
      if pos<=enum!.len then 
        return enum!.rep*enum!.schutz[pos];
      fi;
      
      n:=pos-1; m:=enum!.len;
      q := QuoInt(n, m); pos:= [ q, n - q * m ]+1;

      return KernelOrbitRels(l)[KernelOrbitSCC(l)[pos[1]]][1]*enum[pos[2]];
    end, 
    
    ###########################################################################
    
    NumberElement:=function(enum, f)
      local rep, o, d, i, j;
      
      rep:=enum!.rep;

      if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) or
       RankOfTransformation(f) <> RankOfTransformation(rep) or
        ImageSetOfTransformation(f) <> ImageSetOfTransformation(rep) then
          return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;
      
      o:= KernelOrbit(l); d:=l!.data;
      i:= Position(o, CanonicalTransSameKernel(f));
      
      if i = fail or not o!.truth[d[2][4]][i] then 
        return fail;
      fi;
      
      j:= Position(enum!.schutz,
       PermLeftQuoTransformationNC(rep, o!.rels[i][2]*f));
      
      if j = fail then 
        return fail;
      fi;
      
      return enum!.len*(Position(KernelOrbitSCC(l), i)-1)+j;
    end, 

    ###########################################################################
    
    Membership:=function(elm, enum) 
      return elm in l;
    end,
    
    Length:=enum -> Size(l),

    PrintObj:=function(enum)
      Print( "<enumerator of L-class>");
     return;
    end));

  return enum;
end);

#GGG

# new for 0.1! - GreensHClasses - "for an L-class of a trans. semigp."
#############################################################################

InstallOtherMethod(GreensHClasses, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)
  local s, d, o, m, data, out, f, h, i;

  s:=l!.parent; d:=DClassOfLClass(l); o:=l!.o; 
  m:=NrGreensHClasses(l); data:=GreensHClassRepsData(l); 
  
  out:=EmptyPlist(m);

  for i in [1..m] do 
    
    if HasGreensHClassReps(l) then 
      f:=GreensHClassReps(l)[i];
    else
      f:=HClassRepFromData(s, data[i], o);
    fi;

    h:=CreateHClass(s, data[i], o, f);
    SetLClassOfHClass(h, l); SetDClassOfHClass(h, d);
    out[i]:=h;
  od;

  return out;
end);

# new for 0.1! - GreensHClassReps - "for an L-class of a trans. semigp."
#############################################################################

InstallOtherMethod(GreensHClassReps, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
  local f, cosets, rels, scc, out, k, i, j;

  if HasGreensHClasses(l) then 
    return List(GreensHClasses(l), Representative);
  fi;

  f:=l!.rep; cosets:=KernelOrbitCosets(l); rels:=KernelOrbitRels(l);
  scc:=KernelOrbitSCC(l);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  
  if not HasNrGreensHClasses(l) then 
    SetNrGreensHClasses(l, Length(scc)*Length(cosets));
  fi;

  k:=0;

  for i in scc do 
    i:=rels[i][1]*f;
    for j in cosets do 
      k:=k+1;
      out[k]:=i*j;
    od;
  od;

  return out;
end);

# new for 0.1! - GreensHClassRepsData - "for L-class of trans. semigp."
#############################################################################

InstallOtherMethod(GreensHClassRepsData, "for L-class of trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
  local f, scc, cosets, out, k, data, i, j;

  f:= l!.rep; scc:=KernelOrbitSCC(l);
  cosets:=KernelOrbitCosets(l);

  out:=EmptyPlist(Length(scc)*Length(cosets));

  if not HasNrGreensHClasses(l) then 
    SetNrGreensHClasses(l, Length(scc)*Length(cosets));
  fi;

  k:=0; data:=l!.data;

  for i in scc do 
    for j in cosets do 
      k:=k+1;
      out[k]:=StructuralCopy(data);
      out[k][2][3]:=i; out[k][3]:=data[3]; out[k][4]:=j; 
    od;
  od;

  return out;
end);

# new for 0.1! - GreensLClasses - "for a transformation semigroup"
#############################################################################
# JDM move to greens.gi

InstallMethod(GreensLClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
  local iter, out, i, l;

  Info(InfoCitrusGreens, 4, "GreensLClasses");

  iter:=IteratorOfGreensLClasses(s);
  out:=EmptyPlist(NrGreensLClasses(s));
  i:=0;

  for l in iter do 
    i:=i+1;
    out[i]:=l;
  od;

  return out;
end);

# new for 0.1! - GreensLClassOfElement - "for a trans. semigp and trans."
#############################################################################

InstallOtherMethod(GreensLClassOfElement, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
  local d, l;

  Info(InfoCitrusGreens, 4, "GreensLClassOfElement");

  if not f in s then 
    Info(InfoWarning, 1, "transformation is not an element of the semigroup");
    return fail;
  fi;

  d:=PreInOrbitsOfKernels(s, f, false); l:=d[1][2][3]; 

  if not d[2][1] then #D-class containing f not previously calculated
    d[1][2][3]:=ImageOrbitSCCFromData(s, d[1][2])[1];
    if not d[2][2][3]=fail then 
      d[2][2][3]:=KernelOrbitSCCFromData(s, d[2][2])[1];
    fi;
    d:=StructuralCopy(AddToOrbitsOfKernels(s, TransformationNC(d[1][2][7]), 
     [d[1][2], d[2][2]])); 
    d[1][3]:=l; d[2][3]:=KernelOrbitSCCFromData(s, d[2])[1]; d[3]:=();
  else
    d:=[d[1][2], d[2][2], ImageOrbitCosetsFromData(s, d[2][2])[d[2][2][8]]];
  fi;

  l:=CreateLClass(s, d, [OrbitsOfImages(s), OrbitsOfKernels(s)], 
  LClassRepFromData(s, d));
  return l;
end);

# new for 0.1! - GreensLClassOfElementNC - "for a trans. semigp and trans."
#############################################################################
# JDM test this more!

InstallOtherMethod(GreensLClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
  local n, d, j, img_o, ker_o;

  Info(InfoCitrusGreens, 4, "GreensLClassOfElementNC");

  n:=DegreeOfTransformationSemigroup(s);

  if not DegreeOfTransformation(f)=n then
    Info(InfoWarning, 1, "Usage: trans. semigroup and trans. of equal degree");
    return fail;
  fi;

  d:=PreInOrbitsOfKernels(s, f, false);

  if d[1][1] then #f in s!
    Info(InfoCitrusGreens, 2, "transformation is an element of the semigroup");
    #JDM inefficient as we run PreInOrbitsOfKernels twice!
    return GreensLClassOfElement(s, f);	
  elif OrbitsOfImages(s)!.finished then #f not in s!
    Info(InfoCitrusGreens, 2, "transformation is not an element of the ",
     "semigroup");
    return fail;
  fi;

  Info(InfoCitrusGreens, 2, "transformation may not be an element of the ",
   "semigroup");

  j:=Length(ImageSetOfTransformation(f));
   
  Info(InfoCitrusGreens, 2, "finding orbit of image...");
  img_o:=[]; img_o[j]:=[ForwardOrbitOfImage(s, f![1])];
  #JDM see comments in GreensDClassOfElementNC
  img_o:=rec( finished:=false, orbits:=img_o, gens:=Generators(s), s:=s,
   deg := n, data:=[[j,1,1,1,1,1]], images:=fail, lens:=List([1..n],
   function(x) if x=j then return 1; else return 0; fi; end),
   data_ht:=HTCreate([1,1,1,1,1,1], rec(hashlen:=CitrusHashLen!.rclass_data)));

  Info(InfoCitrusGreens, 2, "finding orbit of kernel...");
  ker_o:=[]; ker_o[j]:=[ForwardOrbitOfKernel(s, f)];
  ker_o:=rec( orbits:=ker_o, gens:=Generators(s), data:=[[[j,1,1,1,1,1],
     [j,1,1,1,1,1]]], kernels:=fail, data_ht:=HTCreate([1,1,1,1,1,1],
     rec(hashlen:=CitrusHashLen!.dclass_data)));

  Info(InfoCitrusGreens, 2, "finding the kernel orbit schutz. group");
  Add(ker_o!.orbits[j][1]!.d_schutz[1], [CreateSchutzGpOfDClass(s,
   [[j,1,1,1,1,1],[j,1,1,1,1,1]], [img_o, ker_o])]);
  HTAdd(ker_o!.data_ht, [j,1,1,1,1,1], 1);

  return CreateLClass(s, [[j,1,1,1,1,1], [j,1,1,1,1,1], ()], 
   [img_o, ker_o], f);
end);

# new for 0.1! - GreensLClassReps - "for a trans. semigroup"
#############################################################################

InstallMethod(GreensLClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)
  local out, iter, i, f;
  Info(InfoCitrusGreens, 4, "GreensLClassReps");

  out:=EmptyPlist(NrGreensLClasses(s));
  iter:=IteratorOfLClassReps(s);
  i:=0;

  for f in iter do 
    i:=i+1;
    out[i]:=f;
  od;

  return out;
end);

#III

# new for 0.1! - Idempotents - "for an L-class of a trans. semigp."
#############################################################################
# maybe make iterator at some point in the future JDM!

InstallOtherMethod(Idempotents, "for an L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
  local n, m, out, f, img, o, scc, j, lookup, i, k;

  if HasIsRegularLClass(l) and not IsRegularLClass(l) then 
    return [];
  fi;

  if NrIdempotentsRClassFromData(l!.parent, l!.data[1], l!.o[1])=0 then
    return [];
  fi;

  n:=DegreeOfTransformation(l!.rep); m:=RankOfTransformation(l!.rep);
  
  if m=n then
    return [TransformationNC([1..n])];
  fi;

  out:=[]; f:=l!.rep; img:=ImageSetOfTransformation(f);
  f:=f![1]; o:=KernelOrbit(l); scc:=KernelOrbitSCC(l);
  j:=0; 

  for i in scc do
    if IsInjectiveTransOnList(o[i], img) then 
      j:=j+1;
      lookup:=EmptyPlist(n);
      for k in [1..m] do
        lookup[o[i][img[k]]]:=img[k];
      od;
      out[j]:=TransformationNC(List(o[i], x-> lookup[x]));
    fi;
  od;

  if not HasNrIdempotents(l) then 
    SetNrIdempotents(l, j);
  fi;

  return out;
end);

# new for 0.1! - ImageOrbitCosets - "for a L-class of trans. semigroup"
###########################################################################
# Notes: returns a transversal of right cosets of SchutzenbergerGroup(d)
# (which is ImgLeft) in ImageOrbitSchutzGp (which is ImgLeft), where d is the
# D-class of l. 

InstallOtherMethod(ImageOrbitCosets, "for a L-class of trans. semigroup",
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]]!.d_schutz[e[4]][e[5]][e[6]][3];
end);

# new for 0.1! - ImageOrbitPerms - for a L-class of a trans. semigp.
############################################################################
  
InstallOtherMethod(ImageOrbitPerms, "for a L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(r)
  local d;  
  d:=r!.data[1];
  return r!.o[1]!.orbits[d[1]][d[2]]!.perms;
end);

# new for 0.1! - IsRegularLClass - "for an L-class of trans. semigroup"
###########################################################################
# JDM: don't see a current need for IsRegularLClassData...

InstallMethod(IsRegularLClass, "for an L-class of trans. semigroup",
[IsGreensClassOfTransSemigp], 
function(l)
  local img, scc, o, i;

  if not IsGreensLClass(l) then 
    return false;
  fi;

  if HasNrIdempotents(l) then 
    return NrIdempotents(l)>0;
  fi;

  if HasIdempotents(l) then 
    return Length(Idempotents(l))>0; 
  fi;

  if HasIsRegularSemigroup(l!.parent) and IsRegularSemigroup(l!.parent) then 
    return true;
  fi;

  img:=ImageSetOfTransformation(l!.rep);
  scc:=KernelOrbitSCC(l); o:=KernelOrbit(l);

  for i in scc do 
    if IsInjectiveTransOnList(o[i], img) then 
      return true;
    fi;
  od;
  
  return false;
end);

# new for 0.1! - IteratorOfGreensLClasses - user function!
###########################################################################

InstallGlobalFunction(IteratorOfGreensLClasses, 
function(s)
  local iter;

  if not IsTransformationSemigroup(s) then
    Info(InfoWarning, 1, "Usage: arg. should be a transformation semigroup.");
     return fail;
  fi;

  Info(InfoCitrusGreens, 4, "IteratorOfGreensLClasses");

  iter:=IteratorByFunctions( rec(

  	  data:=IteratorOfLClassRepsData(s), s:=s,
	
          IsDoneIterator := iter -> IsDoneIterator(iter!.data), 
	
	  NextIterator:= function(iter)
	    local d;
	
            d:=NextIterator(iter!.data);
	
            if d=fail then 
	      return fail;
	    fi;
	
	  return CreateLClass(s, d, [OrbitsOfImages(s), OrbitsOfKernels(s)], 
	   LClassRepFromData(s, d));
	  end,
	
	  ShallowCopy:=iter-> rec(data:=IteratorOfLClassRepsData(s))));

  SetIsIteratorOfGreensLClasses(iter, true);
  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

# new for 0.1! - IteratorOfLClassReps - user function!
###########################################################################

InstallGlobalFunction(IteratorOfLClassReps,
function(s)
  local iter;

  Info(InfoCitrusGreens, 4, "IteratorOfLClassReps");

  if not IsTransformationSemigroup(s) then
    Info(InfoWarning, 1, "Usage: argument should be a transformation",
    " semigroup");
    return fail;
  fi;

  iter:=IteratorByFunctions( rec(

    s:=s, data:=IteratorOfLClassRepsData(s),
  
    IsDoneIterator := iter-> IsDoneIterator(iter!.data),
  
    NextIterator := function(iter)
      if not IsDoneIterator(iter!.data) then 
        return LClassRepFromData(iter!.s, NextIterator(iter!.data));
      fi;
      return fail; 
    end,
  
    ShallowCopy := iter -> rec( data:=IteratorOfLClassRepsData(
    iter!.s))));

  SetIsIteratorOfLClassReps(iter, true);
  SetIsCitrusPkgIterator(iter, true);

  return iter;
end);

# new for 0.1! - IteratorOfLClassRepsData - not a user function
###########################################################################

InstallGlobalFunction(IteratorOfLClassRepsData, 
function(s)
  local o, iter; 

  Info(InfoCitrusGreens, 4, "IteratorOfLClassRepsData");

  o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];

  iter:=IteratorByFunctions( rec(
    
    ShallowCopy := iter -> rec( i:=0, next_value:=[], 
    last_called_by_is_done := false, data:=IteratorOfDClassRepsData),
    
    i:=0, next_value:=[], last_called_by_is_done:=false,
    
    data:=IteratorOfDClassRepsData(s), s:=s,
    
    ######################################################################

    IsDoneIterator:=function(iter) 

      if iter!.last_called_by_is_done then 
        return IsDoneIterator(iter!.data) and 
         iter!.i>Length(iter!.next_value);
      fi;
      
      iter!.last_called_by_is_done:=true;
      iter!.i:=iter!.i+1;
      
      if IsDoneIterator(iter!.data) and iter!.i>Length(iter!.next_value) then 
       return true;
      elif iter!.i>Length(iter!.next_value) then 
        iter!.next_value:=GreensLClassRepsDataFromData(s, 
         NextIterator(iter!.data), o);
        iter!.i:=1;
      fi;
      
      return false;
    end,

    ######################################################################
    
    NextIterator:=function(iter) 
    
      if not iter!.last_called_by_is_done then 
        IsDoneIterator(iter);
      fi;
      
      if IsDoneIterator(iter) then 
        return fail;
      fi;
      
      iter!.last_called_by_is_done:=false;
      return iter!.next_value[iter!.i];
    end));

  ######################################################################
  
  SetIsCitrusPkgIterator(iter, true);
  return iter;
end);

#KKK

# new for 0.1! - KernelOrbit - "for a L-class of a trans. semigp."
############################################################################

InstallOtherMethod(KernelOrbit, "for a L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]];
end);

# new for 0.1! - KernelOrbitCosets - not a user function!
###########################################################################
# Notes: if d=DClassOfLClass(l) then this returns a transversal of right 
# cosets of SchutzenbergerGroup(d) (which is ImgLeft) in 
# KernelOrbitSchutzGp^KerRightToImgLeft(d) (which is ImgLeft after 
# conjugating). 

InstallOtherMethod(KernelOrbitCosets, "for L-class of trans. semigroup",
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)
  local schutz, d_schutz;

  schutz:=KernelOrbitSchutzGpFromData(l!.parent, l!.data[2], l!.o[2]);

  if Size(schutz)=1 then
   return [()];
  fi;

  d_schutz:=KernelOrbitFromData(l!.parent, l!.data, l!.o)!.
     d_schutz[l!.data[2][4]][l!.data[2][5]][l!.data[2][6]][2];

  return RightTransversal(schutz^KerRightToImgLeftFromData(l!.parent, 
   l!.data[2], l!.o[2]), d_schutz);
end);

# new for 0.1! - KernelOrbitRels - "for L-class of trans. semigp"
############################################################################

InstallOtherMethod(KernelOrbitRels,"for  L-class of a trans. semigp",
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  
  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]]!.rels;
end);

# new for 0.1! - KernelOrbitSCC - "for L-class of trans. semigp"
#############################################################################

InstallOtherMethod(KernelOrbitSCC, "for L-class of trans. semigp",
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(d)
  local e;

  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]]!.scc[e[4]];
end);

# new for 0.1! - KernelOrbitSchutzGp - "for a L-class of a trans. semigp."
############################################################################
# Notes: returns the schutz. gp. of the kernel orbit of the L-class, which is
# KerRight.

InstallOtherMethod(KernelOrbitSchutzGp, "for a L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]]!.schutz[e[4]][2];
end);

# new for 0.1! - KernelOrbitStabChain - "for L-class of a trans. semigp."
############################################################################
# Notes: returns true, false, or the stabilizer chain of the right
# Schutzenberger group of the specified kernel orbit. True indicates the 
# Schutz. gp. is the symmetric group, false indicates it is trivial. Note 
# that the right Schutzenberger group is obtained by considering the right 
# action on kernels classes. Use KerRightToImgLeft to switch from the right 
# Schutz. gp. to the left one corresponding a specific D-class. 

InstallOtherMethod(KernelOrbitStabChain, "for L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)
  local e;
  e:=l!.data[2];
  return l!.o[2]!.orbits[e[1]][e[2]]!.schutz[e[4]][1];
end);

# new for 0.1! - KerRightToImgLeft - "for a L-class of a trans. semigp"
#############################################################################

InstallOtherMethod(KerRightToImgLeft, "for a L-class of a trans. semigp",
[IsGreensLClass and IsGreensClassOfTransSemigp],
l-> KerRightToImgLeftFromData(l!.parent, l!.data[2], l!.o[2]));

#LLL

# new for 0.1! - LClassRepFromData - not a user function
############################################################################
# Usage: s = semigroup; d = [image data, kernel data, coset index];
# o = [OrbitsOfImages, OrbitsOfKernels] (optional).

InstallGlobalFunction(LClassRepFromData,
function(arg)
  local s, d, f, o, perms, cosets;

  s:=arg[1]; d:=arg[2];
  f:=CallFuncList(DClassRepFromData,arg);

  if Length(arg)=3 then 
    o:=arg[3];
  else
    o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
  fi;

  perms:=ImageOrbitPermsFromData(s, d[1], o[1]);
  return f*(d[3]/perms[d[1][3]]);
end);

# new for 0.1! - LClassType - "for a transformation semigroup"
############################################################################

InstallMethod(LClassType, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s);

return NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensLClass and 
	 IsGreensClassOfTransSemigp);
end);

#NNN

# new for 0.1! - NrGreensLClasses - "for a transformation semigroup"
#############################################################################
# JDM move this to greens.gi

InstallMethod(NrGreensLClasses, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
  local i, d;

  ExpandOrbitsOfKernels(s); i:=0;
  
  for d in OrbitsOfKernels(s)!.data do 
    i:=i+Length(ImageOrbitCosetsFromData(s, d[2]))* 
     Length(ImageOrbitSCCFromData(s, d[1]));
  od;
  return i;
end);

# new for 0.1! - NrGreensHClasses - "for an L-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrGreensHClasses, "for an L-class of a trans. semigroup", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
  l-> NrGreensRClasses(DClassOfLClass(l)));

# new for 0.1! - NrIdempotents - "for an L-class of a trans. semigp."
############################################################################

InstallOtherMethod(NrIdempotents, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)
  local f, j, img, scc, o, i;
  
  if HasIdempotents(l) then 
    return Length(Idempotents(l));
  fi;
  
  if HasIsRegularLClass(l) and not IsRegularLClass(l) then 
    return 0;
  fi;

  if NrIdempotentsRClassFromData(l!.parent, l!.data[1], l!.o[1])=0 then
    return 0;
  fi;
  
  f:=l!.rep;
  
  if RankOfTransformation(f)=DegreeOfTransformation(f) then
    return 1;
  fi;
  
  j:=0; img:=ImageSetOfTransformation(f);
  scc:=KernelOrbitSCC(l); o:=KernelOrbit(l);
  
  for i in scc do 
    if IsInjectiveTransOnList(o[i], img) then 
      j:=j+1;
    fi;
  od;

  return j;
end);

#PPP

# new for 0.1! - ParentAttr - "for L-class of a trans. semigroup"
############################################################################

InstallMethod(ParentAttr, "for L-class of a trans. semigroup", 
[IsGreensLClass and IsGreensClassOfTransSemigp], x-> x!.parent);

# new for 0.1! - PrintObj - for IsIteratorOfLClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClassReps], 
function(iter)
  Print( "<iterator of L-class reps>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfGreensLClasses
############################################################################

InstallMethod(PrintObj, [IsIteratorOfGreensLClasses], 
function(iter)
  Print( "<iterator of L-classes>");
  return;
end);

#SSS

# new for 0.1! - SchutzenbergerGroup - "for an L-class of a trans. semigp."
#############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for an L-class of a trans. semigp.",
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
  local g, d, x;

  g:=KernelOrbitSchutzGp(l); 

  if Size(g)=1 then 
    return g;
  fi;

  d:=l!.data; x:=ImageOrbitPerms(l)[d[1][3]]; 

  return g^(KerRightToImgLeft(l)*d[3]/x);
end);

# new for 0.1! - Size - "for an L-class of a trans. semigp."
#############################################################################
##  Algorithm C. 

InstallOtherMethod(Size, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)

  Info(InfoCitrusGreens, 4, "Size: for an L-class");

  return Size(KernelOrbitSchutzGp(l))
   *Length(KernelOrbitSCC(l));
end);

#EOF

