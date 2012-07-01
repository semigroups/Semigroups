#############################################################################
##
#W  l.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################
# Notes

# - L-class data should be [unrectified image data, rectified kernel data,
# image orbit coset rep].
# seems like all we really need is l, D-class data [2], coset rep...

#CCC

# new for 0.1! - CreateLClass - not a user function!
#############################################################################
# Usage: s = semigroup; data = [image data, kernel data, coset rep] (image
# data, kernel data any lengths); orbit = [OrbitsOfImages, OrbitsOfKernels];
# rep = representative. 

InstallGlobalFunction(CreateLClass, 
function(s, data, orbit, rep)
  local l;

  #data:=[data[1]{[1..6]}, data[2]{[1..6]}, data[3]];

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
  
  s:=l!.parent; o:=l!.o; 
  data:=o[2]!.data[HTValue(o[2]!.data_ht, l!.data[2])];
  rep:=DClassRepFromData(s, data, o);

  return CreateDClass(s, data, o, rep);
end);

#GGG

# new for 0.1! - GreensHClasses - "for an L-class of a trans. semigp."
#############################################################################

InstallOtherMethod(GreensHClasses, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp],
function(l)
  local s, d, o, m, data, out, f, h, i;

  s:=l!.parent; d:=DClassOfLClass(l); o:=l!.o; 
  m:=NrHClasses(l); data:=HClassRepsData(l); 
  
  out:=EmptyPlist(m);

  for i in [1..m] do 
    
    if HasHClassReps(l) then 
      f:=HClassReps(l)[i];
    else
      f:=HClassRepFromData(s, data[i], o);
    fi;

    h:=CreateHClass(s, data[i], o, f);
    SetLClassOfHClass(h, l); SetDClassOfHClass(h, d);
    out[i]:=h;
  od;

  return out;
end);

# new for 0.5! - GreensHClassOfElementNC - "for an L-class and trans."
#############################################################################
# JDM is there a better way? If not clean it up!

InstallOtherMethod(GreensHClassOfElementNC, "for an L-class and trans.", 
[IsGreensLClass and IsGreensClassOfTransSemigp, IsTransformation],
function(l, f)
  local data, l_ker, g, rep, p, cosets, h;

  data:=ShallowCopy(l!.data);
  l_ker:=Position(KernelOrbit(l), CanonicalTransSameKernel(f));
  data[2][3]:=l_ker;
  
  g:=KernelOrbitRels(l)[l_ker][2]*f*(l!.data[3]/
   ImageOrbitPerms(l)[data[1][3]])^-1;
  
  rep:=DClassRepFromData(ParentAttr(l), l!.data, l!.o);
  p:=KerRightToImgLeft(l)^-1;
  cosets:=KernelOrbitCosets(l);
  
  data[4]:=cosets[PositionCanonical(cosets, PermLeftQuoTransformationNC(rep,
   g)^p)];

  h:=CreateHClass(ParentAttr(l), data, l!.o,
  HClassRepFromData(ParentAttr(l), data, l!.o));
  SetLClassOfHClass(h, l);
  return h;
end);

# new for 0.1! - GreensLClasses - "for a transformation semigroup"
#############################################################################
# JDM move to greens.gi

InstallMethod(GreensLClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
  local iter, out, i, l;

  Info(InfoCitrus, 4, "GreensLClasses");

  iter:=IteratorOfLClasses(s);
  out:=EmptyPlist(NrLClasses(s));
  i:=0;

  for l in iter do 
    i:=i+1;
    out[i]:=l;
  od;

  return out;
end);

#HHH

# new for 0.1! - HClassReps - "for an L-class of a trans. semigp."
#############################################################################

InstallOtherMethod(HClassReps, "for an L-class of a trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
  local f, cosets, rels, scc, out, k, i, j;

  if HasGreensHClasses(l) then 
    return List(GreensHClasses(l), Representative);
  fi;

  f:=l!.rep; cosets:=KernelOrbitCosets(l); rels:=KernelOrbitRels(l);
  scc:=KernelOrbitSCC(l);

  out:=EmptyPlist(Length(scc)*Length(cosets));
  
  if not HasNrHClasses(l) then 
    SetNrHClasses(l, Length(scc)*Length(cosets));
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

# new for 0.1! - HClassRepsData - "for L-class of trans. semigp."
#############################################################################

InstallOtherMethod(HClassRepsData, "for L-class of trans. semigp.", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
  local f, scc, cosets, out, k, data, i, j;

  f:= l!.rep; scc:=KernelOrbitSCC(l);
  cosets:=KernelOrbitCosets(l);

  out:=EmptyPlist(Length(scc)*Length(cosets));

  if not HasNrHClasses(l) then 
    SetNrHClasses(l, Length(scc)*Length(cosets));
  fi;

  k:=0; 
  data:=[StructuralCopy(l!.data[1]), StructuralCopy(l!.data[2]),
  StructuralCopy(l!.data[3])]; 
  for i in scc do 
    for j in cosets do 
      k:=k+1;
      out[k]:=StructuralCopy(data);
      out[k][2][3]:=i; out[k][3]:=data[3]; out[k][4]:=j; 
    od;
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
    Info(InfoCitrus, 2, "the L-class is not regular,");
    return [];
  fi;

  if NrIdempotentsRClassFromData(l!.parent, l!.data[1], l!.o[1])=0 then
    Info(InfoCitrus, 2, "the L-class is not regular,");
    return [];
  fi;

  n:=DegreeOfTransformation(l!.rep); m:=RankOfTransformation(l!.rep);
  
  if m=n then
    Info(InfoCitrus, 2, "the L-class is the group of units,");
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

# new for 0.1! - IsRegularLClass - "for a Green's class of trans. semi."
###########################################################################
# Notes: don't see a current need for IsRegularLClassData...

InstallMethod(IsRegularLClass, "for a Green's class of trans. semi.",
[IsGreensClassOfTransSemigp], 
function(l)
  local img, scc, o, i;

  if not IsGreensLClass(l) then
    Info(InfoCitrus, 2,  l, " is not an L-class,");    
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

# new for 0.1! - IteratorOfLClasses - 
###########################################################################

InstallMethod(IteratorOfLClasses, "for a trans. semigroup",
[IsTransformationSemigroup], 
function(s)
  local iter;

  Info(InfoCitrus, 4, "IteratorOfLClasses");

  iter:=IteratorByFunctions( rec(

  	  data:=IteratorOfLClassRepsData(s), 
	
          IsDoneIterator := iter -> IsDoneIterator(iter!.data), 
	
	  NextIterator:= function(iter)
	    local d;
	
            d:=NextIterator(iter!.data);
	
            if d=fail then 
	      return fail;
	    fi;
	
          return CreateLClass(s, [d[1]{[1..6]}, d[2]{[1..6]}, d[3]],
          [OrbitsOfImages(s), OrbitsOfKernels(s)], 
	   LClassRepFromData(s, d));
	  end,
	
	  ShallowCopy:=iter-> rec(data:=IteratorOfLClassRepsData(s))));

  SetIsIteratorOfLClasses(iter, true);
  return iter;
end);

# new for 0.1! - IteratorOfLClassReps - user function!
###########################################################################

InstallMethod(IteratorOfLClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)
  local iter;

  Info(InfoCitrus, 4, "IteratorOfLClassReps");

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
  return iter;
end);

# new for 0.1! - IteratorOfLClassRepsData - not a user function
###########################################################################

InstallMethod(IteratorOfLClassRepsData, "for a trans. semigroup",
[IsTransformationSemigroup], 
function(s)
  local o, iter; 

  Info(InfoCitrus, 4, "IteratorOfLClassRepsData");

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
        iter!.next_value:=LClassRepsDataFromData(s, 
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
  
  return iter;
end);


# new for 0.1! - LClassReps - "for a trans. semigroup"
#############################################################################

InstallMethod(LClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)
  local out, iter, i, f;
  Info(InfoCitrus, 4, "LClassReps");

  out:=EmptyPlist(NrLClasses(s));
  iter:=IteratorOfLClassReps(s);
  i:=0;

  for f in iter do 
    i:=i+1;
    out[i]:=f;
  od;

  return out;
end);

#NNN

# new for 0.1! - NrHClasses - "for an L-class of a trans. semigroup"
#############################################################################

InstallOtherMethod(NrHClasses, "for an L-class of a trans. semigroup", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
  l-> NrRClasses(DClassOfLClass(l)));

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
    Info(InfoCitrus, 2, "L-class is not regular");
    return 0;
  fi;

  if NrIdempotentsRClassFromData(l!.parent, l!.data[1], l!.o[1])=0 then
    Info(InfoCitrus, 2, "L-class is not regular");
    return 0;
  fi;
  
  f:=l!.rep;
  
  if RankOfTransformation(f)=DegreeOfTransformation(f) then
    Info(InfoCitrus, 2, "L-class is the group of units");
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

#EOF
