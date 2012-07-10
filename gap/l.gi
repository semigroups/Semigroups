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

#EOF
