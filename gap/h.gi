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
# - H-class data is [unrectified image data, unrectified kernel data, image
# orbit coset rep, kernel orbit coset rep]

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

#GGG

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

# new for 0.1! - GroupHClass - "for a D-class of a trans. semigp."
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

#III 

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

  return iter;
end);



#EOF
