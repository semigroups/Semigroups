
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
