#############################################################################
##
#W  iterators.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#technical...

# new for 1.0! - IteratorByIteratorOfIterator
#############################################################################

InstallGlobalFunction(IteratorByIterOfIter,
function(old_iter, components, isnew, convert, filts)
  local iter, filt;

  iter:=IteratorByFunctions(rec(
    
    iter:=old_iter;
    iterofiter:=fail;

    IsDoneIterator:=iter-> IsDoneIterator(iter!.iter) and 
     IsDoneIterator(iter!.iterofiter), 

    NextIterator:=function(iter)
      local iterofiter, next;

      if IsDoneIterator(iter) then 
        return fail;
      fi;
      
      if iter!.iterofiter=fail or IsDoneIterator(iter!.iterofiter) then 
        iterofiter:=Iterator(NextIterator(iter!.iter));
      fi;
      
      repeat
        next:=NextIterator(iterofiter);
      until isnew(iter, next);
      iter!.iterofiter:=iterofiter;
      return convert(iter, next);
    end,

    ShallowCopy:=iter -> rec(iter:=old_iter, iterorfiter:=fail)));
  
  for name in components do 
    iter!.name:=component.name;
  od;

  for filt in filts do
    SetFilterObj(iter, filt);
  od;
  return iter;
end);

# new for 0.7! - IteratorByIterator - "for an iterator and function"
#############################################################################

InstallGlobalFunction(IteratorByIterator,
function(old_iter, convert, filts)
  local iter, filt;
  iter:=IteratorByFunctions(rec(
    data:=old_iter,
    IsDoneIterator:=iter-> IsDoneIterator(iter!.data),
    NextIterator:=function(iter)
      local x;
      x:=NextIterator(iter!.data);
      if x=fail then
        return fail;
      fi;
      return convert(x);
    end,
    ShallowCopy:=iter-> rec(data:=ShallowCopy(old_iter))));
  for filt in filts do
    SetFilterObj(iter, filt);
  od;
  return iter;
end);

# new for 0.7! - ListByIterator - "for an iterator and pos int"
#############################################################################

InstallGlobalFunction(ListByIterator,
function(iter, len)
  local out, i, x;

  out:=EmptyPlist(len);
  i:=0;

  for x in iter do
    i:=i+1;
    out[i]:=x;
  od;

  return out;
end);

# everything else...

# new for 1.0! - Iterator - "for an R-class of an acting semi"
#############################################################################

# Notes: this method makes Iterator of a semigroup much better!!

# same method for regular/inverse

InstallMethod(Iterator, "for an R-class of an acting semigp",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, mults, iter, scc;

  if HasAsSSortedList(r) then 
    iter:=IteratorList(AsSSortedList(r));
  else
    o:=LambdaOrb(r); 
    m:=LambdaOrbSCCIndex(r);
    mults:=LambdaOrbMults(o, m);
    scc:=OrbSCC(o)[m];

    iter:=IteratorByFunctions(rec(

      schutz:=List(SchutzenbergerGroup(r), x-> Representative(r)*x), 
      at:=[0,1],
      m:=Length(scc),
      n:=Size(SchutzenbergerGroup(r)), 

      IsDoneIterator:=iter-> iter!.at[1]=iter!.m and iter!.at[2]=iter!.n,

      NextIterator:=function(iter)
        local at;

        at:=iter!.at;
        
        if at[1]=iter!.m and at[2]=iter!.n then 
          return fail;
        fi;

        if at[1]<iter!.m then
          at[1]:=at[1]+1;
        else
          at[1]:=1; at[2]:=at[2]+1;
        fi;
       
        return iter!.schutz[at[2]]*mults[scc[at[1]]][1];
      end,
      
      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, at:=[0,1], 
       m:=iter!.m, n:=iter!.n)));
    fi;
    
    SetIsIteratorOfRClassElements(iter, true);
    return iter;
end);

# mod for 1.0! - Iterator - "for an acting semigroup"
#############################################################################
# Notes: the previous inverse method used D-classes instead of R-classes.

# same method for regular/inverse 

InstallMethod(Iterator, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter;

  if HasAsSSortedList(s) then 
    iter:=IteratorList(AsSSortedList(s));
    SetIsIteratorOfSemigroup(iter, true);
    return iter;
  fi;

  return IteratorByIterator(IteratorOfRClasses(s), rec(), 
   function(iter, x) return true; end, function(iter, x) return x; end, 
    IsIteratorOfSemigroup);
end);

# new for 0.5! - Iterator - "for a full transformation semigroup"
#############################################################################

# no method required for inverse/regular

InstallMethod(Iterator, "for a full transformation semigroup",
[IsTransformationSemigroup and IsFullTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter;
  
  Info(InfoCitrus, 4, "Iterator: for a full trans. semigroup");

  iter:= IteratorByFunctions( rec(

    s:=s,

    tups:=IteratorOfTuples([1..Degree(s)], Degree(s)),

    NextIterator:=iter-> TransformationNC(NextIterator(iter!.tups)),
  
    IsDoneIterator:=iter -> IsDoneIterator(iter!.tups),
    
    ShallowCopy:= iter -> rec(tups:=IteratorOfTuples([1..Degree(s)],
    Degree(s)))));

  SetIsIteratorOfSemigroup(iter, true);
  return iter;
end);

# mod for 1.0! - Iterator - "for a trivial acting semigroup"
#############################################################################
# Notes: required until Enumerator for a trans. semigp does not call iterator. 
# This works but is maybe not the best!

# same method for regular/inverse

InstallOtherMethod(Iterator, "for a trivial acting semigp", 
[IsActingSemigroup and HasGeneratorsOfSemigroup and IsTrivial], 9999,
function(s)
  return TrivialIterator(Generators(s)[1]);
end);

# new for 1.0! - IteratorOfDClasses - "for an acting semigroup"
#############################################################################

#JDM this should be improved at some point

# different method for regular/inverse

InstallMethod(IteratorOfDClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local iter;
  
  if IsClosed(SemigroupData(s)) then 
    iter:=IteratorList(GreensDClasses(s));
    SetIsIteratorOfDClasses(iter, true);
    return iter;
  fi;

  return IteratorByIterOfIter(IteratorOfRClassData(s), rec(classes:=[]), 
   function(iter, x) 
     return x=fail or ForAll(iter!.classes, d-> not x[4] in d);
   end, 
   function(iter, x) 
    d:=DClassOfRClass(CallFuncList(CreateRClassNC, x)) 
    Add(iter!.classes, d);
    return d;
  end, IsIteratorOfDClasses);
end);

#    last_called_by_is_done:=false,
#
#    next_value:=fail,
#
#    IsDoneIterator:=function(iter)
#      
#      if iter!.last_called_by_is_done then 
#        return iter!.next_value=fail;
#      fi;
#      
#      iter!.last_called_by_is_done:=true;
#      
#      iter!.next_value:=fail;
#       
#     
#    
#        iter!.next_value:=d;
#
#    NextIterator:=function(iter)
#      if not iter!.last_called_by_is_done then 
#        IsDoneIterator(iter);
#      fi;
#      iter!.last_called_by_is_done:=false;
#      return iter!.next_value;
#    end,
#    
#    ShallowCopy:=iter-> rec(classes:=[], R:=IteratorOfRClassData(s),
#     last_called_by_is_done:=false, next_value:=fail)));
#  SetIsIteratorOfDClasses(iter, true);
#  return iter;
#end);

# new for 1.0! - IteratorOfHClasses - "for an acting semigroup"
#############################################################################
# JDM could use IteratorOfRClasses here instead, not sure which is better...

# JDM should be different method for regular/inverse

InstallMethod(IteratorOfHClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local iter;
  
  if HasGreensHClasses(s) then 
    iter:=IteratorList(GreensHClasses(s));
  else
    iter:=IteratorByFunctions( rec( 

      i:=0,

      D:=IteratorOfDClasses(s),

      H:=[],

      last_called_by_is_done:=false,

      next_value:=fail,

      IsDoneIterator:=function(iter)
        
        if iter!.last_called_by_is_done then 
          return iter!.next_value=fail;
        fi;
        
        iter!.last_called_by_is_done:=true;
        iter!.next_value:=fail;
        iter!.i:=iter!.i+1;

        if iter!.i>Length(iter!.H) and not IsDoneIterator(iter!.D) then 
          iter!.i:=1;
          iter!.H:=GreensHClasses(NextIterator(iter!.D));
        fi;
        
        if iter!.i<=Length(iter!.H) then 
          iter!.next_value:=iter!.H[iter!.i];
          return false;
        fi;
          
        return true;
      end,

      NextIterator:=function(iter)
        if not iter!.last_called_by_is_done then 
          IsDoneIterator(iter);
        fi;
        iter!.last_called_by_is_done:=false;
        return iter!.next_value;
      end,
      
      ShallowCopy:=iter-> rec(i:=0, D:=IteratorOfDClasses(s),
       H:=[], last_called_by_is_done:=false, next_value:=fail)));
  fi;

  SetIsIteratorOfHClasses(iter, true);
  return iter;
end);

# new for 1.0! - IteratorOfLClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(IteratorOfLClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local iter;
  
  if HasGreensLClasses(s) then 
    iter:=IteratorList(GreensLClasses(s));
  else
    iter:=IteratorByFunctions( rec( 

      i:=0,

      D:=IteratorOfDClasses(s),

      L:=[],

      last_called_by_is_done:=false,

      next_value:=fail,

      IsDoneIterator:=function(iter)
        
        if iter!.last_called_by_is_done then 
          return iter!.next_value=fail;
        fi;
        
        iter!.last_called_by_is_done:=true;
        iter!.next_value:=fail;
        iter!.i:=iter!.i+1;

        if iter!.i>Length(iter!.L) and not IsDoneIterator(iter!.D) then 
          iter!.i:=1;
          iter!.L:=GreensLClasses(NextIterator(iter!.D));
        fi;
        
        if iter!.i<=Length(iter!.L) then 
          iter!.next_value:=iter!.L[iter!.i];
          return false;
        fi;
          
        return true;
      end,

      NextIterator:=function(iter)
        if not iter!.last_called_by_is_done then 
          IsDoneIterator(iter);
        fi;
        iter!.last_called_by_is_done:=false;
        return iter!.next_value;
      end,
      
      ShallowCopy:=iter-> rec(i:=0, D:=IteratorOfDClasses(s),
       L:=[], last_called_by_is_done:=false, next_value:=fail)));
  fi;

  SetIsIteratorOfLClasses(iter, true);
  return iter;
end);

# new for 1.0! - IteratorOfRClassData - "for an acting semigroup"
#############################################################################

#different method for regular/inverse

InstallMethod(IteratorOfRClassData, "for an acting semigroup",
[IsActingSemigroup],
function(s)

  return IteratorByFunctions( rec( 
    
    i:=1,

    IsDoneIterator:=iter-> IsClosed(SemigroupData(s)) and 
     iter!.i>=Length(SemigroupData(s)),

    NextIterator:=function(iter)
      local data;

      iter!.i:=iter!.i+1;
      
      data:=Enumerate(SemigroupData(s), iter!.i, ReturnFalse);

      if iter!.i>Length(data!.orbit) then 
        return fail;
      fi;
      return data[iter!.i];
    end,
    
    ShallowCopy:=iter-> rec(i:=1)));
end);


# new for 1.0! - IteratorOfRClasses - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(IteratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x->
CallFuncList(CreateRClassNC, x), [IsIteratorOfRClasses]));

# new for 1.0! - IteratorOfDClassReps - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(IteratorOfDClassReps, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfDClasses(s), Representative,
[IsIteratorOfDClassReps]));

# new for 1.0! - IteratorOfHClassReps - "for an acting semigroup"
#############################################################################

#JDM should be a different  method for regular/inverse using
#IteratorOfHClassData (not yet written);

InstallMethod(IteratorOfHClassReps, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfHClasses(s), Representative,
[IsIteratorOfHClassReps]));

# new for 1.0! - IteratorOfLClassReps - "for an acting semigroup"
#############################################################################

# different method for regular/inverse

InstallMethod(IteratorOfLClassReps, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfLClasses(s), Representative,
[IsIteratorOfLClassReps]));

# new for 1.0! - IteratorOfRClassReps - "for an acting semigroup"
#############################################################################

# same method for inverse/regular.

InstallMethod(IteratorOfRClassReps, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x-> x[4],
[IsIteratorOfRClassReps]));

# new for 0.7! - PrintObj - for IsIteratorOfDClassElements
############################################################################
   
InstallMethod(PrintObj, [IsIteratorOfDClassElements],
function(iter)
  Print( "<iterator of D-class>");
  return;
end);

# new for 0.7! - PrintObj - IsIteratorOfHClassElements
############################################################################

InstallMethod(PrintObj, [IsIteratorOfHClassElements],
function(iter)
  Print( "<iterator of H-class>");
  return;
end);

# new for 0.7! - PrintObj - IsIteratorOfLClassElements
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClassElements],
function(iter)
  Print( "<iterator of L-class>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfRClassElements
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassElements],
function(iter)
  Print("<iterator of R-class>");
  return;
end);

# mod for 1.0! - PrintObj - for IsIteratorOfDClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfDClassReps],
function(iter)
  Print("<iterator of D-class reps>");
  return;
end);

# mod for 1.0! - PrintObj - for IsIteratorOfHClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfHClassReps],
function(iter)
  Print("<iterator of H-class reps>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfLClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClassReps], 
function(iter)
  Print( "<iterator of L-class reps>");
  return;
end);

# mod for 1.0! - PrintObj - IsIteratorOfRClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassReps],
function(iter)
  Print("<iterator of R-class reps>");
  return;
end);

# new for 0.1! - PrintObj - "for iterator of D-classes"
############################################################################

InstallMethod(PrintObj, [IsIteratorOfDClasses], 
function(iter)
  Print( "<iterator of D-classes>");
  return;
end);

# new for 0.1! - PrintObj - "for iterator of H-classes"
############################################################################

InstallMethod(PrintObj, [IsIteratorOfHClasses], 
function(iter)
  Print( "<iterator of H-classes>");
  return;
end);
 
# new for 0.1! - PrintObj - for IsIteratorOfLClasses
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClasses],
function(iter)
  Print( "<iterator of L-classes>");
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfRClasses
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClasses],
function(iter)
  Print( "<iterator of R-classes>");
  return;
end); 

# mod for 0.8! - PrintObj - for IsIteratorOfSemigroup
############################################################################

InstallMethod(PrintObj, [IsIteratorOfSemigroup],
function(iter)
  if IsFullTransformationSemigroup(iter!.s) then
    Print("<iterator of full trans. semigroup>");
  elif IsTransformationSemigroup(iter!.s) then
    Print("<iterator of transformation semigroup>");
  elif IsPartialPermSemigroup(iter!.s) and IsInverseSemigroup(iter!.s) then
    Print("<iterator of inverse semigroup>");
  elif IsPartialPermSemigroup(iter!.s) then 
    Print("<iterator of semigroup of partial perms>");
  elif IsSymmetricInverseSemigroup(iter!.s) then 
    Print("<iterator of symmetric inverse semigroup>");
  fi;
  return;
end);

#EOF
