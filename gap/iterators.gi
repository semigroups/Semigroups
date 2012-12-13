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

# new for 1.0! - IteratorByIterOfIter
#############################################################################

InstallGlobalFunction(IteratorByIterOfIter,
function(s, old_iter, convert, filts)
  local iter, filt;

  iter:=IteratorByFunctions(rec(
   
    s:=s,

    iter:=old_iter,
    
    iterofiter:=fail,

    IsDoneIterator:=iter-> IsDoneIterator(iter!.iter) and 
     IsDoneIterator(iter!.iterofiter), 

    NextIterator:=function(iter)
      local iterofiter, next;

      if IsDoneIterator(iter) then 
        return fail;
      fi;
     

      if iter!.iterofiter=fail or IsDoneIterator(iter!.iterofiter) then 
        iter!.iterofiter:=Iterator(convert(NextIterator(iter!.iter)));
      fi;
      
      return NextIterator(iter!.iterofiter);
    end,

    ShallowCopy:=iter -> rec(iter:=old_iter, iterorfiter:=fail)));
  
  for filt in filts do
    SetFilterObj(iter, filt);
  od;
  return iter;
end);

#

InstallGlobalFunction(IteratorByIterator,
function(arg)
  local iter, filt, convert, isnew;
 
  # process incoming functions 
  if NumberArgumentsFunction(arg[2])=1 then 
    convert:=function(iter, x) 
      return arg[2](x);
    end;
  else
    convert:=arg[2];
  fi;

  if IsBound(arg[4]) then 
    if NumberArgumentsFunction(arg[4])=1 then 
      isnew:=function(iter, x)
        return arg[4](x);
      end;
    else
      isnew:=arg[4];
    fi;
  fi;

  # prepare iterator rec()
  if IsBound(arg[5]) then 
    iter:=arg[5];
  else
    iter:=rec();
  fi;

  iter.baseiter:=arg[1]; 
  
  iter.ShallowCopy:=iter-> rec(baseiter:=ShallowCopy(arg[1]));
  
  iter.IsDoneIterator:=iter-> IsDoneIterator(iter!.baseiter);

  # get NextIterator
  if Length(arg)=3 then 
    iter.NextIterator:=function(iter)
      local x;
      x:=NextIterator(iter!.baseiter);
      if x=fail then
        return fail;
      fi;
      return convert(iter, x);
    end;
  else
    iter.NextIterator:=function(iter)
      local baseiter, x;
      baseiter:=iter!.baseiter;
      repeat 
        x:=NextIterator(baseiter);
      until IsDoneIterator(baseiter) or isnew(iter, x);
    
      if IsDoneIterator(baseiter) then 
        return fail;
      fi;
      return convert(iter, x);
    end;
  fi;

  iter:=IteratorByFunctions(iter); 

  for filt in arg[3] do #filters
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

  return IteratorByIterOfIter(s, IteratorOfRClasses(s), x-> x,
   [IsIteratorOfSemigroup]);
end);

# new for 1.0! - Iterator - "for a D-class of an acting semigroup"
#############################################################################

# same method for regular/inverse

InstallMethod(Iterator, "for a D-class of an acting semigroup", 
[IsGreensDClass and IsActingSemigroupGreensClass], 
function(d)
  local iter, s;
  
  if HasAsSSortedList(d) then 
    iter:=IteratorList(AsSSortedList(d));
    SetIsIteratorOfDClassElements(iter, true);
    return iter;
  fi;

  s:=ParentSemigroup(d);
  return IteratorByIterOfIter(s, Iterator(GreensRClasses(d)), x-> x,
   [IsIteratorOfDClassElements]);
end);

# new for 1.0! - Iterator - "for a H-class of an acting semigroup"
#############################################################################

# same method for regular/inverse

InstallMethod(Iterator, "for a H-class of an acting semigroup", 
[IsGreensHClass and IsActingSemigroupGreensClass], 
function(h)
  local iter, s;
  
  if HasAsSSortedList(h) then 
    iter:=IteratorList(AsSSortedList(h));
    SetIsIteratorOfDClassElements(iter, true);
    return iter;
  fi;

  s:=ParentSemigroup(h);
  return IteratorByIterator(Iterator(SchutzenbergerGroup(h)), x->
   Representative(h)*x, [IsIteratorOfHClassElements]);
end);

# new for 1.0! - Iterator - "for an L-class of an acting semi"
#############################################################################

# same method for regular, there should be a different method for inverseJDM!?
# the inverse method will be almost identical to the R-class method, hence we
# should extract the relevant bits from both the L and R method and make a new
# function like in NrIdempotents@ for example. JDM

InstallMethod(Iterator, "for an L-class of an acting semigp",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, mults, iter, scc;

  if HasAsSSortedList(l) then 
    iter:=IteratorList(AsSSortedList(l));
    SetIsIteratorOfLClassElements(iter, true);
    return iter;
  fi;

  o:=RhoOrb(l); 
  m:=RhoOrbSCCIndex(l);
  mults:=RhoOrbMults(o, m);
  scc:=OrbSCC(o)[m];

  iter:=IteratorByFunctions(rec(

    #schutz:=List(SchutzenbergerGroup(r), x-> Representative(r)*x), 
    schutz:=Enumerator(SchutzenbergerGroup(l)),
    at:=[0,1],
    m:=Length(scc),
    n:=Size(SchutzenbergerGroup(l)), 

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
     
      return mults[scc[at[1]]][1]*Representative(l)*iter!.schutz[at[2]];
    end,
    
    ShallowCopy:=iter -> rec(schutz:=iter!.schutz, at:=[0,1], 
     m:=iter!.m, n:=iter!.n)));
  
  SetIsIteratorOfLClassElements(iter, true);
  return iter;
end);


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
    SetIsIteratorOfRClassElements(iter, true);
    return iter;
  fi;

  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
  mults:=LambdaOrbMults(o, m);
  scc:=OrbSCC(o)[m];

  iter:=IteratorByFunctions(rec(

    #schutz:=List(SchutzenbergerGroup(r), x-> Representative(r)*x), 
    schutz:=Enumerator(SchutzenbergerGroup(r)),
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
     
      return Representative(r)*iter!.schutz[at[2]]*mults[scc[at[1]]][1];
    end,
    
    ShallowCopy:=iter -> rec(schutz:=iter!.schutz, at:=[0,1], 
     m:=iter!.m, n:=iter!.n)));
  
  SetIsIteratorOfRClassElements(iter, true);
    return iter;
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
  
  return IteratorByIterator(
    IteratorOfRClassData(s),  # baseiter
    function(iter, x)         # convert
      local d;
      d:=DClassOfRClass(CallFuncList(CreateRClassNC, x));
      Add(iter!.classes, d);
      return d;
    end,
    [IsIteratorOfDClasses], 
    function(iter, x)         #isnew
      return x=fail or ForAll(iter!.classes, d-> not x[4] in d);
     end,
    rec(classes:=[]));        #iter
end);

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
    SetIsIteratorOfHClasses(iter, true);
    return iter;
  fi;

  return IteratorByIterOfIter(s, IteratorOfDClasses(s), GreensHClasses, 
   [IsIteratorOfHClasses]);
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
    SetIsIteratorOfLClasses(iter, true);
    return iter;
  fi;
  
  return IteratorByIterOfIter(s, IteratorOfDClasses(s), GreensLClasses, 
  [IsIteratorOfLClasses]);
end);

# different method for regular/inverse

InstallMethod(IteratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup],
function(s)
  local iter;

  if HasGreensRClasses(s) then 
    iter:=IteratorList(GreensRClasses(s));
    SetIsIteratorOfRClasses(iter, true);
    return iter;
  fi;

  return IteratorByIterator(IteratorOfRClassData(s), x->
   CallFuncList(CreateRClassNC, x), [IsIteratorOfRClasses]);
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

# new for 0.5! - Iterator - "for a full transformation semigroup"
#############################################################################

# no method required for inverse/regular

InstallMethod(Iterator, "for a full transformation semigroup",
[IsTransformationSemigroup and IsFullTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter;
  
  iter:= IteratorByFunctions( rec(

    s:=s,

    tups:=IteratorOfTuples([1..DegreeOfTransformationSemigroup(s)],
     DegreeOfTransformationSemigroup(s)),

    NextIterator:=iter-> TransformationNC(NextIterator(iter!.tups)),
  
    IsDoneIterator:=iter -> IsDoneIterator(iter!.tups),
    
    ShallowCopy:= iter -> rec(tups:=
  
    IteratorOfTuples([1..DegreeOfTransformationSemigroup(s)],
     DegreeOfTransformationSemigroup(s)))));

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
    Print("<iterator of full transformation semigroup>");
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
