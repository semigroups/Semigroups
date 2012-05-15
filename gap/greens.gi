#############################################################################
##
#W  greens.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#############################################################################

# new for 0.1! - \= - "for Green's class and Green's class of trans. semigp."
#############################################################################

InstallMethod(\=, "for Green's class and Green's class of trans. semigp.",
[IsGreensClassOfTransSemigp, IsGreensClassOfTransSemigp],
function(x, y)
  return x!.parent=y!.parent and x!.rep in y and Size(x)=Size(y);
end);

# new for 0.1! - \< - "for Green's class and Green's class of trans. semigp."
#############################################################################

InstallMethod(\<, "for Green's class and Green's class of trans. semigp.", 
[IsGreensClassOfTransSemigp, IsGreensClassOfTransSemigp], ReturnFail);

# new for 0.1! - \= - "for trans. semigp. and trans. semigp."
#############################################################################

InstallMethod(\=, "for a semigp. and a semigp.",
[IsSemigroup and HasGeneratorsOfSemigroup, 
IsSemigroup and HasGeneratorsOfSemigroup],
function(s, t)
  return ForAll(Generators(s), x-> x in t) and 
   ForAll(Generators(t), x-> x in s);
end); 

# fix for 0.5! - \in - "for a transformation semigroup"
#############################################################################
# Notes: not algorithm X. 

InstallMethod(\in, "for a transformation semigroup",
[IsTransformation, IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(f, s)
  local gens, o, data, iter, orbits, images, next;

  if HasAsSSortedList(s) then
    return f in AsSSortedList(s);
  fi;

  gens:=Generators(s);

  if not DegreeOfTransformation(f) = DegreeOfTransformation(gens[1]) then
    Info(InfoCitrus, 2, "trans. has different degree from semigroup.");
    return false;
  fi;

  if not (IsMonoid(s) and IsOne(f)) and RankOfTransformation(f) >
   MaximumList(List(gens, RankOfTransformation)) then
    Info(InfoCitrus, 2, "trans. has larger rank than any element of ",
     "semigroup.");   
    return false;
  fi;
  
  if HasMinimalIdeal(s) and RankOfTransformation(f) <   
   RankOfTransformation(Representative(MinimalIdeal(s))) then
    Info(InfoCitrus, 2, "trans. has smaller rank than any element of ",
     "semigroup."); 
    return false;
  fi;
  
  if HasImagesOfTransSemigroup(s) and not ImageSetOfTransformation(f) in
   ImagesOfTransSemigroup(s) then
    Info(InfoCitrus, 2, "trans. has image that does not occur in ",
     "semigroup");
    return false;
  fi;
  
  if HasKernelsOfTransSemigroup(s) and not CanonicalTransSameKernel(f) in
   KernelsOfTransSemigroup(s) then
    Info(InfoCitrus, 2, "trans. has kernel that does not occur in ", 
     "semigroup");
    return false;
  fi;

  o:=OrbitsOfImages(s); data:=PreInOrbitsOfImages(s, f, false);

  if data[1] then
    return true;
  elif o!.finished then
    return false;
#  elif not HTValue(o!.ht, f)=fail then
#    return true; JDM in lots of places it is assumed after in is called that 
# data of the element is known. With this included the data is not known..
  fi;

  iter:=IteratorOfNewRClassRepsData(s);
  orbits:=o!.orbits; images:=o!.images;

  while not IsDoneIterator(iter) do 
    next:=NextIterator(iter);
    if not data[2][2]=fail then
      if next[2]=data[2][2] and next[4]=data[2][4] and (next[5]=data[2][5] or
       data[2][5]=fail) then
        data:=InOrbitsOfImages(f![1], false, data[2], orbits, images);
      fi;
    else 
      data:=InOrbitsOfImages(f![1], false, data[2], orbits, images);
    fi;

    if data[1] then
      return true;
    fi; 
  od;

  #JDM could also put something in here that returns false if everything,
  #from OrbitsOfImages(s)!.at to the end of OrbitsOfImages(s)!.ht!.o 
  #has rank less than f. Might be a good idea when degree is very high!

  return false;
end);

#HHH

# new for 0.1! - HClassReps - "for a transformation semigp."
############################################################################

InstallMethod(HClassReps, "for a transformation semigp.",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local out, iter, i, f;
  Info(InfoCitrus, 4, "HClassReps");

  out:=EmptyPlist(NrHClasses(s));
  iter:=IteratorOfHClassReps(s);
  i:=0;

  for f in iter do
    i:=i+1;
    out[i]:=f;
  od;

  return out;
end);

#DDD

# new for 0.1! - DClassReps - "for a trans. semigroup"
#############################################################################

InstallMethod(DClassReps, "for a trans. semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
  function(s)

  ExpandOrbitsOfKernels(s);
  return List(OrbitsOfKernels(s)!.data, x-> DClassRepFromData(s, x));
end);

#EEE

# new for 0.1! - Enumerator - "for a transformation semigroup"
#############################################################################
# Notes: this is not an enumerator as I could not get an enumerator to perform 
# well here. 

InstallOtherMethod(Enumerator, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local out, iter, j, i;

  Info(InfoCitrus, 4, "Enumerator: for a trans. semigroup");

  out:=EmptyPlist(Size(s)); 

  iter:=Iterator(s);
  j:=0;

  for i in iter do 
    j:=j+1;
    out[j]:=i;
  od;

  return Immutable(out);
end);

# new for 0.4! - EnumeratorOfRClasses - "for a trans. semigroup"
#############################################################################
# Notes: NumberElement does not work for RClassNCs, JDM maybe it should!

InstallMethod(EnumeratorOfRClasses, "for a trans. semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local enum;

  if HasGreensRClasses(s) then 
    enum:=EnumeratorByFunctions(s, rec(
      
      ElementNumber:=function(enum, pos)
        return GreensRClasses(s)[pos];
      end,

      NumberElement:=function(enum, r)
        
        if not ParentAttr(r)=s then 
          return fail;
        fi;

        if IsRClassNC(r) then 
          return fail;
        fi;

        return RClassIndexFromData(s, r!.data);
      end,

      Membership:=function(r, enum)
        return not Position(enum, r)=fail;
      end,
      
      Length:=enum -> NrRClasses(s),

      PrintObj:=function(enum)
        Print( "<enumerator of R-classes>");
        return;
      end));

    return enum;
  fi;

  enum:=EnumeratorByFunctions(s, rec(
   
    ElementNumber:=function(enum, pos)
      local data, m, iter, i;

      data:=OrbitsOfImages(s)!.data; m:=Length(data);

      if m>=pos then 
        data:=data[pos];
      elif OrbitsOfImages(s)!.finished then 
        return fail;
      else
        iter:=IteratorOfNewRClassRepsData(s);
        for i in [1..pos-m-1] do 
          NextIterator(iter);
        od;
        data:=NextIterator(iter);
      fi;

      if not data=fail then 
        return CreateRClass(s, data, OrbitsOfImages(s),
          RClassRepFromData(s, data));        
      fi;
      return fail;
    end,

    NumberElement:=function(enum, r)

      if not ParentAttr(r)=s then
        return fail;
      fi;

      if IsRClassNC(r) then
        return fail;
      fi;  

      return RClassIndexFromData(s, r!.data);
    end,

    Length:=enum -> NrRClasses(s),

    PrintObj:=function(enum)
      Print( "<enumerator of R-classes>");
      return;
  end));
      
  return enum;
end);

#GGG

# new for 0.1! - GreensHClasses - "for a transformation semigroup"
##############################################################################

InstallMethod(GreensHClasses, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local iter, out, i, h;

  Info(InfoCitrus, 4, "GreensHClasses");

  iter:=IteratorOfHClasses(s);
  out:=EmptyPlist(NrHClasses(s));
  i:=0;

  for h in iter do 
    i:=i+1;
    out[i]:=h;
  od;

  return out;
end);

# new for 0.1! - GreensJClassOfElement - for a trans. semigroup and trans."
#############################################################################

InstallOtherMethod(GreensJClassOfElement, "for a trans. semigroup and trans.",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsTransformation], 
GreensDClassOfElement);

#III

# mod for 0.4! - Idempotents - "for a transformation semigroup"
#############################################################################

InstallOtherMethod(Idempotents, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local n, out, kers, imgs, j, i, ker, img;

  if IsRegularSemigroup(s) then 
    n:=DegreeOfTransformationSemigroup(s);

    if HasNrIdempotents(s) then 
      out:=EmptyPlist(NrIdempotents(s));
    else
      out:=[];
    fi;

    kers:=GradedKernelsOfTransSemigroup(s); 
    imgs:=GradedImagesOfTransSemigroup(s);

    j:=0;
    
    for i in [1..n] do
      for ker in kers[i] do
        for img in imgs[i] do 
          if IsInjectiveTransOnList(ker, img) then 
            j:=j+1;
            out[j]:=IdempotentNC(ker, img);
          fi;
        od;
      od;
    od;

    if not HasNrIdempotents(s) then 
      SetNrIdempotents(s, j);
    fi;
    return out;
  fi;

  return Concatenation(List(GreensRClasses(s), Idempotents));
end);

# new for 0.1! - Idempotents - "for a trans. semigroup and pos. int."
#############################################################################

InstallOtherMethod(Idempotents, "for a trans. semigroup and pos. int.", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup, IsPosInt],
function(s, i)
  local out, n, kers, imgs, j, ker, img, r;
  
  n:=DegreeOfTransformationSemigroup(s);
  
  if i>n then 
    return [];
  fi;

  if HasIdempotents(s) then 
    return Filtered(Idempotents(s), x-> RankOfTransformation(x)=i);
  fi; 

  if HasNrIdempotents(s) then
    out:=EmptyPlist(NrIdempotents(s));
  else
    out:=[];
  fi;

  if IsRegularSemigroup(s) then 

    kers:=GradedKernelsOfTransSemigroup(s)[i]; 
    imgs:=GradedImagesOfTransSemigroup(s)[i];
    j:=0;

    for ker in kers do
      for img in imgs do 
        if IsInjectiveTransOnList(ker, img) then 
          j:=j+1;
          out[j]:=IdempotentNC(ker, img);
        fi;
      od;
    od;

    return out;
  fi;

  for r in GreensRClasses(s) do 
    if RankOfTransformation(r!.rep)=i then 
      out:=Concatenation(out, Idempotents(r));
    fi;
  od;
  return out;
end);

# new for 0.1! - IsGreensClassOfTransSemigp - "for a Green's class"
#############################################################################

InstallMethod(IsGreensClassOfTransSemigp, "for a Green's class",
[IsGreensClass], x-> IsTransformationSemigroup(ParentAttr(x)));

# new for 0.1! - IsGreensClass - "for a Green's class"
#############################################################################

InstallOtherMethod(IsGreensClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensRClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensLClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensHClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensDClass, "for an object", [IsObject], ReturnFalse);

# mod for 0.7! - Iterator - "for a trivial trans. semigp."
#############################################################################
# Notes: required until Enumerator for a trans. semigp does not call iterator. 
# This works but is maybe not the best!

InstallOtherMethod(Iterator, "for a trivial trans. semigp", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup and IsTrivial], 9999,
function(s)
  return TrivialIterator(Generators(s)[1]);
end);

# new for 0.1! - Iterator - "for a transformation semigroup"
#############################################################################

InstallMethod(Iterator, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local iter;

  Info(InfoCitrus, 4, "Iterator: for a trans. semigroup");

  iter:= IteratorByFunctions( rec(

    R:=IteratorOfRClasses(s),

    r:=fail, s:=s,

    NextIterator:=function(iter)

      if IsDoneIterator(iter) then
        return fail;
      fi;

      if iter!.r=fail or IsDoneIterator(iter!.r) then
        iter!.r:=Iterator(NextIterator(iter!.R));
      fi;

      return NextIterator(iter!.r);
    end,

    IsDoneIterator:= iter -> IsDoneIterator(iter!.R) and
     IsDoneIterator(iter!.r),

    ShallowCopy:= iter -> rec(R:=IteratorOfRClasses(s), r:=fail)));

  SetIsIteratorOfSemigroup(iter, true);
  SetIsCitrusPkgIterator(iter, true);

  return iter;
end);

# new for 0.5! - Iterator - "for a full transformation semigroup"
#############################################################################

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
  SetIsCitrusPkgIterator(iter, true);

  return iter;
end);

#NNN

# new for 0.1! - NrIdempotents - "for a transformation semigroup"
#############################################################################

InstallMethod(NrIdempotents, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local i, iter, d;
  
  i:=0;

  if HasIdempotents(s) then 
    return Length(Idempotents(s));
  fi;

  if OrbitsOfKernels(s)!.finished and HasGreensDClasses(s) then 
    for d in GreensDClasses(s) do 
      i:=i+NrIdempotents(d);
    od;
  else
    iter:=IteratorOfRClassRepsData(s);
    for d in iter do 
      i:=i+NrIdempotentsRClassFromData(s, d);
    od;
  fi;

  return i;
end);

# new for 0.1! - NrHClasses - "for a transformation semigroup"
#############################################################################
 
InstallMethod(NrHClasses, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local i, iter, o, d;
  i:=0;

  iter:=IteratorOfDClassRepsData(s);
  o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];

  for d in iter do 
    i:=i+Length(ImageOrbitCosetsFromData(s, d[2], o[2]))*
     Length(ImageOrbitSCCFromData(s, d[1], o[1]))*
      Length(KernelOrbitSCCFromData(s, d[2], o[2]))*
       Length(KernelOrbitCosetsFromData(s, d[2], o[2]));
  od;

  return i;
end);

#OOO

# new for 0.5! - One - "for a transformation semigroup"
#############################################################################

InstallMethod(One, "for a transformation",
[IsTransformation], 10, s-> TransformationNC([1..Degree(s)]*1));

#SSS

# new for 0.1! - Size - "for a transformation semigroup"
#############################################################################
# Algorithm V.

InstallMethod(Size, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)

  Info(InfoCitrus, 4, "Size: for a trans. semigroup");

  ExpandOrbitsOfImages(s);
  return Size(OrbitsOfImages(s));
end);

# new for 0.1! - Size - "for a simple transformation semigroup"
#############################################################################
# JDM check this is actually superior to the above method for Size

InstallOtherMethod(Size, "for a simple transformation semigroup",
[IsSimpleSemigroup and IsTransformationSemigroup],
function(s)
  local gens, ims, kers, H;

  gens:=Generators(s);

  ims:=Size(Set(List(gens, ImageSetOfTransformation)));
  kers:=Size(Set(List(gens, CanonicalTransSameKernel)));
  H:=GreensHClassOfElement(s, gens[1]);

  return Size(H)*ims*kers;
end);

#UUU

# new for 0.1! - UnderlyingSemigroupOfIterator - "for a citrus pkg iterator"
#############################################################################

InstallGlobalFunction(UnderlyingSemigroupOfIterator, 
[IsCitrusPkgIterator], iter-> iter!.s);

#EOF
