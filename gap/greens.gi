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

# new for 1.0! - \= - "for Green's class and Green's class of acting semigp"
#############################################################################

InstallMethod(\=, "for Green's class and class of acting semigp",
[IsActingSemigroupGreensClass, IsActingSemigroupGreensClass],
function(x, y)
  if (IsGreensRClass(x) and IsGreensRClass(y)) or
   (IsGreensLClass(x) and IsGreensLClass(y)) or
   (IsGreensDClass(x) and IsGreensDClass(y)) or
   (IsGreensHClass(x) and IsGreensHClass(y)) then
    return x!.parent=y!.parent and Representative(x) in y;
  fi;
  return x!.parent=y!.parent and Representative(x) in y and
   Size(x)=Size(y);
end);

# new for 1.0! - \< - "for Green's class and Green's class of acting semigp"
#############################################################################

InstallMethod(\<, "for Green's class and class of acting semigp",
[IsActingSemigroupGreensClass, IsActingSemigroupGreensClass],
function(x, y)
  if (IsGreensRClass(x) and IsGreensRClass(y)) or
   (IsGreensLClass(x) and IsGreensLClass(y)) or
   (IsGreensDClass(x) and IsGreensDClass(y)) or
   (IsGreensHClass(x) and IsGreensHClass(y)) then
    return x!.parent=y!.parent and Representative(x) < Representative(y);
  fi;
  return false;
end);

# new for 1.0! - \= - "for an acting semigp and acting semigp"
#############################################################################

InstallMethod(\=, "for an acting semigp and acting semigp",
[IsActingSemigroup and HasGeneratorsOfSemigroup, 
IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s, t)
  return ForAll(Generators(s), x-> x in t) and 
   ForAll(Generators(t), x-> x in s);
end); 

# new for 1.0! - \in - "for acting elt and acting semigp."
#############################################################################
# Algorithm E. 

InstallMethod(\in, "for trans. and R-class of trans. semigp.",
[IsActingElt, IsGreensRClass and IsActingSemigroupGreensClass],
function(f, r)
  local rep, s, data, o, l, schutz, g;

  rep:=Representative(r); 
  s:=ParentAttr(r);

  if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or Degree(f) <> Degree(rep) or
   Rank(f) <> Rank(rep) or RhoFunc(s)(f) <> RhoFunc(s)(rep) then
    Info(InfoCitrus, 1, "degree, rank, or rho value not equal to those of",
    " any of the R-class elements,");
    return false;
  fi;

  data:=r!.data; o:=r!.o;
  
  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;

  l:=Position(o, LambdaFunc(s)(f));

  if l = fail or OrbSCCLookup(o)[l]<>data[1] then 
    return false;
  fi;

  schutz:=LambdaOrbStabChain(o, data[1]);

  if schutz=true then
    Info(InfoCitrus, 3, "Schutz. group of R-class is symmetric group");
    return true;
  fi;

  g:=f*LambdaOrbMults(o, data[1])[l];

  if g=rep then
    Info(InfoCitrus, 3, "transformation with rectified lambda value equals ",
    "R-class representative");
    return true;
  elif schutz=false then
    Info(InfoCitrus, 3, "Schutz. group of R-class is trivial");
    return false;
  fi;

  return SiftedPermutation(schutz, LambdaPerm(s)(rep, g))=();
end);

#AAA

# new for 1.0! - AsList - "for an R-class of an acting semigp."
#############################################################################
# Algorithm D.

InstallOtherMethod(AsList, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)        
  local f, g, elts, o, m, mults, scc, p, i;
  
  f:=Representative(r); 
  o:=r!.o; 
  m:=r!.data[1];
 
  g:=List(LambdaOrbSchutzGp(o, m), x-> f*x);
  elts:=EmptyPlist(Size(r));
  
  mults:=LambdaOrbMults(o, m);
  scc:=OrbSCC(o)[m];
  
  for i in scc do
    p:=mults[i]; 
    Append(elts, g*p^-1);
  od;
  
  return elts;
end);

#CCC

# mod for 1.0! - CreateRClass - not a user function!
#############################################################################
# Usage: s = semigroup; data = lambda orbit data (any length) (specifies where
# in orbit to find the lambda value relating to the R-class); 
# orbit = lambda orbit; rep = representative.

# rep should be with rectified image only!

#JDM use the index here?

InstallGlobalFunction(CreateRClass,
function(arg)
  local r;
  
  r:=Objectify(RClassType(arg[1]), 
   rec(parent:=arg[1], data:=arg[2], o:=arg[3]));

  SetRepresentative(r, arg[4]);
  SetEquivalenceClassRelation(r, GreensRRelation(arg[1]));
  return r;
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

# mod for 1.0! - GreensRClasses - "for an acting semigroup"
##############################################################################

InstallMethod(GreensRClasses, "for an acting semigroup",
[IsActingSemigroup], 
function(s)
  local data, orbit, r, nr, out, i;

  data:=EnumerateSemigroupData(s, infinity, ReturnFalse);
  orbit:=data!.orbit;
  r:=data!.modifier;
  nr:=Length(orbit); 
  
  out:=EmptyPlist(nr);

  for i in [1+r..nr] do 
    out[i-r]:=CallFuncList(CreateRClass, orbit[i]);
  od;
  return out;
end);

# mod for 1.0! - GreensRClassOfElement - "for an acting semigp and elt."
#############################################################################

InstallOtherMethod(GreensRClassOfElement, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local pos;

  if not f in s then
    Error("the element does not belong to the semigroup,");
    return;
  fi;

  pos:=Position(SemigroupData(s), f);
  
  return CallFuncList(CreateRClass, SemigroupData(s)[pos]);
end);

# mod for 1.0! - GreensRClassOfElementNC - "for an acting semigp and elt."
#############################################################################

InstallOtherMethod(GreensRClassOfElementNC, "for an acting semigp and elt",
[IsActingSemigroup, IsActingElt],
function(s, f)
  local pos, o, g;
  
  pos:=HTValue(LambdaHT(s), LambdaFunc(s)(f));
  
  if pos<>fail then 
    o:=GradedLambdaOrbs(s)[pos[1]][pos[2]];
    g:=f*LambdaOrbMults(o, pos[1])[pos[3]];
  else
    o:=GradedLambdaOrb(s, f, false);
    g:=f;
    pos:=[1,1];
  fi;

  return CreateRClass(s, pos, o, g);
end);

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

# new for 1.0! - IteratorOfRClassData - "for an acting semigroup"
#############################################################################

InstallMethod(IteratorOfRClassData, "for an acting semigroup",
[IsActingSemigroup],
function(s)

  return IteratorByFunctions( rec( 
    
    i:=0,

    IsDoneIterator:=iter-> SemigroupData(s)!.finished and 
     iter!.i>=Length(SemigroupData(s)),

    NextIterator:=function(iter)
      local data;

      iter!.i:=iter!.i+1;
      
      data:=EnumerateSemigroupData(s, iter!.i, ReturnFalse);

      if iter!.i>Length(data!.orbit) then 
        return fail;
      fi;
      return data!.orbit[iter!.i];
    end,
    
    ShallowCopy:=iter-> rec(i:=0)));
end);

# new for 1.0! - IteratorOfRClassReps - "for an acting semigroup"
#############################################################################

InstallMethod(IteratorOfRClassReps, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x-> x[4],
[IsIteratorOfRClassReps]));

# new for 1.0! - IteratorOfRClasses - "for an acting semigroup"
#############################################################################

InstallMethod(IteratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x->
CallFuncList(CreateRClass, x), [IsIteratorOfRClasses]));

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

# mod for 1.0! - NrRClasses - "for an acting semigroup"
#############################################################################

InstallMethod(NrRClasses, "for an acting semigroup",       
[IsActingSemigroup and HasGeneratorsOfSemigroup],        
function(s)
  local data;
  
  data:=EnumerateSemigroupData(s, infinity, ReturnFalse);
  return Length(data!.orbit)-data!.modifier;
end);

#OOO

# new for 0.5! - One - "for a transformation semigroup"
#############################################################################

InstallMethod(One, "for a transformation",
[IsTransformation], 10, s-> TransformationNC([1..Degree(s)]*1));

#SSS

# new for 1.0! - Size - "for an R-class of an acting semigp."
#############################################################################
# Algorithm C. 

InstallOtherMethod(Size, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m;

  o:=r!.o; m:=r!.data[1];      
  return Size(LambdaOrbSchutzGp(o, m))*Length(OrbSCC(o)[m]);
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
