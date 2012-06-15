#############################################################################
##
#W  greens.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# for convenience...

# new for 1.0! - GreensClassData - "for a Green's class of an acting semigroup"
##############################################################################

InstallMethod(GreensClassData, "for a Green's class of an acting semigroup", 
[IsActingSemigroupGreensClass], x-> x!.data);

InstallOtherMethod(LambdaOrb, "for a Green's class of an acting semi",
[IsActingSemigroupGreensClass], x-> x!.o);

# mod for 1.0! - ParentAttr - "for Green's class of an acting semigroup"
############################################################################

InstallMethod(ParentAttr, "for a Green's class of an acting semigroup", 
[IsGreensClass and IsActingSemigroupGreensClass], x-> x!.parent);

# new for 1.0! - SchutzenbergerGroup - "for an R-class of an acting semigp."
#############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  return LambdaOrbSchutzGp(r!.o, r!.data[1]);
end);

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
    return ParentAttr(x)=ParentAttr(y) and Representative(x) in y;
  fi;
  return ParentAttr(x)=ParentAttr(y) and Representative(x) in y and
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
    return ParentAttr(x)=ParentAttr(y) and Representative(x) <
     Representative(y);
  fi;
  return false;
end);

# new for 1.0! - \= - "for an acting semigp and acting semigp"
#############################################################################
# JDM move this to semigroups.gi

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

  data:=GreensClassData(r); 
  o:=LambdaOrb(r);
  
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
  o:=LambdaOrb(r); 
  m:=GreensClassData(r)[1];
 
  g:=List(SchutzenbergerGroup(r), x-> f*x);
  elts:=EmptyPlist(Size(r));
  
  mults:=LambdaOrbMults(o, m);
  scc:=OrbSCC(o)[m];
  
  for i in scc do
    p:=mults[i]; 
    Append(elts, g*p^-1);
  od;
  
  return elts;
end);

# new for 0.1! - AsSSortedList - "for a Green's class of an acting semigp"
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for a Green's class of an acting semigp",
[IsGreensClass and IsActingSemigroupGreensClass], 
function(c)
  return ConstantTimeAccessList(EnumeratorSorted(c));
end);

#CCC

# mod for 1.0! - CreateRClass - not a user function!
#############################################################################
# Usage: s = semigroup; data = lambda orbit data (any length) (specifies where
# in orbit to find the lambda value relating to the R-class); 
# orbit = lambda orbit; rep = representative; 
# (optional) position in the orbit of the rep of R-class;
# (optional) position in reps of the list containing reps with the same
# lambda-rho value as the R-class rep

# rep should be with rectified image only!

InstallGlobalFunction(CreateRClass,
function(arg)
  local r, val;
  
  r:=Objectify(RClassType(arg[1]), 
   rec(parent:=arg[1], data:=arg[2], o:=arg[3]));

  if Length(arg)>4 then 
    r!.orbit_pos:=arg[5];
    r!.reps_pos:=arg[6];
    val:=false;
  else
    val:=true;
  fi;

  SetRepresentative(r, arg[4]);
  SetEquivalenceClassRelation(r, GreensRRelation(arg[1]));
  SetIsGreensClassNC(r, val);
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

# new for 0.7! - Enumerator - "for R-class of part perm inverse semigroup"
##############################################################################

InstallMethod(Enumerator, "for R-class of part perm inv semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local mults, scc;

  mults:=LambdaOrbMults(r!.o, r!.data[1]);
  scc:=OrbSCC(r!.o)[r!.data[1]];

  return EnumeratorByFunctions(r, rec(

    schutz:=Enumerator(SchutzenbergerGroup(r)),

    len:=Size(SchutzenbergerGroup(r)),


    #########################################################################

    ElementNumber:=function(enum, pos)
      local n, m, q;

      if pos>Length(enum) then 
        return fail;
      fi;

      if pos<=Length(enum!.schutz) then 
        return Representative(r)*enum!.schutz[pos];
      fi;

      n:=pos-1; m:=enum!.len;
      
      q:=QuoInt(n, m); 
      pos:=[ q, n - q * m]+1;
     
     return enum[pos[2]]/mults[scc[pos[1]]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local s, rep, o, data, l, g, j;

      s:=ParentAttr(r);
      rep:=Representative(r);
      
      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
       Degree(f) <> Degree(rep) or Rank(f) <> Rank(rep) or 
        RhoFunc(s)(f) <> RhoFunc(s)(rep) then 
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=r!.o; data:=r!.data;
      l:=Position(o, LambdaFunc(s)(f));

      if l = fail or OrbSCCLookup(o)[l]<>data[1] then 
        return fail;
      fi;
     
      g:=f*mults[l];

      j:=Position(enum!.schutz, LambdaPerm(s)(rep, g));

      if j=fail then 
        return fail;
      fi;
      return enum!.len*(Position(scc, l)-1)+j;
    end,

    #########################################################################

    Membership:=function(elm, enum)
      return elm in r;
    end,

    Length:=enum-> Size(r),

    PrintObj:=function(enum)
      Print("<enumerator of R-class>");
      return;
    end));
end);

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
  local data, pos;
  
  data:=SemigroupData(s);
  pos:=Position(data, f);
  
  if pos<>fail then 
    return CallFuncList(CreateRClass, data[pos]);
  fi;  

  return CreateRClass(s, [1,1], GradedLambdaOrb(s, f, false), f);
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

# mod for 1.0! - Idempotents - "for a R-class of a trans. semigp."
#############################################################################

InstallOtherMethod(Idempotents, "for a R-class of a trans. semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, out, rho, o, m, scc, j, tester, creator, i;

  if not IsRegularRClass(r) then
    return [];
  fi;
  
  s:=ParentAttr(r);

  if Rank(Representative(r))=Degree(s) then
    return [One(s)];
  fi;

  out:=[]; 
  
  rho:=RhoFunc(s)(Representative(r));
  o:=LambdaOrb(r); 
  m:=GreensClassData(r)[1];
  scc:=OrbSCC(o)[m];
  j:=0;
  tester:=IdempotentLambdaRhoTester(s);
  creator:=IdempotentLambdaRhoCreator(s);

  for i in scc do
    if tester(o[i], rho) then
      j:=j+1;
      out[j]:=creator(o[i], rho);
    fi;
  od;

  if HasNrIdempotents(r) then 
    SetNrIdempotents(r, j);   
  fi;

  return out;
end);

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

# new for 1.0! - IsRegularRClass - "for an R-class of an acting semi"
#############################################################################

InstallMethod(IsRegularRClass, "for an R-class of an acting semigp",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, data, rho, o, m, scc, tester, i;

  if HasNrIdempotents(r) then 
    return NrIdempotents(r)<>0;
  fi;

  s:=ParentAttr(r);
        
  if not IsGreensClassNC(r) then
    if SemigroupData(s)!.repslens[r!.reps_pos]>1 then
      return false;
    fi;
  fi; 

  data:=SemigroupData(ParentAttr(r));
  
  # is r the group of units...
  if Rank(Representative(r))=Degree(s) then
    return true;
  fi;   
 
  rho:=RhoFunc(s)(Representative(r));
  o:=LambdaOrb(r);
  m:=GreensClassData(r)[1];
  scc:=OrbSCC(o)[m];
  tester:=IdempotentLambdaRhoTester(s);

  for i in scc do
    if tester(o[i], rho) then
      return true; 
    fi;
  od;
  return false;
end);

# new for 1.0! - Iterator - "for an R-class of an acting semi"
#############################################################################

InstallMethod(Iterator, "for an R-class of an acting semigp",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, mults, iter, scc;

  o:=LambdaOrb(r); m:=GreensClassData(r)[1];
  mults:=LambdaOrbMults(o, m);
  scc:=OrbSCC(o)[m];

  if HasAsSSortedList(r) then 
    iter:=IteratorList(AsSSortedList(r));
  else
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
       
        return iter!.schutz[at[2]]/mults[scc[at[1]]];
      end,
      
      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, at:=[0,1], 
       m:=iter!.m, n:=iter!.n)));
    fi;
    
    SetIsIteratorOfRClassElements(iter, true);
    return iter;
end);

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

# new for 1.0! - NrIdempotents - "for an R-class of an acting semigp."
#############################################################################

InstallOtherMethod(NrIdempotents, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local s, rho, o, m, scc, nr, tester, i;

  if HasIsRegularRClass(r) and not IsRegularRClass(r) then 
    return 0;
  fi;

  s:=ParentAttr(r);     

  # check if we already know this...
  if not IsGreensClassNC(r) then
    if SemigroupData(s)!.repslens[r!.reps_pos]>1 then 
      return 0;
    fi;
  fi;

  # is r the group of units...
  if Rank(Representative(r))=Degree(s) then
    return 1;
  fi;

  rho:=RhoFunc(s)(Representative(r));
  o:=LambdaOrb(r); 
  m:=GreensClassData(r)[1];
  scc:=OrbSCC(o)[m];
  nr:=0;
  tester:=IdempotentLambdaRhoTester(s);

  for i in scc do
    if tester(o[i], rho) then
      nr:=nr+1;
    fi;
  od;

  return nr;
end);

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

# new for 0.5! - One - "for a transformation"
#############################################################################

InstallMethod(One, "for a transformation",
[IsTransformation], 10, s-> TransformationNC([1..Degree(s)]*1));

#PPP

# mod for 1.0! - PrintObj - IsIteratorOfRClassReps
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassReps],
function(iter)
  Print("<iterator of R-class reps>");
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
  elif IsPartialPermSemigroup(iter!.s) then
    Print("<iterator of semigroup of partial perms>");
  fi;
  return;
end);

# new for 0.1! - PrintObj - for IsIteratorOfRClassElements
############################################################################

InstallMethod(PrintObj, [IsIteratorOfRClassElements],
function(iter)
  Print("<iterator of R-class>");
  return;
end);

#RRR

# mod for 1.0! - RClassReps - "for an acting semigroup"
############################################################################

InstallMethod(RClassReps, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local data, orbit, nr, r, out, i;

  data:=EnumerateSemigroupData(s); 
  orbit:=data!.orbit;
  nr:=Length(orbit);
  r:=data!.modifier;
  out:=EmptyPlist(nr-r);

  for i in [1+r..nr] do 
    out[i-r]:=orbit[i][4];
  od;
  return out;
end);

# new for 0.1! - RClassType - "for a transformation semigroup"
############################################################################

InstallMethod(RClassType, "for a transformation semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensRClass and
         IsActingSemigroupGreensClass);
end);

#SSS

# new for 1.0! - Size - "for an R-class of an acting semigp."
#############################################################################
# Algorithm C. 

InstallOtherMethod(Size, "for an R-class of an acting semigp.",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m;
  o:=r!.o; m:=r!.data[1];      
  
  return Size(SchutzenbergerGroup(r))*Length(OrbSCC(o)[m]);
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
