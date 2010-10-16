#############################################################################
##
#W  properties.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##


# new method for 4.0! 
###########################################################################
# - must find some reasonable examples to test this on.

InstallMethod(IsBand, "for a transformation semigroup", 
[IsTransformationSemigroup], s-> IsCompletelyRegularSemigroup(s) and IsGreensHTrivial(s));
#function(s)

#local d;

#if not IsCompletelyRegularSemigroup(s) then 
#  return false;
#fi;

#for d in IteratorOfRClassRepsData(s) do 
#	if not IsTrivial(RClassSchutzGpFromData(s, d)) then
#		return false;
#	fi;
#od;

#return true;
#end);


# JDM new for 4.0!
#############################################################################

InstallMethod(IsBlockGroup, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
local iter, r, f, o, scc, reg, n, i, j, d;

if IsInverseSemigroup(s) then 
   return true;
elif IsRegularSemigroup(s) then 
   return false;
fi;

iter:=IteratorOfRClassRepsData(s);

for d in iter do
	f:=RClassRepFromData(s, d)![1];
  o:=RClassImageOrbitFromData(s, d);
  scc:=RClassSCCFromData(s, d);
  reg:=false;
  n:=Length(o[1]);
  
	for i in scc do 
		j:=Length(Set(f{o[i]}));
    if j=n and reg then
    	return false;
   	elif j=n then 
    	reg:=true;
    fi;
	od;
od;

return true;
end);

###########################################################################

InstallMethod(IsCliffordSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local gens, idem, f, g;

if HasIsInverseSemigroup(s) and not IsInverseSemigroup(s) then 
	return false;
elif HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
	return false;
elif not IsCompletelyRegularSemigroup(s) then 
  return false;
elif IsGroupAsSemigroup(s) then
  return true;
fi;

gens:=GeneratorsOfSemigroup(s);

#JDM this should be done online...
idem:=List(gens, x->IdempotentNC(KernelOfTransformation(x), 
 ImageSetOfTransformation(x)));

for f in gens do
	for g in idem do
		if not f*g=g*f then 
			return false;
		fi;
	od;
od;

return true;
end);

###########################################################################

InstallMethod(IsCommutativeSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local gens, n, i, j; 

gens:=Generators(s);
n:=Length(gens);

for i in [1..n] do
  for j in [i+1..n] do
    if not gens[i]*gens[j]=gens[j]*gens[i] then 
      return false;
    fi;
  od;
od;

return true;
end);

###########################################################################
#JDM redo the following!
# use Orb and looking for here!

InstallMethod(IsCompletelyRegularSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local gens, f, ht, o, i, g, new, val;

if HasIsRegularSemigroup(s) and not IsRegularSemigroup(s) then 
	return false;
fi;

gens:=Generators(s);

for f in gens do
  ht:=HashTableForImage(f);
  o:=[ImageSetOfTransformation(f)];
  if not OnSets(o[1], f)=o[1] then
    return false;
  fi; 

  for i in o do
    for g in gens do
      new:= OnSets(i,g);
      val:=HTValue(ht, new);
      if val=fail then
        HTAdd(ht, new, true);
        o[Length(o)+1]:=new;
        if not Length(OnSets(new, f))=Length(new) then
          return false;
        fi;
      fi;
    od;
  od;
od;

return true;
end);

###########################################################################
# this test required to avoid conflict with Smallsemi

InstallMethod( IsCompletelySimpleSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
return IsSimpleSemigroup(s) and IsFinite(s);
end);

#############################################################################
#JDM new for 4.0!

InstallMethod(IsGreensLTrivial, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local iter, d;

iter:=IteratorOfGreensDClasses(s);

#JDM here it would be useful to pass OrbitsOfKernels(s)!.orbits to 
# RClassSchutzGpFromData...

for d in iter do 
	if not (Size(LClassSchutzGpFromData(s, d!.data[2]))=1 and 
	 Length(LClassSCCFromData(s, d!.data[2]))=1) then
		return false;
	fi;
od;

return true;
end);

#############################################################################
#JDM new for 4.0!

InstallMethod(IsGreensRTrivial, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local iter, r, d;

if OrbitsOfKernels(s)!.finished then 
	iter:=IteratorOfGreensDClasses(s);
	for d in iter do 
		if not (Size(RClassSchutzGpFromData(s, d!.data[1]))=1 and 
		 Length(RClassSCCFromData(s, d!.data[1]))=1) then
			return false;
		fi;
	od;
	
	return true;
fi;

iter:=IteratorOfRClassRepsData(s); 

#JDM here it would be useful to pass OrbitsOfImages(s)!.orbits to 
# RClassSchutzGpFromData...

for d in iter do 
	if not (Size(RClassSchutzGpFromData(s, d))=1 and 
	 Length(RClassSCCFromData(s, d))=1) then 
		return false;
	fi;
od;

return true;
end);

###########################################################################

#InstallMethod(IsGreensHTrivial, "for a transformation semigroup", 
#[IsTransformationSemigroup], 
#function(s)
#local iter, g;
#JDM only have to check regular D-classes!

#if OrbitsOfKernels(s)!.finished then 
#iter:=IteratorOfGreensDClasses(s);
#	for d in iter do 
#		if not (Size(RClassSchutzGpFromData(s, d!.data[1]))=1 and 
#		 Length(RClassSCCFromData(s, d!.data[1]))=1) then
#			return false;
#		fi;
#	od;
#	
#	return true;
#fi;

#iter:=IteratorOfGreensDClasses(s);
#repeat 
#	g:=SchutzenbergerGroup(NextIterator(iter));
#	if Size(g)>1 then 
#		return false;
#	fi;
#until IsDoneIterator(iter);
#return true;
#end);

###########################################################################
 
InstallMethod(IsGroupAsSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(M)
local gens;

gens:=GeneratorsOfSemigroup(M);

return  ForAll(gens, y-> ImageSetOfTransformation(y)	
		=ImageSetOfTransformation(gens[1]))
 and 
	ForAll(gens, y->KernelOfTransformation(y)
		=KernelOfTransformation(gens[1]))
 and 
	ImageSetOfTransformation(gens[1]^2)=
        ImageSetOfTransformation(gens[1]); #it's a perm. of its image
end);

###########################################################################

InstallOtherMethod(IsInverseSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(M)
local imgs, kers, rclasses, class, ker, strongorb, numb, img, istransv;

if not IsRegularSemigroup(M) then 
   return false;
elif IsCompletelyRegularSemigroup(M) and not HasGreensRClasses(M) then
   return IsCliffordSemigroup(M);
else 
  imgs:=ImagesOfTransSemigroup(M);
  kers:=KernelsOfTransSemigroup(M);
  
  if not Length(imgs)=Length(kers) then 
     return false;
  else
     rclasses:=GreensRClasses(M);

     for class in rclasses do
        class:=GreensRClassData(class);
        ker:=KernelOfTransformation(class!.rep);
        strongorb:=class!.strongorb;
        numb:=0;
        for img in strongorb do
           istransv:=IsTransversal(ker,img);
           if istransv and numb<1 then
              numb:=numb+1;
           elif istransv then 
              return false;
           fi;
        od;
        if numb=0 then 
           return false;
        fi;
     od;

  fi;
  return true;

fi;
end);

#############################################################################
#JDM new for 4.0!

InstallMethod(IsIrredundantGeneratingSet, 
"for a collection of transformations",
[IsTransformationCollection],
function(gens)
return not ForAny(gens, x-> x in Semigroup(Difference(gens, [x])));
end);

#############################################################################
#JDM new for 4.0!

InstallOtherMethod(IsIrredundantGeneratingSet, 
"for a transformation semigroup and collection of transformations",
[IsTransformationSemigroup, IsTransformationCollection],
function(S, gens)

if S=Semigroup(gens) then 
	return IsIrredundantGeneratingSet(gens);
fi;
end);

###########################################################################

InstallMethod(IsLeftZeroSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(M)
local gens, imgs;

gens:=GeneratorsOfSemigroup(M);
imgs:=Set(List(gens, ImageSetOfTransformation));

if Size(imgs)=1 and ForAll(gens, IsIdempotent) then
   return true;
fi;
return false;
end);

#############################################################################

InstallOtherMethod(IsMonoidAsSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup], x-> One(x) in x);

###########################################################################
##  JDM is there a better way? JDM should be regular also! 

InstallMethod(IsOrthodoxSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(M)
local idems, e, f;

idems:=Idempotents(M);

for e in idems do
   for f in idems do
      if not (e*f)^2=e*f then 
         return false;
      fi;
   od;
od;

return true;
  
end);

###########################################################################
##  JDM is there a better way?

InstallMethod(IsRectangularBand, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(M)
local x, y, z, gens;

if not IsSimpleSemigroup(M) then 
   return false;
elif HasIsBand(M) then
   return IsBand(M) and IsSimpleSemigroup(M);
else
   #check the generators

   gens:=GeneratorsOfSemigroup(M);

   for x in gens do
      for y in gens do
         for z in gens do
            if not x*y*z=x*z then 
               return false;
            fi;
         od;
      od;
   od;
   #SetIsBand(M, true)
   return true;
fi; 
  
end);


# new method for 4.0! 
###########################################################################
# JDM check efficiency!

InstallOtherMethod(IsRegularSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local iter, r;
if IsCompletelyRegularSemigroup(s) then 
	return true;
elif HasGreensDClasses(s) then 
	return ForAll(GreensDClasses(s), IsRegularDClass);
elif HasGreensRClasses(s) then 
	return ForAll(GreensRClasses(s), IsRegularRClass);
else
	iter:=IteratorOfGreensRClasses(s);
	
	for r in iter do 
		if not IsRegularRClass(r) then 
			return false;
		fi;
	od; 
	return true;
fi;
end);

###########################################################################

InstallMethod(IsRightZeroSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(M)
local gens, kers;

gens:=GeneratorsOfSemigroup(M);
kers:=Set(List(gens, KernelOfTransformation));

if Length(kers)=1 and ForAll(gens, IsIdempotent) then
   return true;
else
   return false;
fi;

end);

###########################################################################
##  JDM is there a better way?

InstallMethod(IsSemiBand, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(M)

if IsOrthodoxSemigroup(M) then #JDM advantage?
  if IsCompletelyRegularSemigroup(M) and IsBand(M) then 
    return true;
  else
    return false;
  fi;
else
   return Size(M)=Size(Semigroup(Idempotents(M)));
fi;  
end);

###############################################################################

InstallMethod(IsSemilatticeAsSemigroup, [IsSemigroup],
function(s)
return IsBand(s) and IsCommutative(s);
end);

###########################################################################
##  JDM could include if IsCompletelyRegular and HasGreensDClasses etc
##  JDM but this is so fast it might not be worthwhile...

InstallMethod( IsSimpleSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(M)
local pnt, orbit, gens, s, new, g, image;
   	
gens:= GeneratorsOfSemigroup(M);

for g in gens do
  image:=ImageSetOfTransformation(g);
  orbit:=[image];
  for pnt in orbit do
    for s in gens do
      new:= OnSets(pnt,s);
      if not new in orbit then
        Add(orbit, new);
        if not Size(OnSets(new, g))=Size(image) then
          return false;
        fi;
      fi;
    od;
  od;
od;

SetIsCompletelyRegularSemigroup(M,true);
SetIsRegularSemigroup(M, true);

return true;
end);

###########################################################################

InstallOtherMethod(IsZeroSemigroup, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(S)
local zero, x, y;

zero:=MultiplicativeZero(S);

if not zero=fail then
	for x in GeneratorsOfSemigroup(S) do
		for y in GeneratorsOfSemigroup(S) do 
			if not x*y=zero then 
				return false;
			fi;
		od;
	od;
else
	return false;
fi;

return true;
end);

###########################################################################
#JDM new for 3.1.4!
#used to accept IsSemigroup as filter, changed for semex

InstallOtherMethod(IsZeroGroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
local zero, one;

zero:=MultiplicativeZero(S);
one:=MultiplicativeNeutralElement(S);

if not (zero=fail or one=fail) and Length(GreensHClasses(S))=2 then 
	return IsGroupHClass(GreensHClassOfElement(S, one));
fi;

return false;
end);

###########################################################################

InstallOtherMethod(MultiplicativeZero, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(S)
local n, imgs, m, kers, idem;

n:=DegreeOfTransformationSemigroup(S);
imgs:=GradedImagesOfTransSemigroup(S);
m:=PositionProperty([1..n], x-> not Length(imgs[x])=0);

if Length(imgs[m])=1 then
	kers:=GradedKernelsOfTransSemigroup(S); 
	if Length(kers[m])=1 then 
		idem:=Idempotent(kers[m][1], imgs[m][1]);
		if not idem=fail and Size(GreensHClassOfElement(S, idem))=1 then 
			return idem;
		fi;
	fi;
fi;

return fail;
end);

# new for 4.0!
#############################################################################

InstallOtherMethod(SmallGeneratingSet, "for a trans. coll. and pos. int", 
[IsTransformationCollection, IsPosInt], 
function(coll, bound)
local n, a, g, s, i;

n:=DegreeOfTransformation(coll[1]);

Info(InfoMonoidProperties, 3, "checking degrees of transformations in", 
 " collection...");
if not ForAll(coll, f-> Degree(f)=n) then 
	Error("Usage: collection of transformations of equal degree");
fi;

Info(InfoMonoidProperties, 3, "sorting transformations by rank...");
a:=ShallowCopy(coll);
Sort(a, function(f,g) return Rank(f)>Rank(g) and f![1]>g![1]; end);

if Rank(a[1])=n then 
	Info(InfoMonoidProperties, 3, "finding small generating set for unit", 
	" group...");
	g:=Group(List(Filtered(a, f-> Rank(f)=n), AsPermutation));
	s:=Semigroup(List(SmallGeneratingSet(g), f-> AsTransformation(f, n)));
else
	s:=Semigroup(a[1]);
fi;

i:=0;

Info(InfoMonoidProperties, 3, "looping over elements...");

while  i<Length(coll) and Size(s)<bound do 
	i:=i+1;
	if not a[i] in s then 
		s:=Semigroup(Concatenation(Generators(s), [a[i]]));
	fi;
od;

return s;
end);

# new for 4.0!
#############################################################################
#

# should probably be renamed or return a small generating set!? JDM

InstallOtherMethod(SmallGeneratingSet, "for a trans. coll.", 
[IsTransformationCollection], 
function(coll)
local n, a, g, s, i, m, j, max, info;

n:=DegreeOfTransformation(coll[1]);

Info(InfoMonoidProperties, 3, "checking degrees of transformations in", 
 " collection...");
if not ForAll(coll, f-> Degree(f)=n) then 
	Error("Usage: collection of transformations of equal degree");
fi;

Info(InfoMonoidProperties, 3, "sorting transformations by rank...");
a:=ShallowCopy(coll);
Sort(a, function(f,g) return Rank(f)>Rank(g) and f![1]>g![1]; end);

if Rank(a[1])=n then 
	Info(InfoMonoidProperties, 3, "finding small generating set for unit", 
	" group...");
	g:=Group(List(Filtered(a, f-> Rank(f)=n), AsPermutation));
	s:=Semigroup(List(SmallGeneratingSet(g), f-> AsTransformation(f, n)));
else
	s:=Semigroup(a[1]);
fi;

i:=0;
m:=Length(a);
j:=0;
max:=0;
Info(InfoMonoidProperties, 3, "looping over elements...");

info:=false;

if InfoLevel(InfoMonoidProperties)>=3 then 
	info:=true;
fi;

while  i<Length(coll) do 
	i:=i+1;
	
	if info then Print("at ", i, " of ", m, "; ", j, " generators\r"); fi;
	
	if not a[i] in s then 
		j:=j+1;
		s:=ClosureSemigroup(s, [a[i]]);
	fi;
od;

if info then 
	Print("\n");
fi;

return s;
end);

#############################################################################
#JDM there must be better methods than the following for special types of S.
#JDM new for 4.0! JDM is there a better way?

InstallOtherMethod(SmallGeneratingSet, "for a transformation semigroup",
[IsTransformationSemigroup],
function(s)
local n, gens, min, g, t, iter, r, iter_r, f, info;

n:=Degree(s);
gens:=Generators(s);

Info(InfoMonoidProperties, 3, "sorting generators by rank...");
gens:=ShallowCopy(gens);
Sort(gens, function(f,g) return Rank(f)>Rank(g) and f![1]>g![1]; end);
min:=Rank(gens[Length(gens)]);

if Rank(gens[1])=n then 
	Info(InfoMonoidProperties, 3, "finding small generating set for unit", 
	" group...");
	g:=Group(List(Filtered(gens, f-> Rank(f)=n), AsPermutation));
	t:=Semigroup(List(SmallGeneratingSet(g), f-> AsTransformation(f, n)));
else
	t:=Semigroup(gens[1]); #JDM good idea?
fi;

iter:=IteratorOfRClassRepsData(s);

info:=false;

if InfoLevel(InfoMonoidProperties)>=3 then 
	info:=true;
fi;

iter:=IteratorOfGreensRClasses(s);

while not t=s and not Length(Generators(t))>=Length(gens) do 
	r:=NextIterator(iter); f:=r!.rep;
	if min<=Rank(f) and Rank(f)<n then 
		iter_r:=Iterator(r);
		while not IsDoneIterator(iter_r) and not t=s and not 
		 Length(Generators(t))>=Length(gens) do 
			f:=NextIterator(iter_r);
			t:=ClosureSemigroup(t, [f]);
			if info then Print(Length(Generators(t)), " generators\r"); fi;
		od;
	fi;
od;

if t=s then 
	return Generators(t);
fi;

return Generators(s);
end);

#############################################################################

InstallOtherMethod(Size, "for a simple transformation semigroup",
[IsSimpleSemigroup and IsTransformationSemigroup],
function(M)
local gens, ims, kers, H;

gens:=GeneratorsOfSemigroup(M);

ims:=Size(Set(List(gens, ImageSetOfTransformation)));
kers:=Size(Set(List(gens, KernelOfTransformation)));
H:=GreensHClassOfElement(M, gens[1]);
#JDM this could be better if it used the schutz group of the R-class of 
#    any elt.

return Size(H)*ims*kers;
end);

#####################
#JDM why's this commented out? 
#InstallOtherMethod(IsMultiplicativeZero, "for a transformation semigroup", 
#true, [IsTransformationSemigroup, IsTransformation], 0,
#function(S, f)
#
#return f=MultiplicativeZero(S);
#end);