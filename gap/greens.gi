#############################################################################
##
#W  greens.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

# this file (should) contain all the technical and uninteresting functions
# governing Green's relations in MONOID.

#############################################################################



# new for 4.0! \=, "for Green's class and Green's class of trans. semigp."
#############################################################################

InstallMethod( \=, "for Green's class and Green's class of trans. semigp.",
[IsGreensClassOfTransSemigp, IsGreensClassOfTransSemigp],
function(x, y)
  return x!.parent=y!.parent and x!.rep in y and Size(x)=Size(y);
end);

# new for 4.0! - \< - "for Green's class and Green's class of trans. semigp."
#############################################################################

InstallMethod(\<, "for Green's class and Green's class of trans. semigp.", 
[IsGreensClassOfTransSemigp, IsGreensClassOfTransSemigp], ReturnFalse);

#############################################################################

InstallMethod( \=, "for trans. semigp. and trans. semigp.",
[IsTransformationSemigroup, IsTransformationSemigroup],
function(s1, s2)
return ForAll(Generators(s1), x-> x in s2) and 
 ForAll(Generators(s2), x-> x in s1);
end); 

# new for 4.0!
#############################################################################
# keep here 

InstallMethod(IsGreensClassOfTransSemigp, "for a Green's class",
[IsGreensClass], x-> IsTransformationSemigroup(ParentAttr(x)));

InstallOtherMethod(IsGreensClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensRClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensLClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensHClass, "for an object", [IsObject], ReturnFalse);
InstallOtherMethod(IsGreensDClass, "for an object", [IsObject], ReturnFalse);

#############################################################################
# keep here

InstallOtherMethod(GreensJClassOfElement, "for a trans. semigroup and elt",
[IsTransformationSemigroup and HasIsFinite and IsFinite, IsObject], 
function(s,e)
local ec;

ec := EquivalenceClassOfElementNC( GreensJRelation(s), e );
SetIsGreensClass(ec,true);
SetIsGreensJClass(ec,true);
SetIsGreensClassOfTransSemigp(ec, true);
return ec;
end);

# new for 4.0! - NrIdempotents - "for a transformation semigroup"
#############################################################################

InstallMethod(NrIdempotents, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
  local i, iter, d;
  i:=0;

  if HasIdempotents(s) then 
    return Length(Idempotents(s));
  fi;

  if OrbitsOfKernels(s)!.finished then 
    iter:=IteratorOfGreensDClasses(s);
    #JDM would be better to use IteratorOfDClassRepsData
    for d in iter do 
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

#############################################################################
# 

# the efficiency of the below can be improved by improving 
# NrGreensLClasses and NrGreensRClasses for a D-class JDM

InstallMethod(NrGreensHClasses, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local i, iter, d, r;
i:=0;

iter:=IteratorOfGreensDClasses(s);
#JDM would be better to use IteratorOfDClassRepsData

for d in iter do 
	i:=i+NrGreensHClasses(d);
od;

return i;
end);

#############################################################################
#

InstallMethod(IteratorOfIdempotents, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s);
#JDM here!
Error("not yet implemented");


end);

#############################################################################
#  JDM the following should be reviewed! depends on R-classes

InstallOtherMethod(Idempotents, "for a transformation semigroup", 
[IsTransformationSemigroup],
function(s)
local foo, n, bound, out, kers, imgs, min, max, regular, i, ker, f, img, e; 

foo:=function(f, set) #is set a transversal of ker?
local i, j;
j:=[]; 
for i in set do 
	if not f[i] in j then 
		AddSet(j, f[i]);
	else
		return false;
	fi;
od;

return true;
end;

n:=Degree(s);

if HasNrIdempotents(s) then 
	bound:=NrIdempotents(s);
elif (HasSize(s) or OrbitsOfImages(s)!.finished) and HasNrGreensHClasses(s) then 
	bound:=Size(s)/NrGreensHClasses(s);;
elif HasSize(s) or OrbitsOfImages(s)!.finished then 
	bound:=Size(s);
else
	bound:=Sum(List([0..n], k-> Binomial(n,k)*(n-k)^k)); 
fi;

out:=EmptyPlist(bound);

kers:=GradedKernelsOfTransSemigroup(s); 
imgs:=GradedImagesOfTransSemigroup(s);

min:=PositionProperty(imgs, x-> not Length(x)=0); 
max:=First([Length(imgs), Length(imgs)-1..1] , x-> not Length(imgs[x])=0);
regular:=false; 

if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
	regular:=true;
fi;

# loop over all ranks.
for i in [min..max] do
	for ker in kers[i] do
		f:=TABLE_OF_TRANS_KERNEL(ker, n);
		for img in imgs[i] do 
			if foo(f, img) then 
				e:=IdempotentNC(ker, img);
				if regular or e in s then 
					Add(out, e);
				fi;
			fi;
		od;
	od;
od;

return out;
end);

####################################################################################

InstallOtherMethod(Idempotents, "for a trans. semigroup and pos. int.", 
[IsTransformationSemigroup, IsPosInt],
function(s, i)
local foo, n, bound, out, kers, imgs, regular,  ker, f, img, e;

if i>Degree(s) then 
	return fail;
fi;

if HasIdempotents(s) then 
	return Filtered(Idempotents(s), x-> RankOfTransformation(x)=i);
fi; #JDM is this quicker? seems to be.

foo:=function(f, set) #is set a transversal of ker?
local i, j;
j:=[]; 
for i in set do 
	if not f[i] in j then 
		AddSet(j, f[i]);
	else
		return false;
	fi;
od;

return true;
end;

n:=Degree(s);

if HasNrIdempotents(s) then 
	bound:=NrIdempotents(s);
elif (HasSize(s) or OrbitsOfImages(s)!.finished) and HasNrGreensHClasses(s) then 
	bound:=Size(s)/NrGreensHClasses(s);;
elif HasSize(s) or OrbitsOfImages(s)!.finished then 
	bound:=Size(s);
else
	bound:=1000; #JDM good idea MN?
fi;

out:=EmptyPlist(bound);

kers:=GradedKernelsOfTransSemigroup(s)[i]; 
imgs:=GradedImagesOfTransSemigroup(s)[i];

regular:=false; 

if HasIsRegularSemigroup(s) and IsRegularSemigroup(s) then 
	regular:=true;
fi;

for ker in kers do
	f:=TABLE_OF_TRANS_KERNEL(ker, n);
	for img in imgs do 
		if foo(f, img) then 
			e:=IdempotentNC(ker, img);
			if regular or e in s then 
				Add(out, e);
			fi;
		fi;
	od;
od;


return out;
end);


