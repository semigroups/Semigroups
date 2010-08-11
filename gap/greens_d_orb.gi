#############################################################################
##
#W  greens_d_orb.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

#############################################################################
## Notes


# - must work on OrbitsOfKernels/LClasses!!!

##
#############################################################################

InstallMethod(DClassRepsData, "for a trans. semigroup",
[IsTransformationSemigroup], 
function(s)
local gens, one, ht;
Info(InfoMonoidGreens, 4, "DClassRepsData");

if IsTransformationMonoid( s ) then
	gens := GeneratorsOfMonoid( s );
else
	gens := GeneratorsOfSemigroup( s );
fi;

one:=TransformationNC( [ 1 ..  DegreeOfTransformationSemigroup( s ) ] );

ht := HTCreate(one);
HTAdd(ht, one, fail);
ht!.o := [one];

return rec(
  finished:=false,
	data:=[],
	one:=one);
end);

#############################################################################
#

DisplayDClassRepsData:=function(s)
local o;
o:=DClassRepsData(s);

Print("finished: ", o!.finished, "\n");
Print("at: ", o!.at, "\n");
Print("orbit length: ", Length(o!.ht!.o), "\n");
Print("number of reps: ", Length(o!.data), "\n");

return true;
end;

# new for 3.2!
#############################################################################

InstallMethod(GreensDClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, i, o;
Info(InfoMonoidGreens, 4, "GreensDClassReps");

iter:=IteratorOfDClassReps(s); 
for i in iter do 
od;

return List(DClassRepsData(s)!.data, x-> RClassRepFromData(s, x[1]));
end);

#############################################################################
# JDM test the below for efficiency

InstallGlobalFunction(IteratorOfDClassReps, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfDClassReps");

iter:=IteratorByFunctions( rec(
			
			IsDoneIterator := iter-> iter!.chooser(iter, IsDoneIterator)=fail,
			
			NextIterator := iter-> iter!.chooser(iter, NextIterator),
			
			ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
			last_called := NextIterator, last_value := 0, 
			chooser:=iter!.chooser, next:=iter!.next),
			
			i:=0, # in case one iterator is started, then 
			      # another iterator is started. 
			
			s:= s,
			
			r:=IteratorOfRClassReps(s), 
			
			last_called := NextIterator,
				
			last_value := 0,
			
			######################################################################
			
			chooser := function( iter, called_by )
			local o;
			
			if iter!.last_called = IsDoneIterator then 
				iter!.last_called := called_by;
				return iter!.last_value; 
			fi;

			if iter!.last_called = NextIterator then
				iter!.last_called := called_by;
				if iter!.last_value=fail then 
					return fail;
				fi;
				
				o:=DClassRepsData(iter!.s);
				
				if iter!.i < Length(o!.data) then 
					# we already know this rep
					iter!.i:=iter!.i+1;
					iter!.last_value:=RClassRepFromData(iter!.s, 
					 o!.data[iter!.i]);
				elif o!.finished then  
					iter!.last_value:=fail;
				else
					# must find a new rep if it exists
					iter!.i:=iter!.i+1;
					repeat 
						iter!.last_value:=iter!.next(iter);
					until not iter!.last_value=false or iter!.last_value=fail;
				fi;
				return iter!.last_value;
			fi;
			
			end,
			
			######################################################################

			next:=function(iter) 
			local f, o, d_img, d_ker, perms, g;
		
			f:=NextIterator(iter!.r);
			if f=fail then 
				return fail;
			fi;
			
			o:=OrbitsOfImages(iter!.s);
			d_img:=o!.data[Length(o!.data)];
			
			d_ker:=InOrbitsOfKernels(OrbitsOfKernels(s), f);

			if not d_ker[1] then #this is a new element!
				d_ker:=AddToOrbitsOfKernels(s, f, d_ker[2]);
				DClassRepsData(s)!.data[Length(DClassRepsData(s)!.data)+1]:=[d_img, d_ker];
				return f;
			fi;
			return false;
			end
			######################################################################
));

SetIsIteratorOfDClassReps(iter, true);
#SetSemigroupOfIteratorOfDClassReps(iter, s);

return iter;
end);

#############################################################################
#

InstallMethod(PrintObj, [IsIteratorOfDClassReps], 
function(iter)
local s, ker, img;

s:=iter!.s;
ker:=OrbitsOfKernels(s);
img:=OrbitsOfImages(s);

Print( "<iterator of D-class reps, ", Length(OrbitsOfImages(s)!.data), 
" candidates, ",
 SizeDClassRepsData(s), " elements, ", Length(DClassRepsData(s)!.data), 
 " D-classes>");
return;
end);

#############################################################################
# JDM the following is very slow as it has to find the intersection of the 
# the schutz gps every time. Improve this!

InstallGlobalFunction(SizeDClassRepsData, 
function(s)
local data, i, d, l, r;

data:=DClassRepsData(s)!.data;
i:=0;

for d in data do
	r:=RClassSchutzGpFromData(s, d[1])[2];
	l:=LClassSchutzGpFromData(s, d[2])[2];
	i:=i+(Size(r)*Length(RClassSCCFromData(s,d[1]))
	 *Length(LClassSCCFromData(s, d[2]))*Size(l)/Size(Intersection(l,r)));
od;

return i;
end);