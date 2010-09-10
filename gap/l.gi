#############################################################################
##
#W  greens_l_orb.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

#############################################################################
## Notes

# - this file is alphabetized, keep it that way!

# - this file should only contain functions relating to kernels/l-classes!

# - should be cleaned up like r.gi!!! In particular, standardise the inputs!

# Conventions

# -low-level function make as few functions calls as possible, higher level ones
# must use LClassSchutzGp... etc.

# - d_schutz should be moved from OrbitsOfImages to OrbitsOfKernels.

## new for 4.0!
#############################################################################

# the following should be used in IteratorOfLClassReps JDM
# rewrite the following as per r.gi and AddToOrbitsOfImages...

InstallGlobalFunction(AddToOrbitsOfKernels,
function(s, o, f, data)
local j, k, l, m, val, n, g, O, one, gens, reps, schutz, convert; 

j:=data[1]; 	#ker length
k:=data[2]; 	#index of orbit containing ker
l:=data[3]; 	#position of ker in O[j][k]
m:=data[4]; 	#scc of O[j][k] containing ker
val:=data[5]; #position of img in O[j][k]!images_ht[m]
n:=data[6];
g:=data[7]

O := o!.orbits; one:=o!.one; gens:=o!.gens;
#d:=o!.data; ht:=o!.ht; o:=ht!.o;

if k = fail then #new ker and l,m,val,n g=fail

################################################################################

	ker:=KernelOfTransformation(f);

	oo:=Orb(s, ker, OnKernelsAntiAction, rec(
					treehashsize:=NextPrimeInt(Minimum(100000, 
					 3*Binomial(DegreeOfTransformationSemigroup(s), j))), 
					schreier:=true,
					gradingfunc := function(o,x) return Length(x); end, 
					orbitgraph := true, 
					onlygrades:=[Length(ker)], 
					storenumbers:=true));
					
	Enumerate(oo, Stirling2(DegreeOfTransformationSemigroup(s), j));
	
	#strongly connected components
	scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(List(OrbitGraph(oo), 
		Set)), Set));;
	r:=Length(scc);
	oo!.scc:=scc;
	
	#boolean list corresponding to membership in scc[i]
	o!.truth:=List([1..r], i-> BlistList([1..Length(oo)], scc[i]));
	
	#Schreier trees for strongly connected components
	oo!.trees:=EmptyPlist(Length(scc));
	oo!.reverse:=EmptyPlist(Length(scc));
	oo!.trees[1]:=CreateSchreierTreeOfSCC(oo,1); 
	oo!.reverse[1]:=CreateReverseSchreierTreeOfSCC(oo,1);
	
	#representatives of L-classes with image belonging in scc[i] partitioned 
	#according to their kernels
	oo!.reps:=List([1..Length(scc)], x-> []);
	Add(oo!.reps[1], [f]);
	
	oo!.convert:=List([1..Length(scc)], x-> []);
	Add(oo!.convert[1], [AsPermOfKerImg(f)]);
	
	#images of representatives of L-classes with kernel belonging in scc[i]
	oo!.images_ht:=[];
	ht := HashTableForImage(ImageSetOfTransformation(f));
	Add(oo!.images_ht, ht);
	
	#multipliers of scc containing the kernel of f
	oo!.rels:=EmptyPlist(Length(oo));
	oo!.rels:=oo!.rels+MultipliersOfSCCOfKernelOrbit(gens, oo, 1);
	
	#schutzenberger group		
	o!.schutz:=EmptyPlist(Length(scc));
	o!.schutz[1]:=RightSchutzGpOfKerOrbit(gens, oo, f, 1);
	
	#oo!.d_schutz:=List([1..r], x-> [[]]); JDM uncomment this line!
	
	if IsBound(O[j]) then 
		Add(O[j], oo);
	else
		O[j]:=[oo];
	fi;
	
	return [j, Length(O[j], 1, 1, 1, 1];

################################################################################
	
else #old ker
	reps:=O[j][k]!.reps[m]; 
	convert:=O[j][k]!.convert[m];
	
	if not Length(reps)=0 then 
		if not val=fail then #old image
			reps[val][n+1]:=g;
			convert[val][n+1]:=AsPermOfKerImg(f);
		  return [j, k, l, m, val, n+1];
		else #new image
			val:=Length(reps)+1;
			reps[val]:=[g];
			convert[val]:=[AsPermOfKerImg(g)];
			HTAdd(O[j][k]!.images_ht[m], ImageSetOfTransformation( f ), val);
			return [j,k,l,m,val,1];
		fi;
	else
		#we never considered this scc before!
		O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
		O[j][k]!.reverse[m]:=CreateReverseSchreierTreeOfSCC(O[j][k], m);
		oo!.rels:=oo!.rels+MultipliersOfSCCOfKernelOrbit(gens, O[j][k], m);
		O[j][k]!.schutz[m]:=RightSchutzGpOfKerOrbit(gens, O[j][k], g, m);
		O[j][k]!.images_ht[m]:=HashTableForImage(ImageSetOfTransformation(f));
		reps[1]:=[g];
		convert[1]:=[AsPermOfKerImg(g)];
		return [j, k, l, m, 1, 1];
	fi;
fi;

return fail;
end);


#new for 4.0!
#############################################################################
# returns a perm such that i -> ker[i]^f

InstallGlobalFunction(AsPermOfKerImg,
function(f)
local ker;

if not IsBound(f![4]) then 
	ker:=KernelOfTransformation(f);
	f![4]:=MappingPermListList([1..Length(ker)], List(ker, x-> f![1][x[1]]));
fi;

return f![4];
end);

#new for 4.0!
#############################################################################

InstallGlobalFunction(DisplayOrbitsOfKernels, 
function(s)
local o;

o:=OrbitsOfKernels(s);

Print("finished: ", o!.finished, "\n");
Print("orbits: "); View(o!.orbits); Print("\n");
Print("at: ", o!.at, "\n");
Print("ht: "); View(o!.ht); Print("\n");
Print("size: ", SizeOrbitsOfKernels(s), "\n");
Print("L-classes: ", NrLClassesOrbitsOfKernels(s), "\n");
return true;
end);

# new for 4.0!
#############################################################################

InstallMethod(GreensLClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, i, o;
Info(InfoMonoidGreens, 4, "GreensLClassReps");

iter:=IteratorOfLClassReps(s); 
for i in iter do 
od;

return List(OrbitsOfKernels(s)!.data, x-> LClassRepFromData(s, x));
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(HashTableForImage, 
function(img)
local ht;
ht := HTCreate(img, rec( hfd := 100003, treehashsize := 100003 ));
HTAdd(ht, img, 1);

return ht;
end);


#############################################################################
# new!

# Usage: s, f, OrbitsOfKernels(s)!.orbits, d_ker, d_img or s, f

InstallGlobalFunction(InOrbitsOfKernels, 
function(arg)
local s, f, O, j, k, l, m, val, n, g, d, ker, reps, t, schutz, x, h, cosets, 
i, p, r;

s:=arg[1]; f:=arg[2];

if Length(arg)=5 then 
	O:=arg[3];
	j:=arg[4][1]; k:=arg[4][2]; l:=arg[4][3];
	m:=arg[4][4]; val:=arg[4][5]; n:=arg[4][6];
	g:=arg[4][7]; 
	d:=arg[5];
	if k=fail then 
		ker:=KernelOfTransformation(f);
	fi;
	if j=fail then 
		j:=Length(ker);
	fi;
else
	ker:=KernelOfTransformation(f);
	j:=Length(ker);
	k:=fail; l:=fail; m:=fail; val:=fail; n:=0; g:=fail;
	O:=OrbitsOfKernels(s)!.orbits;
	d:=InOrbitsOfImages(s, f); 
	if not d[1] then 
		return [false, [j, fail, fail, fail, fail, 0, fail]];
	fi; #JDM correct?
	d:=d[2];
fi;

if not IsBound(O[j]) then
	return [false, [j, fail, fail, fail, fail, 0, fail]];
fi;

if k=fail then #l=fail, m=fail, g=fail
	k:=0;

	repeat
		k:=k+1;
		l:=Position(O[j][k], ker);
	until not l=fail or k=Length(O[j]);

	if l = fail then 
		return [false, [j, fail, fail, fail, fail, 0, fail]];
	fi;
	m:=PositionProperty(O[j][k]!.truth, x-> x[l]);
	
	if not IsBound(O[j][k]!.rels[l]) then
		return [false, [j,k,l,m,fail, 0, fail]];
	fi;
	g:=O[j][k]!.rels[l][2]*f;
fi;

if val=fail then 
	val:=HTValue(O[j][k]!.images_ht[m], ImageSetOfTransformation(f));
fi;

if val=fail then 
	return [false, [j, k, l, m, fail, 0, g]];
fi;

reps:=O[j][k]!.reps[m][val];
t:=Length(reps);

schutz:=O[j][k]!.schutz[m][1];

if schutz=true then 
	return [true, [j,k,l,m,val,1,g]]; 
fi;

r:=n;

while r<t do
	r:=r+1;
	p:=O[j][k]!.convert[m][val][r]^-1;
	h:=PermLeftQuoTransformationNC(reps[r], g);	
	if SiftedPermutation(schutz, h^p)=() then 
		return [true ,[j,k,l,m,val,r,g]];
	fi;
od;

# JDM something about the following is not so good. 
# RcapLSchutzGpCosetsInRFromData is called many more times than 
# the number of D-classes!? Should store the cosets in the kernel orbit
# so that the cosets are known if we get this far.

cosets:=RcapLSchutzGpCosetsInRFromData(s, OrbitsOfImages(s)!.orbits[d[1]]
 [d[2]], [d, [j,k,l,m,val,n+1,g]]);  
   #JDM n or n+1? or 1 or what?

if not cosets=[] then 
	while n<t do
		n:=n+1;
		p:=O[j][k]!.convert[m][val][n]^-1;
		for i in cosets do 
			h:=PermLeftQuoTransformationNC(reps[n], g);	
			if SiftedPermutation(schutz, (h/i)^p)=() then 
				#Error("");
				return [true ,[j,k,l,m,val,n,g]];
			fi;
		od;
	od;
fi;

return [false, [j,k,l,m,val,n,g]];
end);


# new for 4.0!
#############################################################################
# this should be renamed IteratorOfLClassRepsData and should be 
# rewritten as per IteratorOfRClassReps. Note that this should not be
# used, L-classes should be found via the D-class computation!

InstallGlobalFunction(IteratorOfLClassReps, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfLClassReps");

iter:=IteratorByFunctions( rec(
			
			IsDoneIterator := iter-> iter!.chooser(iter, IsDoneIterator)=fail,
			
			NextIterator := iter-> iter!.chooser(iter, NextIterator),
			
			ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
			last_called := NextIterator, last_value := 0, 
			chooser:=iter!.chooser, next:=iter!.next),
			
			i:=0, # in case one iterator is started, then 
			      # another iterator is started. 
			
			s:= s,
			
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
				
				o:=OrbitsOfKernels(iter!.s);
				
				if iter!.i < Length(o!.data) then 
					# we already know this rep
					iter!.i:=iter!.i+1;
					iter!.last_value:=LClassRepFromData(iter!.s, 
					 o!.data[iter!.i]);
				elif o!.finished then  
					iter!.last_value:=fail;
				else
					# must find a new rep if it exists
					iter!.i:=o!.at;
					repeat 
						iter!.last_value:=iter!.next(iter);
					until not iter!.last_value=false or iter!.last_value=fail;
				fi;
				return iter!.last_value;
			fi;
			
			end,
			
			######################################################################

			next:=function(iter) 
			local s, oo, gens, n, one, ht, o, O, data, i, ker, j, new, k, l, m, x, 
			 reps, img, ht2, images_ht, val, schutz, y, z;
			
			s := iter!.s;
			oo:=OrbitsOfKernels(s);
			gens := oo!.gens;
			n := oo!.deg;
			one := oo!.one;
			ht:= oo!.ht;
			o := ht!.o;
			O := oo!.orbits;
			data := oo!.data;
			
			if iter!.i=Length(o) then 
				oo!.finished:=true;
				return fail;
			fi;
			
			iter!.i:=iter!.i+1;
			oo!.at:=iter!.i;
			i := iter!.i;

			if iter!.i=1 then #move this to OrbitsOfKernels! JDM
				HTAdd(ht, one, true);
			fi;

			ker:=KernelOfTransformation(o[i]);
			j:=Length(ker);
			new:=false;
			
			#check if ker has been seen before
			if IsBound(O[j]) then
				k:=0;
				repeat
					k:=k+1;
					l:=Position(O[j][k], ker);
				until not l=fail or k=Length(O[j]);
				
				if l=fail then k:=fail; fi;
			else 
				k:=fail;
			fi;
			
			if k = fail then #ker has not been seen before
				new:=true; x:=o[i]; 
				
				if IsTransformationMonoid( s ) or not o[i] = one then
					ForwardOrbitOfKernelNC(s, o[i]);
				fi;
				
			else #ker has been seen before
				if IsTransformationMonoid( s ) or not o[i] = one then
					
					m:=PositionProperty(O[j][k]!.truth, x-> x[l]); #the scc containing ker 
					reps:=O[j][k]!.reps[m]; #reps of L-classes corresponding to scc[m] 
																	#partitioned by image.
					#schutz:=O[j][k]!.schutz[m];
					
					if not IsBound(O[j][k]!.rels[l]) then 
						#we never considered this scc before!
						O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
						O[j][k]!.reverse[m]:=CreateReverseSchreierTreeOfSCC(O[j][k], m);
						MultipliersOfSCCOfKernelOrbit(gens, O[j][k], m);
						x:= O[j][k]!.rels[l][2]*o[i]; #ker(x)=O[j][k][scc[m][1]]
						#schutz[Length(schutz)+1]:=
						
						O[j][k]!.schutz[m]:=RightSchutzGpOfKerOrbit(gens, O[j][k], x, m);;
						img:=ImageSetOfTransformation(x);
						ht2 := HTCreate(img, 
						 rec( hfd := 100003, treehashsize := 100003 ));
						HTAdd(ht2, img, 1);
						O[j][k]!.images_ht[m]:=ht2;
						reps[Length(reps)+1]:=[x];
						data[Length(data)+1]:=[j, k, l, m, 1, 1];
						new:=true; 
		
					else #we have considered scc before
						images_ht:=O[j][k]!.images_ht[m];
						#if Length(scc[m])=1 then do nothing fi;
						x:= O[j][k]!.rels[l][2]*o[i]; #ker(x)=O[j][k][scc[m][1]]
						val:=HTValue(images_ht, ImageSetOfTransformation(x));
						
						if not val=fail then #image seen before
							#schutz:=schutz[val][1];
							schutz:=O[j][k]!.schutz[m][1];
							if not schutz=true then 
								if not ForAny(reps[val], y->  SiftedPermutation(schutz, 
								 PermRightQuoTransformationNC(y, x))=()) then #JDM was left!
									reps[val][Length(reps[val])+1]:=x;
									data[Length(data)+1]:=[j, k, l, m, val, Length(reps[val])];
								#schutz[val][Length(schutz[val])+1]:=
								# RightSchutzGpOfKerOrbit(gens, O[j][k], x, m);
									new:=true;
								fi;
							fi;
						else #new image
							#schutz[Length(schutz)+1]:=
							# RightSchutzGpOfKerOrbit(gens, O[j][k], x, m);
							reps[Length(reps)+1]:=[x];
							data[Length(data)+1]:=[j, k, l, m, Length(reps), 1];
							HTAdd(images_ht, ImageSetOfTransformation( x ), 
							 Length(reps));
							new:=true; 
						fi;
					fi;
				fi;
			fi;
			
			if new then #install new pts in the orbit
				
				for y in [1..Length(gens)] do
					z:=x*gens[y]; 
					if HTValue(ht, z)=fail then  
						HTAdd(ht, z, true);
						o[Length(o)+1]:=z;
					fi;
				od;
				
				if IsTransformationMonoid( s ) or not o[i] = one then 
					return x;
				fi;
			fi;
			
			return false;
			end
			######################################################################
));

SetIsIteratorOfLClassReps(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);

return iter;
end);

###########################################################################
#

InstallGlobalFunction(KerRightToImgLeft,
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2];

if Length(arg)=3 then 
	o:=arg[3];
else
	o:=OrbitsOfKernels(s)!.orbits;
fi;

return o[d[1]][d[2]]!.convert[d[4]][d[5]][d[6]];
end);

###########################################################################
# 

InstallGlobalFunction(LClassData, function(list)
return Objectify(NewType(NewFamily("Green's L Class Data", IsGreensLClassData), 
IsGreensLClassData and IsGreensLClassDataRep), list);
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassKernelOrbitFromData,
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
end);

############################################################################

InstallGlobalFunction(LClassRelsFromData, 
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.rels;
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassRepFromData,
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.reps[d[4]][d[5]][d[6]];
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassSchutzGpFromData, 
function(s, d)
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.schutz[d[4]][2];
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassStabChainFromData, 
function(s, d)
#return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.schutz[d[4]][d[5]][1];
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.schutz[d[4]][1];
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(LClassSCCFromData,
function(s,d)
#Info(InfoMonoidGreens, 4, "LClassSCCFromData");
return OrbitsOfKernels(s)!.orbits[d[1]][d[2]]!.scc[d[4]];
end);


#############################################################################
# j is the index of the scc we are computing the multipliers for!

InstallGlobalFunction(MultipliersOfSCCOfKernelOrbit,
function(gens, o, j)
local rels, scc, i, f, g, k, tup, h;

#rels:=o!.rels;
rels:=EmptyPlist(Length(o));
scc:=o!.scc[j];

for i in scc do
	#reversed used below as we have a left action not right as in R-classes!
	f:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCForward(o, j, i)));
	# OnKernelAntiAction(o[scc[1]], f)=o[i]
	g:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCBack(o, j, i)));
	# OnKernelsAntiAction(o[i], g)=o[scc[1]] 
	

	#JDM check the efficiency of the following, alternatives: 
	# - could multiply g by some permutation!?
	# - take the inverse of f as a binary relation..
	# - instead of doing this just find the order of the permutation on o[scc[1]]
	#   corresponding to f*g
	# - use a while loop...

	tup:=OnTuplesOfSetsAntiAction(OnTuplesOfSetsAntiAction(o[scc[1]], f), g);
	if not tup=o[scc[1]] then 
		g:=g*(f*g)^(Order(PermListList(tup, o[scc[1]]))-1);
	fi;
	rels[i]:=[f,g];
od;

return rels;
end);

#############################################################################
#

InstallGlobalFunction(NrLClassesOrbitsOfKernels,
function(s)
local i, j, k, l, m, c;

#Info(InfoMonoidGreens, 4, "NrLClassesOrbitsOfKernel");

c:=OrbitsOfKernels(s);
m:=[];

c:=c!.orbits;

for i in c do
  for j in i do 
    for k in j!.reps do 
      for l in k do 
        Add(m, Length(l));
      od;
    od;
  od;
od;

m:=Sum(m);

if OrbitsOfKernels(s)!.finished then 
	SetNrGreensLClasses(s, m);
fi;
return m;
end);

#############################################################################

InstallMethod(OrbitsOfKernels, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
local gens;#, n, one, ht;

if IsTransformationMonoid( s ) then
	gens := GeneratorsOfMonoid( s );
else
	gens := GeneratorsOfSemigroup( s );
fi;

#n := DegreeOfTransformationSemigroup( s );
#one := TransformationNC( [ 1 .. n ] );

#JDM keep the below for use in the IteratorOfLClassRepsData
#ht := HTCreate(one);
#HTAdd(ht, one, true);
#for i in gens do 
#	HTAdd(ht, i, true);
#od;

#ht!.o := Concatenation([one], gens);

return rec(
#  finished:=false,
  orbits:=EmptyPlist(DegreeOfTransformationSemigroup(s))#, 
#  at:=0, 
  gens:=gens,
#  s:=s,
#	deg := n,
#	one := one,
#	ht:=ht,
#	data:=[]
);
end);



# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfLClassReps], 
function(iter)
local O, s;

s:=UnderlyingSemigroupOfIterator(iter);
O:=OrbitsOfKernels(s);

Print( "<iterator of L-class reps, ", Length(O!.ht!.o), " candidates, ", 
 SizeOrbitsOfKernels(s), " elements, ", NrLClassesOrbitsOfKernels(s), 
 " L-classes>");
return;
end);

#############################################################################
#

InstallMethod( PrintObj, "for object in `IsGreensLClassData'",
[ IsGreensLClassData and IsGreensLClassDataRep],
function( obj )
Print( "GreensLClassData( ", obj!.rep,  " )" );
end );

# new for 4.0!
#############################################################################
# JDM check this for efficiency!

#gens are the generators of the semigroup
#o is orbit
#f is a representative of scc with index k
#k is the index of scc containing index of image of f

# JDM could make the following shorter by using LeftSchutzGpOfKerOrbit!

InstallGlobalFunction(RightSchutzGpOfKerOrbit,
function(gens, o, f, k) 
local scc, bound, g, rels, t, graph, is_sym, i, j;

scc:=o!.scc[k];

if Length(o[scc[1]])<1000 then 
	bound:=Factorial(Length(o[scc[1]]));
else
	bound:=infinity;
fi;

g:=Group(());
rels:=o!.rels;
t:=o!.truth;
graph:=OrbitGraph(o);
is_sym:=false;

for i in scc do 
	for j in [1..Length(gens)] do 
		if IsBound(graph[i][j]) and t[k][graph[i][j]] then
  		g:=ClosureGroup(g,  PermLeftQuoTransformationNC(f, rels[graph[i][j]][2] * 
  		 (gens[j] * (rels[i][1] * f))));
		fi; 
		if Size(g)>=bound then 
		  is_sym:=true;
			break;
		fi;
	od;
	if Size(g)>=bound then 
		break;
	fi;
od;

g:=g^(AsPermOfKerImg(f)^-1);

if not is_sym then 
	return [StabChainImmutable(g), g];
else
	return [is_sym, g];
fi;
end);

#############################################################################
#

InstallGlobalFunction(LeftSchutzGpOfKerOrbit,
function(gens, o, f, k) 
local scc, bound, g, rels, t, graph, is_sym, i, j;

scc:=o!.scc[k];

if Length(o[scc[1]])<1000 then 
	bound:=Factorial(Length(o[scc[1]]));
else
	bound:=infinity;
fi;

g:=Group(());
rels:=o!.rels;
t:=o!.truth;
graph:=OrbitGraph(o);
is_sym:=false;

for i in scc do 
	for j in [1..Length(gens)] do 
		if IsBound(graph[i][j]) and t[k][graph[i][j]] then
  		g:=ClosureGroup(g,  PermLeftQuoTransformationNC(f, rels[graph[i][j]][2] * 
  		 (gens[j] * (rels[i][1] * f)))); 
		fi; 
		if Size(g)>=bound then 
		  is_sym:=true;
			break;
		fi;
	od;
	if Size(g)>=bound then 
		break;
	fi;
od;

if not is_sym then 
	return [StabChainImmutable(g), g];
else
	return [is_sym, g];
fi;
end);

# new for 4.0!
#############################################################################
# returns the size of the semigroup so far

InstallGlobalFunction(SizeOrbitsOfKernels, 
function(s)
local i, c, o, m, val;

i:=0;

#Info(InfoMonoidGreens, 4, "SizeOrbitsOfKernels");

c:=OrbitsOfKernels(s)!.orbits;

for o in Concatenation(Compacted(c)) do 
  for m in [1..Length(o!.scc)] do
    if IsBound(o!.schutz[m]) then 
      #for val in [1..Length(o!.schutz[m])] do 
      	#for l in o!.schutz[j][k] do 
      		#Error("");
      		#i:=i+Size(o!.schutz[m][val][2])*Length(o!.scc[m])*
      		# Length(o!.reps[m][val]);
      		i:=i+Size(o!.schutz[m][2])*
      		 Length(o!.scc[m])*Length(Concatenation(o!.reps[m]));
      	#od;
      #od;
    fi;
  od;
od;

#if OrbitsOfKernels(s)!.finished and not HasSize(s) then 
#	SetSize(s, i);
#fi; JDM

return i;
end);

#############################################################################
# 

InstallMethod( ViewObj, "for L-class data",
[ IsGreensLClassData and IsGreensLClassDataRep],
function( obj )
Print( "GreensLClassData( ", obj!.rep, ", ", obj!.strongorb,", ", obj!.relts,
", ", obj!.invrelts,", ", obj!.schutz, " )" );
end );
