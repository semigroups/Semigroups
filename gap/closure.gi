


#############################################################################
# s <- old semigroup, t <- new semigroup, new <- new generators,
# j, k from O[j][k]

# JDM clean the following up! It's an extra mess due to the fact you can't make
# structural copies of hash tables!

InstallGlobalFunction(AddGeneratorsToOrbitsOfImages, 
function(s, t, new, j, k)
local O, o, filt, scc, r, m, old_scc, old_reps, ht, data, n, d, i, f, y, z, oo,
 o_t, val, l, reps, gens, g, h, kernels_ht, max, images;

O:=ImageOrbitFromData(s, [j,k]);
o:=StructuralCopy(O);
o!.ht:=HashTableForImages(o[1]);
for i in [1..Length(O)] do 
	HTAdd(o!.ht, O[i], i);
od;

AddGeneratorsToOrbit(o, new);

gens:=o!.gens;

Unbind(o!.truth); Unbind(o!.trees);
Unbind(o!.reps); 
o!.kernels_ht:=EmptyPlist(Length(O!.scc));
o!.perms:=EmptyPlist(Length(o));

filt:=function(o, scc) return not ForAny(OrbitsOfImages(t)!.orbits[j],
 x-> o[scc[1]] in x); end;

if Length(O!.scc)>1 or Length(o)>Length(O) then 
	scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(
	 OrbitGraphAsSets(o)), Set));;
	scc:=Filtered(scc, x-> filt(o,x));
else
	scc:=StructuralCopy(O!.scc);
fi;

r:=Length(scc);
o!.schutz:=List([1..r], x-> []);
o!.reps:=List([1..r], x-> []);
o!.scc:=scc;

o!.truth:=List([1..r], i-> BlistList([1..Length(o)], scc[i]));
o!.trees:=List([1..r], x-> CreateSchreierTreeOfSCC(o, x));

OrbitsOfImages(t)!.orbits[j][k]:=o;
ht:=OrbitsOfImages(t)!.ht;
data:=OrbitsOfImages(t)!.data;

for m in [1..r] do 
  old_scc:=Filtered([1..Length(O!.scc)], i-> O!.scc[i][1] in scc[m]);

  if not old_scc=[] then #scc contains an old scc
		old_reps:=StructuralCopy(O!.reps{old_scc});
		max:=List(old_scc, m-> Length(O!.reps[m]));
		max:=PositionProperty(old_scc, m-> Length(O!.reps[m])=Maximum(max));
		
		#kernels_ht:=O!.kernels_ht[old_scc[max]];
		kernels_ht:=HashTableForKernels(KernelOfTransformation(
		 old_reps[max][1][1]));
		
		for i in [1..Length(old_reps[max])] do 
			HTAdd(kernels_ht, KernelOfTransformation(old_reps[max][i][1]), i);
		od;

		o!.kernels_ht[m]:=kernels_ht;
		data:=Concatenation(data, List([1..Length(old_reps[max])], val-> 
		 [j, k, scc[m][1], m, val, 1]));
		
		o!.perms:=o!.perms+MultipliersOfSCCOfImageOrbit(gens, o, m);
		#JDM use old perms as far as possible!
		for i in [1..Length(old_scc)] do 
			old_reps[i]:=List(old_reps[i], x-> x*o!.perms[O!.scc[old_scc[i]][1]]);
		od;
		
		#old_reps[max]:=List(old_reps[max], x-> x*o!.perms[O!.scc[old_scc[max]][1]]);
		o!.schutz[m]:=CreateImageOrbitSchutzGp(gens, o, old_reps[max][1][1], m);
		#reuse old schutz gp! JDM prev.line was o!.reps[m][1][1]
		
		if Size(o!.schutz[m][2])=Size(O!.schutz[old_scc[max]][2]) then 
			o!.reps[m]:=old_reps[max];
			old_reps:=old_reps{Concatenation([1..max-1], [max+1..Length(old_reps)])};
		else
			for i in [1..Length(old_reps[max])] do 
				o!.reps[m][Length(o!.reps[m])+1]:=[old_reps[max][i][1]];
				old_reps[1][i]:=old_reps[max][i]{[2..Length(old_reps[max][i])]};
			od;
		fi;
		
		for f in Concatenation(o!.reps[m]) do 
			for g in new do  
				h:=g*f;
				val:=HTValue(ht, h);
				if val=fail then 
					HTAdd(ht, h, true);
					ht!.o[Length(ht!.o)+1]:=h;
				fi;
			od;
		od;
		
		if not old_reps=[] and not o!.schutz[m]=true then 
		
			o_t:=OrbitsOfImages(t)!.orbits; images:=OrbitsOfImages(t)!.images;
			old_reps:=List(old_reps, Concatenation); 
			
			for i in [1..Length(old_reps)] do
				for n in [1..Length(old_reps[i])] do 
					f:=old_reps[i][n];
					d:=InOrbitsOfImages(t, f, o_t, [j, k, scc[m][1], m, fail, 0], images); 
					if not d[1] then #AddToOrbitsOfImages
						val:=d[2][5]; n:=d[2][6]; reps:=o!.reps[m];
						if not val=fail then #old kernel
							reps[val][n+1]:=f;
							data[Length(data)+1]:=[j, k, scc[m][1], m, val, n+1];
						else #new kernel
							val:=Length(reps)+1;
							reps[val]:=[f];
							data[Length(data)+1]:=[j, k, scc[m][1], m, val, 1];
							HTAdd(kernels_ht, KernelOfTransformation( f ), val);
						fi;
						
						for g in new do  
							h:=g*f;
							val:=HTValue(ht, h);
							if val=fail then 
								HTAdd(ht, h, true);
								ht!.o[Length(ht!.o)+1]:=h;
							fi;
						od;
					fi;
				od;
			od;
		fi;
	else #new points in the orbit!
		f:=o!.reps[1][1][1]*EvaluateWord(gens, 
		 TraceSchreierTreeForward(o, scc[m][1]));
		o!.reps[m]:=[[f]];
		o!.kernels_ht[m]:=HashTableForKernels(
		 KernelOfTransformation(f));
		o!.perms:=o!.perms+MultipliersOfSCCOfImageOrbit(gens, o, m);
		o!.schutz[m]:=CreateImageOrbitSchutzGp(gens, o, f, m);

		#install descendants of f in OrbitsOfImages(t)!.ht
		ht:=OrbitsOfImages(t)!.ht; oo:=ht!.o;
		for y in [1..Length(gens)] do
			z:=gens[y]*f;
			if HTValue(ht, z)=fail then 
				HTAdd(ht, z, true);
				oo[Length(oo)+1]:=z;
				#schreier words here JDM
			fi;
		od;

		data[Length(data)+1]:=[j,k,scc[m][1],m,1,1];
	fi;
od;

return true;
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(ClosureSemigroup,
function(s, coll)

if not IsTransformationSemigroup(s) or not (IsTransformationCollection(coll)
 or IsTransformation(coll)) then 
	Error("Usage: transformation semigroup and transformation or ", 
	"collection of transformations.");
fi;

if IsTransformationSemigroup(coll) then 
	coll:=Generators(coll);
elif IsTransformation(coll) then 
	coll:=[coll];
fi;

return ClosureSemigroupNC(s, Filtered(coll, x-> not x in s));
end);

# new for 4.0!
#############################################################################
# this should be properly installed and tested!! JDM
# JDM clean this up!
# JDM addition of data_ht and lens should be investigated here!

InstallGlobalFunction(ClosureSemigroupNC,
function(s, new)
local t, o_s, o_t, j, n, orbits, gens, i, k, type, data, ht, val, g, h, f, l, 
 o, d;

if new=[] then 
	return s;
fi;

if IsTransformationMonoid(s) then 
	t:=Monoid(Concatenation(Generators(s), new));
else
	t:=Semigroup(Concatenation(Generators(s), new));
fi;

# initialize the R-class reps orbit!
###############################################################################

o_s:=OrbitsOfImages(s);
ht:= HTCreate(new[1]);;
HTAdd(ht, o_s!.one, true);

for f in new do 
	HTAdd(ht, f, true);
od;

ht!.o:= Concatenation([o_s!.one], new); 

for i in [o_s!.at+1..Length(o_s!.ht!.o)] do 
	g:=o_s!.ht!.o[i];
	val:=HTValue(ht, g);
	if val=fail then 
		HTAdd(ht, g, true);
		ht!.o[Length(ht!.o)+1]:=g;
	fi;
od;

###############################################################################

n:=o_s!.deg;

o_t:= Objectify(NewType(FamilyObj(t), IsOrbitsOfImages), 
rec( finished:=false,
     orbits:=EmptyPlist(n),
     lens:=List([1..n], x-> 0), 
     images:=HTCreate(ImageSetOfTransformation(new[1])),
     at:=0, 
     gens:=Generators(t),
     s:=t,
     deg := n, 
     one := o_s!.one,
     ht:=ht,
     data:=EmptyPlist(Length(o_s!.data)), 
     data_ht:=HTCreate([1,1,1,1,1,1])
));

SetOrbitsOfImages(t, o_t);

###############################################################################

j:=Maximum(List(new, Rank));
orbits:=o_t!.orbits;
data:=o_t!.data;

for i in [n,n-1..1] do 
	if IsBound(o_s!.orbits[i]) then 
		if i>j then 
			orbits[i]:=StructuralCopy(o_s!.orbits[i]);
			Append(data, RClassRepsDataFromOrbits(orbits[i], i));
			#JDM could avoid using RClassRepsDataFromOrbits
			# if the data was stored in the orbits when created. 
		else
			orbits[i]:=[];
			for k in [1..Length(o_s!.orbits[i])] do 
				AddGeneratorsToOrbitsOfImages(s, t, new, i, k);
			od;
		fi;
	fi;
od;

return t;

end);

