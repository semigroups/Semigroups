#############################################################################
##
#W  d.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$

#############################################################################
## Notes

# - get rid of transformations altogether in this file, just use f![1]
# everywhere!

# - MN can you separate the C-function ImageAndKernelOfTransformation into 
#   ImageSetOfTransformation and KernelOfTransformation, so that the results
#   are cached in f? If it is faster!?

# - implement false for DClassLStabChain!

# - ensure foo is replaced with the optimized version

# - remove info statements from functions that are called many many times!

# -  called from
#LClassRepFromData( s, d[2] ) called from
#DClassRepFromData( s, [ d_img, d_ker ] ) called from
#GreensDClassOfElement( semi, elm ) called from
#ImagesElm( rel, rep ) called from
#Enumerator( coll ) called from

# when run on a D-class ImagesElm should work!

# - MNMN is IsTransversal a candidate for a C function? Maybe even IdempotentNC

# - is an iterator/aslist method required for D-classes!? maybe not.

# - IteratorOfGreensDClasses(s, func, vals);

# - NrRegularDClasses(s)

# - what is GroupHClassOfGreensDClass??

# - if I do Number(GreensDClasses(s), x-> Size(x)>1) then it takes ages to 
# return an answer and seems to be doing something with CollectionsFamily. 
# check this out. 

# - use RClassImageOrbitFromData instead of RClassImageOrbit

# - use ImageAndKernelOfTransformation

# - implement RClassSchutzGp=false when it is trivial!

# - don't use underlyingcollection in enumerators!

# - check that wherever we have called d:=InOrbitsOfImages we do not perform
# f*perms[l] afterwards but instead use d[7]!

# - ensure EmptyPlist is properly used...

##
#############################################################################
#RENAME

# - RClassImageOrbitFromData -> DClassImageOrbitFromData

# - LClassKernelOrbitFromData -> DClassKernelOrbitFromData

# - LClassStabChainFromData -> DClassLStabChainFromData

# - RClassImageOrbit (for a D-class) -> DClassImageOrbit

#############################################################################

InstallMethod( \=, "for D-class and D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp, IsGreensDClass and 
IsGreensClassOfTransSemigp],
function(d1, d2)
  return d1!.parent=d2!.parent and d1!.rep in d2;
end);

############################################################################

InstallMethod( \<, "for D-class and D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp, IsGreensDClass and 
IsGreensClassOfTransSemigp],
function(d1, d2)
  return d1!.parent=d2!.parent and d1!.rep < d2!.rep;
end);

# herehere jdmjdm in review and clean up. 
#############################################################################

InstallOtherMethod(\in, "for trans. and D-class of trans. semigp.",
[IsTransformation, IsGreensDClass and IsGreensClassOfTransSemigp],
function(f, d)
  local rep, img, o, i, data, ker, schutz, p, cosets;

  rep:=d!.rep; 

  if DegreeOfTransformation(f) <> DegreeOfTransformation(rep) then
    return false;
  fi;
  
  img:=ImageAndKernelOfTransformation(f)[1];

  if Length(img) <> RankOfTransformation(rep) then
    return false;
  fi;
  
  o:=DClassImageOrbit(d);
  i:= Position(o, img);
  data:=d!.data;

  if i = fail or not o!.truth[data[1][4]][i] then 
    return false;
  fi;

  f:=f*o!.perms[i]; #adjust image of f so that it is equal o[scc[1]]
  o:=DClassKernelOrbit(d);
  ker:=CanonicalTransSameKernel(f);

  i:=Position(o, ker);

  if i = fail or not o!.truth[d!.data[2][4]][i] then 
    return false;
  fi;

  f:=o!.rels[i][2]*f; #adjust kernel of f so that it is equal o[scc[1]]

  schutz:=DClassLStabChain(d);

  if schutz=true then
    return true;
  fi;

  #if schutz=false then do something

  #if f=rep then do something

  p:=KerRightToImgLeftFromData(d!.parent, d!.data, d!.o)^-1;
  cosets:=DClassRCosets(d);
  f:= PermLeftQuoTransformationNC(rep, f);

  for i in cosets do 
    if SiftedPermutation(schutz, (f/i)^p)=() then 
      return true;
    fi;
  od;

  return false;
end);

# new for 4.0! AddToOrbitsOfKernels - not a user function!
#############################################################################
# Usage: s = the semigroup; f = transformation (R-class rep with rectified
# image); data = [image data, kernel data] (without true/false, where image data
# is the the data corresponding to f with rectified image and kernel and not the 
# original f, so that data[1][3]=o[j][k]!.scc[m][1]!, only 6 components); 
# o = [OrbitsOfImages(s), OrbitsOfKernels(s)] (optional)

# JDM perhaps things could be speed up by taking everything with a ! in it
# and making it an argument?!

InstallGlobalFunction(AddToOrbitsOfKernels,
function(arg)
  local s, f, data, o, j, k, l, m, val, n, g, O, gens, d, lens, kernels, oo, reps, 
   convert, d_schutz, r_reps, i;
  
  s:=arg[1]; f:=arg[2]; data:=arg[3]; 

  if Length(arg)=4 then 
    o:=arg[4];
  else
    o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
  fi;

  j:=data[2][1];    # ker length
  k:=data[2][2];    # index of orbit containing ker
  l:=data[2][3];    # position of ker in O[j][k]
  m:=data[2][4];    # scc of O[j][k] containing ker
  val:=data[2][5];  # position of img in O[j][k]!images_ht[m]
  n:=data[2][6];    # the length of O[j][k]!.reps[m][val]
  g:=data[2][7];    # O[j][k]!.rels[l][2]*f
  #i:=data[2][8]    # the index of the coset rep. 

  O := o[2]!.orbits; gens:=o[1]!.gens; d:=o[2]!.data; lens:=o[2]!.lens;
  kernels:=o[2]!.kernels; #data_ht:=o[2]!.data_ht JDM

  if k = fail then #new ker and l, m, val, n, g, i=fail

  ################################################################################

    lens[j]:=lens[j]+1;
    oo:=ForwardOrbitOfKernel(s, f, kernels, gens);
          
    if IsBound(O[j]) then 
      O[j][lens[j]]:=oo;
    else
      O[j]:=[oo];
    fi;
          
    for i in oo do 
      HTAdd(kernels, i, lens[j]);
    od;
    
    data[2]:=[j, lens[j], 1, 1, 1, 1]; #JDM return i here?
    #JDM double check that either the line below is required or we never enter 
    #here without data[1][3] equaling the first index in the scc. 
    #data[1][3]:=OrbitsOfImages(s)!.orbits[data[1][1]][data[1][2]]!.scc[data[1][4]][1];

    Add(oo!.r_reps[1][1][1], data[1]);
    Add(oo!.d_schutz[1], [SchutzGpOfDClass(s, data, o)]);
    
    d[Length(d)+1]:=data;

  ################################################################################
          
  else #old ker
    reps:=O[j][k]!.reps[m]; 
    convert:=O[j][k]!.convert[m];
    d_schutz:=O[j][k]!.d_schutz[m];
    r_reps:=O[j][k]!.r_reps[m];

    if not Length(reps)=0 then #considered this scc before
      if not val=fail then #old image
        reps[val][n+1]:=g;
        data[2]:=[j, k, l, m, val, n+1]; #JDM return i here?
        convert[val][n+1]:=AsPermOfKerImg(f); #JDM change this...
        d_schutz[val][n+1]:=SchutzGpOfDClass(s, data, o);
        r_reps[val][n+1]:=[data[1]];
        d[Length(d)+1]:=data;
      else #new image
        val:=Length(reps)+1;
        reps[val]:=[g];
        data[2]:=[j,k,l,m,val,1];
        convert[val]:=[AsPermOfKerImg(g)];
        d_schutz[val]:=[SchutzGpOfDClass(s, data, o)];
        r_reps[val]:=[[data[1]]];
        d[Length(d)+1]:=data;
        HTAdd(O[j][k]!.images_ht[m], ImageSetOfTransformation( f ), val);
      fi;
    else #we never considered this scc before!
      
    ##############################################################################
      
      O[j][k]!.trees[m]:=CreateSchreierTreeOfSCC(O[j][k], m);
      O[j][k]!.reverse[m]:=CreateReverseSchreierTreeOfSCC(O[j][k], m);
      reps[1]:=[g];
      r_reps[1]:=[[data[1]]];
      convert[1]:=[AsPermOfKerImg(g)];
      O[j][k]!.images_ht[m]:=HashTableForImagesFixedSize(f![1]);
      O[j][k]!.rels:=O[j][k]!.rels+MultipliersOfSCCOfKernelOrbit(gens, O[j][k], m);
      O[j][k]!.schutz[m]:=RightSchutzGpOfKerOrbit(gens, O[j][k], g, m);
      data[2]:=[j, k, l, m, 1, 1];
      d_schutz[1]:=[SchutzGpOfDClass(s, data, o)];
      d[Length(d)+1]:=data;
    fi;
  fi;

  return data;
end);

# new for 4.0!
#############################################################################
# this should be removed after the library method for AsSSortedList 
# for a Green's class is removed. The default AsSSortedList for a collection
# is what should be used (it is identical)!

InstallOtherMethod(AsSSortedList, "for a D-class of trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
Info(InfoMonoidGreens, 4, "AsSSortedList: for a D-class");
return ConstantTimeAccessList(EnumeratorSorted(d));
end);

# new for 4.0!
#############################################################################
#JDM should be s, rep, data, orbit where orbit is optional and the default
# is OrbitsOfImages(s), OrbitsOfKernels(s)

InstallGlobalFunction(CreateDClass, 
function(s, data, orbit, rep)
local d;

data:=List(data, x-> x{[1..6]});

d:=Objectify(DClassType(s), rec(parent:=s, data:=data, 
o:=orbit, rep:=rep));
SetRepresentative(d, rep);
SetEquivalenceClassRelation(d, GreensDRelation(s));
return d;
end);

###########################################################################
# 

InstallGlobalFunction(DClassData, function(list)
return Objectify(NewType(NewFamily("Green's D Class Data", IsGreensDClassData), 
IsGreensDClassData and IsGreensDClassDataRep), list);
end);

#############################################################################
#

InstallMethod(DClassImageOrbit, "for a D-class of a trans. semigp", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
  local data;
  data:=d!.data;
  return d!.o[1]!.orbits[data[1]][data[2]];
end);


# new for 4.0!
############################################################################

InstallGlobalFunction(DClassKernelOrbitFromData,
function(arg)
  local s, d;

  s:=arg[1]; d:=arg[2][2];

  if Length(arg)=3 then
    return arg[3][2]!.orbits[d[1]][d[2]];
  fi;

  return OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
end);

# new for 4.0!
############################################################################

InstallMethod(DClassKernelOrbit, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]];
end);

# new for 4.0!
############################################################################

InstallGlobalFunction(DClassLStabChainFromData,
function(arg)
  local s, d, o;

  s:=arg[1]; d:=arg[2][2];

  if Length(arg)=3 then
    o:=arg[3][2]!.orbits[d[1]][d[2]];
  else
    o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
  fi;

  return o!.schutz[d[4]][1];
end);

# new for 4.0!
############################################################################

InstallMethod(DClassLStabChain, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local e;
  e:=d!.data[2];
  return d!.o[2]!.orbits[e[1]][e[2]]!.schutz[e[4]][1];
end);

#new for 4.0!
###########################################################################

InstallGlobalFunction(DClassLCosetsFromData, 
function(arg)
local s, d, o;

Error("not yet implemented");

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.d_schutz[d[4]][d[5]][d[6]][4]; #JDM currently nothing stored here!
end);



#############################################################################
# s <- semigroup; d <- d!.data ; o <- d!.o

InstallGlobalFunction(DClassRClassRepsDataFromData,
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

if IsBound(o!.r_reps[d[4]][d[5]][d[6]]) then 
	return o!.r_reps[d[4]][d[5]][d[6]];
fi;
return fail;
end);


#new for 4.0!
###########################################################################

InstallMethod(DClassRCosets, "for a D-class of trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
  local D;
  D:=d!.data[2];
  return d!.o[2]!.orbits[D[1]][D[2]]!.d_schutz[D[4]][D[5]][D[6]][3];
end);

#new for 4.0!
###########################################################################

InstallMethod(DClassLCosets, "for a D-class of trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local schutz;

schutz:=LClassSchutzGp(d);

if Size(schutz)=1 then 
	return [()];
fi;

return RightTransversal(schutz^KerRightToImgLeft(d),
	SchutzenbergerGroup(d));
end);


#new for 4.0!
###########################################################################

InstallGlobalFunction(DClassRCosetsFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.d_schutz[d[4]][d[5]][d[6]][3];
end);

#############################################################################
# returns a trans. with kernel and img in the first positions of their
# scc's. 

InstallGlobalFunction(DClassRepFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.reps[d[4]][d[5]][d[6]];
end);


#############################################################################
#

InstallGlobalFunction(DClassKernelSCC,
d-> LClassSCCFromData(d!.parent, d!.data[2], d!.o[2]));

#new for 4.0!
#############################################################################

InstallGlobalFunction(DClassSchutzGpFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.d_schutz[d[4]][d[5]][d[6]][2];
end);

#new for 4.0!
#############################################################################
#

InstallGlobalFunction(DClassStabChainFromData, 
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.d_schutz[d[4]][d[5]][d[6]][1];
end);

#new for 4.0!
#############################################################################

InstallOtherMethod(DClassOfRClass, "for an R-class of a trans. semigroup",
[IsGreensRClass and IsGreensClassOfTransSemigp], 
r-> GreensDClass(r));

#############################################################################
# 

InstallMethod(DClassType, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s);

return NewType( FamilyObj( s ), IsEquivalenceClass and 
	 IsEquivalenceClassDefaultRep and IsGreensDClass and 
	 IsGreensClassOfTransSemigp);
end);

#############################################################################
# JDM maybe insert some more info here?

InstallMethod( Display, "for D-class data'",
[ IsGreensDClassData and IsGreensDClassDataRep],
function( obj )
Print( "GreensDClassData: ", obj!.rep,  " )" );
end );

# new for 4.0!
#############################################################################
# not a user function!

InstallGlobalFunction(DisplayOrbitsOfKernels,
function(s)
  local o, k, i, j;

  o:=OrbitsOfKernels(s);

  Print("finished: \t", o!.finished, "\n");
  Print("orbits: \t");

  if ForAny([1..Degree(s)], j-> IsBound(o!.orbits[j])) then
    k:=0;
    for i in o!.orbits do
      for j in i do
        if k=1 then
          Print("\t\t");
        else
          k:=1;
        fi;
        View(j); Print("\n");
      od;
    od;
  else
    Print("\n");
  fi;

  Print("size: \t\t", SizeOrbitsOfKernels(s), "\n");
  Print("D-classes: \t", Length(o!.data), "\n");
  #Print("data ht: \t"); View(o!.data_ht); Print("\n");
  Print("kernels: \t"); View(o!.kernels); Print("\n");
  return true;
end);

# new for 4.0!
#############################################################################
# JDM test this function more!

InstallOtherMethod(Enumerator, "for a D-class of trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local enum, h;

Info(InfoMonoidGreens, 4, "Enumerator: for a D-class");

enum:=EnumeratorByFunctions(d, rec(
	
	m:=Size(GreensRClasses(d)[1]),
	
	###########################################################################
	
	ElementNumber:=function(enum, pos)
	local q, n, m, R;
	
	if pos>Length(enum) then 
		return fail;
	fi;
	R:=GreensRClasses(d);
	n:=pos-1;
	m:=enum!.m;
	
	q := QuoInt(n, m);
	pos:= [ q, n - q * m ]+1;

	return Enumerator(R[pos[1]])[pos[2]];
	end, 
	
	###########################################################################
	
	NumberElement:=function(enum, f)
	local R, i, j;
	
	R:=GreensRClasses(d);
	for i in [1..Length(R)] do 
		j:=Position(Enumerator(R[i]), f);
		if not j=fail then 
			return enum!.m*(i-1)+j;
		fi;
	od;
	return fail;
	
	end, 

	###########################################################################
	
	Membership:=function(elm, enum) 
	return elm in d; #the D-class itself!
	end,
	
	Length:=enum -> Length(GreensRClasses(d)) #NrGreensRClasses? JDM
	 *Size(GreensRClasses(d)[1]),

	PrintObj:=function(enum)
	Print( "<enumerator of D-class>");
	return;
	end));

return enum;
end);

#new for 4.0!
#############################################################################

InstallGlobalFunction(ExpandOrbitsOfKernels, 
function(s)
local iter, i;
iter:=IteratorOfNewDClassReps(s);
for i in iter do od;
return true;
end);

# new for 4.0! ForwardOrbitOfKernel
#############################################################################
# not a user function!

# Usage: s = semigroup; f = transformation; kernels =
# OrbitsOfKernels(s)!.kernels; gens = GeneratorsAsListOfImages(s).

# maybe this should take the image data as input also! and then d_schutz should 
# in this function

InstallGlobalFunction(ForwardOrbitOfKernel, 
function(arg)
  local s, f, kernels, gens, ker, deg, j, bound, treehashsize, o, scc, r, i,
  gens_imgs;
  
  s:=arg[1]; f:=arg[2];

  if Length(arg)>=3 then 
    kernels:=arg[3];
  else
    kernels:=fail;
  fi;

  if Length(arg)=4 then 
    gens_imgs:=arg[4];
  else
    gens_imgs:=GeneratorsAsListOfImages(s);
  fi;

  gens:=Generators(s);

  ker:=CanonicalTransSameKernel(f![1]); # JDM check if f![6] is bound first.
  deg:=DegreeOfTransformationSemigroup(s);
  j:=MaximumList(ker); # rank!
          
  if deg<1000 then 
    bound:=Stirling2(DegreeOfTransformationSemigroup(s), j);
    treehashsize:=3*bound;
  else
    bound:=infinity;
    treehashsize:=100000;
  fi;
          
  o:=Orb(gens_imgs, ker, function(f,g) return CanonicalTransSameKernel(f{g}); end,
         rec( treehashsize:=NextPrimeInt(Minimum(100000, treehashsize)),  
          schreier:=true,
          gradingfunc := function(o,x) return [MaximumList(x), x]; end, 
          orbitgraph := true,
          onlygrades:=function(x, y)
           return x[1]=j and (y=fail or HTValue(y, x[2])=fail); end,
          onlygrades:=kernels, 
          storenumbers:=true,
          log:=true));

  SetIsMonoidPkgImgKerOrbit(o, true);
  o!.img:=false; #for ViewObj method
  Enumerate(o, bound);

  scc:=Set(List(STRONGLY_CONNECTED_COMPONENTS_DIGRAPH(OrbitGraphAsSets(o)), Set));;

  r:=Length(scc);
  o!.scc:=scc;
  o!.scc_lookup:=ListWithIdenticalEntries(Length(o), 1);

  if Length(scc)>1 then
    for i in [2..r] do
      o!.scc_lookup{scc[i]}:=ListWithIdenticalEntries(Length(scc[i]), i);
    od;  
  fi;

  #boolean list corresponding to membership in scc[i]
  o!.truth:=List([1..r], i-> BlistList([1..Length(o)], scc[i]));
  o!.trees:=EmptyPlist(r);
  o!.reverse:=EmptyPlist(r);
  o!.trees[1]:=CreateSchreierTreeOfSCC(o,1); 
  o!.reverse[1]:=CreateReverseSchreierTreeOfSCC(o,1);

  #representatives of L-classes with kernel belonging in scc[i] partitioned 
  #according to their kernels
  o!.reps:=List([1..r], x-> []);
  Add(o!.reps[1], [f]);

  #R-class reps corresponding to D-class with rep in o!.reps
  o!.r_reps:=List([1..r], x-> []);
  Add(o!.r_reps[1], [[]]); 

  #JDM modify the following!
  o!.convert:=List([1..r], x-> []);
  Add(o!.convert[1], [AsPermOfKerImg(f)]);

  #images of representatives of L-classes with kernel belonging in scc[i]
  o!.images_ht:=[HashTableForImagesFixedSize(f![1])];

  #multipliers of scc containing the kernel of f
  o!.rels:=EmptyPlist(Length(o));
  o!.rels:=o!.rels+MultipliersOfSCCOfKernelOrbit(gens, o, 1);
          
  #schutzenberger group
  o!.schutz:=EmptyPlist(r);
  o!.schutz[1]:=RightSchutzGpOfKerOrbit(gens, o, f, 1);

  o!.d_schutz:=List([1..r], x-> []);

  return o;
end);

#new for 4.0!
#############################################################################

InstallMethod(GeneratorsAsListOfImages, "for a trans. semigp.", 
[IsTransformationSemigroup],
s-> List(Generators(s), f-> f![1]));

#new for 4.0!
#############################################################################
# think about removing this and replacing it with GreensDClassOfRClass? JDM

InstallOtherMethod(GreensDClass, "for an R-class of a trans. semigroup", 
[IsGreensRClass and IsGreensClassOfTransSemigp], 
function(r)
local s, f, d, data, j, o1, o2;

s:=r!.parent; f:=r!.rep;
d:=InOrbitsOfKernels(s, f);

if d[1] or d[2] then # f in s!
	data:=[d[3][1]]; 
	Info(InfoMonoidGreens, 2, "transformation is an element of the semigroup");
	if d[2] then # f in existing D-class
		data[2]:=d[3][2];
	else #f not in existing D-class
		data:=AddToOrbitsOfKernels(s, f, d[3]);
	fi;
	
	return CreateDClass(s, data, [OrbitsOfImages(s), 
	OrbitsOfKernels(s)], DClassRepFromData(s, data));
elif OrbitsOfImages(s)!.finished then #f not in s!
	Info(InfoMonoidGreens, 2, "transformation is not an element of the ",
	 "semigroup");
	return fail;
fi;

Info(InfoMonoidGreens, 2, "transformation may not be an element of the ",
 "semigroup");

j:=Length(ImageSetOfTransformation(f));

o1:=r!.o;

Info(InfoMonoidGreens, 2, "finding orbit of kernel...");
o2:=[];
o2[j]:=[ForwardOrbitOfKernel(s, f, function(o, scc) return scc[1]=1; end)];

d:=[j,1,1,1,1,1];
o2:=rec(gens:=Generators(s), orbits:=o2, data:=[]);

Info(InfoMonoidGreens, 2, "finding the Schutzenberger group...");
Add(o2!.orbits[j][1]!.d_schutz[1], 
 [SchutzGpOfDClass(s, [r!.data, d])]);

return CreateDClass(s, [r!.data, d], [o1, o2], f);
end);

#new for 4.0!
#############################################################################
# JDM test!

InstallOtherMethod(GreensDClass, "for an L-class of a trans. semigroup", 
[IsGreensLClass and IsGreensClassOfTransSemigp], 
function(l)
local s, d, o, rep;

s:=l!.parent;
d:=l!.data{[1,2]};
o:=l!.o;
#JDM couldn't the below be DClassRepFromData(s, l!.data, o)?
rep:=LClassRepFromData(s, Concatenation(d, [[1,1]]), o);

return CreateDClass(s, d, o, rep);
end);

#new for 4.0!
#############################################################################
# JDM test!

InstallOtherMethod(GreensDClass, "for an H-class of a trans. semigroup", 
[IsGreensHClass and IsGreensClassOfTransSemigp], 
function(h)
local s, d, o, rep;

s:=h!.parent;
d:=h!.data;
o:=h!.o;
rep:=DClassRepFromData(s, d, o);

d:=d{[1,2]}; #JDM this line can be omitted when things are cleanup!

return CreateDClass(s, d, o, rep);
end);


# new for 4.0!
#############################################################################
# JDM test the efficiency of this function!

InstallMethod(GreensDClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)
local iter, out, i;

iter:=IteratorOfGreensDClasses(s);
out:=EmptyPlist(Length(OrbitsOfKernels(s)!.data));
#JDM is the previous a good idea?

for i in iter do 
	out[Length(out)+1]:=i;
od;

return out;
end);


# new for 4.0!
#############################################################################

InstallOtherMethod(GreensDClassOfElement, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, D;

if not f in s then 
	Info(InfoWarning, 1, "transformation is not an element of the semigroup");
	return fail;
fi;

d:=InOrbitsOfKernels(s, f);

if not d[2] then #orbit of kernel not previously calculated!
	#d[3][1][3]:=RClassSCCFromData(s, d[3][1])[1]; JDM this line required no?
	d:=AddToOrbitsOfKernels(s, d[3][1][7], d[3]); 
	 #d[3][1][7] is f with rectified image!
	D:=OrbitsOfKernels(s)!.data;
	D[Length(D)+1]:=List(d, x-> x{[1..6]});
else
	d:=d[3];
fi;

d:=CreateDClass(s, d, [OrbitsOfImages(s), OrbitsOfKernels(s)], 
 DClassRepFromData(s, d));

return d;
end);

# new for 4.0!
#############################################################################
# the first part below should look very much like GreensDClassOfElement JDM
# for some reason it does not...

InstallOtherMethod(GreensDClassOfElementNC, "for a trans. semigp and trans.", 
[IsTransformationSemigroup, IsTransformation],
function(s, f)
local d, o1, o2, j, data;

Info(InfoMonoidGreens, 4, "GreensDClassOfElementNC");

d:=InOrbitsOfKernels(s, f);

if d[1] then 
	data:=[d[3][1]];
	Info(InfoMonoidGreens, 2, "transformation is an element of the semigroup");
	if d[2] then 
		data[2]:=d[3][2];
	else
		data:=AddToOrbitsOfKernels(s, f, d[3]);
	fi;
	
	return CreateDClass(s, data, [OrbitsOfImages(s), 
	OrbitsOfKernels(s)], DClassRepFromData(s, data));
elif OrbitsOfImages(s)!.finished then #f not in s!
	Info(InfoMonoidGreens, 2, "transformation is not an element of the ",
	 "semigroup");
	return fail;
fi;

Info(InfoMonoidGreens, 2, "transformation may not be an element of the ",
 "semigroup");

j:=Length(ImageSetOfTransformation(f));

Info(InfoMonoidGreens, 2, "finding orbit of image...");
o1:=[];
o1[j]:=[ForwardOrbitOfImage(s, f)[1]];
Info(InfoMonoidGreens, 2, "finding orbit of kernel...");
o2:=[];
o2[j]:=[ForwardOrbitOfKernel(s, f)];

d:=[j,1,1,1,1,1];

o1:=rec( finished:=false, orbits:=o1, gens:=Generators(s), s:=s, 
 deg := DegreeOfTransformationSemigroup(s), data:=[d]);
o2:=rec( orbits:=o2, gens:=Generators(s), data:=[d]);

Info(InfoMonoidGreens, 2, "finding the Schutzenberger group");
Add(o2!.orbits[j][1]!.d_schutz[1], [SchutzGpOfDClass(s, [d,d])]);

return CreateDClass(s, [d, d], [o1, o2], f);
end);

# new for 4.0!
#############################################################################

InstallMethod(GreensDClassReps, "for a trans. semigroup", 
[IsTransformationSemigroup], 
function(s)

ExpandOrbitsOfKernels(s);
return List(OrbitsOfKernels(s)!.data, x-> DClassRepFromData(s, x));
end);

#############################################################################

InstallOtherMethod(GreensLClassRepsData, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local f, scc, m, out, k, data, i, j;

f:=Representative(d);
scc:=RClassSCC(d);
m:=Length(DClassRCosets(d));

out:=EmptyPlist(Length(scc)*m);
SetNrGreensLClasses(d, Length(scc)*m);

k:=0;
data:=d!.data;

for i in scc do 
	for j in [1..m] do 
		k:=k+1;
		out[k]:=ShallowCopy(data);
		out[k][3]:=[i,j];
	od;
od;

return out;
end);

#############################################################################
# maybe write iterator/enumerator later! JDM

InstallOtherMethod(GreensLClassReps, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local perms, cosets, f, out, i, j, k;

# is the following worth it? JDM 
if HasGreensLClassRepsData(d) then 
	return List(GreensLClassRepsData(d), x-> 
	 LClassRepFromData(d!.parent, x, d!.o));
fi;

perms:=RClassPerms(d){RClassSCC(d)};
cosets:=DClassRCosets(d);
f:=Representative(d);

out:=EmptyPlist(Length(perms)*Length(cosets));
SetNrGreensLClasses(d, Length(perms)*Length(cosets));
k:=0;

for i in perms do 
	for j in cosets do 
		k:=k+1;
		out[k]:=f*(j/i);
	od;
od;

return out;
end);

#############################################################################
# maybe write iterator/enumerator later! JDM

InstallOtherMethod(GreensRClassRepsData, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local f, rels, cosets, j, k, m, out, val, l, a, b, g, data, orbits, images;

out:=DClassRClassRepsDataFromData(d!.parent, d!.data, d!.o);

if Length(out)=NrGreensRClasses(d) then 
	return out;
fi;

f:=Representative(d);
rels:=LClassRels(d){LClassSCC(d)};;
cosets:=DClassLCosets(d);

j:=d!.data[1][1]; k:=d!.data[1][2]; l:=d!.data[1][3]; m:=d!.data[1][4];
val:=List(rels, x-> HTValue(DClassImageOrbit(d)!.kernels_ht[m], 
 KernelOfTransformation(x[1]*f)));
# maybe the above should be stored or produced in such a way that we don't 
# have to search! 

out:=EmptyPlist(Length(rels)*Length(cosets));
SetNrGreensRClasses(d, Length(rels)*Length(cosets));
orbits:=d!.o[1]!.orbits; images:=d!.o[1]!.images;

for a in [1..Length(rels)] do
	g:=rels[a][1]*f; 
	for b in [1..Length(cosets)] do 
		g:=g*cosets[b]^-1;
		data:=InOrbitsOfImages(d, g, [j, k, l, m, val[a], 0, fail], orbits, images);
		#could do SiftedPermutation directly here, maybe speed things up?
		if not data[1] then 
			data:=AddToOrbitsOfImages(d, g, data[2], d!.o[1]);
		else 
			data:=data[2];
		fi;
		out[Length(out)+1]:=data;
	od;
od;

return out;
end);

#############################################################################

InstallOtherMethod(GreensRClassReps, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local rels, cosets, f, out, i, j, g;

if HasGreensRClassRepsData(d) then 
	return List(GreensRClassRepsData(d), x-> 
	 RClassRepFromData(d!.parent, x, d!.o[1]));
else 
	out:=DClassRClassRepsDataFromData(d!.parent, d!.data, d!.o);
	if not out=fail then
		SetGreensRClassRepsData(d, out);
		return List(GreensRClassRepsData(d), x-> 
		 RClassRepFromData(d!.parent, x, d!.o[1]));
	fi;
fi;

rels:=LClassRels(d){LClassSCC(d)};;
cosets:=DClassLCosets(d);
f:=Representative(d);

out:=EmptyPlist(Length(rels)*Length(cosets));
SetNrGreensRClasses(d, Length(rels)*Length(cosets));

for i in rels do
	g:=i[1]*f;
	for j in cosets do 
		out[Length(out)+1]:=g*j^-1;
	od;
od;

return out;
end);

#############################################################################
#

InstallOtherMethod(GreensLClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local s, o, m, out, i, f, l, data;

s:=d!.parent; o:=d!.o; m:=NrGreensLClasses(d);
out:=EmptyPlist(m); 

for i in [1..m] do 
	data:=GreensLClassRepsData(d)[i]; 
	if HasGreensLClassReps(d) then 
		f:=GreensLClassReps(d)[i];
	else
		f:=LClassRepFromData(s, data, o);
	fi;
	l:=CreateLClass(s, data, o, f);
	SetGreensDClass(l, d);
	out[i]:=l;
od;

return out;
end);

#############################################################################
# JDM could this be better/more efficient!

InstallOtherMethod(GreensHClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)

return Concatenation(List(GreensRClasses(d), GreensHClasses));
end);


#############################################################################
#

InstallOtherMethod(GreensRClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local s, o, out, f, r, data;

s:=d!.parent; o:=d!.o[1]; 
out:=EmptyPlist(NrGreensRClasses(d)); 

for data in GreensRClassRepsData(d) do 
	f:=RClassRepFromData(s, data, o);
	r:=CreateRClass(s, data, o, f);
	SetGreensDClass(r, d);
	out[Length(out)+1]:=r;#JDM change this line!
od;

return out;
end);

#############################################################################
#

InstallOtherMethod(GreensRClassOfElement, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp, IsTransformation], 
function(d, f)

if not f in d then 
	Info(InfoWarning, 1, "transformation is not an element of the D-class");
	return fail;
fi;

Error("not yet implemented");

end);



#############################################################################
# JDM is this correct? 
# could also try finding the idempotents of one R-class and then multiplying
# them as in GreensRClassReps
# JDM improve as per l.gi and r.gi


InstallOtherMethod( Idempotents, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local ker, n, i, j, k, out, img, m, reps;

if HasIsRegularDClass(d) and not IsRegularDClass(d) then 
	return [];
fi;

ker:=DClassKernelOrbit(d){DClassKernelSCC(d)};
n:=Length(d!.rep![1]);
reps:=[];

for i in [1..Length(ker)] do #JDM replace this with TABLE_OF_TRANS_KERNEL?
	j:=[1..n];
	for k in [1..Length(ker[i])] do
		j{ker[i][k]}:=List([1..Length(ker[i][k])], x-> k);
	od;
	reps[i]:=j;
od;

out:= [];
img:=RClassImageOrbitFromData(d!.parent, d!.data[1], d!.o[1])
 {RClassSCCFromData(d!.parent, d!.data[1], d!.o[1])};
#replace these as above!

m:=Length(img[1]);

for i in img do 
	for j in [1..Length(ker)] do 
		if Length(Set(reps[j]{i}))=m then 
			out[Length(out)+1]:=IdempotentNC(ker[j], i); #JDM TransformationNC?
		fi;
	od;
od;

return out;
end);

# new for 4.0!
#############################################################################
# not a user function

# Usage: s = semigroup, f = element, o = [OrbitsOfImages(s)!.orbits,
# OrbitsOfImages(s)!.orbits], d = [image data, kernel data] (including
# true/false). 

# return the data of the D-class containing f but where both l values are for f
# rather than the D-class. 

InstallGlobalFunction(PreInOrbitsOfKernels, 
function(arg)
  local s, f, kernels, o, data;
  
  s:=arg[1]; f:=arg[2];
  kernels:=OrbitsOfKernels(s)!.kernels;

  if Length(arg)>=3 then 
    o:=arg[3];
  else
    o:=[OrbitsOfImages(s)!.orbits, OrbitsOfKernels(s)!.orbits];
  fi;

  if Length(arg)>=4 then 
    data:=arg[4];
  else
    data:=[PreInOrbitsOfImages(s, f, o[1]), [, [fail, fail, fail, fail, fail , 0,
    fail, fail]]]; 
  fi;
  
  if not data[1][1] then # f not in OrbitsOfImages(s)
    return data;
  fi;
  
  return InOrbitsOfKernels(s, f, data, o, kernels);
end);

# new for 4.0! InOrbitsOfKernels
#############################################################################
# not a user function

# Usage: s = semigroup, f = element, data = [image data, kernel data] (including
# true/false), o = [OrbitsOfImages(s)!.orbits, OrbitsOfKernels(s)!.orbits],
# kernels = OrbitsOfKernels(s)!.kernels

# note that f is converted to be an R-class representative at the start here. 
# In particular, data[1][2] corresponds to the original f whereas data[2][2]
# corresponds to f with rectified image (or alternatively to the representative
# of the R-class containing f). More precisely, data[1][2][3] is the position of
# the image of the original f in orbit, and not the o[scc[1]]. 

# JDM should [img, ker] be included as data[2][2][9]?

InstallGlobalFunction(InOrbitsOfKernels, 
function(s, f, data, o, kernels)
  local j, k, l, m, val, n, g, r, ker, schutz, reps, i, p, cosets, t, h;

  j:=data[2][2][1]; k:=data[2][2][2]; l:=data[2][2][3]; m:=data[2][2][4]; 
  val:=data[2][2][5]; n:=data[2][2][6]; g:=data[2][2][7]; r:=data[2][2][8];
  o:=o[2]; 
  
  f:=data[1][2][7];

  if k=fail then 
    if IsBound(f![6]) then 
      ker:=f![6];
    else
      ker:=CanonicalTransSameKernel(f![1]);
      f![6]:=ker;
    fi;

    if j=fail then 
      j:=MaximumList(ker);
    fi;
  fi;

  if not IsBound(o[j]) then
    return [data, [false, [j, fail, fail, fail, fail, 0, fail, fail]]];
  fi;

  if k=fail then #l=fail, m=fail, g=fail
    
    k:=HTValue(kernels, ker);

    if k=fail then 
      return [data, [false, [j, fail, fail, fail, fail, 0, fail, fail]]];
    fi;

    l:=Position(o[j][k], ker);
    m:=o[j][k]!.scc_lookup[l];
          
    if not IsBound(o[j][k]!.rels[l]) then
      return [data, [false, [j, k, l, m, fail, 0, fail, fail]]];
    fi;
  
    g:=o[j][k]!.rels[l][2]*f;
  
  fi;

  if g=fail then 
    g:=o[j][k]!.rels[l][2]*f;
  fi;

  if val=fail then 
    val:=HTValue(o[j][k]!.images_ht[m], ImageSetOfTransformation(f));
    if val=fail then 
      return [data, [false, [j, k, l, m, fail, 0, g, fail]]];
    fi;
  fi;
  
  schutz:=o[j][k]!.schutz[m][1];
  
  if schutz=true then 
    return [data, [true, [j, k, l, m, val, 1, g, 1]]]; 
  fi;

  reps:=o[j][k]!.reps[m][val];
  i:=Length(reps);

  while n<i do
    n:=n+1;
    if schutz=false then 
      if reps[n]=g then 
        return [data, [true, [j, k, l, m, val, n, g, r]]];
      fi;
    else 
      p:=o[j][k]!.convert[m][val][n]^-1;
      cosets:=o[j][k]!.d_schutz[m][val][n][3]; #DClassRCosets
      t:=Length(cosets);
      h:=PermLeftQuoTransformationNC(reps[n], g);
      
      for r in [1..t] do 
        if SiftedPermutation(schutz, (h/cosets[r])^p)=() then 
          return [data, [true, [j, k, l, m, val, n, g, r]]];
        fi;
      od;
    fi;
  od;

  return [data, [false, [j, k, l, m, val, n, g, r]]];
end);

#############################################################################
# JDM test further for efficiency in comparison with the old method!

InstallOtherMethod(IsRegularDClass, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)

if HasIdempotents(d) then 
	return not Idempotents(d)=[];
fi;

return IsRegularRClassData(d!.parent, d!.data[1], d!.o[1], d!.rep);
end);

#############################################################################

InstallGlobalFunction(IteratorOfDClassReps, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfDClassReps");

iter:=IteratorByFunctions( rec(
	
	ShallowCopy := iter -> rec( i:=0, s:=iter!.s, 
	last_called := NextIterator, last_value := 0, 
	chooser:=iter!.chooser, next:=iter!.next),
	
	i:=0, # representative index i.e. which representative we are at
	
	s:= s,

	next_value:=fail,
	
	last_called_by_is_done:=false,
	
	r:=IteratorOfRClassRepsData(s),
	
	######################################################################

	IsDoneIterator:=function(iter) 
	local s, O, d_img, f, d_ker, d, r_reps;

	if iter!.last_called_by_is_done then 
		return iter!.next_value=fail;
	fi;
	
	iter!.last_called_by_is_done:=true;
	
	s:=iter!.s;
	O:=OrbitsOfKernels(s);

	iter!.next_value:=fail;
	
	if iter!.i < Length(O!.data) then 
		iter!.i:=iter!.i+1;
		iter!.next_value:=DClassRepFromData(s, O!.data[iter!.i]);
		return false;
	elif O!.finished then  
		return true;
	fi;
	
	for d_img in iter!.r do  
		f:=RClassRepFromData(s, d_img);
		d_ker:=InOrbitsOfKernels(s, f, [d_img, 
		 [d_img[1], fail, fail, fail, fail, 0]]);
		if not d_ker[2] then #f not in existing D-class
			d_ker:=AddToOrbitsOfKernels(s, f, d_ker[3]);
			iter!.i:=iter!.i+1;
			iter!.next_value:=DClassRepFromData(s, d_ker);
			return false;
		else #store R-class in kernel orbit/ JDM clean up the following clause
			d:=d_ker[3][2];
			r_reps:=DClassKernelOrbitFromData(s, d_ker[3])!.r_reps[d[4]][d[5]][d[6]];
			r_reps[Length(r_reps)+1]:=d_img;
		fi;
	od;
	
	O!.finished:=true;
	return true;
	end,

	######################################################################
	
	NextIterator:=function(iter) 
	
	if not iter!.last_called_by_is_done then 
		IsDoneIterator(iter);
	fi;

	iter!.last_called_by_is_done:=false;
	return iter!.next_value;
	end
	######################################################################
));

SetIsIteratorOfDClassReps(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);

return iter;
end);

# new for 4.0!
#############################################################################

InstallGlobalFunction(IteratorOfGreensDClasses, 
function(s)
local iter;

Info(InfoMonoidGreens, 4, "IteratorOfGreensDClasses");

iter:=IteratorByFunctions( rec(
	
	i:=0,
	
	s:=s, 
	
	reps:=IteratorOfDClassReps(s),
	
	IsDoneIterator := iter -> IsDoneIterator(iter!.reps), 
	
	NextIterator:= function(iter)
	local c, rep, d;
	
	rep:=NextIterator(iter!.reps);
	
	if rep=fail then 
		return fail;
	fi;
	
	iter!.i:=iter!.i+1;
	d:=OrbitsOfKernels(iter!.s)!.data[iter!.i];

	return CreateDClass(s, d, [OrbitsOfImages(s), 
	 OrbitsOfKernels(s)], rep);
	end,
	
	ShallowCopy:=iter-> rec(i:=0, s:=iter!.s, reps:=IteratorOfRClassReps(s))
));

SetIsIteratorOfGreensDClasses(iter, true);
SetUnderlyingSemigroupOfIterator(iter, s);
return iter;
end);

# new for 4.0!
###########################################################################
#

InstallGlobalFunction(IteratorOfNewDClassReps, 
function(s)
local o, iter;

o:=OrbitsOfKernels(s);
iter:=IteratorOfDClassReps(s);
iter!.i:=Length(o!.data); 
return iter;
end);

# new for 4.0!
###########################################################################
# permutation converting a perm. of ker. classes to one of img elts

InstallGlobalFunction(KerRightToImgLeftFromData,
function(arg)
local s, d, o;

s:=arg[1]; d:=arg[2][2];

if Length(arg)=3 then 
# if IsOrbit(arg[3]) then o:=arg[3]!!! JDM for the sake of convenience!
	o:=arg[3][2]!.orbits[d[1]][d[2]];
else
	o:=OrbitsOfKernels(s)!.orbits[d[1]][d[2]];
fi;

return o!.convert[d[4]][d[5]][d[6]];
end);


#############################################################################

InstallMethod(KerRightToImgLeft, "for a D-class of a trans. semigp",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
d-> KerRightToImgLeftFromData(d!.parent, d!.data, d!.o));

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
  		g:=ClosureGroup(g,  PermLeftQuoTransformationNC(f, 
  		 rels[graph[i][j]][2] * (gens[j] * (rels[i][1] * f)))); 
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
# check efficiency of this method versus taking the permutation 
# p:=MappingPermListList(hh{kk}, o[scc[1]]) and then doing
# g*(f*g)^(Order(p)-1); JDM

InstallGlobalFunction(MultipliersOfSCCOfKernelOrbit,
function(gens, o, j)
  #local rels, scc, f, g, ff, gg, hh, p, i;
  local rels, scc, f, g, ff, gg, hh, kk, i;

  rels:=EmptyPlist(Length(o));
  scc:=o!.scc[j];

  for i in scc do
    #reversed used below as we have a left action not right as in R-classes!
    f:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCForward(o, j, i)));
    # OnKernelAntiAction(o[scc[1]], f)=o[i]
    g:=EvaluateWord(gens, Reversed(TraceSchreierTreeOfSCCBack(o, j, i)));
    # OnKernelsAntiAction(o[i], g)=o[scc[1]] 
  
    ff:=f![1]; gg:=g![1];
    hh:=o[scc[1]]{ff};
    kk:=gg;
    
    if hh{kk}=o[scc[1]] then 
      rels[i]:=[f, g];
    else
      repeat
        kk:=kk{ff{gg}};
        #kk:=kk{gg};
      until hh{kk}=o[scc[1]]; 

      rels[i]:=[f, TransformationNC(kk)];
    fi;
  od;

  return rels;
end);

# new for 4.0!
#############################################################################

InstallMethod(NrGreensDClasses, "for a transformation semigroup", 
[IsTransformationSemigroup], 
function(s)

Info(InfoMonoidGreens, 4, "NrGreensDClasses");
ExpandOrbitsOfKernels(s);
return Length(OrbitsOfKernels(s)!.data);
end);


# new for 4.0!
#############################################################################

InstallOtherMethod(NrGreensLClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)

return Length(DClassRCosets(d))*Length(RClassSCCFromData(d!.parent, d!.data[1],
d!.o[1]));
end);

#############################################################################

InstallOtherMethod(NrGreensRClasses, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local s, f, o, rels, cosets, out, i, j;

if HasGreensRClassReps(d) then 
	return Length(GreensRClassReps(d));
fi;

s:=d!.parent;
f:=d!.rep;
o:=d!.o;
d:=d!.data;

rels:=Length(LClassSCCFromData(s, d[2], o[2]));
cosets:=RightTransversal(LClassSchutzGpFromData(s, d[2], o[2]), 
 DClassSchutzGpFromData(s, d, o)^(KerRightToImgLeftFromData(s, d, o)^-1));
return rels*Length(cosets);
end);

#############################################################################

InstallOtherMethod(NrGreensHClasses, "for a D-class of a trans. semigroup",
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
return NrGreensRClasses(d)*NrGreensLClasses(d);
end);


#############################################################################

InstallOtherMethod(NrIdempotents, "for an D-class", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local ker, n, i, j, k, out, img, m, reps;

if HasIdempotents(d) then 
	return Length(Idempotents(d));
fi;

if HasIsRegularRClass(d) and not IsRegularRClass(d) then 
	return 0;
fi;

ker:=DClassKernelOrbit(d){DClassKernelSCC(d)};
n:=Length(d!.rep![1]);
reps:=[];

for i in [1..Length(ker)] do 
	j:=[1..n];
	for k in [1..Length(ker[i])] do
		j{ker[i][k]}:=List([1..Length(ker[i][k])], x-> k);
	od;
	reps[i]:=j;
od;

out:= 0;
img:=RClassImageOrbitFromData(d!.parent, d!.data[1], d!.o[1])
 {RClassSCCFromData(d!.parent, d!.data[1], d!.o[1])};
#replace these as above! DClassImageOrbit, DClassImageSCC!

m:=Length(img[1]);

for i in img do 
	for j in [1..Length(ker)] do 
		if Length(Set(reps[j]{i}))=m then 
			out:=out+1;
		fi;
	od;
od;

return out;
end);


# new for 4.0!
#############################################################################

InstallMethod(OrbitsOfKernels, "for a transformation semigroup",
[IsTransformationSemigroup], 
function(s)
  local n, lens, kernels, gens, data_ht, data;

  n:=DegreeOfTransformationSemigroup(s);

  return Objectify(NewType(FamilyObj(s), IsOrbitsOfKernels), rec(
    finished:=false,
    orbits:=EmptyPlist(n), 
    lens:=[1..n]*0, #lens[j]=Length(orbits[j])
    kernels:=HTCreate([1..n]),
    gens:=Generators(s),
    s:=s,
    #data_ht:=HTCreate([[1,1,1,1,1,1],[1,1,1,1,1,1,1]]),
    data:=[]));
end);

#############################################################################
#

InstallMethod(ParentAttr, "for a D-class of a trans. semigroup", 
[IsGreensDClass and IsGreensClassOfTransSemigp], x-> x!.parent);

#############################################################################

InstallMethod(PartialOrderOfDClasses, "for a semigroup", 
[IsTransformationSemigroup], 
function(s)
local d, n, out, gens, data, o, i, a, b, c;

d:= GreensDClasses(s);
n:=Length(d);
out:= List([1..n], x->EmptyPlist(n));
gens:=Generators(s);
data:=OrbitsOfKernels(s)!.data;
o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];

for i in [1..Length(d)] do
	#AddSet(out[i], i);
	for a in gens do
		for c in GreensRClassReps(d[i]) do
			b:=InOrbitsOfKernels(s, a * c, [], o)[3][2]{[1..6]};
			b[3]:=LClassSCCFromData(s, b, o[2])[1];
			#Error("");
			
			AddSet(out[i], PositionProperty(data, x-> x[2]=b));
			#JDM replace OrbitsOfKernels(s)!.data and OrbitsOfImages(s)!.data
			# with hash tables.
		od;
		for c in GreensLClassReps(d[i]) do
			b:=InOrbitsOfKernels(s, c * a, [], o)[3][2]{[1..6]};
			b[3]:=LClassSCCFromData(s, b, o[2])[1];
			AddSet(out[i], PositionProperty(data, x-> x[2]=b));
		od;
	od;
od;

#transitive closure JDM maybe not required??

#for i in [1..Length(class)] do
#	for j in [1..Length(class)] do
#		if j in poset[i] then 
#			poset[i]:=Union(poset[j], poset[i]);
#		fi;
#	od;
#od;

return out;
#return Graph(Group(()), [1..Length(class)], OnPoints, function(x,y) return y in poset[x]; end, true); ;
end);

#############################################################################
#

InstallMethod(PrintObj, [IsOrbitsOfKernels], 
function(o)
Print("<orbits of kernels; ", 
SizeOrbitsOfKernels(o!.s), " elements; ", Length(o!.data), 
" D-classes>");
end);

#############################################################################
#

InstallMethod(PrintObj, [IsIteratorOfDClassReps], 
function(iter)
local s;

s:=iter!.s;

Print( "<iterator of D-class reps, ", Length(OrbitsOfImages(s)!.data), 
" candidates, ",
 SizeOrbitsOfKernels(s), " elements, ", Length(OrbitsOfKernels(s)!.data), 
 " D-classes>");
return;
end);

# new for 4.0!
############################################################################

InstallMethod(PrintObj, [IsIteratorOfGreensDClasses], 
function(iter)
Print( "<iterator of D-classes>");
return;
end);

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
  local scc, max, bound, g, rels, t, graph, is_sym, i, j;

  scc:=o!.scc[k];
  max:=MaximumList(o[scc[1]]);

  if max<1000 then 
    bound:=Factorial(max);
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

  #g:=g^(AsPermOfKerImg(f)^-1); BIG CHANGE HERE!! JDMJDMJDM 

  if not is_sym then 
    return [StabChainImmutable(g), g];
  else
    return [is_sym, g];
  fi;
end);

# new for 4.0!
############################################################################

InstallOtherMethod(SchutzenbergerGroup, "for a D-class of a trans. semigp.",
[IsGreensDClass and IsGreensClassOfTransSemigp], 
function(d)
local dd;
dd:=d!.data[2];
return DClassKernelOrbitFromData(d!.parent, d!.data, d!.o)!.
 d_schutz[dd[4]][dd[5]][dd[6]][2];
end);


# new for 4.0! SchutzGpOfDClass - not a user function!
#############################################################################
# Usage: s = semigroup; d = [image data, kernel data]; o = [OrbitsOfImages(s),
# OrbitsOfKernels(s)] (optional).  

# jdmjdm herehere in trying to get this to work!!

InstallGlobalFunction(SchutzGpOfDClass, 
function(arg)
  local s, d, o, g, h, p;

  s:=arg[1]; d:=arg[2];

  if Length(arg)=3 then 
    o:=arg[3];
  else
    o:=[OrbitsOfImages(s), OrbitsOfKernels(s)];
  fi;

  g:=RClassSchutzGpFromData(s, d[1], o[1]);
  
  if not Size(g)=1 then 
    h:=DClassLStabChainFromData(s, d[2], o[2]);
    p:=KerRightToImgLeftFromData(s, d, o)^-1;
    if not h=true then 
      h:=Intersection(g, DClassLSchutzGpFromData(s, d[2], o[2]);
    else
      h:=LClassSchutzGpFromData(s, d[2], o[2]);
      h:=SubgroupProperty(g, x -> x^p in h);
      #JDM if both LClassSchutzGp and RClassSchutzGp are the symmetric group
      # then take the symmetric group on the intersection of MovedPoints. 
      # this is much faster than using Intersection...
    fi;
  else
    h:=g;
  fi;

  return [StabChainImmutable(h), h , RightTransversal(g, h)];
end);

# new for 4.0!
#############################################################################

InstallOtherMethod(Size, "for a D-class of a trans. semigp.", 
[IsGreensDClass and IsGreensClassOfTransSemigp],
function(d)
local r, l, s, o, D;

s:=d!.parent;
o:=d!.o;
D:=d!.data;

r:=RClassSchutzGpFromData(s, D[1], o[1]);
l:=LClassSchutzGpFromData(s, D[2], o[2]);

return (Size(r)*Length(RClassSCCFromData(s, D[1], o[1]))
*Length(LClassSCCFromData(s, D[2], o[2])))*Size(l)/
Size(SchutzenbergerGroup(d));
end);


#############################################################################
#

InstallGlobalFunction(SizeOrbitsOfKernels, 
function(s)
local data, i, d, l, r, o_r, o_l;

data:=OrbitsOfKernels(s)!.data;
i:=0;

for d in data do
	o_r:=RClassImageOrbitFromData(s, d[1]);
	r:=RClassSchutzGpFromData(s, d[1]);
	o_l:=DClassKernelOrbitFromData(s, d);
	l:=LClassSchutzGpFromData(s, d[2]);
	i:=i+(Size(r)*Length(RClassSCCFromData(s, d[1]))
	 *Length(LClassSCCFromData(s, d[2]))*Size(l)/
	 Size(DClassSchutzGpFromData(s,  d)));
od;

return i;
end);

#############################################################################
# 

InstallMethod( ViewObj, "for D-class data",
[ IsGreensDClassData and IsGreensDClassDataRep],
function( obj )
Print( "GreensDClassData( ", obj!.rep, " )" );
end );
