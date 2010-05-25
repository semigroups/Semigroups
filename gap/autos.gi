#############################################################################
##
#W  autos.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#-if there was a way to make a presentation from R-classes, then we could use an
# R-class version of the algorithm! (perhaps try this with FroidurePinSimpleAlg)

#JDM Add method for OnPoints and OnTuples currently inside Automorphism group in 
#dev/gap/grape.gi


###########################################################################

InstallOtherMethod(AutomorphismGroup, "for a semigroup", 
[IsTransformationSemigroup], 
function(S)
return AutomorphismsSemigroupInGroup(S, fail, List([1..5], ReturnFalse)); 
end);

###########################################################################

InstallMethod(AutomorphismGroup, "for a zero group", [IsZeroGroup], 
function(zg)
local g, gens, autsg, auts;

g:=UnderlyingGroupOfZG(zg);
autsg:=AutomorphismGroup(g);

if IsTrivial(autsg) then 
	return Group(List(Elements(autsg), x-> ZeroGroupAutomorphism(zg, x)));
else 

	gens:=List(GeneratorsOfGroup(autsg), x-> ZeroGroupAutomorphism(zg, x));

	g:=Group(gens);

	SetIsAutomorphismGroup(g, true);
	SetIsAutomorphismGroupOfSemigroup(g, true);
	SetIsAutomorphismGroupOfZeroGroup(g, true);
	SetIsGroupOfAutomorphisms(g, true);
	SetIsFinite(g, true);
	SetNiceMonomorphism(g,  GroupHomomorphismByImagesNC(g, autsg, 
	 GeneratorsOfGroup(g), List(GeneratorsOfGroup(g), x-> x!.grpauto)));
	SetIsHandledByNiceMonomorphism(g, true);
	SetNiceObject(g, autsg);
	UseIsomorphismRelation(autsg, g);

return g;
fi;
end);

###########################################################################

InstallMethod(AutomorphismGroup, "for a RZMS", [IsReesZeroMatrixSemigroup],
function(rms)

Print("#I It appears that the `grape' package is not fully installed." , 
 "As a\n#I consequence this function is not available.\n");
return fail;
end);

###########################################################################

InstallOtherMethod(AutomorphismGroup, "for a zero semigroup", [IsZeroSemigroup], 
function(ZS)
local n, g1, g2, elms, G, hom, sym, iso;

Info(InfoMonoidAutos, 2, "AutomorphismGroup: computing the automorphism group",  
" of a zero semigroup");

n:=Size(ZS);
if n=2 then 
	hom:=SemigroupHomomorphismByImagesNC(ZS, ZS, Elements(ZS));
	SetIsOne(hom, true);
	G:=Group(SemigroupHomomorphismByImagesNC(ZS, ZS, Elements(ZS)));
	SetAsSSortedList(G, [hom]);
	SetIsAutomorphismGroupOfZeroSemigroup(G, true);

  return G;
else 
  elms:=Elements(ZS);

  g1:=SemigroupHomomorphismByImagesNC(ZS, ZS, Concatenation([elms[1]], 
   elms{[3..n]}, [elms[2]]));
  g2:=SemigroupHomomorphismByImagesNC(ZS, ZS, Concatenation(elms{[1,3,2]}, 
   elms{[4..n]}));

  SetIsInjective(g1, true); SetIsInjective(g2, true);
  SetIsSurjective(g2, true); SetIsSurjective(g1, true);
  SetIsTotal(g1, true); SetIsTotal(g2, true);
	SetInverseGeneralMapping(g1, SemigroupHomomorphismByImagesNC(ZS, ZS, 
	  Concatenation([elms[1]], Reversed(elms{[3..n]}), [elms[2]])));
	SetInverseGeneralMapping(g2, g2);
	G:=Group(g1, g2);
	sym:=GroupWithGenerators([PermList(Concatenation([1],[3..n], [2])),(2,3)]);
	
	#iso:= GroupHomomorphismByImagesNC(G, sym, [g1, g2], GeneratorsOfGroup(sym));
	#SetInverseGeneralMapping(iso, GroupHomomorphismByImagesNC(sym, G,  
	#GeneratorsOfGroup(sym), [g1, g2]));
	
	iso:=MappingByFunction(G, sym, function(x)
	#local str;
	#str:=List(x!.imgslist{[2..Length(x!.imgslist)]}, y-> String(y));
	#str:=List(str, y-> Int(y{[2..Length(y)]})); 
	#return PermList(str);
	return PermList(List(x!.imgslist{[2..Length(x!.imgslist)]}, y-> y![1]));
	end);
	SetInverseGeneralMapping(iso, MappingByFunction(sym, G, function(x)
	local perm;
	
	perm:=List([1..LargestMovedPoint(sym)], y->y^x-1);
	perm:=List(perm, ZeroSemigroupElt);
	
	return SemigroupHomomorphismByImagesNC(ZS, ZS, perm); end));
	
	SetIsomorphismPermGroup(G, iso); 
	SetNiceMonomorphism(G, IsomorphismPermGroup(G));
	SetIsHandledByNiceMonomorphism(G, true);
	SetNiceObject(G, Range(NiceMonomorphism(G)));
	UseIsomorphismRelation(G, Range(NiceMonomorphism(G)));
	SetIsAutomorphismGroupOfZeroSemigroup(G, true);

	return G;
fi;

end);

###########################################################################

InstallOtherMethod(AutomorphismGroup, "for a Rees matrix semigroup",
[IsReesMatrixSemigroup],
function(rms)
local G, mat, m, n, autograph, autogroup, A, inner, transversal1, transversal2,
g, h, l, t, tup, new, elts, id, hom;

G:=UnderlyingSemigroupOfReesMatrixSemigroup(rms);

mat:=SandwichMatrixOfReesMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

if n=1 and m=1 then

	#rms is just a group in disguise!
	Info(InfoMonoidAutos, 2, "ReesMatrixSemigroup is a group");
	Info(InfoMonoidAutos, 2, "computing automorphism group of underlying group");

	autogroup:=AutomorphismGroup(G);
	A:=Group(List(GeneratorsOfGroup(autogroup), x-> 
	 RMSIsoByTriple(rms, rms, [(), x, [One(G), One(G)]])));

	SetIsAutomorphismGroup(A, true);
	SetIsGroupOfAutomorphisms(A, true);
	SetIsFinite(A, true);
	hom:=GroupHomomorphismByImagesNC(A, autogroup, GeneratorsOfGroup(A),
	 GeneratorsOfGroup(autogroup));
	SetInverseGeneralMapping(hom, GroupHomomorphismByImagesNC(autogroup, A, 
	 GeneratorsOfGroup(autogroup), GeneratorsOfGroup(A)));
	SetNiceMonomorphism(A, hom);
	SetIsHandledByNiceMonomorphism(A, true);
	SetNiceObject(A, Range(NiceMonomorphism(A)));
	UseIsomorphismRelation(A, autogroup);
	
	return A;

elif n=2 and m=1 then 
	autograph:=Group((2,3));
elif n>2 and m=1 then 
	autograph:=Group((2,3), PermList(Concatenation([1],[3..n+m],[2]))); 
else 
	autograph:=DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
fi;

Info(InfoMonoidAutos, 2, "the automorphism group of the graph has size ", 
 Size(autograph));

Info(InfoMonoidAutos, 2, "computing automorphism group of underlying group");
autogroup:=AutomorphismGroup(G);
Info(InfoMonoidAutos, 2, "the automorphism group of underlying group has size ", 
 Size(autogroup));
Info(InfoMonoidAutos, 3, "computing inner automorphisms of underlying group");

if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;

Info(InfoMonoidAutos, 2, "Aut G/Inn G has size ", Length(transversal1));

transversal2:=RightTransversal(G, Centre(G));
Info(InfoMonoidAutos, 2, "G/Z(G) has size ", Length(transversal2));

Info(InfoMonoidAutos, 2, "search space has ", Size(autograph), "x", 
 Length(transversal1), "x", Length(transversal2), "=", 
 Size(autograph)*Length(transversal1)*Length(transversal2), " elements");

#JDM with the commented out lines rather than the current version the algorithm
#JDM runs about half the speed. 
#id:=RMSIsoByTriple(rms, rms, [One(autograph), One(autogroup), List([1..m+n], x-
#> One(G))]);
#A:=Group(id);
#SetAsSSortedList(A, [id]);
#elts:=[];
A:=[];

for l in autograph do 

	for t in transversal1 do 

		for tup in transversal2 do
	  #if Size(A)<Size(autograph)*Length(transversal1)*Length(transversal2) then 
			new:=RMSInducedFunction(rms, l, t, tup); 	
			if new[1]=true then
				new:=RMSIsoByTriple(rms, rms, [l, t, new[2]]);
				#if not new in A then 
					#AddSet(A, new); 
					#A:=ClosureGroupDefault(A, new);
					Add(A, new);
				#fi;
			fi;
		#fi;
		od; 
	od;
od;

A:=Group(A);
SetAsList(A, GeneratorsOfGroup(A));
SetIsAutomorphismGroup(A, true);
SetIsGroupOfAutomorphisms(A, true);
SetIsAutomorphismGroupOfRMS(A, true);
SetIsFinite(A, true);

#JDM maybe there should be a RectangularBand automorphism group command...
if IsTrivial(UnderlyingSemigroupOfReesMatrixSemigroup(rms)) then 
	SetIsomorphismPermGroup(A, GroupHomomorphismByImagesNC(A, autograph, 
	 GeneratorsOfGroup(A), List(GeneratorsOfGroup(A), x-> x!.triple[1])));
	SetInverseGeneralMapping(IsomorphismPermGroup(A), 
	GroupHomomorphismByImagesNC(autograph, A, 
	 GeneratorsOfGroup(Range(IsomorphismPermGroup(A))), 
	 List(GeneratorsOfGroup(Range(IsomorphismPermGroup(A))), x-> 
	 RMSIsoByTriple(rms, rms, [x, One(autogroup), List([1..m+n], x-> One(G))]))));
	SetNiceMonomorphism(A, IsomorphismPermGroup(A));
	SetIsHandledByNiceMonomorphism(A, true);
	UseIsomorphismRelation(A, Range(NiceMonomorphism(A)));
fi;

return A;
end);

###########################################################################

InstallOtherMethod(AutomorphismsSemigroupInGroup, 
"for a trans. semigroup and a group", 
[IsTransformationSemigroup, IsGroup],
function(S, G)
return AutomorphismsSemigroupInGroup(S, G, [false, false, false, false, true]); 
end);

###########################################################################

InstallMethod(AutomorphismsSemigroupInGroup, 
"for a trans. semigroup, object, and list", 
[IsTransformationSemigroup, IsObject, IsList],
function(S, superlist, bvals)

Print("#I It appears that the `grape' package is not fully installed.", 
" As a\n#I consequence this function is not available.\n");
return fail;
end);

#############################################################################

InstallMethod(CompositionMapping2, "for objects in `IsRMSIsoByTriple'", 
IsIdenticalObj, [IsGeneralMapping and IsRMSIsoByTripleRep, IsGeneralMapping and
IsRMSIsoByTripleRep],
function(a1, a2)
local n, l1, l2, g1, g2, f1, f2;

n:=RowsOfReesMatrixSemigroup(Source(a1))
   +ColumnsOfReesMatrixSemigroup(Source(a1));

l1:=a1!.triple[1]; l2:=a2!.triple[1];
g1:=a1!.triple[2]; g2:=a2!.triple[2];
f1:=a1!.triple[3]; f2:=a2!.triple[3];

return RMSIsoByTriple(Source(a1), Range(a2), [l1*l2, g1*g2, List([1..n], 
 x->f2[x^l1]*f1[x]^g2)]);
end);

#############################################################################

InstallMethod(CompositionMapping2, "for objects in `IsRZMSIsoByTriple'",
IsIdenticalObj, [IsGeneralMapping and IsRZMSIsoByTripleRep, 
IsGeneralMapping and IsRZMSIsoByTripleRep],
function(a1, a2)
local n, l1, l2, g1, g2, f1, f2;

n:=RowsOfReesZeroMatrixSemigroup(Source(a1))
   +ColumnsOfReesZeroMatrixSemigroup(Source(a1));

l1:=a1!.triple[1]; l2:=a2!.triple[1];
g1:=a1!.triple[2]; g2:=a2!.triple[2];
f1:=a1!.triple[3]; f2:=a2!.triple[3];

return RZMSIsoByTriple(Source(a1), Range(a2), [l1*l2, g1*g2, 
List([1..n], x->f2[x^l1]*f1[x]^g2)]);
end);

###########################################################################

InstallMethod(CompositionMapping2, "for objects in IsZeroGroupAutomorphismRep", 
IsIdenticalObj, [IsEndoGeneralMapping and IsZeroGroupAutomorphismRep, 
IsEndoGeneralMapping and IsZeroGroupAutomorphismRep],
function(a1, a2)

return ZeroGroupAutomorphism(Source(a1), a1!.grpauto*a2!.grpauto);
end);

###########################################################################

InstallMethod( CompositionMapping2,
"for inner automorphisms of semigroups and semigroup homomorphism",
IsIdenticalObj, [IsInnerAutomorphismOfSemigroup, IsSemigroupHomomorphism],
function( inn, hom )

if not IsIdenticalObj( Source( inn ), Source( hom ) )  then
  return fail;
elif IsOne(hom) then 
  return inn;
elif IsOne(inn) then 
  return hom;
fi;

if IsMonoid(Source(hom)) then 
  return  SemigroupHomomorphismByImagesOfGensNC( Source( hom ),
   Range(inn), OnTuples(OnTuples(GeneratorsOfMonoid(Source(hom)), hom ), inn));
else
  return SemigroupHomomorphismByImagesOfGensNC( Source( hom ),
   Range(inn), OnTuples( OnTuples(GeneratorsOfSemigroup(Source(hom)), hom ),
    inn));
fi;
end );

###########################################################################

InstallMethod( CompositionMapping2,
"for semigroup homomorphism and inner automorphism of semigroups",
IsIdenticalObj, [IsSemigroupHomomorphism , IsInnerAutomorphismOfSemigroup],
function( hom, inn )

if not IsIdenticalObj( Source( inn ), Source( hom ) )  then
  return fail;
elif IsOne(hom) then 
  return inn;
elif IsOne(inn) then 
  return hom;
fi;

if IsMonoid(Source(inn)) then
  return SemigroupHomomorphismByImagesOfGensNC( Source( inn ),
   Range(hom), OnTuples(OnTuples(GeneratorsOfMonoid(Source(inn)), inn), hom));
else 
  return SemigroupHomomorphismByImagesOfGensNC( Source( inn ),
   Range(hom), OnTuples(OnTuples(GeneratorsOfSemigroup(Source(inn)), inn), 
    hom));
fi;
end);

###########################################################################

InstallMethod( CompositionMapping2,
"for two inner automorphisms of semigroups",
IsIdenticalObj, [IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup,
IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup ],
function( inn1, inn2 )

if not IsIdenticalObj( Source( inn1 ), Source( inn2 ) )  then
  return fail;
fi;

return InnerAutomorphismOfSemigroupNC( Source( inn1 ),
 ConjugatorOfInnerAutomorphismOfSemigroup( inn2 )
  * ConjugatorOfInnerAutomorphismOfSemigroup( inn1 ) );
end);

#############################################################################

InstallMethod(ImagesElm, "for an RMS element under a mapping by a triple",
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
IsGeneralMapping and IsRMSIsoByTripleRep, IsReesMatrixSemigroupElement ],
function( triple, x)
return [ImagesRepresentative(triple, x)];
end);

#############################################################################

InstallMethod(ImagesElm, "for an RZMS element under a mapping by a triple",
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
IsGeneralMapping and IsRZMSIsoByTripleRep, IsReesZeroMatrixSemigroupElement ],
function( triple, x)

return [ImagesRepresentative(triple, x)];
end);

###########################################################################

InstallMethod(ImagesElm, "for a zero group automorphism",
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
IsEndoGeneralMapping and IsZeroGroupAutomorphismRep, IsZeroGroupElt ],
function( triple, x)

return [ImagesRepresentative(triple, x)];
end);

###########################################################################

InstallMethod(ImagesElm, "for semigroup homomorphism by function",
FamSourceEqFamElm, [IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup,
IsTransformation],
function( hom, x)

return [ImagesRepresentative(hom, x)];
end);

#############################################################################

InstallMethod(ImagesRepresentative,
"for an RMS element under a mapping by a triple",
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
IsGeneralMapping and IsRMSIsoByTripleRep, IsReesMatrixSemigroupElement ],
function( triple, x)
local g, i, j, lambda, gamma, f, m;

m:=RowsOfReesMatrixSemigroup(Source(triple));

g := UnderlyingElementOfReesMatrixSemigroupElement(x);
i := RowIndexOfReesMatrixSemigroupElement(x);
j := ColumnIndexOfReesMatrixSemigroupElement(x)+m;
lambda := triple!.triple[1];
gamma := triple!.triple[2]; 
f := triple!.triple[3];

return ReesMatrixSemigroupElementNC(Range(triple), i^lambda,
f[i]*ImageElm(gamma,g)/f[j], j^lambda-m);
end);

#############################################################################

InstallMethod(ImagesRepresentative, 
"for an RZMS element under a mapping by a triple",
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
IsGeneralMapping and IsRZMSIsoByTripleRep, IsReesZeroMatrixSemigroupElement ],
function( triple, x)
local g, i, j, lambda, gamma, f, m;

if not x=MultiplicativeZero(Source(triple)) then
  m:=RowsOfReesZeroMatrixSemigroup(Source(triple));

  g := UnderlyingElementOfReesZeroMatrixSemigroupElement(x);
  i := RowIndexOfReesZeroMatrixSemigroupElement(x);
  j := ColumnIndexOfReesZeroMatrixSemigroupElement(x)+m;
  lambda := triple!.triple[1];
  gamma := triple!.triple[2]; 
  f := triple!.triple[3];

  return ReesZeroMatrixSemigroupElementNC(Range(triple), i^lambda,
  f[i]*ImageElm(gamma,g)/f[j], j^lambda-m);
else 
  return x;
fi;

end);

#############################################################################

InstallMethod(ImagesRepresentative, "for a zero group automorphism", 
[ IsTotal and IsSingleValued and IsEndoGeneralMapping and 
IsZeroGroupAutomorphismRep, IsZeroGroupElt ],
function( aut, x)

if IsMultiplicativeZero(x) then 
	return x;
fi;

return ZeroGroupElt(ImagesRepresentative(aut!.grpauto, 
UnderlyingGroupEltOfZGElt(x)));

end);

#############################################################################

InstallMethod( ImagesRepresentative, "for inner automorphism of semigroup",
FamSourceEqFamElm, [IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, 
IsTransformation],
function( hom, g )
return g ^ ConjugatorOfInnerAutomorphismOfSemigroup( hom );
end );

###########################################################################

InstallMethod( InnerAutomorphismOfSemigroup, 
"for trans. semigroup and permutation",
[IsTransformationSemigroup, IsPerm],
function( S, g )
local hom, gens, imgs, new;

hom:=InnerAutomorphismOfSemigroupNC(S, g);

if g=() then 
	return hom;
fi;

gens:=GeneratorsOfSemigroup(S);
imgs:=OnTuples(gens, hom);

if ForAll(imgs, x-> x in S) then 
	new:=Semigroup(imgs);
	if ForAll(GeneratorsOfSemigroup(S), x-> x in new) then 
		return hom;
	fi;
fi;

return fail;
end);

###########################################################################

#JDM why the higher rank filter?

InstallMethod(InnerAutomorphismsAutomorphismGroup, 
"for the automorphism group of a semigroup", 
[IsAutomorphismGroupOfSemigroup], 31,
function(G)

return InnerAutomorphismsOfSemigroup(Range(One(G)));
end);

###########################################################################
#JDM why the higher rank filter?

InstallMethod(InnerAutomorphismsAutomorphismGroup, 
"for automorphism group of zero group", 
[IsAutomorphismGroupOfZeroGroup], 30, 
function(autg)
local zg, g, gens, inner;

zg:=Source(Identity(autg));
g:=UnderlyingGroupOfZG(zg);
inner:=InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(g));

if not IsTrivial(inner) then 
	gens:=List(GeneratorsOfGroup(inner), x-> ZeroGroupAutomorphism(zg, x));
else 
	gens:=One(autg);
fi;

g:=Group(gens);
SetNiceMonomorphism(g,  GroupHomomorphismByImagesNC(g, inner, 
 GeneratorsOfGroup(g), List(GeneratorsOfGroup(g), 
  function(x) 
  if IsOne(x) then 
  	return One(inner);
  else 
  	return x!.grpauto;
  fi;
  end )));

SetIsHandledByNiceMonomorphism(g, true);
SetNiceObject(g, inner);
UseIsomorphismRelation(inner, g);
SetIsInnerAutomorphismsOfZeroGroup(g, true);

return g;
end);

###########################################################################

InstallMethod( InnerAutomorphismOfSemigroupNC, 
"for trans. semigroup and permutation", 
[ IsTransformationSemigroup, IsPerm ],
function( S, g )
local fam, hom;
fam:= ElementsFamily( FamilyObj( S ) );
hom:= Objectify( NewType( GeneralMappingsFamily( fam, fam ), 
IsInnerAutomorphismOfSemigroup and IsSPGeneralMapping and IsSingleValued and 
IsTotal and IsInjective and IsSurjective and RespectsMultiplication and 
IsAttributeStoringRep ), rec() );

SetConjugatorOfInnerAutomorphismOfSemigroup( hom, g );
SetSource( hom, S );
SetRange(  hom, S );

return hom;
end );

###########################################################################

InstallMethod(InnerAutomorphismsOfSemigroup, 
"for a transformation semigroup", 
[IsTransformationSemigroup],
function(S)
local gens, id, G;
return InnerAutomorphismsOfSemigroupInGroup(S, fail, false);
end);

###########################################################################

InstallOtherMethod(InnerAutomorphismsOfSemigroup, "for a zero group", 
[IsZeroGroup],
function(ZG)
return InnerAutomorphismsAutomorphismGroup(AutomorphismGroup(ZG));
end);

###########################################################################

InstallOtherMethod(InnerAutomorphismsOfSemigroupInGroup, 
"for a transformation semigroup and group", 
[IsTransformationSemigroup, IsGroup],
function(S, G)
return InnerAutomorphismsOfSemigroupInGroup(S, G, false);
end);

###########################################################################

InstallOtherMethod(InnerAutomorphismsOfSemigroupInGroup, 
"for a trans. semigroup, object, and boolean", 
[IsTransformationSemigroup, IsObject, IsBool],
function(S, superlist, bval)
local constants, m, n, inner, stop, OnSets3, useryn, gens, id, G, H, doit, 
perms, orb, l, x, image, hom, new, bad, autos;

if not (superlist=fail or IsGroup(superlist)) then 
	Error("<superlist> should be a group or fail");
	return fail;
fi;

#########

	useryn:=function(helptext)
	local keyed, out;
	
	repeat 
	
		keyed:=CHAR_INT(ReadByte(InputTextUser()));

		if keyed = 'y' or keyed = 'Y' then 
			out:=true;
		elif keyed ='n' or keyed = 'N' then 
			out:=false;
		elif keyed ='h' or keyed = 'H' then 
			Info(InfoMonoidAutos, 4, helptext);
			out:=fail;
		else
			Info(InfoMonoidAutos, 4, "Error: input must be y, n, or h (for help)!");
			out:=fail;
		fi;
	until not out=fail;
	return out;
	end; 
	
#########

constants:=Set(Flat(GradedImagesOfTransSemigroup(S)[1]));
n:=DegreeOfTransformationSemigroup(S);
Info(InfoMonoidAutos, 2, "InnerAutomorphisms: computing stabiliser of images");

m:=0;

if not Length(constants)=0 and not Length(constants)=n then 
	inner:=Stabilizer(SymmetricGroup(n), AsSet([constants, Difference([1..n], 
	constants)]), OnSetsSets);
else
	inner:=SymmetricGroup(n);
fi;

stop:=false;

repeat 
	m:=m+1;
	if not IsEmpty(GradedImagesOfTransSemigroup(S)[m]) then 
		inner:=Stabilizer(inner, Set(GradedImagesOfTransSemigroup(S)[m]), 
		OnSetsSets);
		if IsTrivial(inner) then 
			stop:=true;		
		fi;
	fi;
until m=n-1 or stop;

if not IsTrivial(inner) then 
	Info(InfoMonoidAutos, 3, "stabilizer has size ", 
	Size(inner));
	OnSets3:=function(x, g)
		return Set(List( x, i-> OnSetsSets(i, g)));
	end;
	
	Info(InfoMonoidAutos, 2, "computing stabiliser of kernels");

	m:=1;
	stop:=false;

	repeat 
		m:=m+1;
		if not IsEmpty(GradedKernelsOfTransSemigroup(S)[m]) then 
			inner:=Stabilizer(inner, Set(GradedKernelsOfTransSemigroup(S)[m]), 
			 OnSets3);
			if IsTrivial(inner) then 
				stop:=true;		
			fi;
		fi;
	until m=n-1 or stop;

	Info(InfoMonoidAutos, 3, "stabilizer has size ", Size(inner));
fi; #stabilizer of kernels

if IsMonoid(S) then 
		gens:=GeneratorsOfMonoid(S);
else
		gens:=GeneratorsOfSemigroup(S); 
fi;
	
if IsTrivial(inner) then 
	id:=SemigroupHomomorphismByImagesOfGensNC(S, S, gens);
	SetIsOne(id, true);
	inner:=Group(id);
	SetAsSSortedList(inner, [id]);
	SetIsInnerAutomorphismsOfSemigroup(inner, true);
	SetInnerAutomorphismsOfSemigroup(S, inner);
	return inner; 
fi;

Info(InfoMonoidAutos, 2, "InnerAutomorphisms: computing the stabilizer of generators under \n#I  the action of the stabilizers of images and kernels");

G:=OrbitStabilizer(inner, AsSet(gens), OnSets); #setwise stabilizer
H:=OrbitStabilizer(G.stabilizer, gens, OnTuples); #pointwise stabilizer

Info(InfoMonoidAutos, 2, "InnerAutomorphisms: the inner automorphism search space contains \n#I  ", Length(G.orbit), " elements");

perms:=AsList(RightTransversal(inner, G.stabilizer));
orb:=AsList(RightTransversal(G.stabilizer, H.stabilizer));

if InfoLevel(InfoMonoidAutos)=4 and not superlist=fail then 
	Info(InfoMonoidAutos, 4, "InnerAutomorphisms: the size of the super group is ", Size(superlist));
	Info(InfoMonoidAutos, 4, "######################################################################");
	Info(InfoMonoidAutos, 4, "InnerAutomorphisms: filter elements in the super group online? (y/n/h) #");
	Info(InfoMonoidAutos, 4, "######################################################################");
	bval:=useryn("Filtering the elements online is only advisable if the size of \n#I  the super group is comparable or less than the size of the inner search \n#I  space.");
fi;

Info(InfoMonoidAutos, 1, "InnerAutomorphisms: computing inner automorphisms");

l:=0;

if not bval then 
	inner:=Group(orb);
	SetAsSSortedList(inner, AsSet(orb));
else 
	bad:=Filtered(orb, x-> SemigroupHomomorphismByImagesOfGensNC(S, S, OnTuples(gens, x)) in superlist);
	inner:=Group(bad);
	SetAsSSortedList(inner, AsSet(bad));
fi;

bad:=[];

repeat
	l:=l+1; 
	x:=perms[l];
	image:=OnTuples(gens, x);
	if not image=gens then
		if not x in inner and not x in bad then
			if ForAll(image, x-> x in S) then 
				new:=Semigroup(image);
				if ForAll(gens, g-> g in new) then 
					if (bval and SemigroupHomomorphismByImagesOfGensNC(S, S, image) 
					 in superlist) or not bval then
						inner:=ClosureGroupDefault(inner, x);
					fi;
				else
					#Info(InfoMonoidAutos, 4, "InnerAutomorphisms: images don't generate");
					bad:=Union(bad, Orbit(inner, image, OnTuples)); 
				fi;
			else
				#Info(InfoMonoidAutos, 4, "InnerAutomorphisms: images not all in semigroup");
			fi;
		fi;
	fi;
	#Print("InnerAutomorphisms: automorphisms ", Size(inner), " nonautomorphisms ", Length(bad), " counter ", l, "\r");
until 2*Size(inner)+Length(bad)>Length(perms) or l=Length(perms);

if not bval and not superlist=fail then 
	Info(InfoMonoidAutos, 2, "######################################################################");
	Info(InfoMonoidAutos, 2, "AutomorphismGroup: finding the intersection of the superlist and the \n#I  automorphism group"); 
	new:=Group(());
	SetAsSSortedList(new, [()]);
	
	for x in inner do
		if not x in new and InnerAutomorphismOfSemigroupNC(S, x) in superlist then 
			new:=ClosureGroupDefault(new, x);
		fi;
	od;
	inner:=new;
fi;

autos:=Group(List(SmallGeneratingSet(inner), x-> 
 InnerAutomorphismOfSemigroupNC(S, x)));
SetAsSSortedList(autos, SetX(inner, x-> InnerAutomorphismOfSemigroupNC(S, x)));
SetIsInnerAutomorphismsOfSemigroup(autos, true);
return autos;

end);

#############################################################################

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'", 
[IsEndoGeneralMapping and IsRMSIsoByTripleRep],
function(a)
local l, g, f, n; 

n:=RowsOfReesMatrixSemigroup(Source(a))
   +ColumnsOfReesMatrixSemigroup(Source(a));

l:=a!.triple[1]; g:=a!.triple[2]; f:=a!.triple[3];

return RMSIsoByTriple(Range(a), Source(a), [l^-1, g^-1, List([1..n],x-> 
(f[x^l]^(g^-1))^-1)]);
end);

#############################################################################

InstallMethod(InverseGeneralMapping, "for objects in `IsRMSIsoByTriple'", 
[IsEndoGeneralMapping and IsRMSIsoByTripleRep and IsOne], x-> x);

#############################################################################

InstallMethod(InverseGeneralMapping, "for objects in `IsRZMSIsoByTriple'",
[IsGeneralMapping and IsRZMSIsoByTripleRep],
function(a)
local l, g, f, n; 

n:=RowsOfReesZeroMatrixSemigroup(Source(a))
    +ColumnsOfReesZeroMatrixSemigroup(Source(a));

l:=a!.triple[1]; g:=a!.triple[2]; f:=a!.triple[3];

return RZMSIsoByTriple(Range(a), Source(a), [l^-1, g^-1, List([1..n],x-> 
(f[x^l]^(g^-1))^-1)]);
end);

###########################################################################

InstallMethod(InverseGeneralMapping, 
"for objects in IsZeroGroupAutomorphismRep",
[IsEndoGeneralMapping and IsZeroGroupAutomorphismRep], 
function(a)

return ZeroGroupAutomorphism(Source(a), a!.grpauto^-1);
end);

###########################################################################

InstallMethod( InverseGeneralMapping,
"for inner automorphism of semigroup",
[ IsInnerAutomorphismOfSemigroup ],
inn -> InnerAutomorphismOfSemigroupNC( Source( inn ), 
Inverse( ConjugatorOfInnerAutomorphismOfSemigroup( inn ) ) ) );

#############################################################################

InstallMethod(IsOne, "for objects in `IsRMSIsoByTriple'",
[IsGeneralMapping and IsRMSIsoByTripleRep], 99, 
function(triple)

return IsOne(triple!.triple[1]) and IsOne(triple!.triple[2]) and 
ForAll(triple!.triple[3], IsOne);
end);

#############################################################################

InstallMethod(IsOne, "for objects in `IsRZMSIsoByTriple'",
[IsEndoGeneralMapping and IsRZMSIsoByTripleRep],
function(triple)

return IsOne(triple!.triple[1]) and IsOne(triple!.triple[2]) and 
 ForAll(triple!.triple[3], IsOne);
end);

#############################################################################

InstallMethod(IsOne,  "for inner automorphism of semigroup",
[IsInnerAutomorphismOfSemigroup],
x-> IsOne(ConjugatorOfInnerAutomorphismOfSemigroup(x)));

#############################################################################

InstallMethod(One, "for inner automorphisms of semigroup",
[IsInnerAutomorphismOfSemigroup],  
x -> InnerAutomorphismOfSemigroupNC(Source(x), ()));

#############################################################################

InstallMethod(PreImagesRepresentative,
"for an RMS element under a mapping by a triple", 
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
IsGeneralMapping and IsRMSIsoByTripleRep, IsReesMatrixSemigroupElement ],
function(triple, x)
return ImagesRepresentative(triple^-1, x);
end);

#############################################################################

InstallMethod(PreImagesRepresentative,  
"for an RZMS element under a mapping by a triple", 
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
IsGeneralMapping and IsRZMSIsoByTripleRep, IsReesZeroMatrixSemigroupElement ],
function(triple, x)
return ImagesRepresentative(triple^-1, x);
end);

###########################################################################

InstallMethod(PreImagesRepresentative,   "for a zero group automorphism", 
FamSourceEqFamElm, [ IsTotal and IsSingleValued and 
IsEndoGeneralMapping and IsZeroGroupAutomorphismRep, IsZeroGroupElt ],
function(aut, x)

if IsMultiplicativeZero(x) then 
  return x;
fi;

return ImagesRepresentative(aut!.grpauto^-1, UnderlyingGroupEltOfZGElt(x));
end);

###########################################################################

InstallMethod( PreImagesRepresentative, "for inner automorphism of semigroup",
FamRangeEqFamElm, [ IsInnerAutomorphismOfSemigroup, IsTransformation ],
function( hom, g )
return g ^ ( ConjugatorOfInnerAutomorphismOfSemigroup( hom ) ^ -1 );
end );

#############################################################################

InstallMethod( PrintObj, "for object in `IsRMSIsoByTriple'",
[IsGeneralMapping and IsRMSIsoByTripleRep], 
function( obj )
Print( "RMSIsoByTriple ( ",Source(obj),",", obj!.triple, " )" );
end );

#############################################################################

InstallMethod( PrintObj, "for object in `IsRZMSIsoByTriple'",
[IsGeneralMapping and IsRZMSIsoByTripleRep],
function( obj )
Print( "RZMSIsoByTriple ( ", Source(obj), "," , obj!.triple, " )" );
end );

#############################################################################

InstallMethod( PrintObj, "for inner automorphism of semigroup",
[ IsInnerAutomorphismOfSemigroup ],
function( inn )
Print( "InnerAutomorphism( ", Source( inn ), ", ",
 ConjugatorOfInnerAutomorphismOfSemigroup( inn ), " )" );
end );

#############################################################################

InstallOtherMethod(RightTransStabAutoGroup, 
"for a zero semigroup, mult. elt. collection, and function",
[IsZeroSemigroup, IsMultiplicativeElementCollection, IsFunction], 
function(S, elts, func)
local imgs, iso, stab;
#JDM some error handling here.

#imgs:=List(elts, x-> String(x));
#imgs:=List(imgs, x-> Int(x{[2..Length(x)]})+1);
imgs:=List(elts, x->x![1]+1);

iso:=IsomorphismPermGroup(AutomorphismGroup(S));
stab:=OrbitStabilizer(Range(iso), imgs, func);

#return [OnTuples(AsList(RightTransversal(Range(iso), stab.stabilizer)), 
#InverseGeneralMapping(iso)), List(stab.orbit, x-> List(x, y-> 
#ZeroSemigroupElt(y-1)))]; 

#JDM could use enumerator here instead of AsList?
return OnTuples(AsList(RightTransversal(Range(iso), stab.stabilizer)), 
InverseGeneralMapping(iso));
end);

###########################################################################

InstallOtherMethod(RightTransStabAutoGroup, "for a Rees matrix semigroup",  
[IsReesMatrixSemigroup, IsReesMatrixSemigroupElementCollection, IsFunction],
function(rms, elts, func)

local G, mat, m, n, autograph, autogroup, inner, transversal1, transversal2, 
g, h, l, t, new, rmsgens, tup, trans, tuples, A, indices;

if not ForAll(elts, x-> x in rms) then 
	Error("<elts> must all lie in <rms>");
	return fail;
fi; #JDM make NC version? to omit this?

G:=UnderlyingSemigroupOfReesMatrixSemigroup(rms);

mat:=SandwichMatrixOfReesMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

if n=1 and m=1 then
	
	#rms is just a group in disguise!
	Info(InfoMonoidAutos, 2, "ReesMatrixSemigroup is a group");
	Info(InfoMonoidAutos, 2, "computing automorphism group of underlying group");

	G:=IsomorphismPermGroup(rms);
	autogroup:=RightTransversal(AutomorphismGroup(Range(G)), 
	 Stabilizer(AutomorphismGroup(Range(G)), OnTuples(elts, G), func));
	
	A:=List(autogroup, x-> RMSIsoByTriple(rms, rms, [(), x, [One(G), One(G)]]));

	return A;

elif n=2 and m=1 then 
	autograph:=Group((2,3));
elif n>2 and m=1 then 
	autograph:=Group((2,3), PermList(Concatenation([1],[3..n+m],[2]))); 
else 
	autograph:=DirectProduct(SymmetricGroup(m), SymmetricGroup(n));
fi;

Info(InfoMonoidAutos, 2, "the automorphism group of the graph", 
 " has size ", Size(autograph));

Info(InfoMonoidAutos, 2, "computing automorphism group of underlying group");
autogroup:=AutomorphismGroup(G);
Info(InfoMonoidAutos, 2, "the automorphism group of underlying group has size ", 
Size(autogroup));

Info(InfoMonoidAutos, 3, "computing inner automorphisms of underlying group");
if not IsTrivial(autogroup) then 
	inner:=InnerAutomorphismsAutomorphismGroup(autogroup);
	transversal1:=RightTransversal(autogroup, inner);
else
	transversal1:=Elements(autogroup);
fi;

Info(InfoMonoidAutos, 2, "Aut G/Inn G has size ", Length(transversal1));

transversal2:=RightTransversal(G, Centre(G));
Info(InfoMonoidAutos, 2, "G/Z(G) has size ", Length(transversal2));

Info(InfoMonoidAutos, 2, "search space has ", Size(autograph), "x", 
Length(transversal1), "x", Length(transversal2), "=", 
Size(autograph)*Length(transversal1)*Length(transversal2), " elements");

trans:=[];
tuples:=[];
#rmsgens:=OnTuples(gens, iso);
#indices:=List(gens, IndexPeriodOfTransformation);#JDM does this help much?

for l in autograph do 

	for t in transversal1 do 

		for tup in transversal2 do
				new:=RMSInducedFunction(rms, l, t, tup); 	
				if new[1]=true then
					new:=RMSIsoByTriple(rms, rms, [l, t, new[2]]);
					tup:=func(elts, new);
					if not tup in tuples then
						AddSet(tuples, tup); 
						Add(trans, new);
					fi;
			fi;
		od;
	od;
od;

#return [trans, tuples]; #JDM changes
#JDM could also return <tuples> if desirable
return trans;

end);

#############################################################################
##	JDM for the example given in the manual example this appears to run more 
##	slowly than just running automorphism group (although the overall 
##	computation is longer)

InstallOtherMethod(RightTransStabAutoGroup, "for a RZMS",
[IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroupElementCollection, 
IsFunction],
function(rms, elts, func)

Print("#I It appears that the `grape' package is not fully installed.", 
 " As a\n#I consequence this function is not available.\n");
return fail;

#JDM see grape.gi!!

end);

###########################################################################

InstallMethod(RightTransversal, 
[IsAutomorphismGroupOfZeroGroup, IsInnerAutomorphismsOfZeroGroup], 
function(G, H)
local zg;
zg:=Source(Identity(G));
return List(RightTransversal(NiceObject(G), NiceObject(H)), x-> 
 ZeroGroupAutomorphism(zg, x));
end);

#############################################################################

InstallMethod(RMSInducedFunction, "for a RMS", 
[IsReesMatrixSemigroup, IS_PERM, IsGeneralMapping, IsMultiplicativeElement], 

function(rms, l, g, groupelt)
local mat, m, n, imagelist, i, j;

imagelist:=[];
mat:=SandwichMatrixOfReesMatrixSemigroup(rms);
m:=Length(mat[1]); n:=Length(mat);

imagelist[1]:=groupelt;

imagelist{[m+1..n+m]}:=List([m+1..n+m], v-> mat[v^l-m][1^l]*imagelist[1]*
 (mat[v-m][1]^g)^-1);
imagelist{[2..m]}:=List([2..m], v-> (mat[(m+1)^l-m][v^l])^-1*imagelist[m+1]*
 (mat[(m+1)-m][v]^g));

for i in [2..m] do 
  for j in [m+2..n+m] do 
    if not mat[j^l-m][i^l]=imagelist[j]*mat[j-m][i]^g*imagelist[i]^-1 then 
      return [false, imagelist];
    fi;
  od;
od;

return [true, imagelist];

end);

#############################################################################
#JDM JDM JDM the following is only commented out to allow for a new method in 
#the dev version. If doing a new release before moving things over from dev, 
#then uncomment this function.

InstallGlobalFunction(RMSIsoByTriple, 
function(rms1, rms2, triple)
  local fam1, fam2, mapfam, map;

  fam1 :=  ElementsFamily(FamilyObj(rms1));
  fam2 :=  ElementsFamily(FamilyObj(rms2));
  mapfam := GeneralMappingsFamily(fam1,fam2);	
  map := rec( triple := triple);
  Objectify(NewType(mapfam, IsGeneralMapping and IsSPGeneralMapping and 
   IsTotal and IsSingleValued and IsInjective and IsSurjective and 
    RespectsMultiplication and IsRMSIsoByTripleRep), map);
  SetSource(map, rms1);
  SetRange(map, rms2);
  IsOne(map);
  
  return map;
end);

#############################################################################

InstallMethod(RZMSGraph, "for a RZMS", [IsReesZeroMatrixSemigroup],
function(rms)

Print("#I It appears that the `grape' package is not fully installed.", 
"As a\n#I consequence this function is not available.\n");
return fail;

end);

#############################################################################

InstallMethod(RZMSInducedFunction, "for a RZMS",
[IsReesZeroMatrixSemigroup, IS_PERM, IsGeneralMapping, 
 IsMultiplicativeElement, IsList],
function(rms, l, g, groupelt, component)

Print("#I It appears that the `grape' package is not fully installed. As a",
 "\n#I consequence this function is not available.\n");
return fail;
end);

#############################################################################

InstallMethod(RZMStoRZMSInducedFunction, "for a RZMS", 
[IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup, IS_PERM, 
IsGeneralMapping, IsList],
function(rms1, rms2, l, g, groupelts)

Print("#I It appears that the `grape' package is not fully installed.", 
"As a\n#I consequence this function is not available.\n");
return fail;
end);

#############################################################################
#JDM JDM JDM the following is only commented out to allow for a new method in 
#the dev version. If doing a new release before moving things over from dev, 
#then uncomment this function.

InstallGlobalFunction(RZMSIsoByTriple, 
function(rms1, rms2, triple)
local fam1, fam2, mapfam, map;

fam1 :=  ElementsFamily(FamilyObj(rms1));
fam2 :=  ElementsFamily(FamilyObj(rms2));
mapfam := GeneralMappingsFamily(fam1,fam2);	
map := rec( triple := triple);
Objectify(NewType(mapfam, IsGeneralMapping and 
IsSPGeneralMapping and IsTotal and IsSingleValued and IsInjective and 
IsSurjective and RespectsMultiplication and IsRZMSIsoByTripleRep), map);
SetSource(map, rms1);
SetRange(map, rms2);

return map;
end);

###########################################################################

InstallMethod(UnderlyingGroupAutoOfZeroGroupAuto, 
"for a zero group automorphism", 
[IsEndoGeneralMapping and IsZeroGroupAutomorphismRep], x-> x!.grpauto);

###########################################################################

InstallMethod( ViewObj, "for object in `IsRMSIsoByTriple'",
[IsGeneralMapping and IsRMSIsoByTripleRep], 
function( obj ) 
Print( obj!.triple );
end );

###########################################################################

InstallMethod( ViewObj, "for object in `IsRZMSIsoByTriple'",
[IsGeneralMapping and IsRZMSIsoByTripleRep],
x->  Print(x!.triple ));

###########################################################################

InstallMethod( ViewObj, "for inner auto. of trans. semigroup",
[IsInnerAutomorphismOfSemigroup],
x-> Print( "^", ConjugatorOfInnerAutomorphismOfSemigroup(x) ));


###########################################################################

InstallGlobalFunction(ZeroGroupAutomorphism,
function( zg, auto)

local fam, mapfam, map;

fam :=  ElementsFamily(FamilyObj(zg));
mapfam := GeneralMappingsFamily(fam,fam);	

auto:=rec(grpauto:=auto);

Objectify(NewType(mapfam, IsEndoGeneralMapping and 
IsSPGeneralMapping and IsTotal and IsSingleValued and IsInjective and IsSurjective and RespectsMultiplication and RespectsOne and RespectsZero and IsZeroGroupAutomorphismRep), auto);

SetSource(auto, zg);
SetRange(auto, zg);
SetIsInjective(auto, true);
SetIsSurjective(auto, true);
SetIsTotal(auto, true);

return auto;
end);

#############################################################################

InstallMethod(\=, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj, 
[IsEndoGeneralMapping and IsRMSIsoByTripleRep, IsEndoGeneralMapping and 
 IsRMSIsoByTripleRep],
function(triple1, triple2)

if triple1!.triple[1]=triple2!.triple[1] and 
triple1!.triple[2]=triple2!.triple[2] and triple1!.triple[3]=triple2!.triple[3] 
then 

	return true;

else 

	return OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple1)=OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple2);
fi; 

end);

###########################################################################

InstallMethod(\=, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj, 
[IsEndoGeneralMapping and IsRZMSIsoByTripleRep, IsEndoGeneralMapping and 
IsRZMSIsoByTripleRep], 
function(triple1, triple2)

if triple1!.triple[1]=triple2!.triple[1] and triple1!.triple[2]=triple2!.triple[2] and triple1!.triple[3]=triple2!.triple[3] then 

	return true;

else 

	return OnTuples(GeneratorsOfSemigroup(Source(triple1)), 
	 triple1)=OnTuples(GeneratorsOfSemigroup(Source(triple1)), triple2);
fi; 

end);

###########################################################################

InstallMethod(\=, "for objects in IsZeroGroupAutomorphismRep", IsIdenticalObj, 
[IsZeroGroupAutomorphismRep, IsZeroGroupAutomorphismRep], 
function(a1, a2)

return ForAll(GeneratorsOfMonoid(Source(a1)), 
   x -> ImageElm(a1,x) = ImageElm(a2,x));
end);

###########################################################################

InstallMethod(\=, "inner automorphism and inner automorphism", IsIdenticalObj, 
[IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, 
IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup],
function(inn1, inn2)

return OnTuples(GeneratorsOfSemigroup(Source(inn1)), inn1)=OnTuples(GeneratorsOfSemigroup(Source(inn2)), inn2);

end);

###########################################################################

InstallMethod(\=, "for inner auto. and semigp homo. by imgs of gens.", 
IsIdenticalObj, [IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, 
IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep],
function(inn, hom)

if IsMonoid(Source(inn)) then 
	return OnTuples(GeneratorsOfMonoid(Source(inn)), inn)=hom!.imgsgens;
else
	return OnTuples(GeneratorsOfSemigroup(Source(inn)), inn)=hom!.imgsgens;
fi;
end);

###########################################################################

InstallMethod(\=, "for semigp homo. by imgs of gens. and inner auto.",
IsIdenticalObj, [IsSemigroupHomomorphism and 
IsSemigroupHomomorphismByImagesOfGensRep, IsSemigroupHomomorphism and 
IsInnerAutomorphismOfSemigroup],
function(hom, inn)

if IsMonoid(Source(inn)) then 
	return OnTuples(GeneratorsOfMonoid(Source(inn)), inn)=hom!.imgsgens;
else
	return OnTuples(GeneratorsOfSemigroup(Source(inn)), inn)=hom!.imgsgens;
fi;
end);

#############################################################################

InstallMethod(\<, "for objects in `IsRMSIsoByTriple'", IsIdenticalObj,
[IsGeneralMapping and IsRMSIsoByTripleRep, IsGeneralMapping and 
 IsRMSIsoByTripleRep],  
function(triple1, triple2)

return (triple1!.triple[1]<triple2!.triple[1]) or
 (triple1!.triple[1]=triple2!.triple[1] and
  triple1!.triple[2]<triple2!.triple[2]) or 
 (triple1!.triple[1]=triple2!.triple[1] and
  triple1!.triple[2]=triple2!.triple[2] and 
  triple1!.triple[3]<triple2!.triple[3]);
end);

#############################################################################

InstallMethod(\<, "for objects in `IsRZMSIsoByTriple'", IsIdenticalObj,
[IsGeneralMapping and IsRZMSIsoByTripleRep, IsGeneralMapping and 
 IsRZMSIsoByTripleRep],
function(triple1, triple2)

return (triple1!.triple[1]<triple2!.triple[1]) or
 (triple1!.triple[1]=triple2!.triple[1] and 
  triple1!.triple[2]<triple2!.triple[2]) or 
 (triple1!.triple[1]=triple2!.triple[1] and 
  triple1!.triple[2]=triple2!.triple[2] and 
  triple1!.triple[3]<triple2!.triple[3]);
end);

#############################################################################

InstallMethod(\<, "for objects in IsZeroGroupAutomorphismRep", IsIdenticalObj, 
[IsZeroGroupAutomorphismRep, IsZeroGroupAutomorphismRep],
function(a1, a2)
return a1!.grpauto<a2!.grpauto;
end);

#############################################################################

InstallMethod(\<, "for . and inner auto.", 
IsIdenticalObj, [IsSemigroupHomomorphism and 
IsSemigroupHomomorphismByImagesOfGensRep, IsSemigroupHomomorphism and
IsInnerAutomorphismOfSemigroup],
function(hom, inn)

if IsMonoid(Source(inn)) then 
  return OnTuples(GeneratorsOfMonoid(Source(inn)), inn)>hom!.imgsgens;
else 
  return OnTuples(GeneratorsOfSemigroup(Source(inn)), inn)>hom!.imgsgens;
fi;
end);

#############################################################################

InstallMethod(\<, "for inner auto. and semigp homo. by imgs of gens",
IsIdenticalObj, [IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, 
IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep],
function(inn, hom)

if IsMonoid(Source(inn)) then 
	return OnTuples(GeneratorsOfMonoid(Source(inn)), inn)<hom!.imgsgens;
else
	return OnTuples(GeneratorsOfSemigroup(Source(inn)), inn)<hom!.imgsgens;
fi;

end);

#############################################################################

InstallMethod(\<, "inner automorphism and inner automorphism", IsIdenticalObj,
[IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup, 
IsSemigroupHomomorphism and IsInnerAutomorphismOfSemigroup],
function(inn1, inn2)

return OnTuples(GeneratorsOfSemigroup(Source(inn1)), inn1) < 
OnTuples(GeneratorsOfSemigroup(Source(inn2)), inn2);
end);


