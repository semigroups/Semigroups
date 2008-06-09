##
## semihomo.gi
## Version 3.1.1
## Mon Jun  9 09:26:11 BST 2008
##

###########################################################################
##
##	<#GAPDoc Label="SemigroupHomomorphismByFunction">
##	<ManSection><Heading>SemigroupHomomorphismByFunction</Heading>
##	<Oper Name="SemigroupHomomorphismByFunction" Arg="S, T, func"/>
##	<Oper Name="SemigroupHomomorphismByFunctionNC" Arg="S, T, func"/>
##	<Description>
##	returns a semigroup homomorphism with representation
##	<C>IsSemigroupHomomorphismByFunctionRep</C> from the semigroup <C>S</C> to 
##	the semigroup <C>T</C> defined by the function <C>func</C>. <P/>
##
##	<C>SemigroupHomomorphismByFunction</C> will find an isomorphism from 
##	<C>S</C> to a finitely presented semigroup or monoid (using 
##	<Ref Oper="IsomorphismFpSemigroup"/> or <Ref Oper="IsomorphismFpMonoid"/>) 
##	and then check that the list of values under <C>func</C> of the generators 
##	of <C>S</C> satisfy the relations of this presentation. <P/>
##
##	<C>SemigroupHomomorphismByFunctionNC</C> does not check that <C>func</C> 
##	defines a homomorphism and, in this case <C>S</C> and <C>T</C> 
##	can be semigroups, <M>D</M>-classes, <M>H</M>-classes or any combination of 
##	these.<P/> 
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 4, 3, 5, 2 ] ), 
##  &gt; Transformation( [ 2, 3, 1, 1, 2 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; gens:=[ Transformation( [ 1, 5, 1, 2, 1 ] ), 
##  &gt; Transformation( [ 5, 1, 4, 3, 2 ] ) ];;
##  gap&gt; T:=Semigroup(gens);;
##  gap&gt; idem:=Random(Idempotents(T));;
##  gap&gt; hom:=SemigroupHomomorphismByFunction(S, T, x-&gt; idem);
##  SemigroupHomomorphism ( &lt;semigroup with 2 generators&gt;-&gt;&lt;semigroup with 
##  2 generators&gt;)
##  gap&gt; hom:=SemigroupHomomorphismByFunctionNC(S, T, x-&gt; idem);
##  SemigroupHomomorphism ( &lt;semigroup with 2 generators&gt;-&gt;&lt;semigroup with 
##  2 generators&gt;)
##	</Example> <!-- semihomo.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(SemigroupHomomorphismByFunction, "for a semigroup, semigroup, function", true, 
[IsSemigroup, IsSemigroup, IsFunction], 0, 
function(S, T, func)
local iso, conc, fp, free, rels, imgsgens, imgs, rel;

if IsMonoid(S) then 
	iso:=IsomorphismFpMonoid(S);
	conc:= OnTuples(GeneratorsOfMonoid(S), iso);
	fp:=GeneratorsOfMonoid(Range(iso));
	free:=FreeGeneratorsOfFpMonoid(Range(iso));
	rels:=RelationsOfFpMonoid(Range(iso));
	imgsgens:=List(GeneratorsOfMonoid(S), func);
else 
	iso:=IsomorphismFpSemigroup(S);
	conc:= OnTuples(GeneratorsOfSemigroup(S), iso);
	fp:=GeneratorsOfSemigroup(Range(iso));
	free:=FreeGeneratorsOfFpSemigroup(Range(iso));
	rels:=RelationsOfFpSemigroup(Range(iso));
	imgsgens:=List(GeneratorsOfSemigroup(S), func);
fi;

imgs:=Permuted(imgsgens, MONOIDPermListList(fp, conc)); 

rel:=First(rels, x-> not MappedWord(x[1], free, imgs)=MappedWord(x[2], free, imgs));

if rel=fail then 
	return SemigroupHomomorphismByFunctionNC(S, T, func);   
else 
	Info(InfoMonoid, 4, "the relation ", rel, " is not satisfied in the image");
	return fail;
fi;

end);

################

InstallMethod(SemigroupHomomorphismByFunctionNC, "for a semigroup, semigroup, function", true, 
[IsSemigroup, IsSemigroup, IsFunction], 0, 
function(S, T, func)
local hom, filter;

hom:=rec(func:=func);
  
Objectify(NewType( GeneralMappingsFamily
    ( ElementsFamily( FamilyObj( S ) ),
      ElementsFamily( FamilyObj( T ) ) ), IsSemigroupHomomorphism 
      and IsSemigroupHomomorphismByFunctionRep), hom);
	
  SetSource(hom, S);
  SetRange(hom, T);
  #IsOne(hom);
  return hom;
end);

################

InstallOtherMethod(SemigroupHomomorphismByFunctionNC, "for a D-class, semigroup, function", true, 
[IsGreensDClass, IsSemigroup, IsFunction], 0, 
function(S, T, func)
local hom, filter;

hom:=rec(func:=func);
  
Objectify(NewType( GeneralMappingsFamily
    ( ElementsFamily( FamilyObj( S ) ),
      ElementsFamily( FamilyObj( T ) ) ), IsSemigroupHomomorphism 
      and IsSemigroupHomomorphismByFunctionRep), hom);
	
  SetSource(hom, S);
  SetRange(hom, T);
  
  return hom;
end);

################

InstallOtherMethod(SemigroupHomomorphismByFunctionNC, "for a semigroup, D-class, function", true, 
[IsSemigroup,IsGreensDClass,IsFunction], 0, 
function(S, T, func)
local hom, filter;

hom:=rec(func:=func);
  
Objectify(NewType( GeneralMappingsFamily
    ( ElementsFamily( FamilyObj( S ) ),
      ElementsFamily( FamilyObj( T ) ) ), IsSemigroupHomomorphism 
      and IsSemigroupHomomorphismByFunctionRep), hom);
	
  SetSource(hom, S);
  SetRange(hom, T);
  
  return hom;
end);

################

InstallOtherMethod(SemigroupHomomorphismByFunctionNC, "for a D-class, D-class, function", true, 
[IsGreensDClass, IsGreensDClass, IsFunction], 0, 
function(S, T, func)
local hom, filter;

hom:=rec(func:=func);
  
Objectify(NewType( GeneralMappingsFamily
    ( ElementsFamily( FamilyObj( S ) ),
      ElementsFamily( FamilyObj( T ) ) ), IsSemigroupHomomorphism 
      and IsSemigroupHomomorphismByFunctionRep), hom);
	
  SetSource(hom, S);
  SetRange(hom, T);
  
  return hom;
end);

################

InstallOtherMethod(SemigroupHomomorphismByFunctionNC, "for an H-class, semigroup, function", true, 
[IsGreensHClass, IsSemigroup, IsFunction], 0, 
function(S, T, func)
local hom, filter;

hom:=rec(func:=func);
  
Objectify(NewType( GeneralMappingsFamily
    ( ElementsFamily( FamilyObj( S ) ),
      ElementsFamily( FamilyObj( T ) ) ), IsSemigroupHomomorphism 
      and IsSemigroupHomomorphismByFunctionRep), hom);

  SetSource(hom, S);
  SetRange(hom, T);
  
  return hom;
end);

################

InstallOtherMethod(SemigroupHomomorphismByFunctionNC, "for a semigroup, H-class, function", true, 
[IsSemigroup, IsGreensHClass, IsFunction], 0, 
function(S, T, func)
local hom, filter;

hom:=rec(func:=func);
  
Objectify(NewType( GeneralMappingsFamily
    ( ElementsFamily( FamilyObj( S ) ),
      ElementsFamily( FamilyObj( T ) ) ), IsSemigroupHomomorphism 
      and IsSemigroupHomomorphismByFunctionRep), hom);

  SetSource(hom, S);
  SetRange(hom, T);
  
  return hom;
end);

################

InstallMethod(ImagesRepresentative, "for semigroup homomorphism by function",  FamSourceEqFamElm, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep, IsMultiplicativeElement], 
function(hom, elt)

return hom!.func(elt);

end);

################

InstallMethod( ViewObj, "for semigroup homomorphism by function",[IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep],
function( obj )
Print( "SemigroupHomomorphism ( " );
ViewObj(Source(obj));
Print( "->" );
ViewObj(Range(obj));
Print( ")" );
end );

################

InstallMethod( PrintObj, "for semigroup homomorphism by function",[IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep],
function( obj )
Print( "SemigroupHomomorphismByFunction ( ", Source(obj), "->", Range(obj), ")" );
end );

################

InstallMethod(ImagesElm, "for semigroup homomorphism by function", FamSourceEqFamElm, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep, IsMultiplicativeElement],
function( hom, x)
return [ImagesRepresentative(hom, x)];
end);

################
# this corresponds to hom1*hom2

InstallMethod(CompositionMapping2, "for semi. homo. by func., semi. homo. by func.", true, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep, IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep], 0,
function(hom2, hom1)
local func;

if not IsSubset(Source(hom2), Range(hom1)) then 
  Error("source of <hom2> must contain range of <hom1>");
fi;

func:=function(x) 
return hom2!.func(hom1!.func(x));
end;

return SemigroupHomomorphismByFunctionNC(Source(hom1), Range(hom2), func);

end);

################
# this corresponds to hom1*hom2

InstallMethod(CompositionMapping2, "for semi. homo. by func., RZMS iso triple.", true, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep, IsSemigroupHomomorphism and IsRZMSIsoByTripleRep], 0,
function(hom2, hom1)
local func;

if not IsSubset(Source(hom2), Range(hom1)) then 
  Error("source of <hom2> must contain range of <hom1>");
fi;

func:=function(x) 
return hom2!.func(ImageElm(hom1, x));
end;

return SemigroupHomomorphismByFunctionNC(Source(hom1), Range(hom2), func);

end);

################
# this corresponds to hom1*hom2

InstallMethod(CompositionMapping2, "for RZMS iso. triple, semi. homo. by func.", true, [ IsSemigroupHomomorphism and IsRZMSIsoByTripleRep, IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep], 0,
function(hom2, hom1)
local func;

if not IsSubset(Source(hom2), Range(hom1)) then 
  Error("source of <hom2> must contain range of <hom1>");
fi;

func:=function(x) 
return ImageElm(hom2, hom1!.func(x));
end;

return SemigroupHomomorphismByFunctionNC(Source(hom1), Range(hom2), func);

end);

################
# this corresponds to hom1*hom2

InstallMethod(CompositionMapping2, "for semi. homo. by func., RMS iso triple.", true, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep, IsSemigroupHomomorphism and IsRMSIsoByTripleRep], 0,
function(hom2, hom1)
local func;

if not IsSubset(Source(hom2), Range(hom1)) then 
  Error("source of <hom2> must contain range of <hom1>");
fi;

func:=function(x) 
return hom2!.func(ImageElm(hom1, x));
end;

return SemigroupHomomorphismByFunctionNC(Source(hom1), Range(hom2), func);

end);

################
# this corresponds to hom1*hom2

InstallMethod(CompositionMapping2, "for RMS iso. triple, semi. homo. by func.", true, [ IsSemigroupHomomorphism and IsRMSIsoByTripleRep, IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep], 0,
function(hom2, hom1)
local func;

if not IsSubset(Source(hom2), Range(hom1)) then 
  Error("source of <hom2> must contain range of <hom1>");
fi;

func:=function(x) 
return ImageElm(hom2, hom1!.func(x));
end;

return SemigroupHomomorphismByFunctionNC(Source(hom1), Range(hom2), func);

end);

################
# this corresponds to hom1*hom2

InstallMethod(CompositionMapping2, "for semi. homo. by function, semi. homo. by image", true, [ IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep, IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesRep], 0, 
function(hom2, hom1)
local func;

func:=function(x)
return hom2!.func(ImageElm(hom1, x));
end;

return SemigroupHomomorphismByFunctionNC(Source(hom1), Range(hom2), func);

end);

################
# this corresponds to hom1*hom2

InstallMethod(CompositionMapping2, "for semi. homo. by function, semi. homo. by image", true, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesRep,  IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep], 0, 
function(hom2, hom1)
local func;

func:=function(x)
return ImageElm(hom2, hom1!.func(x));
end;

return SemigroupHomomorphismByFunctionNC(Source(hom1), Range(hom2), func);

end);

################
# this corresponds to hom1*hom2

InstallMethod(CompositionMapping2, "for semi. homo. by function, semi. homo. by image", true, [IsGroupHomomorphism,  IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep], 0, 
function(hom2, hom1)
local func;

func:=function(x)
return ImageElm(hom2, hom1!.func(x));
end;

return SemigroupHomomorphismByFunctionNC(Source(hom1), Range(hom2), func);

end);

################
# this corresponds to hom1*hom2

InstallMethod(CompositionMapping2, "for semi. homo. by function, semi. homo. by image", true, [ IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep, IsGroupHomomorphism], 0, 
function(hom2, hom1)
local func;

func:=function(x)
return hom2!.func(ImageElm(hom1, x));
end;

return SemigroupHomomorphismByFunctionNC(Source(hom1), Range(hom2), func);

end);

################

InstallMethod(\=, "for semigroup homomorphisms by function", IsIdenticalObj, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep, IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep],  0,
function(hom1, hom2)

#JDM do we need to check that Sources and Ranges are equal? YES

return List(GeneratorsOfSemigroup(Source(hom1)), x-> hom1!.func(x))=
	List(GeneratorsOfSemigroup(Source(hom2)), x-> hom2!.func(x));
#hom1!.func=hom2!.func;

end);

################

InstallMethod(PreImagesRepresentative,  "for semigroup homomorphism by function",  FamRangeEqFamElm, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep and IsInjective and IsSurjective and IsTotal, IsMultiplicativeElement],
function(hom, elt)

return First(Source(hom), x-> ImageElm(hom, x)=elt);

end);

################

InstallMethod(IsOne, "for semigroup homomorphism by function", [IsSemigroupHomomorphism and IsSemigroupHomomorphismByFunctionRep],  0,
function(hom)

	return OnTuples(GeneratorsOfSemigroup(Source(hom)), hom)=GeneratorsOfSemigroup(Source(hom));

end);

###########################################################################
##
##	<#GAPDoc Label="SemigroupHomomorphismByImagesOfGens">
##	<ManSection><Heading>SemigroupHomomorphismByImagesOfGens</Heading>
##	<Oper Name="SemigroupHomomorphismByImagesOfGens" Arg="S, T, list"/>
##	<Oper Name="SemigroupHomomorphismByImagesOfGensNC" Arg="S, T, list"/>
##	<Description>
##	returns a semigroup homomorphism with representation
##	<C>IsSemigroupHomomorphismByImagesOfGensRep</C> from  <C>S</C> to 
##	<C>T</C> where the image of the <C>i</C>th generator of <C>S</C> is the 
##	<C>i</C>th position in <C>list</C>.
##
##	<C>SemigroupHomomorphismByImagesOfGens</C> will find an isomorphism from 
##	<C>S</C> to a finitely presented semigroup or monoid (using 
##	<Ref Attr="IsomorphismFpSemigroup"/> or <Ref Attr="IsomorphismFpMonoid"/>) 
##	and then check that <C>list</C> satisfies the relations of this 
##	presentation. <P/>
##
##	<C>SemigroupHomomorphismByImagesOfGensNC</C> does not check that <C>list</C> 
##	induces a homomorphism. <P/>
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 1, 4, 3, 5, 2 ] ), 
##  &gt; Transformation( [ 2, 3, 1, 1, 2 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; gens:=[ Transformation( [ 1, 5, 1, 2, 1 ] ), 
##  &gt; Transformation( [ 5, 1, 4, 3, 2 ] ) ];;
##  gap&gt; T:=Semigroup(gens);;
##  gap&gt; SemigroupHomomorphismByImagesOfGens(S, T, GeneratorsOfSemigroup(T));
##  fail
##  gap&gt; SemigroupHomomorphismByImagesOfGens(S, S, GeneratorsOfSemigroup(S));
##  SemigroupHomomorphismByImagesOfGens ( &lt;trans. semigroup of size 161 with 
##  2 generators&gt;-&gt;&lt;trans. semigroup of size 161 with 2 generators&gt;)
##	</Example> <!-- semihomo.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(SemigroupHomomorphismByImagesOfGensNC, "for a semigroup, semigroup, list", true, 
[IsSemigroup, IsSemigroup, IsList], 0, 
function(S, T, imgsgens)
local hom, filter;

  if IsMonoid(S) and Length(GeneratorsOfMonoid(S))<>Length(imgsgens) then
    Error("homomorphism must be defined on all generators");
  elif not IsMonoid(S) and Length(GeneratorsOfSemigroup(S))<>Length(imgsgens) then 
    Error("homomorphism must be defined on all generators");
  fi;

hom:=rec(imgsgens:=imgsgens);

Objectify(NewType( GeneralMappingsFamily
    ( ElementsFamily( FamilyObj( S ) ),
      ElementsFamily( FamilyObj( T ) ) ), IsSemigroupHomomorphism 
      and IsSemigroupHomomorphismByImagesOfGensRep), hom);

  SetSource(hom, S);
  SetRange(hom, T);
  
  return hom;
end);

################

InstallMethod(SemigroupHomomorphismByImagesOfGens, "for a semigroup, semigroup, list", true, 
[IsSemigroup, IsSemigroup, IsList], 0, 
function(S, T, imgsgens)
local iso, conc, fp, imgs, free, rel, rels;

if IsMonoid(S) then 
	iso:=IsomorphismFpMonoid(S);
	conc:= OnTuples(GeneratorsOfMonoid(S), iso);
	fp:=GeneratorsOfMonoid(Range(iso));
	free:=FreeGeneratorsOfFpMonoid(Range(iso));
	rels:=RelationsOfFpMonoid(Range(iso));
else 
	iso:=IsomorphismFpSemigroup(S);
	conc:= OnTuples(GeneratorsOfSemigroup(S), iso);
	fp:=GeneratorsOfSemigroup(Range(iso));
	free:=FreeGeneratorsOfFpSemigroup(Range(iso));
	rels:=RelationsOfFpSemigroup(Range(iso));
fi;

imgs:=Permuted(imgsgens, MONOIDPermListList(fp, conc)); 
#make sure the generators are in the right order

rel:=First(rels, x-> not MappedWord(x[1], free, imgs)=MappedWord(x[2], free, imgs));

if rel=fail then 
	return SemigroupHomomorphismByImagesOfGensNC(S, T, imgsgens);   
else 
	Info(InfoMonoid, 4, "the relation ", rel, " is not satisfied in the image");
	return fail;
fi;

end);

################

InstallMethod(SemigroupHomomorphismByImagesOfGens, "for a semigroup, semigroup, list", true, 
[IsFpSemigroup, IsSemigroup, IsList], 0, 
function(S, T, imgs)
local free, rel;

free:=FreeGeneratorsOfFpSemigroup(S);

rel:=First(RelationsOfFpSemigroup(S), x-> not MappedWord(x[1], free, imgs)=MappedWord(x[2], free, imgs));

if rel=fail then 
	return SemigroupHomomorphismByImagesOfGensNC(S, T, imgs);
else 
	Info(InfoMonoid, 1, "the relation ", rel, " is not satisfied");
	return fail;
fi;

end);

################

InstallMethod(SemigroupHomomorphismByImagesOfGens, "for a semigroup, semigroup, list", true, 
[IsFpMonoid, IsSemigroup, IsList], 0, 
function(S, T, imgs)
local free, rel;

free:=FreeGeneratorsOfFpMonoid(S);

rel:=First(RelationsOfFpMonoid(S), x-> not MappedWord(x[1], free, imgs)=MappedWord(x[2], free, imgs));

if rel=fail then 
	return SemigroupHomomorphismByImagesOfGensNC(S, T, imgs);
else 
	Info(InfoMonoid, 1, "the relation ", rel, " is not satisfied");
	return fail;
fi;

end);

################

InstallMethod(ImagesRepresentative, "for semigroup homomorphism by images",  FamSourceEqFamElm, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep, IsMultiplicativeElement], 
function(hom, elt)
local pos, iso, imgs, gens;

if IsMonoid(Source(hom)) then 
  gens:=GeneratorsOfMonoid(Source(hom));
else
  gens:=GeneratorsOfSemigroup(Source(hom));
fi;

pos:=Position(gens, elt); 
if not pos=fail then 
   return hom!.imgsgens[pos];
fi;
 
if IsMonoid(Source(hom)) then 
	iso:=IsomorphismFpMonoid(Source(hom));
	imgs:=List(Flat(List(GeneratorsOfMonoid(Range(iso)), x-> PreImageElm(iso, x))), x-> ImageElm(hom, x));
	#get the images of the generators in the right order 
	#JDM is there a better way?
	return MappedWord(UnderlyingElement(ImageElm(iso, elt)), FreeGeneratorsOfFpMonoid(Range(iso)), imgs); 
else
	iso:=IsomorphismFpSemigroup(Source(hom));
	imgs:=List(Flat(List(GeneratorsOfSemigroup(Range(iso)), x-> PreImageElm(iso, x))), x-> ImageElm(hom, x));
	#get the images of the generators in the right order 
	#JDM is there a better way?
	return MappedWord(UnderlyingElement(ImageElm(iso, elt)), FreeGeneratorsOfFpSemigroup(Range(iso)), imgs); 
fi;

end);

################

InstallMethod( ViewObj, "for semigroup homomorphism by images",
   [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep],   
   function( obj )
      Print( "SemigroupHomomorphismByImagesOfGens ( ", Source(obj), "->", Range(obj), ")" );
end );

################

InstallMethod( PrintObj, "for semigroup homomorphism by images",
   [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep],   
   function( obj )
      Print( "SemigroupHomomorphismByImagesOfGens ( ", Source(obj), "->", Range(obj), ")" );
end );

################

InstallMethod(ImagesElm, "for semigroup homomorphism by images",
      FamSourceEqFamElm, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep, IsMultiplicativeElement],
      function( hom, x)
	
      return [ImagesRepresentative(hom, x)];
end);

################

InstallMethod(CompositionMapping2, "for semigroup homomorphism by images of gens", true, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep, IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep], 0,
function(hom2, hom1)
local imgsgens;

if not IsSubset(Source(hom2), Range(hom1)) then 
  Error("source of <hom2> must contain range of <hom1>");
fi;

imgsgens:=List(hom1!.imgsgens, x-> ImageElm(hom2, x));

return SemigroupHomomorphismByImagesOfGensNC(Source(hom1), Range(hom2), imgsgens);

end);

################

InstallMethod(\=, "for semigroup homomorphism by images of gens", IsIdenticalObj, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep, IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep],  0,
function(hom1, hom2)

return hom1!.imgsgens=hom2!.imgsgens;

end);

################

InstallMethod(\<, "for semigroup homomorphism by images of gens.", IsIdenticalObj, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep, IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep],  0,
function(hom1, hom2)

return hom1!.imgsgens<hom2!.imgsgens;

end);

################

InstallMethod(PreImagesRepresentative,  "for semigroup homomorphism by images",  FamRangeEqFamElm, [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep and IsInjective and IsSurjective and IsTotal, IsMultiplicativeElement],
function(hom, elt)

return First(Source(hom), x-> ImageElm(hom, x)=elt);

end);

################

InstallMethod(InverseGeneralMapping, "for semigroup homomorphism by images of gens", [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep and IsInjective and IsSurjective and IsTotal],  0,
function(hom)
local i;

i:=0;
repeat
	i:=i+1;
until IsOne(hom^i);

return hom^(i-1);

end);

################

InstallMethod(IsOne, "for semigroup homomorphism by images of gens", [IsSemigroupHomomorphism and IsSemigroupHomomorphismByImagesOfGensRep],  0,
function(hom)

if IsMonoid(Source(hom)) then 
	return hom!.imgsgens=GeneratorsOfMonoid(Source(hom));
else 
	return hom!.imgsgens=GeneratorsOfSemigroup(Source(hom));
fi;

end);

###########################################################################
##
##	<#GAPDoc Label="SemigroupHomomorphismByImages">
##	<ManSection><Heading>SemigroupHomomorphismByImages</Heading>
##	<Oper Name="SemigroupHomomorphismByImages" Arg="S, T, list"/>
##	<Oper Name="SemigroupHomomorphismByImagesNC" Arg="S, T, list"/>
##	<Description>
##	returns a semigroup homomorphism with representation
##	<C>IsSemigroupHomomorphismByImagesRep</C> from  <C>S</C> to 
##	<C>T</C> where the image of the <C>i</C>th element of <C>S</C> is the 
##	<C>i</C>th position in <C>list</C>.
##
##	<C>SemigroupHomomorphismByImages</C> will find an isomorphism from 
##	<C>S</C> to a finitely presented semigroup or monoid (using 
##	<Ref Attr="IsomorphismFpSemigroup"/> or <Ref Attr="IsomorphismFpMonoid"/>) 
##	and then check that <C>list</C> satisfies the relations of this 
##	presentation. <P/>
##
##	<C>SemigroupHomomorphismByImagesNC</C> does not check that <C>list</C> 
##	induces a homomorphism. <P/>
##
##	<Example>
##  gap&gt; gens:=[ Transformation( [ 2, 3, 4, 2, 4 ] ),
##  &gt; Transformation( [ 3, 4, 2, 1, 4 ] ) ];;
##  gap&gt; S:=Semigroup(gens);;
##  gap&gt; gens:=[ Transformation( [ 2, 4, 4, 1, 2 ] ),
##  &gt; Transformation( [ 5, 1, 1, 5, 1 ] ) ];;
##  gap&gt; T:=Semigroup(gens);;
##  gap&gt; idem:=Transformation( [ 5, 5, 5, 5, 5 ] );;
##  gap&gt; list:=List([1..Size(S)], x-&gt; idem);;
##  gap&gt; hom:=SemigroupHomomorphismByImages(S, T, list);
##  SemigroupHomomorphismByImagesOfGens ( &lt;trans. semigroup of size 164 with 
##  2 generators&gt;-&gt;&lt;trans. semigroup with 2 generators&gt;)
##  gap&gt; SemigroupHomomorphismByImagesNC(S, T, list);
##  SemigroupHomomorphismByImages ( &lt;trans. semigroup of size 164 with 
##  2 generators&gt;-&gt;&lt;trans. semigroup with 2 generators&gt;)
##	</Example> <!-- semihomo.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(SemigroupHomomorphismByImages, "for a semigroup, semigroup, list", [IsSemigroup, IsSemigroup, IsList], 
function(S, T, list)
local iso, conc, fp, imgs, free, rel, rels, imgsgens;

if Size(S)<>Length(list) then
	Error("<S> and <list> must have the same size");
fi;

if IsMonoid(S) then 
	iso:=IsomorphismFpMonoid(S);
	conc:= OnTuples(GeneratorsOfMonoid(S), iso);
	fp:=GeneratorsOfMonoid(Range(iso));
	free:=FreeGeneratorsOfFpMonoid(Range(iso));
	rels:=RelationsOfFpMonoid(Range(iso));
	imgsgens:=list{List(GeneratorsOfMonoid(S), x-> Position(Elements(S), x))};

else 
	iso:=IsomorphismFpSemigroup(S);
	conc:= OnTuples(GeneratorsOfSemigroup(S), iso);
	fp:=GeneratorsOfSemigroup(Range(iso));
	free:=FreeGeneratorsOfFpSemigroup(Range(iso));
	rels:=RelationsOfFpSemigroup(Range(iso));
	imgsgens:=list{List(GeneratorsOfSemigroup(S), x-> Position(Elements(S), x))};
fi;

imgs:=Permuted(imgsgens, MONOIDPermListList(fp, conc)); 
#make sure the generators are in the right order

rel:=First(rels, x-> not MappedWord(x[1], free, imgs)=MappedWord(x[2], free, imgs));
if rel=fail then 
	return SemigroupHomomorphismByImagesOfGensNC(S, T, imgsgens);   
else 
	Info(InfoMonoid, 4, "the relation ", rel, " is not satisfied in the image");
	return fail;
fi;
end);

################

InstallMethod(\<, "for semigroup homomorphisms", IsIdenticalObj, [IsSemigroupHomomorphism, IsSemigroupHomomorphism],  0,
function(hom1, hom2)
local gens;

if Source(hom1)<>Source(hom2) or Range(hom1)<>Range(hom2) then 
	Error("source and range of homomorphisms must be equal");
fi;
if IsMonoid(Source(hom1)) then 
	gens:=GeneratorsOfMonoid(Source(hom1));
else 
	gens:=GeneratorsOfSemigroup(Source(hom2));
fi;

return OnTuples(gens, hom1)<OnTuples(gens, hom2);

end);

################

InstallMethod(\=, "for semigroup homomorphisms", IsIdenticalObj, [IsSemigroupHomomorphism, IsSemigroupHomomorphism],  0,
function(hom1, hom2)
local gens;

if Source(hom1)<>Source(hom2) or Range(hom1)<>Range(hom2) then 
	Error("source and range of homomorphisms must be equal");
fi;
if IsMonoid(Source(hom1)) then 
	gens:=GeneratorsOfMonoid(Source(hom1));
else 
	gens:=GeneratorsOfSemigroup(Source(hom2));
fi;

return OnTuples(gens, hom1)=OnTuples(gens, hom2);

end);