#############################################################################
##
#W  general.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##


##  <#GAPDoc Label="generaltop">
##  This chapter contains general functions needed to support functions in the 
##	MONOID package.
##	<#/GAPDoc>


###########################################################################
##
##  <#GAPDoc Label="EnumeratorOfCartesian">
##  <ManSection> 
##  <Func Name="EnumeratorOfCartesian" Arg="enums"/>
##  <Description>
##  returns a simple enumerator of the cartesian product of the list of 
##	enumerators <C>enums</C>.
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>

InstallGlobalFunction(EnumeratorOfCartesian, 
function ( enums )
local  enum;

if IsEmpty( enums )  then
	return Immutable( [  ] );
elif Length( enums ) = 1 then
	return enums;
fi;

enum := EnumeratorByFunctions( CollectionsFamily( FamilyObj( enums ) ), rec(
ElementNumber := function ( enum, pos )
local nn, t, base, i;
nn:=pos-1;
t := [  ];

base:=Product(List(enums{[1..Length(enums)]}, Length));

for i in [1..Length(enums)] do
	base:=base/Length(enums[Length(enums)-i+1]);
	t[Length(enums)-i+1]:=enums[Length(enums)-i+1][QuoInt(nn, base)+1];
	nn:=RemInt(nn, base);
od;

return t;
end,

NumberElement := function ( enum, elm )
local i, numb;

numb:=1;

for i in [1..Length(enums)] do 
 numb:=numb+(Position(enums[i], elm[i])-1)* Product(List(enums{[1..i-1]}, Length));
od;
return numb;

end,
Length := function ( enum )
	return Product(List(enums, Length));
end, 
PrintObj := function ( enum )
	Print( "<enumerator of cartesian product>" );
	return;
end
));
return enum;
end);

###########################################################################
##
##  <#GAPDoc Label="MONOIDPermListList">
##  <ManSection> 
##  <Func Name="MONOIDPermListList" Arg="gens1, gens2"/>
##  <Description>
##  returns the permutation that maps the free generators of <C>gens1</C> to the 
##	free generators <C>gens2</C>.
##	</Description>  
##	</ManSection>
##	<#/GAPDoc>


InstallMethod(MONOIDPermListList, "for gens. of fp semigroup", 
true, [IsList, IsList], 0, 
function(gens1, gens2)
local int1, int2;

int1:=List(gens1, x-> Int(String(x){[2..Length(String(x))]}));
int2:=List(gens2, x-> Int(String(x){[2..Length(String(x))]}));

return PermListList(int1, int2);

end);