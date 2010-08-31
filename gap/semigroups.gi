#############################################################################
##
#W  semigroups.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

# the functions in this file define different types of semigroups and 
# transformation semigroups

#JDM all the things in this file should be moved to semex...

# JDM ZeroGroupElt should be modified so that it takes a zero group as an 
# additional argument and returns an element in that zero group. As it is it 
# functions but isn't great.

InstallMethod(PrintObj,  "for a full transformation semigroup (monoid pkg)",
[IsTransformationSemigroup and IsFullTransformationSemigroup], 
function(s)
Print("<full transformation semigroup on ", 
DegreeOfTransformationSemigroup(s), " pts>");
end);

InstallMethod(ViewObj,  "for a full transformation semigroup (monoid pkg)",
[IsTransformationSemigroup and IsFullTransformationSemigroup], 
function(s)
Print("<full transformation semigroup on ", 
DegreeOfTransformationSemigroup(s), " pts>");
end);


###########################################################################
# JDM the following should be added to the library
# JDM new for 3.2!

InstallMethod(\=, [IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup], 
function(r1, r2)
return UnderlyingSemigroupOfReesZeroMatrixSemigroup(r1) = 
 UnderlyingSemigroupOfReesZeroMatrixSemigroup(r2) and 
  SandwichMatrixOfReesZeroMatrixSemigroup(r1) = 
   SandwichMatrixOfReesZeroMatrixSemigroup(r2);
end);

###########################################################################
# JDM new for 3.2!

InstallMethod(IsSubset, [IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup], 
function(r1, r2)

if r1=r2 then 
	return true;
else
  Info(InfoWarning, 1, "not yet implemented!"); #JDM
  return fail;
fi;
end);

###########################################################################

InstallGlobalFunction( SingularSemigroup, 
function(n)
local img, x, S, T;
img:=Concatenation([1..n-1], [n-1]);
x:=TransformationNC(img);
S:=FullTransformationSemigroup(n);

T:=SubsemigroupNC(S, Idempotents(GreensDClassOfElement(S, x)));

SetIsSemiBand(T, true);
SetIsRegularSemigroup(T, true);
SetIsCompletelyRegularSemigroup(T, false);
SetIsCliffordSemigroup(T, false);
SetIsOrthodoxSemigroup(T, false);
SetIsSimpleSemigroup(T, false);
SetIsGroupAsSemigroup(T, false);
SetIsCommutativeSemigroup(T, false);
SetIsInverseSemigroup(T, false);
SetIsBand(T, false);
SetIsRectangularBand(T, false);
SetIsZeroSemigroup(T, false);
SetIsLeftZeroSemigroup(T, false);
SetIsRightZeroSemigroup(T, false);

SetSize(T, n^n-Factorial(n));

#JDM Find Green's classes etc by reference to ParentAttr(T);

return T;

end) ;

###########################################################################

InstallMethod(OrderPreservingSemigroup, [IsPosInt],
function(n)
local add, gens, subtract, u, v, M, free, F, rels, i, j, fpM, iso;

Info(InfoMonoid, 2, "the command O returns the same value");

add:=function(list, pos)
list[pos]:=list[pos]+1;
  return list;
end;

subtract:=function(list, pos)
	list[pos]:=list[pos]-1;
	return list;
end;

u:=List([1..n-1], i-> Transformation(add([1..n], i)));
v:=List([1..n-1], i-> Transformation(subtract([1..n], n-i+1)));

gens:=Concatenation(u,v);
M:=Monoid(gens);

#free:=Concatenation(List([1..n-1], x-> Concatenation("u", String(x))), List([1..n-1], x-> Concatenation("v", String(x))));
#F:=FreeMonoid(free);
#u:=GeneratorsOfMonoid(F){[1..n-1]};
#v:=GeneratorsOfMonoid(F){[n..2*n-2]};

#rels:=[[u[1]*u[2]*u[1], u[1]*u[2]], [v[1]*v[2]*v[1], v[1]*v[2]]];

#for i in [1..n-1] do 
#	if not i=1 then 
#		Add(rels, [v[n-i]*u[i], u[i]*v[n-i+1]]);
#		Add(rels, [u[n-i]*v[i], v[i]*u[n-i+1]]);
#	fi;
#	Add(rels, [v[n-i]*u[i], u[i]]);
#	Add(rels, [u[n-i]*v[i], v[i]]);
#	for j in Difference([1..n-1], [n-i, n-i+1]) do
#		Add(rels, [u[i]*v[j], v[j]*u[i]]);
#	od;
#od;

#fpM:=F/rels;

#iso:=SemigroupHomomorphismByImagesOfGensNC(fpM, M, gens);
#inv:=SemigroupHomomorphismByImagesOfGensNC(M, fpM, GeneratorsOfMonoid(fpM));

#SetInverseGeneralMapping(inv, iso);
#SetInverseGeneralMapping(iso, inv);

#SetIsInjective(iso, true);
#SetIsSingleValued(iso, true);
#SetIsSurjective(iso, true);
#SetIsTotal(iso, true);

#SetIsInjective(inv, true);
#SetIsSingleValued(inv, true);
#SetIsSurjective(inv, true);
#SetIsTotal(inv, true);

#SetIsomorphismFpMonoid(M, inv);
#SetIsomorphismTransformationSemigroup(fpM, iso); 

SetSize(M, Binomial(2*n-1, n-1));
#SetSize(fpM, Binomial(2*n-1, n-1));
SetSmallGeneratingSet(M, Concatenation(u, [Transformation(Concatenation([1], [1..n-1]))]));

return M;
end);

###########################################################################

InstallMethod(ZeroSemigroup, [IsPosInt], 
function( n )	
local m, gens, x;

gens:=List([1..n-1], ZeroSemigroupElt);

m:=Semigroup(gens);
SetIsZeroSemigroup(m, true);
SetSize(m, n);
SetAsSSortedList(m, Concatenation([gens[1]^2], gens));
SetGreensRClasses(m, List(Elements(m), x-> GreensRClassOfElement(m, x)));

for x in GreensRClasses(m) do
	SetSize(x, 1);
	SetAsSSortedList(x, [Representative(x)]);
	SetIsGreensLClass(x, true);
	SetIsGreensHClass(x, true);
	SetIsGreensDClass(x, true);
	if not Representative(x)=MultiplicativeZero(m) then 
		SetIdempotents(x, []);
	else
		SetIdempotents(x, [Representative(x)]);
	fi;
od;

SetGreensLClasses(m, GreensRClasses(m));
SetGreensHClasses(m, GreensRClasses(m));
SetGreensDClasses(m, GreensRClasses(m));
SetOne(m, fail);

return m;

end);

#############################################################################

InstallMethod( ZeroSemigroupElt, [IsInt],
        function(n)
    local   fam,  new;

    if not n>=0 then 
      Error("must be a nonnegative integer");
    else
      fam:=FamilyObj(n);
   
      new:=[n];
      Objectify(ZeroSemigroupDefaultType(ZeroSemigroupFamily(fam)), new);

      return new;
    fi;
end);

###########################################################################
##
#M  ZeroSemigroupFamily( <fam> ) 
#M  ZeroSemigroupDefaultType( <fam>)
##

InstallMethod(ZeroSemigroupFamily, [IsFamily], 
        function(fam)
    local   afam;
    afam := NewFamily("ZeroSemigroupElt", IsZeroSemigroupElt);

    return afam;
end);

###########################################################################

InstallMethod(ZeroSemigroupDefaultType, [IsFamily],
        function(fam) 
    return NewType(fam, IsZeroSemigroupEltRep and 
                   IsZeroSemigroupElt);
end);

###########################################################################

InstallOtherMethod( IsMultiplicativeZero, [IsZeroSemigroupElt], 
x-> x![1]=0);

###########################################################################

InstallMethod(\*,        IsIdenticalObj,
        [IsZeroSemigroupElt, IsZeroSemigroupElt],
        function(z1,z2)
return  MultiplicativeZero(z1);

end);

###########################################################################

InstallMethod(\=,         IsIdenticalObj,
        [IsZeroSemigroupElt, IsZeroSemigroupElt],
        function(z1,z2)
return  z1![1]=z2![1];

end);

###########################################################################

InstallMethod(\<,         IsIdenticalObj,
        [IsZeroSemigroupElt, IsZeroSemigroupElt],
        function(z1,z2)
return  z1![1]<z2![1];

end);

###########################################################################

InstallMethod(MultiplicativeZeroOp, [IsZeroSemigroupElt],
       function(z)
   
    return ZeroSemigroupElt(0);
end);

###########################################################################

InstallMethod(MultiplicativeZero, [IsZeroSemigroupElt],
       function(z)
   
    return ZeroSemigroupElt(0);
end);

###########################################################################

InstallMethod(ViewObj, [IsZeroSemigroupElt],
        function(z)
    if z![1] = 0 then
        Print("0");
        return;
    else Print("z", z![1]);
    fi;
end);

###########################################################################

InstallMethod(PrintObj, [IsZeroSemigroupElt],
        function(z)
    if z![1] = 0 then
        Print("0");
        return;
    else Print("z", z![1]);
    fi;
end);

###########################################################################
# for compatibility with Smallsemi, waiting for AD response re: proposed 
# solutions. The test files have been modified when the following is 
# reinstated/resolved.

#if not IsBound(GAPInfo.PackagesInfo.smallsemi) then 

#InstallMethod(ViewObj, [IsZeroSemigroup], 
#function(s)
#Print("<zero semigroup with ", Size(s), " elements>");
#return;
#end);

#fi;

###########################################################################

InstallMethod(ZeroGroup, [IsGroup and HasGeneratorsOfGroup], 
function( s )
local m, gens;

gens:=List(GeneratorsOfGroup(s), ZeroGroupElt);
Add(gens, MultiplicativeZeroOp(gens[1]));
m:=Monoid(gens);
SetUnderlyingGroupOfZG(m, s);
SetIsZeroGroup(m, true);
SetMultiplicativeZero(m, gens[Length(gens)]);
SetIsOne(MultiplicativeZero(m), false);
SetIsRegularSemigroup(m, true);
SetIsFinite(m, true);

#JDM new
SetIsZero(MultiplicativeZero(m), true);
#

if IsAbelian(UnderlyingGroupOfZG(m)) then 
	SetIsCommutative(m, true);
else
	SetIsCommutative(m, false);
fi;

return m;

end);

###########################################################################
##
#M  ZeroGroupFamily( <fam> ) 
#M  ZeroGroupDefaultType( <fam>)
##

InstallMethod(ZeroGroupFamily, [IsFamily], 
        function(fam)
    local   afam;
    afam := NewFamily(Concatenation("ZeroGroupFamily(",fam!.NAME,")"),
                    IsZeroGroupElt);
    return afam;
end);

###########################################################################

InstallMethod(ZeroGroupDefaultType, [IsFamily],
        function(fam) 
    return NewType(fam, IsZeroGroupEltRep and 
                   IsZeroGroupElt);
end);

###########################################################################

InstallMethod( ZeroGroupElt, [IsMultiplicativeElement and IsAssociativeElement and IsMultiplicativeElementWithOne and IsMultiplicativeElementWithInverse],

function(se)
local fam, l;
fam := FamilyObj(se);
l := [ se ];
Objectify(ZeroGroupDefaultType(ZeroGroupFamily(fam)), l);

if IsOne(se) then 
	SetIsOne(l, true);
else 
	SetIsOne(l, false);
fi;

return l;
end);

###########################################################################

InstallMethod(PrintObj, [IsZeroGroup],
function(zg)

Print("<zero group with ", Length(GeneratorsOfMonoid(zg)), " generators>");

return;

end);

###########################################################################

InstallMethod(ViewObj, [IsZeroGroup],
function(zg)

Print("<zero group with ", Length(GeneratorsOfMonoid(zg)), " generators>");

return;

end);

###########################################################################

InstallOtherMethod(Center, "for a zero group", [IsZeroGroup], 
function(zg)
local cent;

cent:=Center(UnderlyingGroupOfZG(zg));

return Concatenation([MultiplicativeZero(zg)], List(cent, ZeroGroupElt));
end); 

###########################################################################

InstallMethod(AsSSortedList, "for a zero group", [IsZeroGroup], 
function(zg)

return Concatenation([MultiplicativeZero(zg)],List(AsSSortedList(UnderlyingGroupOfZG(zg)), ZeroGroupElt));
end); 

###########################################################################

InstallMethod(Enumerator, "for a zero group", [IsZeroGroup], 
function(zg)

return EnumeratorByFunctions(zg, rec(
ElementNumber:=function(enum, pos) 

if pos=1 then 
	return MultiplicativeZero(zg);
else
	return ZeroGroupElt(Enumerator(UnderlyingGroupOfZG(zg))[pos-1]);
fi;
end,
NumberElement:=function(enum, elm)
if IsMultiplicativeZero(zg, elm) then 
	return 1;
else
	return Position(Enumerator(UnderlyingGroupOfZG(zg)), UnderlyingGroupEltOfZGElt(elm))+1;
fi;
end));
end);

###########################################################################

InstallMethod(UnderlyingGroupEltOfZGElt, [IsZeroGroupElt], x->x![1]); 

###########################################################################
#JDM this should maybe go to properties.gi

InstallOtherMethod(SmallGeneratingSet, [IsZeroGroup], 
function(zg)
local sm;

sm:=List(SmallGeneratingSet(UnderlyingGroupOfZG(zg)), ZeroGroupElt);
Add(sm, MultiplicativeZero(zg));

return sm;

end);

###########################################################################

InstallMethod(\*, IsIdenticalObj, [IsZeroGroupElt, IsZeroGroupElt],
#InstallMethod(\*, true, [IsZeroGroupElt, IsZeroGroupElt],
function(me1,me2)
if me1![1] = fail  then
	return me1;
elif me2![1] = fail then
	return me2;
else
	return ZeroGroupElt(me1![1] * me2![1]);
fi;
end);

###########################################################################
#The zero comes first!

InstallMethod(\<, IsIdenticalObj, [IsZeroGroupElt, IsZeroGroupElt],
function(me1,me2)
if me1![1] = fail then
	return me2![1] <> fail;
elif me2![1] = fail then
	return false;
else
	return me1![1] < me2![1];
fi;
end);

###########################################################################

InstallMethod(\=, IsIdenticalObj, [IsZeroGroupElt, IsZeroGroupElt],
function(me1,me2) 
if me1![1] = fail then
	return me2![1] = fail;
elif me2![1] = fail then
	return false;
else
	return me1![1] = me2![1];
fi;
end);

###########################################################################

InstallMethod(One, [IsZeroGroupElt],
function(me)
return ZeroGroupElt(One(me![1]));
end);

###########################################################################

InstallMethod(MultiplicativeZeroOp, [IsZeroGroupElt],
function(me)
local l;
l := [ fail];
Objectify(ZeroGroupDefaultType(FamilyObj(me)),l);
return l;
end);

###########################################################################

InstallOtherMethod(IsMultiplicativeZero, [IsZeroGroupElt], 
function(x)
return x![1]=fail;
end);

###########################################################################

InstallMethod(InverseOp, [IsZeroGroupElt], 
function(x)

if IsMultiplicativeZero(x) then
	return fail;
fi;

return ZeroGroupElt(x![1]^-1);

end);

###########################################################################

InstallMethod(Size, [IsZeroGroup], 
function(ZG)

return Size(UnderlyingGroupOfZG(ZG))+1;
end);

###########################################################################

InstallMethod(PrintObj, [IsZeroGroupElt],
function(me)
if me![1] = fail then
	Print("<adjoined zero>");
	return;
fi;

Print("ZeroGroup(");
Print(me![1]);
Print(")");
end);

###########################################################################

InstallMethod(ViewObj, [IsZeroGroupElt],
function(me)
if me![1] = fail then
	Print("0");
	return;
fi;
ViewObj(me![1]);
end);

###########################################################################
#JDM this should be renamed RandomTransformationSemigroup

InstallGlobalFunction(RandomTransformationSemigroup,
function(m,n)

return Semigroup(Set(List([1..m], x-> RandomTransformation(n))));

end);

###########################################################################
#JDM this should be renamed RandomTransformationMonoid

InstallGlobalFunction(RandomTransformationMonoid,
function(m,n)

return Monoid(Set(List([1..m], x-> RandomTransformation(n))));

end);

###########################################################################
##  returns a smallish generating set for the Rees matrix semigroup <rms>.

#JDM shouldn't this be a method for SmallGeneratingSet?

InstallMethod(GeneratorsOfSemigroup, "for a RMS", true, [IsReesMatrixSemigroup], 0,
function(rms)
local g, I, Lambda, p, gens;

g:=UnderlyingSemigroupOfReesMatrixSemigroup(rms);

if not IsGroup(g) then 
  Error("only Rees matrix semigroups over groups supported");
fi;

I:=RowsOfReesMatrixSemigroup(rms);
I:=[Minimum(2,I)..I];
Lambda:=ColumnsOfReesMatrixSemigroup(rms);
Lambda:=[Minimum(2,Lambda)..Lambda];

p:=SandwichMatrixOfReesMatrixSemigroup(rms)[1][1];

gens:=SetX(SmallGeneratingSet(g), x-> ReesMatrixSemigroupElement(rms,  1, x*p^-1, 1));

if not (Lambda=[1] and I=[1] and p=()) then 
	gens:=Union(gens, List(I, x-> ReesMatrixSemigroupElement(rms,  x, p^-1, 1)));
	gens:=Union(gens, List(Lambda, x-> ReesMatrixSemigroupElement(rms,  1, p^-1, x)));
fi;

return gens;
end);

###########################################################################

#JDM shouldn't this be a method for SmallGeneratingSet?

InstallMethod(GeneratorsOfSemigroup, "for a RZMS", true, [IsReesZeroMatrixSemigroup], 0,
function(rms)
local zg, g, I, L, p, gens, zero, ZGgens, i, j;

zg:=UnderlyingSemigroupOfReesZeroMatrixSemigroup(rms);

if not IsZeroGroup(zg) then 
  Error("only Rees matrix semigroups over zero groups are supported");
fi;

i:=0;
repeat 
	i:=i+1;
	j:=PositionProperty(SandwichMatrixOfReesZeroMatrixSemigroup(rms)[i], x-> not IsMultiplicativeZero(x));
until j<>fail;

I:=RowsOfReesZeroMatrixSemigroup(rms);
I:=Difference([1..I],[j]);
L:=ColumnsOfReesZeroMatrixSemigroup(rms);
L:=Difference([1..L],[i]);

p:=SandwichMatrixOfReesZeroMatrixSemigroup(rms)[i][j];

ZGgens:=Filtered(SmallGeneratingSet(zg), x-> not IsMultiplicativeZero(x));

gens:=SetX(ZGgens, x-> ReesZeroMatrixSemigroupElement(rms,  j, x*p^-1, 1));

if not (L=[1] and I=[1] and p=()) then 
	gens:=Union(gens, List(I, x-> ReesZeroMatrixSemigroupElement(rms,  x, p^-1, 1)));
	gens:=Union(gens, List(L, x-> ReesZeroMatrixSemigroupElement(rms,  1, p^-1, x)));
fi;

return gens;
end);

###########################################################################

InstallGlobalFunction(RandomReesMatrixSemigroup, 
function(rows, cols, n)
local G, mat;

if IsPosInt(n) then 
	G:=SmallGroup(n, Random([1..NrSmallGroups(n)]));
	G:=Range(IsomorphismPermGroup(G));
elif IsPermGroup(n) then 
	G:=n;
else
	Error("3rd argument must be a pos. int. or a perm. group");
fi;

#regular matrix

mat:=[List([1..cols], x-> One(G))];
for n in [2..rows] do 
	Add(mat, List([1..cols], function(x) 
	if x=1 then 
		return One(G);
	else 
		return Random(G);
	fi; end));
od;

return ReesMatrixSemigroup(G, mat);

end);

###########################################################################

InstallGlobalFunction(RandomReesZeroMatrixSemigroup, 
function(rows, cols, maxdeg)
local n, gens, G, mat;

repeat
	n:=AbsoluteValue(Random(Integers));
until not n=0;

gens:=List([1..n], x-> Random(SymmetricGroup(maxdeg)));

G:=ZeroGroup(Group(gens));
mat:=[];
for n in [1..rows] do 
	Add(mat, List([1..cols], function(x)
	local y;
	y:=Random([2..3*Size(G)]);
	if y<=Size(G) then 
		return Enumerator(G)[y];
	else
		return MultiplicativeZero(G);
	fi; end));
od;

return ReesZeroMatrixSemigroup(G, mat);

end);

###########################################################################
## no-check version of the library function of the same name.

InstallGlobalFunction(ReesMatrixSemigroupElementNC,
function(R, i, a, lambda)
	local S, elt;

	elt := Objectify(FamilyObj(R)!.wholeSemigroup!.eType, rec());
	SetUnderlyingElementOfReesMatrixSemigroupElement(elt, a);
	SetColumnIndexOfReesMatrixSemigroupElement(elt, lambda);
	SetRowIndexOfReesMatrixSemigroupElement(elt, i);
	return elt;
end);

###########################################################################
## no-check version of the library function of the same name.

InstallGlobalFunction(ReesZeroMatrixSemigroupElementNC,
function(R, i, a, lambda)
	local S, elt;

if a=MultiplicativeZero(UnderlyingSemigroupOfReesZeroMatrixSemigroup(R)) then
	return MultiplicativeZero(R);		
else
	elt := Objectify(FamilyObj(R)!.wholeSemigroup!.eType, rec());
	SetReesZeroMatrixSemigroupElementIsZero(elt, false);
	SetUnderlyingElementOfReesZeroMatrixSemigroupElement(elt, a);
	SetColumnIndexOfReesZeroMatrixSemigroupElement(elt, lambda);
	SetRowIndexOfReesZeroMatrixSemigroupElement(elt, i);
	return elt;
fi;
end);

###########################################################################

InstallMethod(KiselmanSemigroup, "for a pos int", true, [IsPosInt], 0, 
function(n)
local F, a, rels, i, j, S;
F:=FreeMonoid(n);
a:=GeneratorsOfMonoid(F);

rels:=List([1..n], i-> [a[i]^2, a[i]]);

for i in [1..n-1] do 
	for j in [i+1..n] do 
		Add(rels, [a[i]*a[j]*a[i], a[j]*a[i]*a[j]]);
		Add(rels, [a[j]*a[i]*a[j], a[j]*a[i]]);
	od;
od;

S:=F/rels;
SetIsFinite(S, true);
SetAutomorphismGroup(S, Group(IdentityMapping(S)));
SetIdempotents(S, List(Combinations([1..n]), x-> Product(List(Reversed(x), y-> a[y]))));
#JDM SetIsomorphismMatrixSemigroup
#JDM include sizes for those values where it is known...

return S;
end);

###############################################################################
# an m-generated semilattice on n points
#JDM new for 3.2!

InstallGlobalFunction(RandomSemilatticeAsSemigroup,  
function(m,n)
local out, img, i;

if not IsPosInt(m) or not IsPosInt(n) then 
  Info(InfoWarning, "Usage: pos. int. and pos. int.");
	return fail;
fi;

out:=List([1..m], x-> ListWithIdenticalEntries(n,n));

for i in [1..m] do 
  img:=AsSet(List([1..Random([1..n])], x-> Random([1..n])));
  out[i]{img}:=img;
  out[i]:=TransformationNC(out[i]);
od;

return Semigroup(out);
end);