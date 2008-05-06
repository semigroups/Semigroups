##
## semigroups.gi
## Version 3.1
## Fri May  2 17:42:56 BST 2008
##

# the functions in this file define different types of semigroups and 
# transformation semigroups

# JDM ZeroGroupElt should be modified so that it takes a zero group as an 
# additional argument and returns an element in that zero group. As it is it 
# functions but isn't great.

###########################################################################
##
##	<#GAPDoc Label="SingularSemigroup">
##	<ManSection>
##	<Oper Name="SingularSemigroup" Arg="n"/>
##	<Description>
##	creates the semigroup of singular transformations of degree <M>n</M>. That 
##	is, the semigroup of all transformations of the <M>n</M>-element set <M>
##	\{1,2,...,n\}</M> that are non-invertible.  <P/>
##
##	This semigroup is known to be regular, idempotent generated (satisfies 
##	<Ref Prop="IsSemiBand"/>), and has size <M>n^n-n!</M>.
##	<Example>
##  gap&gt; S:=SingularSemigroup(6);
##  &lt;semigroup with 30 generators&gt;
##  gap&gt; Size(S);
##  45936
##  gap&gt; IsRegularSemigroup(S);
##  true
##  gap&gt; IsSemiBand(S);
##  true
##	</Example> <!-- semigroups.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

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
##
##	<#GAPDoc Label="OrderPreservingSemigroup">
##	<ManSection>
##	<Oper Name="OrderPreservingSemigroup" Arg="n"/>
##	<Description>
##	returns the semigroup of order preserving transformations of the <M>n</M>-
##	element set <M>\{1,2,...,n\}</M>. That is, the mappings <M>f</M> such that 
##	<M>i\leq j</M> implies <M>f(i)\leq f(j)</M> for all <M>i,j</M> in 
##	<M>\{1,2,...,n\}</M>.  <P/>
##
##	This semigroup is known to be regular, idempotent generated (satisfies 
##	<Ref Prop="IsSemiBand"/>), and has size <C>Binomial(2*n-1, n-1)</C>.
##
##	The generators used here are those given in <Cite Key="gomes1"/> and the 
##	presentation obtained using <Ref Attr="IsomorphismFpMonoid"/> is that given 
##	in <Cite Key="arthur1"/>.
##
##	<Example>
##  gap&gt; S:=OrderPreservingSemigroup(5);
##  &lt;monoid with 8 generators&gt;
##  gap&gt; IsSemiBand(S);
##  true
##  gap&gt; IsRegularSemigroup(S);
##  true
##  gap&gt; Size(S)=Binomial(2*5-1, 5-1);
##  true
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(OrderPreservingSemigroup, [IsPosInt],
function(n)
local add, gens, subtract, u, v, M, free, F, rels, i, j, fpM, iso;

Info(InfoMonoid, 2, "the command <O> returns the same value");

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

free:=Concatenation(List([1..n-1], x-> Concatenation("u", String(x))), 
		List([1..n-1], x-> Concatenation("v", String(x))));
F:=FreeMonoid(free);
u:=GeneratorsOfMonoid(F){[1..n-1]};
v:=GeneratorsOfMonoid(F){[n..2*n-2]};

rels:=[[u[1]*u[2]*u[1], u[1]*u[2]], [v[1]*v[2]*v[1], v[1]*v[2]]];

for i in [1..n-1] do 
	if not i=1 then 
		Add(rels, [v[n-i]*u[i], u[i]*v[n-i+1]]);
		Add(rels, [u[n-i]*v[i], v[i]*u[n-i+1]]);
	fi;
	Add(rels, [v[n-i]*u[i], u[i]]);
	Add(rels, [u[n-i]*v[i], v[i]]);
	for j in Difference([1..n-1], [n-i, n-i+1]) do
		Add(rels, [u[i]*v[j], v[j]*u[i]]);
	od;
od;

fpM:=F/rels;

iso:=SemigroupHomomorphismByImagesOfGensNC(fpM, M, gens);

SetIsomorphismFpMonoid(M, InverseGeneralMapping(iso));
SetIsomorphismTransformationSemigroup(fpM, iso); 

SetSize(M, Binomial(2*n-1, n-1));
SetSize(fpM, Binomial(2*n-1, n-1));
SetSmallGeneratingSet(M, Concatenation(u, [Transformation(Concatenation([1], [1..n-1]))]));

return M;
end);

###########################################################################
##
##	<#GAPDoc Label="ZeroSemigroup">
##	<ManSection>
##	<Oper Name="ZeroSemigroup" Arg="n"/>
##	<Description>
##	returns the <E>zero semigroup</E> <M>S</M> of order <M>n</M>. That is, the 
##	unique semigroup up to isomorphism of order <M>n</M> such that there exists 
##	an element <M>0</M> in <M>S</M> such that <M>xy=0</M> for all <M>x,y</M> in 
##	<M>S</M>.<P/>
##
##	A zero semigroup is generated by its nonzero elements, has trivial Green's 
##	relations, and is not regular. 
##
##	<Example>
##  gap&gt; S:=ZeroSemigroup(10);
##  &lt;zero semigroup with 10 elements&gt;
##  gap&gt; Size(S);
##  10
##  gap&gt; GeneratorsOfSemigroup(S);
##  [ z1, z2, z3, z4, z5, z6, z7, z8, z9 ]
##  gap&gt; Idempotents(S);
##  [ 0 ]
##  gap&gt; IsZeroSemigroup(S);
##  true
##  gap&gt; GreensRClasses(S);
##  [ {0}, {z1}, {z2}, {z3}, {z4}, {z5}, {z6}, {z7}, {z8}, {z9} ]
##	</Example> <!-- semigroups.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

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
##
##	<#GAPDoc Label="ZeroSemigroupElt">
##	<ManSection>
##	<Oper Name="ZeroSemigroupElt" Arg="n"/>
##	<Description>
##	returns the zero semigroup element <C>zn</C> where <C>n</C> is a positive 
##	integer and z0 is the multiplicative zero.<P/>
##
##	The zero semigroup element <C>zn</C> belongs to every zero semigroup with 
##	degree <M>m</M> at least <M>n</M>.
##
##	<Example>
##  gap&gt; ZeroSemigroupElt(0);
##  0
##  gap&gt; ZeroSemigroupElt(4);
##  z4
##	</Example> <!-- semigroups.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

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

################

InstallMethod(ZeroSemigroupDefaultType, [IsFamily],
        function(fam) 
    return NewType(fam, IsZeroSemigroupEltRep and 
                   IsZeroSemigroupElt);
end);

################

InstallOtherMethod( IsMultiplicativeZero, [IsZeroSemigroupElt], 
x-> x![1]=0);

################

InstallMethod(\*,        IsIdenticalObj,
        [IsZeroSemigroupElt, IsZeroSemigroupElt],
        function(z1,z2)
return  MultiplicativeZero(z1);

end);

################

InstallMethod(\=,         IsIdenticalObj,
        [IsZeroSemigroupElt, IsZeroSemigroupElt],
        function(z1,z2)
return  z1![1]=z2![1];

end);

################

InstallMethod(\<,         IsIdenticalObj,
        [IsZeroSemigroupElt, IsZeroSemigroupElt],
        function(z1,z2)
return  z1![1]<z2![1];

end);

################

InstallMethod(MultiplicativeZeroOp, [IsZeroSemigroupElt],
       function(z)
   
    return ZeroSemigroupElt(0);
end);

################

InstallMethod(MultiplicativeZero, [IsZeroSemigroupElt],
       function(z)
   
    return ZeroSemigroupElt(0);
end);

################

InstallMethod(ViewObj, [IsZeroSemigroupElt],
        function(z)
    if z![1] = 0 then
        Print("0");
        return;
    else Print("z", z![1]);
    fi;
end);

################

InstallMethod(PrintObj, [IsZeroSemigroupElt],
        function(z)
    if z![1] = 0 then
        Print("0");
        return;
    else Print("z", z![1]);
    fi;
end);

################

InstallMethod(ViewObj, [IsZeroSemigroup], 
function(S)
Print("<zero semigroup with ", Size(S), " elements>");
return;
end);

###########################################################################
##
##	<#GAPDoc Label="ZeroGroup">
##	<ManSection>
##	<Oper Name="ZeroGroup" Arg="G"/>
##	<Description>
##	returns the monoid obtained by adjoining a zero element to <C>G</C>. That 
##	is, the monoid <M>S</M> obtained by adjoining a zero element <M>0</M> to 
##	<M>G</M> with <M>g0=0g=0</M> for all <M>g</M> in <M>S</M>.
##	<Example>
##  gap&gt; S:=ZeroGroup(CyclicGroup(10));
##  &lt;zero group with 3 generators&gt;
##  gap&gt; IsRegularSemigroup(S);
##  true
##  gap&gt; Elements(S);
##  [ 0, &lt;identity&gt; of ..., f1, f2, f1*f2, f2^2, f1*f2^2, f2^3, f1*f2^3, f2^4, 
##    f1*f2^4 ]
##  gap&gt; GreensRClasses(S);
##  [ {&lt;adjoined zero&gt;}, {ZeroGroup(&lt;identity&gt; of ...)} ]
##	</Example> <!-- semigroups.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

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

################

InstallMethod(ZeroGroupDefaultType, [IsFamily],
        function(fam) 
    return NewType(fam, IsZeroGroupEltRep and 
                   IsZeroGroupElt);
end);

###########################################################################
##
##	<#GAPDoc Label="ZeroGroupElt">
##	<ManSection>
##	<Oper Name="ZeroGroupElt" Arg="g"/>
##	<Description>
##	returns the zero group element corresponding to the group element <C>g</C>. 
##	The function <C>ZeroGroupElt</C> is only used to create an object in the 
##	correct category during the creation of a zero group using 
##	<Ref Oper="ZeroGroup"/>.
##	<Example>
##  gap&gt; ZeroGroupElt(Random(DihedralGroup(10)));;
##  gap&gt; IsZeroGroupElt(last);
##  true
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

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

################

InstallMethod(PrintObj, [IsZeroGroup],
function(zg)

Print("<zero group with ", Length(GeneratorsOfMonoid(zg)), " generators>");

return;

end);

################

InstallMethod(ViewObj, [IsZeroGroup],
function(zg)

Print("<zero group with ", Length(GeneratorsOfMonoid(zg)), " generators>");

return;

end);

################

InstallOtherMethod(Center, "for a zero group", [IsZeroGroup], 
function(zg)
local cent;

cent:=Center(UnderlyingGroupOfZG(zg));

return Concatenation([MultiplicativeZero(zg)], List(cent, ZeroGroupElt));
end); 

################

InstallMethod(AsSSortedList, "for a zero group", [IsZeroGroup], 
function(zg)

return Concatenation([MultiplicativeZero(zg)],List(AsSSortedList(UnderlyingGroupOfZG(zg)), ZeroGroupElt));
end); 

################

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
##
##	<#GAPDoc Label="UnderlyingGroupEltOfZGElt">
##	<ManSection>
##	<Attr Name="UnderlyingGroupEltOfZGElt" Arg="g"/>
##	<Description>
##	returns the group element from which the zero group element <C>g</C> was 
##	constructed.
##	<Example>
##  gap&gt; G:=DihedralGroup(10);;
##  gap&gt; S:=ZeroGroup(G);;
##  gap&gt; Elements(S);
##  [ 0, &lt;identity&gt; of ..., f1, f2, f1*f2, f2^2, f1*f2^2, f2^3, f1*f2^3, f2^4, 
##    f1*f2^4 ]
##  gap&gt; x:=last[5];
##  f1*f2
##  gap&gt; UnderlyingGroupEltOfZGElt(x);
##  f1*f2
##	</Example> <!-- semigroups.tst -->
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallMethod(UnderlyingGroupEltOfZGElt, [IsZeroGroupElt], x->x![1]); 

###########################################################################
##
#M  SmallGeneratingSet( <ZG> ) 
##
##  returns a small generating set for the zero group <ZG> by finding a 
##  small generating set for the underlying group.
## 

InstallOtherMethod(SmallGeneratingSet, [IsZeroGroup], 
function(zg)
local sm;

sm:=List(SmallGeneratingSet(UnderlyingGroupOfZG(zg)), ZeroGroupElt);
Add(sm, MultiplicativeZero(zg));

return sm;

end);

################

InstallMethod(\*, IsIdenticalObj, [IsZeroGroupElt, IsZeroGroupElt],
function(me1,me2)
if me1![1] = fail  then
	return me1;
elif me2![1] = fail then
	return me2;
else
	return ZeroGroupElt(me1![1] * me2![1]);
fi;
end);

################

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

################

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

################

InstallMethod(One, [IsZeroGroupElt],
function(me)
return ZeroGroupElt(One(me![1]));
end);

################

InstallMethod(MultiplicativeZeroOp, [IsZeroGroupElt],
function(me)
local l;
l := [ fail];
Objectify(ZeroGroupDefaultType(FamilyObj(me)),l);
return l;
end);

################

InstallOtherMethod(IsMultiplicativeZero, [IsZeroGroupElt], 
function(x)
return x![1]=fail;
end);

################

InstallMethod(InverseOp, [IsZeroGroupElt], 
function(x)

if IsMultiplicativeZero(x) then
	return fail;
fi;

return ZeroGroupElt(x![1]^-1);

end);

################

InstallMethod(Size, [IsZeroGroup], 
function(ZG)

return Size(UnderlyingGroupOfZG(ZG))+1;
end);

################

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

################

InstallMethod(ViewObj, [IsZeroGroupElt],
function(me)
if me![1] = fail then
	Print("0");
	return;
fi;
ViewObj(me![1]);
end);

###########################################################################
##
##	<#GAPDoc Label="RandomSemigroup">
##	<ManSection>
##	<Attr Name="RandomSemigroup" Arg="m, n"/>
##	<Description>
##	returns a random transformation semigroup of degree <C>n</C> with <C>m</C> 
##	generators.
##	<Example>
##  gap&gt; S:=RandomSemigroup(5,5);
##  &lt;semigroup with 5 generators&gt;
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallGlobalFunction(RandomSemigroup,
function(m,n)

return Semigroup(Set(List([1..m], x-> RandomTransformation(n))));

end);

###########################################################################
##
##	<#GAPDoc Label="RandomMonoid">
##	<ManSection>
##	<Attr Name="RandomMonoid" Arg="m, n"/>
##	<Description>
##	returns a random transformation monoid of degree <C>n</C> with <C>m</C> 
##	generators.
##	<Example>
##  gap&gt; S:=RandomMonoid(5,5);
##  &lt;semigroup with 5 generators&gt;
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallGlobalFunction(RandomMonoid,
function(m,n)

return Monoid(Set(List([1..m], x-> RandomTransformation(n))));

end);

###########################################################################
##
#M  GeneratorsOfSemigroup( <rms> )
##  
##  returns a smallish generating set for the Rees matrix semigroup <rms>.
## 

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
##
#M  GeneratorsOfSemigroup( <rzms> )
##
##	returns a smallish generating set for the Rees 0-matrix semigroup
##	<rzms>. This function assumes that the matrix of the semigroup is regular, 
##	so there is a nonzero element in every row and every column of the matrix.
## 


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
##
##	<#GAPDoc Label="RandomReesMatrixSemigroup">
##	<ManSection>
##	<Func Name="RandomReesMatrixSemigroup" Arg="i, j, deg"/>
##	<Description>
##	returns a random Rees matrix semigroup with a <M>i</M> by <M>j</M> sandwich 
##	matrix over a permutation group with maximum degree <M>deg</M>. 
##	<Example>
##  gap&gt; S:=RandomReesMatrixSemigroup(4,5,5);
##  Rees Matrix Semigroup over Group([ (1,5,3,4), (1,3,4,2,5) ])
##  [ [ (), (), (), (), () ], 
##  [ (), (1,3,5)(2,4), (1,3,5)(2,4), (1,5,3), (1,5,3) ], 
##  [ (), (1,3,5), (1,5,3)(2,4), (), (1,5,3) ], 
##  [ (), (), (1,3,5)(2,4), (2,4), (2,4) ] ]
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

InstallGlobalFunction(RandomReesMatrixSemigroup, 
function(rows, cols, maxdeg)
local n, gens, G, mat;

repeat
	n:=AbsoluteValue(Random(Integers));
until not n=0;

gens:=List([1..n], x-> Random(SymmetricGroup(maxdeg)));

G:=Group(gens);

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
##
##	<#GAPDoc Label="RandomReesZeroMatrixSemigroup">
##	<ManSection>
##	<Func Name="RandomReesZeroMatrixSemigroup" Arg="i, j, deg"/>
##	<Description>
##	returns a random Rees <M>0</M>-matrix semigroup with a <M>i</M> by <M>j</M> 
##	sandwich matrix over a permutation group with maximum degree <M>deg</M>. 
##
##	<Example>
##  gap&gt; S:=RandomReesZeroMatrixSemigroup(2,3,2);
##  Rees Zero Matrix Semigroup over &lt;zero group with 2 generators&gt;
##  gap&gt; SandwichMatrixOfReesZeroMatrixSemigroup(S);
##  [ [ 0, (), 0 ], [ 0, 0, 0 ] ]
##	</Example>
##	</Description>
##	</ManSection>
##	<#/GAPDoc>

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
##
#F	ReesMatrixSemigroupElementNC(R, i, a, lambda)
#F	ReesZeroMatrixSemigroupElementNC(R, i, a, lambda)
##
##	a no check version of the library function of the same name.
##

InstallGlobalFunction(ReesMatrixSemigroupElementNC,
function(R, i, a, lambda)
	local S, elt;

	elt := Objectify(FamilyObj(R)!.wholeSemigroup!.eType, rec());
	SetUnderlyingElementOfReesMatrixSemigroupElement(elt, a);
	SetColumnIndexOfReesMatrixSemigroupElement(elt, lambda);
	SetRowIndexOfReesMatrixSemigroupElement(elt, i);
	return elt;
end);

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