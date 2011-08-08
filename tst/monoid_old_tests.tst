

# Group ring

gap> g:=CyclicGroup(3);;
gap> r:=GF(2);;
gap> gr:=GroupRing(r, g);;
gap> iso:=IsomorphismTransformationSemigroup(gr);;
gap> s:=Range(iso);;
gap> Size(s);
8
gap> SmallGeneratingSet(s);;
gap> s:=Semigroup(IrredundantGeneratingSubset(last));;
gap> NrGreensDClasses(s);
4
gap> List(GreensDClasses(s), Size);
[ 3, 1, 3, 1 ]
gap> PartialOrderOfDClasses(s);
[ [ 1, 2, 3, 4 ], [ 2 ], [ 2, 3 ], [ 2, 4 ] ]
gap> IsRegularSemigroup(s);
true
gap> ForAll(s, x-> x in s);
true
gap> MultiplicativeNeutralElement(s);
Transformation( [ 1 .. 8 ] )
gap> List(s, x-> InversesOfTransformation(s, x)); 
[ [ Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8 ] ) ], 
  [ Transformation( [ 1, 6, 7, 4, 3, 8, 5, 2 ] ) ], 
  [ Transformation( [ 1, 8, 5, 4, 7, 2, 3, 6 ] ) ], 
  [ Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] ) ], 
  [ Transformation( [ 1, 5, 7, 1, 3, 3, 5, 7 ] ) ], 
  [ Transformation( [ 1, 7, 3, 1, 5, 5, 7, 3 ] ) ], 
  [ Transformation( [ 1, 3, 5, 1, 7, 7, 3, 5 ] ) ], 
  [ Transformation( [ 1, 4, 1, 4, 1, 4, 1, 4 ] ) ] ]
gap> IsInverseSemigroup(s);
true
gap> IsMonoidAsSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> i:=MinimalIdeal(s);
<semigroup with 1 generator>
gap> Size(i);
1
gap> MultiplicativeZero(s);
Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1 ] )
gap> MultiplicativeZero(s) in i;
true
gap> h:=List(GreensDClasses(s), GroupHClass);;
gap> List(h, x-> StructureDescription(x));
[ "C3", "1", "C3", "1" ]
gap> IsCliffordSemigroup(s);
true

gap> a:=IdempotentNC([ 1, 2, 1, 1, 2, 3, 4, 5 ], [3,5,6,7,8])*(3,5);;
gap> b:=IdempotentNC([ 1, 2, 1, 1, 2, 3, 4, 5 ], [3,5,6,7,8])*(3,6,7,8);;
gap> s:=Semigroup(a,b);;
gap> IsGroupAsSemigroup(s);
true

gap> gens:=[Transformation([3,5,3,3,5,6]), Transformation([6,2,4,2,2,6])];;
gap> S:=Semigroup(gens);;
gap> GroupHClass(GreensDClassOfElement(S, Elements(S)[1]));
{Transformation( [ 5, 5, 5, 5, 5, 6 ] )}
gap> IsomorphismPermGroup(last);
MappingByFunction( {Transformation( [ 5, 5, 5, 5, 5, 6 
 ] )}, Group(()), <Operation "AsPermutation"> )

gap> gens:=[ Transformation( [ 4, 4, 8, 8, 8, 8, 4, 8 ] ),
>   Transformation( [ 8, 2, 8, 2, 5, 5, 8, 8 ] ),
>   Transformation( [ 8, 8, 3, 7, 8, 3, 7, 8 ] ),
>   Transformation( [ 8, 6, 6, 8, 6, 8, 8, 8 ] ) ];;
gap> S:=Semigroup(gens);;
gap> Size(S);
30
gap> NrGreensDClasses(S);
6
gap> List(GreensDClasses(S), Size);
[ 9, 9, 1, 1, 9, 1 ]
gap> IsRegularSemigroup(S);
false
gap> NrGreensRClasses(S);
12
gap> NrGreensLClasses(S);
12
gap> IsBlockGroup(S);
false
gap> NrIdempotents(S);
15
gap> List(GreensDClasses(S), IsRegularDClass);
[ true, false, true, true, true, true ]
gap> d:=GreensDClasses(S)[2];
{Transformation( [ 6, 6, 8, 8, 8, 8, 6, 8 ] )}
gap> GreensRClasses(d);
[ {Transformation( [ 6, 6, 8, 8, 8, 8, 6, 8 ] )}, 
  {Transformation( [ 8, 6, 8, 6, 8, 8, 8, 8 ] )}, 
  {Transformation( [ 8, 8, 8, 6, 8, 8, 6, 8 ] )} ]
gap> GreensLClasses(d);
[ {Transformation( [ 6, 6, 8, 8, 8, 8, 6, 8 ] )}, 
  {Transformation( [ 5, 5, 8, 8, 8, 8, 5, 8 ] )}, 
  {Transformation( [ 3, 3, 8, 8, 8, 8, 3, 8 ] )} ]
gap> SchutzenbergerGroup(d);
Group(())
gap> h:=List(GreensDClasses(S), GroupHClass);
[ {Transformation( [ 2, 2, 8, 8, 8, 8, 2, 8 ] )}, fail, 
  {Transformation( [ 8, 2, 8, 2, 5, 5, 8, 8 ] )}, 
  {Transformation( [ 8, 8, 3, 7, 8, 3, 7, 8 ] )}, 
  {Transformation( [ 8, 5, 5, 8, 5, 8, 8, 8 ] )}, 
  {Transformation( [ 8, 8, 8, 8, 8, 8, 8, 8 ] )} ]
gap> MultiplicativeNeutralElement(S); 
fail
gap> IsMonoidAsSemigroup(S);         
false
gap> GroupOfUnits(S);
fail
gap> MultiplicativeZero(S);
Transformation( [ 8, 8, 8, 8, 8, 8, 8, 8 ] )
gap> h:=Filtered(h, x-> not x=fail);                             
[ {Transformation( [ 2, 2, 8, 8, 8, 8, 2, 8 ] )}, 
  {Transformation( [ 8, 2, 8, 2, 5, 5, 8, 8 ] )}, 
  {Transformation( [ 8, 8, 3, 7, 8, 3, 7, 8 ] )}, 
  {Transformation( [ 8, 5, 5, 8, 5, 8, 8, 8 ] )}, 
  {Transformation( [ 8, 8, 8, 8, 8, 8, 8, 8 ] )} ]
gap> List(h, StructureDescription);
[ "1", "1", "1", "1", "1" ]
gap> IsGreensHTrivial(S);
true
gap> IsGreensLTrivial(S);
false
gap> IsGreensRTrivial(S);
false
gap> NrIdempotents(S);
15
gap> IsIdempotentGenerated(S);
true
gap> IsSemiband(S);
true
gap> IsCommutative(S);
false
gap> IsBand(S);
false



