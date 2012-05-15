#############################################################################
##
#W  properties.tst
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#ReadTest(Filename(DirectoriesPackageLibrary("citrus","tst"),"properties.tst"));
gap> START_TEST("Citrus package: properties.tst");
gap> LoadPackage("citrus", false);;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

#
gap> file:=Concatenation(CitrusDir(), "/examples/misc.citrus.gz");;
gap> semis:=List([1..22], i-> Semigroup(ReadCitrus(file, i)));;
gap> List([1..15], i-> IsCompletelyRegularSemigroup(semis[i]));   
[ false, true, false, false, false, true, true, true, true, true, false, 
  false, false, false, true ]
gap> List([15..22], i-> IsCompletelyRegularSemigroup(semis[i]));  
[ true, false, true, false, false, false, false, false ]

#
gap> s:=Semigroup(Transformation([3,3,2,6,2,4,4,6]),
> Transformation([3,3,2,6,2,4,4,6]));;
gap> IsSimpleSemigroup(s);
true

#
gap> g1:=Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] );;
gap> g2:=Transformation( [ 2, 3, 4, 5, 6, 8, 7, 1, 2, 2 ] );;
gap> s:=Semigroup(Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation( [ 2, 3, 4, 5, 6, 8, 7, 1, 2, 2 ] ));;
gap> IsSimpleSemigroup(s);
true

#
gap> g1:=Transformation([2,1,1,2,1]);;
gap> g2:=Transformation([3,4,3,4,4]);;
gap> g3:=Transformation([3,4,3,4,3]);;
gap> g4:=Transformation([4,3,3,4,4]);;
gap> s:=Semigroup(Transformation([2,1,1,2,1]), Transformation([3,4,3,4,4]), 
> Transformation([3,4,3,4,3]), Transformation([4,3,3,4,4]));;
gap> IsCompletelySimpleSemigroup(s);
true

#
gap> s:=Semigroup(Transformation([4,4,4,1,1,6,7,8,9,10,11,1]),
> Transformation([6,6,6,7,7,1,4,8,9,10,11,7]),
> Transformation([8,8,8,9,9,10,11,1,4,6,7,9]),
> Transformation([2,2,2,4,4,6,7,8,9,10,11,4]),
> Transformation([1,1,1,5,5,6,7,8,9,10,11,5]),
> Transformation([1,1,4,4,4,6,7,8,9,10,11,1]),
> Transformation([1,1,7,4,4,6,7,8,9,10,11,6]));;
gap> IsCompletelySimpleSemigroup(s);
true

#
gap> s:=Semigroup(Transformation([1,2,2,1,2]),
> Transformation([3,4,3,4,4]),
> Transformation([3,4,3,4,3]),
> Transformation([4,3,3,4,4]));;
gap> IsCompletelySimpleSemigroup(s);
true

#
gap> s:=semis[12];;
gap> d:=GreensDClassOfElement(s, 
> Transformation( [ 12, 2, 1, 3, 6, 6, 12, 2, 3, 3, 11, 3 ] ));;
gap> g:=GroupHClassOfGreensDClass(d);;
gap> s:=Semigroup(AsList(g));;
gap> IsGroupAsSemigroup(s);
true
gap> IsGroupAsSemigroup(Range(IsomorphismTransformationSemigroup(
> PrimitiveGroup(5,2))));
true
gap> IsGroupAsSemigroup(semis[11]);
false

#
gap> List(semis, IsCliffordSemigroup);
[ false, true, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false, false, false, false, false, false, false 
 ]
gap> ForAll(GreensDClasses(semis[2]), x-> Length(GreensHClasses(x))=1 and 
> IsRegularDClass(x));
true
gap> IsCliffordSemigroup(semis[2]);
true
gap> ForAll(GreensDClasses(semis[2]), x-> Length(GreensHClasses(x))=1 and 
> IsRegularDClass(x));
true

#
gap> s:=Semigroup(Transformation([1,2,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4]),
> Transformation([1,2,3,4,5,6,7,4,4,4,4,4,4,4,4,4,4,4,4,4,4]),
> Transformation([1,2,3,4,5,6,7,8,9,10,11,4,4,4,4,4,4,4,4,4,4]),
> Transformation([1,2,3,4,4,4,4,4,4,4,4,12,13,14,15,16,4,4,4,4,4]),
> Transformation([1..21]*1));; 
gap> IsCliffordSemigroup(s);
true
gap> IsCommutativeSemigroup(s);
true
gap> Size(s);
5
gap> ForAll(GreensDClasses(s), x-> Length(GreensHClasses(x))=1 and 
> IsRegularDClass(x));
true

#
gap> List(semis, IsRegularSemigroup);
[ false, true, false, false, false, true, true, true, true, true, false, 
  false, true, true, true, false, true, false, false, false, false, true ]
gap> IsInverseSemigroup(s);
true
gap> IsBand(s);
true
gap> List(semis, IsBand);
[ false, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false, false, false, false, false, false, 
  false, false ]

#
gap> s:=ReesMatrixSemigroup(Group(()), [[(), (), ()], [(), (), ()], 
> [(), (), ()], [(), (), ()]]);;
gap> s:=Range(IsomorphismTransformationSemigroup(s));;
gap> IsRectangularBand(s);
true
gap> IsBand(s); 
true

#
gap> IsSemiband(FullTransformationSemigroup(4));
false
gap> Size(Semigroup(Idempotents(FullTransformationSemigroup(4))));
233
gap> 4^4-Factorial(4)+1;
233
gap> ForAll(semis, x-> not IsIdempotentGenerated(x));
true
gap> List(semis, IsOrthodoxSemigroup);
[ false, true, false, false, false, false, false, true, true, true, false, 
  false, false, false, true, false, true, false, false, false, false, false ]

#
gap> s:=semis[8];;
gap> Size(s);;
gap> t:=Semigroup(Idempotents(s));;
gap> IsBand(t);    
true
gap> IsRectangularBand(t);
false

#
gap> s:=ReesMatrixSemigroup(Group(()), [[(), (), (), (), (), ()]]);;
gap> s:=Range(IsomorphismTransformationSemigroup(s));;
gap> IsLeftZeroSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false

#
gap> s:=ReesMatrixSemigroup(Group(()), [[()], [()], [()], [()], [()], 
> [()]]);;
gap> s:=Range(IsomorphismTransformationSemigroup(s));;
gap> IsRightZeroSemigroup(s);
true
gap> IsLeftZeroSemigroup(s);
false
gap> List(semis, IsLeftZeroSemigroup);    
[ false, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false, false, false, false, false, false, 
  false, false ]
gap> List(semis, IsRightZeroSemigroup);
[ false, false, false, false, false, false, false, false, false, false, 
  false, false, false, false, false, false, false, false, false, false, 
  false, false ]

#
gap> gens:=[ Transformation( [ 2, 2, 4, 4, 6, 6, 8, 8, 10, 10, 12, 12, 2 ] ), 
> Transformation( [ 1, 1, 3, 3, 5, 5, 7, 7, 9, 9, 11, 11, 3 ] ), 
> Transformation( [ 1, 7, 3, 9, 5, 11, 7, 1, 9, 3, 11, 5, 5 ] ), 
> Transformation( [ 7, 7, 9, 9, 11, 11, 1, 1, 3, 3, 5, 5, 7 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsSimpleSemigroup(s);
true
gap> IsCompletelySimpleSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 1, 2, 4, 3, 6, 5, 4 ] ), 
> Transformation( [ 1, 2, 5, 6, 3, 4, 5 ] ), 
> Transformation( [ 2, 1, 2, 2, 2, 2, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsCompletelyRegularSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 2, 4, 5, 3, 7, 8, 6, 9, 1 ] ), 
> Transformation( [ 3, 5, 6, 7, 8, 1, 9, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsGroupAsSemigroup(s);
true
gap> IsCommutativeSemigroup(s);
true

#
gap> gens:=[Transformation([1,2,4,5,6,3,7,8]),
> Transformation([3,3,4,5,6,2,7,8]),
> Transformation([1,2,5,3,6,8,4,4])];;
gap> s:=Semigroup(gens);;
gap> IsCliffordSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 1 ] ), 
> Transformation( [ 2, 2, 2, 5, 5, 5, 8, 8, 8, 2 ] ), 
> Transformation( [ 3, 3, 3, 6, 6, 6, 9, 9, 9, 3 ] ), 
> Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 4 ] ), 
> Transformation( [ 1, 1, 1, 4, 4, 4, 7, 7, 7, 7 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsBand(s);
true
gap> IsRectangularBand(s);
true

#
gap> s:=FullTransformationSemigroup(4);;
gap> d:=GreensDClassOfElement(s, Transformation( [ 1, 2, 3, 1 ] ));;
gap> t:=Semigroup(Elements(d));;
gap> IsSemiband(t);
true

#
gap> gens:=[ Transformation( [ 1, 1, 1, 4, 5, 4 ] ), 
>  Transformation( [ 1, 2, 3, 1, 1, 2 ] ), 
>  Transformation( [ 1, 2, 3, 1, 1, 3 ] ), 
>  Transformation( [ 5, 5, 5, 5, 5, 5 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsOrthodoxSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 2, 1, 4, 3, 5 ] ), 
>  Transformation( [ 3, 2, 3, 1, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsRightZeroSemigroup(s);
false

#
gap> gens:=[Transformation( [ 1, 2, 3, 3, 1 ] ), 
>  Transformation( [ 1, 2, 4, 4, 1 ] )];;
gap> s:=Semigroup(gens);;
gap> IsRightZeroSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 2, 1, 4, 3, 5 ] ), 
>  Transformation( [ 3, 2, 3, 1, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsRightZeroSemigroup(s);
false

#
gap> gens:=[Transformation([1,2,3,3,1]), Transformation([1,2,3,3,3])];;
gap> s:=Semigroup(gens);;
gap> IsLeftZeroSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 4, 7, 6, 3, 1, 5, 3, 6, 5, 9 ] ), 
>  Transformation( [ 5, 3, 5, 1, 9, 3, 8, 7, 4, 3 ] ), 
>  Transformation( [ 5, 10, 10, 1, 7, 6, 6, 8, 7, 7 ] ), 
>  Transformation( [ 7, 4, 3, 3, 2, 2, 3, 2, 9, 3 ] ), 
>  Transformation( [ 8, 1, 3, 4, 9, 6, 3, 7, 1, 6 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsZeroSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 1, 4, 2, 6, 6, 5, 2 ] ), 
> Transformation( [ 1, 6, 3, 6, 2, 1, 6 ] ) ];;
gap> s:=Semigroup(gens);;
gap> MultiplicativeZero(s);
Transformation( [ 1, 1, 1, 1, 1, 1, 1 ] )

#
gap> gens:=[Transformation([1,2,1,3,3]), Transformation([2,2,3,5,5])];;
gap> s:=Monoid(gens);;
gap> IsBlockGroup(s);
true

#
gap> gens:=[ Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ),
>   Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsBlockGroup(s);
true

#
gap> gens:=[ Transformation( [ 4, 6, 5, 2, 1, 3 ] ),
>   Transformation( [ 6, 3, 2, 5, 4, 1 ] ),
>   Transformation( [ 1, 2, 4, 3, 5, 6 ] ),
>   Transformation( [ 3, 5, 6, 1, 2, 3 ] ),
>   Transformation( [ 5, 3, 6, 6, 6, 2 ] ),
>   Transformation( [ 2, 3, 2, 6, 4, 6 ] ),
>   Transformation( [ 2, 1, 2, 2, 2, 4 ] ),
>   Transformation( [ 4, 4, 1, 2, 1, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsBlockGroup(s);
false

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ),
>   Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ),
>   Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsLTrivial(s);
false

#
gap> gens:=[Transformation([1,2,1,3,3]), Transformation([2,2,3,5,5])];;
gap> s:=Monoid(gens);;
gap> IsLTrivial(s);
true
gap> d:=DClass(s, Transformation( [ 2, 2, 1, 1, 1 ] ));;
gap> IsLTrivial(d);
true

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ),
>   Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ),
>   Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> iter:=IteratorOfDClasses(s);
<iterator of D-classes>
gap> repeat d:=NextIterator(iter); until IsDoneIterator(iter) or IsLTrivial(d);
gap> d;
{Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] )}
gap> IsLTrivial(d);
true
gap> IsRTrivial(d);
true
gap> Size(d);
1
gap> IsRTrivial(d);   
true
gap> repeat d:=NextIterator(iter); until IsDoneIterator(iter) or 
> not IsLTrivial(d) and IsRTrivial(d);
gap> d;;
gap> IsLTrivial(d);
false
gap> IsRTrivial(d);
true
gap> NrLClasses(d);
1
gap> NrRClasses(d);
4560
gap> IsRTrivial(s);
false

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);
<semigroup with 3 generators>
gap> IsRTrivial(s);
false

#
gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Monoid(gens);;
gap> IsRTrivial(s);
false
gap> IsHTrivial(s);
false

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsAperiodicSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ),
>   Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ),
>   Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ),
>   Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] ) ];;
gap> s:=Monoid(gens);;
gap> IsCombinatorialSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsAperiodicSemigroup(s);
false

#
gap> gens:=[Transformation( [ 13, 10, 9, 5, 1, 5, 13, 13, 8, 2, 7, 2, 6 ] ),
> Transformation( [ 6, 11, 12, 10, 4, 10, 13, 5, 8, 5, 11, 6, 9 ] )];;
gap> s:=Semigroup(gens);;
gap> IsAperiodicSemigroup(s);
false

#
gap> gens:=[Transformation( [ 12, 10, 8, 5, 1, 5, 12, 12, 8, 2, 6, 2 ] ),
> Transformation( [ 5, 6, 10, 11, 10, 4, 10, 12, 5, 7, 4, 10 ] ),
> Transformation( [ 6, 8, 12, 5, 4, 8, 10, 7, 4, 1, 10, 11 ] )];;
gap> s:=Monoid(gens);;
gap> IsAperiodicSemigroup(s);
false

#
gap> gens:=[Transformation([2,3,4,5,1,8,7,6,2,7]),
> Transformation([5,4,1,2,3,7,6,5,4,1]),
> Transformation([2,1,4,3,2,1,4,4,3,3])];;
gap> s:=Monoid(gens);;
gap> IsAperiodicSemigroup(s);
false

#
gap> gens:=[Transformation([1,2,1,3,3]), Transformation([2,2,3,5,5])];;
gap> s:=Monoid(gens);;
gap> IsAperiodicSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 1, 3, 2, 6, 5, 4, 8, 7, 9, 10 ] ), 
>  Transformation( [ 1, 2, 6, 4, 8, 3, 9, 5, 7, 10 ] ), 
>  Transformation( [ 1, 10, 10, 10, 10, 10, 7, 8, 10, 10 ] ), 
>  Transformation( [ 1, 10, 3, 10, 10, 6, 10, 10, 10, 10 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsInverseSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 1, 4, 5, 16, 2, 11, 13, 7, 12, 8, 15, 6, 14, 10, 9, 3, 17
> ] ), 
> Transformation( [ 1, 17, 17, 17, 17, 6, 7, 8, 9, 10, 11, 17, 17, 17, 17, 16, 17 ] ), 
> Transformation( [ 1, 2, 3, 17, 17, 6, 17, 17, 17, 17, 11, 17, 17, 14, 15, 16, 17 ] ), 
> Transformation( [ 1, 2, 17, 4, 17, 17, 7, 17, 17, 10, 17, 17, 13, 17, 15, 16,
> 17 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsInverseSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 1, 2, 10, 4, 5, 13, 7, 8, 15, 3, 11, 16, 
> 6, 14, 9, 12, 17 ] ), 
> Transformation( [ 1, 8, 10, 4, 5, 6, 14, 2, 15, 3, 11, 12, 13, 7, 9, 16, 17 ] ), 
> Transformation( [ 1, 8, 17, 4, 5, 17, 14, 2, 17, 17, 11, 17, 17, 7, 17, 17, 17 ] ), 
>  Transformation( [ 1, 2, 17, 4, 8, 17, 7, 5, 17, 17, 14, 17, 17, 11, 17, 17, 17 ] ), 
>  Transformation( [ 1, 17, 4, 10, 9, 17, 17, 17, 15, 3, 11, 17, 17, 17, 5, 17, 17 ] ), 
>  Transformation( [ 1, 17, 4, 3, 15, 17, 17, 17, 9, 10, 11, 17, 17, 17, 5, 17, 17 ] ), 
>  Transformation( [ 1, 17, 17, 17, 5, 6, 7, 17, 9, 17, 17, 17, 13, 14, 15, 17, 17 ] ), 
>  Transformation( [ 1, 2, 17, 17, 5, 17, 17, 8, 9, 17, 17, 12, 17, 17, 15, 16, 17 ] ), 
>  Transformation( [ 1, 17, 3, 17, 5, 17, 7, 17, 17, 10, 17, 12, 17, 14, 17, 16, 17 ] ), 
>  Transformation( [ 1, 17, 17, 4, 5, 6, 17, 17, 17, 17, 11, 12, 13, 17, 17, 16, 17 ] ), 
>  Transformation( [ 1, 2, 3, 17, 5, 6, 17, 8, 17, 10, 17, 17, 13, 17, 17, 17,
>  17 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsInverseSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 1, 2, 4, 3, 6, 5 ] ), 
> Transformation( [ 1, 2, 3, 4, 5, 6 ] ), Transformation( [ 6, 4, 3, 2, 5, 3 ] ), 
> Transformation( [ 5, 3, 4, 2, 2, 1 ] ),
>   Transformation( [ 2, 4, 6, 4, 5, 3 ] ), Transformation( [ 4, 2, 4, 3, 6, 5 ] ),
>   Transformation( [ 2, 4, 4, 3, 6, 5 ] ), Transformation( [ 5, 6, 4, 4, 3, 2 ] ),
>   Transformation( [ 2, 2, 3, 4, 5, 6 ] ), Transformation( [ 3, 4, 2, 2, 2, 1 ] ),
>   Transformation( [ 1, 2, 4, 2, 3, 3 ] ), Transformation( [ 1, 2, 3, 4, 3, 2 ] ),
>   Transformation( [ 6, 4, 2, 3, 2, 3 ] ), Transformation( [ 6, 4, 2, 2, 1, 1 ] ),
>   Transformation( [ 6, 4, 2, 3, 4, 4 ] ), Transformation( [ 5, 3, 3, 2, 4, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> RedundantGenerator(s, gens);
Transformation( [ 1, 2, 3, 4, 5, 6 ] )
gap> gens:=Difference(gens, [last]);;
gap> RedundantGenerator(s, gens);
Transformation( [ 3, 4, 2, 2, 2, 1 ] )
gap> gens:=Difference(gens, [last]);;
gap> RedundantGenerator(s, gens);
fail

#
gap> gens:=[ Transformation( [ 5, 1, 4, 6, 2, 3 ] ),
> Transformation( [ 1, 2, 3, 4, 5, 6 ] ),
> Transformation( [ 4, 6, 3, 4, 2, 5 ] ),
> Transformation( [ 5, 4, 6, 3, 1, 3 ] ),
> Transformation( [ 2, 2, 6, 5, 4, 3 ] ),
> Transformation( [ 3, 5, 5, 1, 2, 4 ] ),
> Transformation( [ 6, 5, 1, 3, 3, 4 ] ),
> Transformation( [ 1, 3, 4, 3, 2, 1 ] ) ];;
gap> RedundantGenerator(gens);
Transformation( [ 1, 2, 3, 4, 5, 6 ] )
gap> gens:=Difference(gens, [last]);;
gap> RedundantGenerator(gens);
Transformation( [ 4, 6, 3, 4, 2, 5 ] )
gap> gens:=Difference(gens, [last]);;
gap> RedundantGenerator(gens);
fail

#
gap> gens:=[ Transformation( [ 1, 2, 2 ] ), Transformation( [ 1, 2, 1 ] ), 
>   Transformation( [ 2, 2, 3 ] ), Transformation( [ 3, 2, 3 ] ), 
>   Transformation( [ 1, 3, 3 ] ), Transformation( [ 1, 1, 3 ] ) ]
> ;;
gap> s:=Semigroup(gens);;
gap> IsIdempotentGenerated(s);
true

#
gap> RedundantGenerator(s, gens);
Transformation( [ 1, 2, 2 ] )
gap> gens:=Difference(gens, [last]);                                       
[ Transformation( [ 1, 1, 3 ] ), Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 2, 3 ] ), 
  Transformation( [ 3, 2, 3 ] ) ]
gap> RedundantGenerator(s, gens);
Transformation( [ 1, 1, 3 ] )
gap> gens:=Difference(gens, [last]);
[ Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 3, 3 ] ), 
  Transformation( [ 2, 2, 3 ] ), Transformation( [ 3, 2, 3 ] ) ]
gap> RedundantGenerator(s, gens);
Transformation( [ 3, 2, 3 ] )
gap> gens:=Difference(gens, [last]);
[ Transformation( [ 1, 2, 1 ] ), Transformation( [ 1, 3, 3 ] ), 
  Transformation( [ 2, 2, 3 ] ) ]
gap> RedundantGenerator(s, gens);
fail
gap> Size(Semigroup(gens)); 3^3-6;
21
21

#
gap> gens:=[Transformation( [ 2, 6, 1, 8, 5, 3, 8, 8 ] ),
> Transformation( [ 3, 7, 6, 4, 5, 2, 1, 8 ] )];;
gap> s:=Semigroup(gens);;
gap> i:=MinimalIdeal(s);;
gap> MultiplicativeZero(s);
Transformation( [ 8, 8, 8, 8, 5, 8, 8, 8 ] )
gap> IsLeftZeroSemigroup(i);
true

#
gap> gens:=[Transformation([2,3,4,5,6,7,8,9,1]),
> Transformation([4,2,3,4,5,6,7,8,9])];;
gap> s:=Semigroup(gens);;
gap> i:=MinimalIdeal(s);;
gap> gens:=Generators(i);;
gap> repeat
> f:=RedundantGenerator(gens); if not f=fail then 
> gens:=Difference(gens, [f]);
> fi;
> until f=fail;
gap> i:=Semigroup(gens);;
gap> IsLeftZeroSemigroup(i);
false
gap> IsSimpleSemigroup(i);
true
gap> IsRightZeroSemigroup(i);
false
gap> MultiplicativeZero(i);
fail
gap> One(i);
fail

#
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ), Transformation( [ 2, 4, 1, 2 ] ),
>   Transformation( [ 3, 1, 1, 3 ] ), Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> s:=Semigroup(GeneratorsOfSemigroup(s));;
gap> IsMonoidAsSemigroup(s);
true
gap> IsomorphismTransformationMonoid(s);
MappingByFunction( <semigroup with 5 generators>, <monoid with 
4 generators>, function( x ) ... end, function( x ) ... end )
gap> i:=MinimalIdeal(s);;
gap> Size(i);
4
gap> IsLeftZeroSemigroup(i);
false
gap> IsRightZeroSemigroup(i);
true
gap> IsSynchronizingSemigroup(i);
true

#
gap> gens:=[Transformation([2,1,4,5,3,7,8,9,10,6]),
> Transformation([1,2,4,3,5,6,7,8,9,10]),
> Transformation([1,2,3,4,5,6,10,9,8,7]),
> Transformation([9,1,4,3,6,9,3,4,3,9])];;
gap> s:=Monoid(gens);;
gap> g:=GroupOfUnits(s);;
gap> t:=Range(IsomorphismTransformationSemigroup(g));;
gap> IsMonoidAsSemigroup(t);
true
gap> t:=Range(IsomorphismTransformationMonoid(t));;
gap> IsomorphismPermGroup(t);
MappingByFunction( <monoid with 3 generators>, <permutation group with 
3 generators>, <Operation "AsPermutation">, function( x ) ... end )

#
gap> gens:=[Transformation([4,4,4,1,1,6,7,8,9,10,11,1]),
> Transformation([6,6,6,7,7,1,4,8,9,10,11,7]),
> Transformation([8,8,8,9,9,10,11,1,4,6,7,9]),
> Transformation([2,2,2,4,4,6,7,8,9,10,11,4]),
> Transformation([1,1,1,5,5,6,7,8,9,10,11,5]),
> Transformation([1,1,4,4,4,6,7,8,9,10,11,1]),
> Transformation([1,1,7,4,4,6,7,8,9,10,11,6])];;
gap> s:=Semigroup(gens);;
gap> IsOrthodoxSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ),
>   Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ),
>   Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsOrthodoxSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsOrthodoxSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ),
>   Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ),
>   Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ),
>   Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] ) ];;
gap> s:=Monoid(gens);;
gap> IsOrthodoxSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsOrthodoxSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>  Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> IsOrthodoxSemigroup(s);
false

#
gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Monoid(gens);;
gap> IsOrthodoxSemigroup(s);
true

#
gap> gens:=[Transformation([2,3,4,5,1,8,7,6,2,7]),
> Transformation( [ 3, 8, 7, 4, 1, 4, 3, 3, 7, 2 ] )];;
gap> s:=Monoid(gens);;
gap> i:=MinimalIdeal(s);;
gap> IsRectangularBand(i);
true

#
gap> gens:=[ Transformation( [ 1, 4, 6, 2, 5, 3, 7, 8 ] ),
>   Transformation( [ 6, 3, 2, 7, 5, 1, 8, 8 ] ) ];
[ Transformation( [ 1, 4, 6, 2, 5, 3, 7, 8 ] ), 
  Transformation( [ 6, 3, 2, 7, 5, 1, 8, 8 ] ) ]
gap> s:=Semigroup(gens);;
gap> i:=MinimalIdeal(s);;
gap> IsRectangularBand(i);
true
gap> MultiplicativeZero(i);
Transformation( [ 8, 8, 8, 8, 5, 8, 8, 8 ] )

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ),
>   Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ),
>   Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> i:=MinimalIdeal(s);;
gap> IsRectangularBand(s);
false
gap> IsSimpleSemigroup(s);
false
gap> IsRectangularBand(i);
true
gap> IsRightZeroSemigroup(i);
true

#
gap> rms:=ReesMatrixSemigroup(Group(()), List([1..4], x-> List([1..3], y->
> ())));;
gap> s:=IsomorphismTransformationSemigroup(rms);;
gap> s:=Range(s);;
gap> IsRectangularBand(s);
true
gap> IsRegularSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] ),
>   Transformation( [ 3, 1, 4, 2, 5, 2, 1, 6, 1, 7 ] ),
>   Transformation( [ 3, 8, 1, 9, 9, 4, 10, 5, 10, 6 ] ),
>   Transformation( [ 4, 7, 6, 9, 10, 1, 3, 6, 6, 2 ] ),
>   Transformation( [ 5, 9, 10, 9, 6, 3, 8, 4, 6, 5 ] ),
>   Transformation( [ 6, 2, 2, 7, 8, 8, 2, 10, 2, 4 ] ),
>   Transformation( [ 6, 2, 8, 4, 7, 5, 8, 3, 5, 8 ] ),
>   Transformation( [ 7, 1, 4, 3, 2, 7, 7, 6, 6, 5 ] ),
>   Transformation( [ 7, 10, 10, 1, 7, 9, 10, 4, 2, 10 ] ),
>   Transformation( [ 10, 7, 10, 8, 8, 7, 5, 9, 1, 9 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsRegularSemigroup(s);
false

#
gap> gens:=[Transformation([2,1,4,5,3,7,8,9,10,6]),
> Transformation([1,2,4,3,5,6,7,8,9,10]),
> Transformation([1,2,3,4,5,6,10,9,8,7]),
> Transformation([9,1,4,3,6,9,3,4,3,9])];;
gap> s:=Monoid(gens);;
gap> IsRegularSemigroup(s);
false

#
gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Monoid(gens);;
gap> IsInverseSemigroup(s);
false
gap> t:=Semigroup(Idempotents(s));;
gap> IsSemilatticeAsSemigroup(t);
false
gap> IsBand(t);
true
gap> Size(t);
10
gap> IsOrthodoxSemigroup(t);
true

#
gap> gens:=[Transformation( [ 2, 3, 4, 5, 1, 8, 7, 6, 2, 7 ] ),
> Transformation([2,3,4,5,6,8,7,1,2,2])];;
gap> s:=Monoid(gens);;
gap> s:=Semigroup(Idempotents(Monoid(gens)));;
gap> IsSemilatticeAsSemigroup(s);
false
gap> IsBand(s);
true

#
gap> gens:=[ Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ),
>   Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] ) ];
[ Transformation( [ 5, 6, 7, 3, 1, 4, 2, 8 ] ), 
  Transformation( [ 3, 6, 8, 5, 7, 4, 2, 8 ] ) ]
gap> s:=Semigroup(Idempotents(Monoid(gens)));;
gap> Size(s);
94
gap> IsSemilatticeAsSemigroup(s);
true

#
gap> s:=FullTransformationSemigroup(3);;
gap> j:=0;;
gap> for f in s do
> for g in s do
> if IsSynchronizingSemigroup(Semigroup(f,g)) then j:=j+1; fi;
> od;
> od;
gap> j;
549

#
gap> gens:=[ Transformation( [ 4, 6, 5, 2, 1, 3 ] ),
>   Transformation( [ 6, 3, 2, 5, 4, 1 ] ),
>   Transformation( [ 1, 2, 4, 3, 5, 6 ] ),
>   Transformation( [ 3, 5, 6, 1, 2, 3 ] ),
>   Transformation( [ 5, 3, 6, 6, 6, 2 ] ),
>   Transformation( [ 2, 3, 2, 6, 4, 6 ] ),
>   Transformation( [ 2, 1, 2, 2, 2, 4 ] ),
>   Transformation( [ 4, 4, 1, 2, 1, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> g:=Range(IsomorphismPermGroup(GroupOfUnits(s)));;
gap> IsZeroGroup(Range(InjectionZeroMagma(g)));
true
gap> IsZeroGroup(s);
false

#
gap> gens:=List(Tuples([1,2], 4), x-> TransformationNC(Concatenation([1,1], x)));;
gap> s:=Semigroup(gens);;
gap> RedundantGenerator(s, gens);
Transformation( [ 1, 1, 1, 1, 1, 1 ] )
gap> IsZeroSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10 ] ), 
>  Transformation( [ 3, 6, 9, 1, 4, 7, 2, 5, 8, 10, 10 ] ), 
>  Transformation( [ 3, 6, 9, 7, 1, 4, 5, 8, 2, 10, 10 ] ), 
>  Transformation( [ 8, 2, 5, 5, 4, 5, 5, 2, 8, 10, 10 ] ), 
>  Transformation( [ 4, 4, 8, 4, 4, 2, 4, 4, 5, 10, 10 ] ) ];;
gap> s:=Semigroup(gens);;
gap> MultiplicativeNeutralElement(s);
Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10 ] )

#
gap> [ Transformation( [ 3, 6, 9, 1, 4, 7, 2, 5, 8 ] ),
>   Transformation( [ 3, 6, 9, 7, 1, 4, 5, 8, 2 ] ),
>   Transformation( [ 8, 2, 5, 5, 4, 5, 5, 2, 8 ] ),
>   Transformation( [ 4, 4, 8, 4, 4, 2, 4, 4, 5 ] ),
>   Transformation( [ 7, 5, 5, 7, 3, 7, 5, 5, 3 ] ),
>   Transformation( [ 7, 3, 3, 3, 3, 7, 5, 3, 3 ] ),
>   Transformation( [ 3, 5, 3, 3, 3, 3, 7, 7, 5 ] ),
>   Transformation( [ 3, 7, 3, 3, 5, 5, 7, 7, 7 ] ),
>   Transformation( [ 3, 3, 3, 7, 5, 5, 7, 5, 7 ] ),
>   Transformation( [ 3, 5, 5, 3, 7, 3, 7, 5, 7 ] ),
>   Transformation( [ 3, 3, 3, 5, 5, 3, 7, 5, 5 ] ),
>   Transformation( [ 5, 5, 7, 5, 7, 3, 5, 3, 7 ] ),
>   Transformation( [ 3, 5, 5, 3, 7, 3, 3, 5, 3 ] ),
>   Transformation( [ 7, 3, 7, 7, 7, 3, 3, 5, 7 ] ),
>   Transformation( [ 5, 3, 7, 3, 7, 5, 3, 5, 3 ] ),
>   Transformation( [ 5, 5, 7, 5, 7, 3, 7, 7, 5 ] ) ];;
gap> s:=Semigroup(last);;
gap> MultiplicativeNeutralElement(s);
Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] )

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ),
>   Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ),
>   Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsAbundantSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 2, 6, 7, 2, 6, 1, 1, 5 ] ),
>   Transformation( [ 3, 8, 1, 4, 5, 6, 7, 1 ] ),
>   Transformation( [ 4, 3, 2, 7, 7, 6, 6, 5 ] ),
>   Transformation( [ 7, 1, 7, 4, 2, 5, 6, 3 ] ) ];;
gap> s:=Monoid(gens);;
gap> IsAbundantSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ),
>   Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ),
>   Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ),
>   Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsAbundantSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>   Transformation( [ 4, 2, 1, 5, 5 ] ),
>   Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsAbundantSemigroup(s);
true

#
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> IsAbundantSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 2, 4, 1, 1 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> IsAbundantSemigroup(s);
true
gap> IsRegularSemigroup(s);
false

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
>  Transformation( [ 1, 4, 1, 2 ] ),
>  Transformation( [ 3, 4, 2, 2 ] ),
>  Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> IsAbundantSemigroup(s);
true
gap> IsRegularSemigroup(s);
false

#
gap> gens:=[Transformation([2,1,4,5,3,7,8,9,10,6]),
> Transformation([1,2,4,3,5,6,7,8,9,10]),
> Transformation([1,2,3,4,5,6,10,9,8,7]),
> Transformation([9,1,4,3,6,9,3,4,3,9])];;
gap> s:=Monoid(gens);;
gap> IsAbundantSemigroup(s);
true
gap> IsRegularSemigroup(s);
false

#
gap> gens:=[Transformation( [ 1, 4, 11, 11, 7, 2, 6, 2, 5, 5, 10 ] ),
> Transformation( [ 2, 4, 4, 2, 10, 5, 11, 11, 11, 6, 7 ] )];;
gap> s:=Monoid(gens);;
gap> IsAdequateSemigroup(s);
false
gap> gens:=[Transformation([2,1,4,5,3,7,8,9,10,6]),
> Transformation([1,2,4,3,5,6,7,8,9,10]),
> Transformation([1,2,3,4,5,6,10,9,8,7]),
> Transformation([9,1,4,3,6,9,3,4,3,9])];;
gap> s:=Monoid(gens);;
gap> IsAdequateSemigroup(s);
false

#
gap> file:=Concatenation(CitrusDir(), "/examples/graph8c.citrus.gz");;
gap> ReadCitrus(file, 1303);;
gap> s:=Semigroup(last);;
gap> t:=IdempotentGeneratedSubsemigp(s);;
gap> Size(t);
105

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

#
gap> Unbind(semis); Unbind(file); Unbind(s); Unbind(d); 
gap> Unbind(g); Unbind(gens); Unbind(t); Unbind(i); Unbind(f);
gap> Unbind(g); Unbind(rms);

#
gap> STOP_TEST( "Citrus package: properties.tst", 10000);
