#############################################################################
##
#W  reesmat.tst
#Y  Copyright (C) 2011-15                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: reesmat.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#T# ReesMatTest2 some semigroups to which the methods in Semigroups should not apply...
gap> R:=ReesZeroMatrixSemigroup(POI(5), [[0,0,0], [0,0,0]]);
<Rees 0-matrix semigroup 3x2 over <inverse partial perm monoid on 5 pts
 with 5 generators>>
gap> R:=Semigroup(Generators(R));
<subsemigroup of 3x2 Rees 0-matrix semigroup with 1512 generators>
gap> IsActingSemigroup(R);
false
gap> R:=ReesZeroMatrixSemigroup(Group(()), [[0,0,0], [0,0,0]]);
<Rees 0-matrix semigroup 3x2 over Group(())>
gap> R:=ReesZeroMatrixSemigroup(Group(()), [[0,0,0], [0,0,0]]);
<Rees 0-matrix semigroup 3x2 over Group(())>
gap> R:=Semigroup(Generators(R));                              
<subsemigroup of 3x2 Rees 0-matrix semigroup with 6 generators>
gap> IsActingSemigroup(R);
false
gap> R:=ReesZeroMatrixSemigroup(POI(5), [[PartialPerm([],[]),0],
> [0, PartialPerm([], [])]]);
<Rees 0-matrix semigroup 2x2 over <inverse partial perm monoid on 5 pts
 with 5 generators>>
gap> R:=Semigroup(Generators(R));                                              
<subsemigroup of 2x2 Rees 0-matrix semigroup with 1008 generators>
gap> IsActingSemigroup(R);
false

#T# ReesMatTest3 find a source of interesting subsemigroups of Rees 0-matrix semigroups...
gap> S:=Semigroup( 
>  Transformation( [ 1, 1, 2, 7, 9, 8, 5, 9, 6 ] ), 
>  Transformation( [ 1, 1, 7, 2, 8, 9, 9, 5, 6 ] ), 
>  Transformation( [ 1, 2, 2, 3, 6, 5, 5, 7 ] ), 
>  Transformation( [ 1, 2, 2, 3, 6, 9, 9, 7, 5 ] ), 
>  Transformation( [ 1, 2, 2, 3, 7, 5, 5, 7 ] ), 
>  Transformation( [ 1, 2, 2, 3, 7, 9, 9, 7, 5 ] ), 
>  Transformation( [ 1, 2, 3, 1 ] ), Transformation( [ 1, 2, 3, 3 ] ), 
>  Transformation( [ 1, 2, 3, 6, 5, 6 ] ), 
>  Transformation( [ 1, 2, 3, 9, 5, 6, 7, 8, 9 ] ), 
>  Transformation( [ 1, 2, 7, 8, 9, 5, 5, 9, 6 ] ), 
>  Transformation( [ 1, 2, 8, 2, 7, 9, 9, 6, 5 ] ), 
>  Transformation( [ 1, 2, 8, 2, 9, 7, 6, 9, 5 ] ), 
>  Transformation( [ 2, 1, 1, 3, 6, 5, 5, 7 ] ), 
>  Transformation( [ 2, 1, 1, 3, 6, 9, 9, 7, 5 ] ), 
>  Transformation( [ 2, 1, 1, 3, 7, 5, 5, 7 ] ), 
>  Transformation( [ 2, 1, 1, 3, 7, 9, 9, 7, 5 ] ), 
>  Transformation( [ 2, 1, 1, 6, 9, 7, 6, 9, 5 ] ), 
>  Transformation( [ 2, 1, 1, 8, 9, 6, 7, 9, 5 ] ), 
>  Transformation( [ 2, 1, 1, 8, 9, 7, 6, 9, 5 ] ), 
>  Transformation( [ 2, 1, 8, 1, 7, 9, 9, 6, 5 ] ), 
>  Transformation( [ 2, 1, 8, 6, 9, 7, 6, 9, 5 ] ), 
>  Transformation( [ 5, 5, 8, 1, 7, 2, 1, 6 ] ), 
>  Transformation( [ 5, 8, 2, 3, 6, 9, 9, 7, 1 ] ), 
>  Transformation( [ 5, 8, 7, 2, 9, 1, 1, 9, 6 ] ), 
>  Transformation( [ 6, 6, 9, 4, 8, 1, 1, 8, 9 ] ), 
>  Transformation( [ 6, 7, 1, 8, 9, 5, 5, 9, 2 ] ), 
>  Transformation( [ 6, 7, 6, 4, 5, 1, 2 ] ), 
>  Transformation( [ 6, 7, 7, 8, 9, 1, 2, 9, 5 ] ), 
>  Transformation( [ 6, 7, 7, 8, 9, 2, 1, 9, 5 ] ), 
>  Transformation( [ 6, 7, 8, 1, 9, 2, 1, 9, 5 ] ), 
>  Transformation( [ 6, 7, 9, 4, 5, 1, 2, 8, 9 ] ), 
>  Transformation( [ 7, 6, 8, 2, 9, 1, 1, 9, 5 ] ), 
>  Transformation( [ 7, 6, 8, 6, 9, 2, 1, 9, 5 ] ), 
>  Transformation( [ 8, 5, 2, 7, 9, 6, 6, 9, 1 ] ), 
>  Transformation( [ 9, 9, 2, 7, 8, 6, 6, 5, 1 ] ), 
>  Transformation( [ 9, 9, 3, 2, 6, 5, 8, 7, 1 ] ), 
>  Transformation( [ 9, 9, 3, 4, 5, 6, 7, 8, 1 ] ), 
>  Transformation( [ 9, 9, 8, 7, 2, 6, 6, 1, 5 ] ) );;
gap> R:=PrincipalFactor(DClasses(S)[40]);
<Rees 0-matrix semigroup 26x5 over Group([ (5,8)(6,9), (1,6,9), (1,6) ])>
gap> U:=MaximalSubsemigroups(R){[31..36]};
[ <subsemigroup of 26x5 Rees 0-matrix semigroup with 53 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 53 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 53 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 53 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 53 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 53 generators> ]
gap> V:=Semigroup(MultiplicativeZero(R), 
> RMSElement(R, 13, (1,6)(5,8),3),
> RMSElement(R, 1,(1,6),3), 
> RMSElement(R, 7,(1,6)(5,8),3), 
> RMSElement(R, 23,(5,8),2), 
> RMSElement(R, 22,(1,6),1), 
> RMSElement(R,11,(1,9),5), 
> RMSElement(R,2,(1,6),5), 
> RMSElement(R,24,(1,6)(5,8),4), 
> RMSElement(R,6,(1,9)(5,8),1), 
> RMSElement(R,15,(1,9)(5,8),2), 
> RMSElement(R,22,(1,9),1));;

#T# ReesMatTest4 from attributes.xml...

#T# ReesMatTest5 StuctureDescriptionMaximalSubgroups
gap> StructureDescriptionMaximalSubgroups(U[1]);
[ "1", "C2 x C2" ]
gap> StructureDescriptionMaximalSubgroups(V);
[ "1", "D12" ]

#T# ReesMatTest6 StructureDescriptionSchutzenbergerGroups 
gap> StructureDescriptionSchutzenbergerGroups(U[5]);
[ "1", "C2 x C2" ]

#T# ReesMatTest7 MinimalDClass
gap> List(U, MinimalDClass);
[ {0}, {0}, {0}, {0}, {0}, {0} ]
gap> MinimalDClass(V);
{0}

#T# ReesMatTest8 MaximalDClasses
gap> MaximalDClasses(V);
[ {(13,(1,6)(5,8),3)}, {(22,(1,6),1)} ]
gap> MaximalDClasses(U[4]);
[ {(1,(),1)} ]

#T# ReesMatTest9 PrincipalFactor
gap> D:=Filtered(DClasses(V), IsRegularClass)[2];
{(13,(1,6)(5,8),3)}
gap> inj:=InjectionPrincipalFactor(D);; inv:=InverseGeneralMapping(inj);;
gap> ForAll(D, x-> (x^inj)^inv=x);                                         
true
gap> ForAll(D, x-> ForAll(D, y-> (not x*y in D) or (x*y)^inj=x^inj*y^inj));
true

#T# ReesMatTest10 SmallGeneratingSet
gap> Length(SmallGeneratingSet(V))<Length(Generators(V));
true
gap> Apply(U, x-> Semigroup(SmallGeneratingSet(x)));

#T# ReesMatTest11 MinimalIdeal
gap> MinimalIdeal(V);
<simple Rees 0-matrix semigroup ideal with 1 generator>
gap> List(U, MinimalIdeal);
[ <simple Rees 0-matrix semigroup ideal with 1 generator>, 
<simple Rees 0-matrix semigroup ideal with 1 generator>, 
<simple Rees 0-matrix semigroup ideal with 1 generator>, 
<simple Rees 0-matrix semigroup ideal with 1 generator>, 
<simple Rees 0-matrix semigroup ideal with 1 generator>, 
<simple Rees 0-matrix semigroup ideal with 1 generator> ]

#T# ReesMatTest12 IsomorphismPermGroup
gap> R:=ReesZeroMatrixSemigroup(QuaternionGroup(IsPermGroup, 8), [[()]]);;
gap> T:=Semigroup(Filtered(Generators(R), x-> x![1]<>0));;
gap> iso:=IsomorphismPermGroup(T);              
MappingByFunction( <subsemigroup of 1x1 Rees 0-matrix semigroup 
 with 2 generators>, Group([ (1,5,3,7)(2,8,4,6), (1,2,3,4)
(5,6,7,8) ]), function( x ) ... end, function( x ) ... end )
gap> inv:=InverseGeneralMapping(iso);
MappingByFunction( Group([ (1,5,3,7)(2,8,4,6), (1,2,3,4)
(5,6,7,8) ]), <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>
 , function( x ) ... end, function( x ) ... end )
gap> ForAll(T, x-> (x^iso)^inv=x);
true
gap> ForAll(T, x-> ForAll(T, y-> (x*y)^iso=x^iso*y^iso));                   
true
gap> iso:=IsomorphismPermGroup(MinimalIdeal(V));
MappingByFunction( <simple Rees 0-matrix semigroup ideal with 1 generator>
, Group(()), function( x ) ... end, function( x ) ... end )
gap> inv:=InverseGeneralMapping(iso);
MappingByFunction( Group(()), <simple Rees 0-matrix semigroup ideal with
 1 generator>, function( x ) ... end, function( x ) ... end )
gap> ForAll(MinimalIdeal(V), x-> (x^iso)^inv=x);         
true
gap> ForAll(MinimalIdeal(V), x-> ForAll(MinimalIdeal(V), y-> 
> (x*y)^iso=x^iso*y^iso));
true

#T# ReesMatTest13 GroupOfUnits
gap> R:=Semigroup(Generators(R));
<subsemigroup of 1x1 Rees 0-matrix semigroup with 3 generators>
gap> GroupOfUnits(R);
<subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>
gap> GroupOfUnits(V);
fail
gap> GroupOfUnits(U[5]);
fail

#T# ReesMatTest14 IdempotentGeneratedSubsemigroup
gap> eV:=IdempotentGeneratedSubsemigroup(V);
<subsemigroup of 26x5 Rees 0-matrix semigroup with 9 generators>
gap> Size(eV);
15
gap> Size(V);
505
gap> List(U, IdempotentGeneratedSubsemigroup);;
gap> List(last, Size);
[ 47, 47, 47, 47, 47, 47 ]
gap> last2[1]=last2[2];
true

#T# ReesMatTest15 IrredundantGeneratingSubset
gap> IrredundantGeneratingSubset(V);
[ (1,(1,6),3), (2,(1,6),5), (6,(1,9)(5,8),1), (7,(1,6)(5,8),3), (11,(1,9),5), 
  (13,(1,6)(5,8),3), (15,(1,9)(5,8),2), (22,(1,6),1), (23,(5,8),2), 
  (24,(1,6)(5,8),4) ]
gap> Length(last);
10
gap> U[1]=Semigroup(IrredundantGeneratingSubset(U[1]));
true

#T# ReesMatTest16 from greens.xml...

#T# ReesMatTest17 MultiplicativeNeutralElement (for an H-class)
gap> H:=First(HClasses(V), IsRegularClass);
{0}
gap> MultiplicativeNeutralElement(H);
0
gap> H:=First(HClasses(V), x-> not IsRegularClass(x));
{(23,(6,9),3)}
gap> MultiplicativeNeutralElement(H);
fail
gap> H:=First(HClasses(U[5]), IsRegularClass);
{(17,(1,9)(5,8),5)}
gap> e:=MultiplicativeNeutralElement(H);
(17,(1,9)(5,8),5)
gap> e^2;
(17,(1,9)(5,8),5)
gap> ForAll(H, x-> x*e=x and e*x=x);
true
gap> H:=First(HClasses(U[5]), x-> not IsRegularClass(x));
{(21,(1,9,6)(5,8),5)}
gap> MultiplicativeNeutralElement(H);
fail

#T# ReesMatTest18 StructureDescription (for an H-class) 
gap> H:=First(HClasses(U[5]), IsRegularClass);;
gap> StructureDescription(H);
"C2 x C2"

#T# ReesMatTest19 Random
gap> Random(V);;
gap> List(U, Random);;
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `*' on 2 arguments
gap> ForAll([1..5], x-> last[x] in U[x]);
false

#T# ReesMatTest20 DClassOf.Class etc
gap> H:=First(HClasses(V), x-> not IsRegularClass(x));
{(23,(6,9),3)}
gap> DClass(H);
{(13,(1,6)(5,8),3)}
gap> RClass(H);
{(23,(6,9),3)}
gap> LClass(H);
{(13,(1,6)(5,8),3)}
gap> LClass(H)<LClass(V, Representative(V));
false

#T# ReesMatTest21 DClasses etc...
gap> RClasses(V);
[ {0}, {(13,(1,6)(5,8),3)}, {(1,(1,6),3)}, {(7,(1,6)(5,8),3)}, 
  {(23,(6,9),3)}, {(22,(1,6),1)}, {(11,(1,6),3)}, {(2,(6,9),3)}, 
  {(24,(5,8)(6,9),3)}, {(6,(1,9)(5,8),1)}, {(15,(1,6,9),3)}, {(22,(),3)}, 
  {(6,(1,9,6)(5,8),3)} ]
gap> LClasses(V);
[ {0}, {(13,(1,6)(5,8),3)}, {(13,(1,9,6),2)}, {(13,(1,9)(5,8),5)}, 
  {(13,(1,9)(5,8),4)}, {(1,(1,6),3)}, {(1,(1,9,6)(5,8),2)}, {(1,(1,9),5)}, 
  {(1,(1,9),4)}, {(1,(5,8),3)}, {(1,(6,9),2)}, {(1,(1,6,9)(5,8),5)}, 
  {(1,(1,6,9)(5,8),4)}, {(1,(5,8)(6,9),3)}, {(1,(),2)}, {(1,(1,6)(5,8),5)}, 
  {(1,(1,6)(5,8),4)}, {(1,(1,9,6)(5,8),3)}, {(1,(1,6),2)}, {(1,(5,8),5)}, 
  {(1,(5,8),4)}, {(1,(1,9),3)}, {(1,(1,6,9)(5,8),2)}, {(1,(6,9),5)}, 
  {(1,(6,9),4)}, {(1,(1,6,9),3)}, {(1,(1,9)(5,8),2)}, {(1,(1,9,6),5)}, 
  {(1,(1,9,6),4)}, {(1,(1,9,6),3)}, {(1,(1,6)(5,8),2)}, {(1,(),5)}, 
  {(1,(),4)}, {(1,(1,9)(5,8),3)}, {(1,(1,6,9),2)}, {(1,(5,8)(6,9),5)}, 
  {(1,(5,8)(6,9),4)}, {(1,(1,6,9)(5,8),3)}, {(1,(1,9),2)}, 
  {(1,(1,9,6)(5,8),5)}, {(1,(1,9,6)(5,8),4)}, {(1,(1,6)(5,8),3)}, 
  {(1,(1,9,6),2)}, {(1,(1,9)(5,8),5)}, {(1,(1,9)(5,8),4)}, {(1,(),3)}, 
  {(1,(5,8)(6,9),2)}, {(1,(1,6,9),5)}, {(1,(1,6,9),4)}, {(1,(6,9),3)}, 
  {(1,(5,8),2)}, {(1,(1,6),5)}, {(1,(1,6),4)}, {(22,(1,6),1)}, 
  {(15,(1,6,9),3)}, {(15,(1,9)(5,8),2)}, {(15,(1,9,6),5)}, {(15,(1,9,6),4)}, 
  {(15,(5,8)(6,9),3)}, {(15,(),2)}, {(15,(1,6)(5,8),5)}, {(15,(1,6)(5,8),4)}, 
  {(15,(5,8),3)}, {(15,(6,9),2)}, {(15,(1,6,9)(5,8),5)}, 
  {(15,(1,6,9)(5,8),4)}, {(15,(1,9)(5,8),3)}, {(15,(1,6,9),2)}, 
  {(15,(5,8)(6,9),5)}, {(15,(5,8)(6,9),4)}, {(15,(1,9,6),3)}, 
  {(15,(1,6)(5,8),2)}, {(15,(),5)}, {(15,(),4)}, {(15,(1,6),3)}, 
  {(15,(1,9,6)(5,8),2)}, {(15,(1,9),5)}, {(15,(1,9),4)}, {(15,(1,9),3)}, 
  {(15,(1,6,9)(5,8),2)}, {(15,(6,9),5)}, {(15,(6,9),4)}, 
  {(15,(1,9,6)(5,8),3)}, {(15,(1,6),2)}, {(15,(5,8),5)}, {(15,(5,8),4)}, 
  {(15,(1,6)(5,8),3)}, {(15,(1,9,6),2)}, {(15,(1,9)(5,8),5)}, 
  {(15,(1,9)(5,8),4)}, {(15,(1,6,9)(5,8),3)}, {(15,(1,9),2)}, 
  {(15,(1,9,6)(5,8),5)}, {(15,(1,9,6)(5,8),4)}, {(15,(6,9),3)}, 
  {(15,(5,8),2)}, {(15,(1,6),5)}, {(15,(1,6),4)}, {(15,(),3)}, 
  {(15,(5,8)(6,9),2)}, {(15,(1,6,9),5)}, {(15,(1,6,9),4)}, {(22,(),3)}, 
  {(22,(5,8)(6,9),2)}, {(22,(1,6,9),5)}, {(22,(1,6,9),4)} ]
gap> DClasses(V);
[ {0}, {(13,(1,6)(5,8),3)}, {(1,(1,6),3)}, {(22,(1,6),1)}, {(15,(1,6,9),3)}, 
  {(22,(),3)} ]
gap> NrHClasses(V);
131
gap> NrLClasses(V);
106
gap> RClassReps(U[2]);
[ (17,(1,9)(5,8),5), (2,(5,8)(6,9),5), (21,(1,6,9)(5,8),5), 
  (19,(1,6,9)(5,8),5), (5,(5,8)(6,9),5), (16,(1,6,9)(5,8),5), (25,(),5), 
  (26,(1,6),5), (12,(5,8)(6,9),5), (3,(5,8)(6,9),5), (18,(1,6,9)(5,8),5), 
  (1,(),5), (11,(6,9),5), (8,(1,6)(5,8),5), (7,(1,9,6),5), (13,(1,6,9),5), 
  (15,(1,9),5), (4,(),5), (20,(1,9,6),5), (9,(1,6),5), (10,(),5), 
  (14,(5,8)(6,9),5), (24,(),5), (23,(1,6),5), (22,(),5), (6,(1,6),5), 0 ]
gap> LClassReps(R);
[ (1,(1,5,3,7)(2,8,4,6),1), 0 ]
gap> DClassReps(U[4]);
[ (25,(),4), 0 ]

#T# ReesMatTest22 MultiplicativeZero
gap> List(U, MultiplicativeZero);
[ 0, 0, 0, 0, 0, 0 ]
gap> ForAll(last, IsIdempotent);
true

#T# ReesMatTest23 GroupHClass
gap> D:=Filtered(DClasses(U[1]), IsRegularClass)[2];
{0}
gap> GroupHClass(D);
{0}
gap> StructureDescription(last);
"1"
gap> D:=First(DClasses(V), IsRegularClass);         
{0}
gap> GroupHClass(D);
{0}
gap> StructureDescription(last);
"1"

#T# ReesMatTest24 Idempotents
gap> Idempotents(V);
[ 0, (13,(1,9,6),3), (13,(1,9,6),2), (7,(),3), (7,(),2), (23,(1,6),5), 
  (23,(1,6),4), (22,(),1), (11,(1,6),3), (11,(1,6),2), (2,(5,8)(6,9),5), 
  (2,(5,8)(6,9),4), (24,(),5), (24,(),4), (6,(1,6),1) ]
gap> ForAll(last, IsIdempotent);
true
gap> Idempotents(U[2]);
[ (17,(1,9)(5,8),5), (2,(5,8)(6,9),5), (5,(5,8)(6,9),5), (25,(),5), 
  (3,(5,8)(6,9),5), (4,(),5), (24,(),5), (23,(1,6),5), (17,(1,9)(5,8),4), 
  (2,(5,8)(6,9),4), (5,(5,8)(6,9),4), (25,(),4), (3,(5,8)(6,9),4), (4,(),4), 
  (24,(),4), (23,(1,6),4), (21,(1,9,6)(5,8),2), (19,(1,9,6)(5,8),2), 
  (16,(1,9,6)(5,8),2), (26,(1,9),2), (12,(1,6)(5,8),2), (18,(1,9,6)(5,8),2), 
  (11,(1,6),2), (8,(1,9)(5,8),2), (7,(),2), (13,(1,9,6),2), (20,(),2), 
  (9,(1,9),2), (21,(1,9,6)(5,8),3), (19,(1,9,6)(5,8),3), (16,(1,9,6)(5,8),3), 
  (26,(1,9),3), (12,(1,6)(5,8),3), (18,(1,9,6)(5,8),3), (11,(1,6),3), 
  (8,(1,9)(5,8),3), (7,(),3), (13,(1,9,6),3), (20,(),3), (9,(1,9),3), 
  (1,(),1), (15,(1,9),1), (10,(),1), (14,(5,8)(6,9),1), (22,(),1), 
  (6,(1,6),1), 0 ]
gap> ForAll(last, IsIdempotent);
true

#T# ReesMatTest25 IsRegularClass
gap> Number(RClasses(V), IsRegularClass);
9
gap> Number(DClasses(V), IsRegularClass);
3
gap> NrRegularDClasses(V);
3
gap> Number(DClasses(U[4]), IsRegularClass);
2
gap> NrRegularDClasses(U[4]);
2
gap> Number(LClasses(U[4]), IsRegularClass);
6

#T# ReesMatTest26 NrIdempotents
gap> NrIdempotents(V)=Length(Idempotents(V));
true
gap> ForAll(U, x-> NrIdempotents(x)=Length(Idempotents(x)));
true
gap> List(DClasses(V), NrIdempotents);
[ 1, 12, 0, 2, 0, 0 ]
gap> List(RClasses(V), NrIdempotents);
[ 1, 2, 0, 2, 2, 1, 2, 2, 2, 1, 0, 0, 0 ]
gap> List(LClasses(R), NrIdempotents);
[ 1, 1 ]

#T# ReesMatTest27 PartialOrderOfDClasses
gap> PartialOrderOfDClasses(V);
[ [ 1 ], [ 1, 2, 3, 5 ], [ 1, 3, 6 ], [ 1, 4, 6 ], [ 1, 5, 6 ], [ 1, 6 ] ]
gap> PartialOrderOfDClasses(U[1]); 
[ [ 1, 2 ], [ 2 ] ]
gap> PartialOrderOfDClasses(U[2]);
[ [ 1, 2 ], [ 2 ] ]

#T# ReesMatTest28 from properties.xml...
gap> IsBand(V);
false
gap> List(U, IsBand);
[ false, false, false, false, false, false ]
gap> IsBlockGroup(V);
false
gap> List(U, IsBlockGroup);
[ false, false, false, false, false, false ]
gap> IsBrandtSemigroup(V); 
false
gap> List(U, IsBrandtSemigroup);
[ false, false, false, false, false, false ]
gap> IsCliffordSemigroup(V);
false
gap> List(U, IsCliffordSemigroup);
[ false, false, false, false, false, false ]
gap> IsCommutativeSemigroup(V);
false
gap> List(U, IsCommutativeSemigroup);
[ false, false, false, false, false, false ]
gap> IsCompletelyRegularSemigroup(V);
false
gap> List(U, IsCompletelyRegularSemigroup); 
[ false, false, false, false, false, false ]
gap> IsDTrivial(V);
false
gap> IsRTrivial(V);
false
gap> IsLTrivial(V);
false
gap> IsHTrivial(V);
false
gap> List(U, IsGroupAsSemigroup);
[ false, false, false, false, false, false ]
gap> List(U, IsIdempotentGenerated);
[ false, false, false, false, false, false ]
gap> List(U, IsInverseSemigroup);
[ false, false, false, false, false, false ]
gap> R:=ReesZeroMatrixSemigroup(QuaternionGroup(IsPermGroup, 8), 
> [[(), (), ()]]);;
gap> R:=Semigroup(Difference(Generators(R), [MultiplicativeZero(R)]));
<subsemigroup of 3x1 Rees 0-matrix semigroup with 4 generators>
gap> IsRightSimple(R);
false
gap> IsLeftSimple(R); 
true
gap> IsCompletelyRegularSemigroup(R);
true
gap> R:=ReesZeroMatrixSemigroup(Group(()),  [[(), (), (), (), ()]]);;
gap> R:=Semigroup(Difference(Generators(R), [MultiplicativeZero(R)]));  
<subsemigroup of 5x1 Rees 0-matrix semigroup with 5 generators>
gap> IsLeftZeroSemigroup(R);
true
gap> IsRightZeroSemigroup(R);
false
gap> IsMonogenicSemigroup(R);
false
gap> List(U, IsMonogenicSemigroup);
[ false, false, false, false, false, false ]
gap> R:=ReesZeroMatrixSemigroup(Group(()),  [[()]]);                  
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> R:=Semigroup(Generators(R));                                     
<subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>
gap> IsMonoidAsSemigroup(R);
true
gap> List(U, IsMonoidAsSemigroup);
[ false, false, false, false, false, false ]
gap> IsomorphismTransformationSemigroup(R);
MappingByFunction( <subsemigroup of 1x1 Rees 0-matrix semigroup 
 with 2 generators>, <commutative transformation monoid 
 on 2 pts with 1 generator>, function( x ) ... end, function( x ) ... end )
gap> IsOrthodoxSemigroup(R);
true
gap> IsOrthodoxSemigroup(V);
false
gap> List(U, IsOrthodoxSemigroup);
[ true, true, true, true, true, true ]
gap> R:=ReesZeroMatrixSemigroup(Group(()), [[(), (), ()], [(), (), ()]]);  
<Rees 0-matrix semigroup 3x2 over Group(())>
gap> R:=Semigroup(Difference(Generators(R), [MultiplicativeZero(R)]));     
<subsemigroup of 3x2 Rees 0-matrix semigroup with 4 generators>
gap> IsRectangularBand(R);
true
gap> IsRectangularBand(V);              
false
gap> List(U, IsRectangularBand);
[ false, false, false, false, false, false ]
gap> List(U, IsRegularSemigroup);
[ true, true, true, true, true, true ]
gap> IsRegularSemigroup(V);      
false
gap> UU:=IdempotentGeneratedSubsemigroup(R);  
<subsemigroup of 3x2 Rees 0-matrix semigroup with 3 generators>
gap> IsSemilatticeAsSemigroup(UU);
false
gap> IsSimpleSemigroup(V);        
false
gap> List(U, IsSimpleSemigroup); 
[ false, false, false, false, false, false ]
gap> R;
<subsemigroup of 3x2 Rees 0-matrix semigroup with 4 generators>
gap> IsSimpleSemigroup(V);
false
gap> IsSimpleSemigroup(R);
true
gap> f:=IsomorphismReesMatrixSemigroup(R); g:=InverseGeneralMapping(f);;
MappingByFunction( <subsemigroup of 3x2 Rees 0-matrix semigroup 
 with 4 generators>, <Rees matrix semigroup 3x2 over Group(())>
 , function( x ) ... end, function( x ) ... end )
gap> ForAll(R, x-> (x^f)^g=x); 
true
gap> ForAll(R, x-> ForAll(R, y-> (x*y)^f=x^f*y^f));
true
gap> R:=ReesZeroMatrixSemigroup(Group(()), [[(), (), ()], [(), (), ()]]);
<Rees 0-matrix semigroup 3x2 over Group(())>
gap> R:=Semigroup(Generators(R));                                      
<subsemigroup of 3x2 Rees 0-matrix semigroup with 5 generators>
gap> IsZeroGroup(R);
false
gap> IsZeroRectangularBand(R);
true
gap> IsZeroSimpleSemigroup(R);
true
gap> R:=ReesZeroMatrixSemigroup(Group(()), [[(),(),()], [(),0,0], [(),0,0]]); 
<Rees 0-matrix semigroup 3x3 over Group(())>
gap> R:=ReesZeroMatrixSubsemigroup(R, [2,3], Group(()), [2,3]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> R:=Semigroup(Generators(R));                                
<subsemigroup of 3x3 Rees 0-matrix semigroup with 4 generators>
gap> Size(R);
5
gap> IsZeroSemigroup(R);
true
gap> IsZeroSemigroup(V);
false

#T# ReesMatTest29 from semigroups.xml ...
gap> gens:=Generators(V);;
gap> V:=Semigroup(gens[1]);
<subsemigroup of 26x5 Rees 0-matrix semigroup with 1 generator>
gap> for i in [2..12] do 
> V:=ClosureSemigroup(V, gens[i]);
> od;
gap> V;
<subsemigroup of 26x5 Rees 0-matrix semigroup with 11 generators>
gap> Size(V);
505

#T# ReesMatTest30 from slp.xml...
gap> x:=RMSElement(V, 22,(1,6,9),1);;
gap> Factorization(V, x);
[ 6, 10, 10, 6 ]
gap> EvaluateWord(Generators(V), last);
(22,(1,6,9),1)
gap> x:=MultiplicativeZero(R);;
gap> Factorization(R, x);
[ 1, 1 ]
gap> EvaluateWord(Generators(R), last);
0
gap> x:=RMSElement(U[4], 26,(6,9),5);;
gap> Factorization(U[4], x);
[ 7, 24, 8, 5 ]
gap> EvaluateWord(Generators(U[4]), last);
(26,(6,9),5)

#T# ReesMatTest31: Issue 108: IsRegularSemigroup for a RZMS returned false negative
gap> t1 := Transformation( [ 4, 3, 1, 3 ] );;
gap> t2 := Transformation( [ 3, 3, 2, 2 ] );;
gap> T := Semigroup([ t1, t2 ]);;
gap> IsRegularSemigroup(T);
true
gap> IsGroup(T);
false
gap> mat := [ [ t2, t1 ], [ t1, t2 ] ];;

#TODO this takes 3 seconds, ugh! No good method for Generators 
gap> R := ReesZeroMatrixSemigroup(T, mat);;
gap> (CompareVersionNumbers(GAPInfo.Version,"4.7.7") and IsRegularSemigroup(R))
> or not CompareVersionNumbers(GAPInfo.Version,"4.7.7"); 
true

#T# ReesMatTest31
# JDM: the following lines are commented out until we have a deterministic
# method for AutomorphismGroup of a ReesMatrixSemigroup...
#
# AutomorphismGroup of a ReesMatrixSemigroup
#gap> G:=Group(());;
#gap> mat:=List([1..5], x-> List([1..5], y-> ()));;
#gap> M:=ReesMatrixSemigroup(G, mat);
#<Rees matrix semigroup 5x5 over Group(())>
#gap> AutomorphismGroup(M);
#<automorphism group of <Rees matrix semigroup 5x5 over Group(())> with 
#5 generators>
#gap> Size(last);
#14400
#gap> G:=Group((1,2,3,4,5));;
#gap> mat:=[ [ (), (), (), (), () ], 
#> [ (), (1,4,2,5,3), (1,3,5,2,4), (), (1,4,2,5,3) ],     
#>   [ (), (1,2,3,4,5), (1,2,3,4,5), (), () ], 
#>   [ (), (1,4,2,5,3), (1,5,4,3,2), (1,3,5,2,4), () ], 
#>   [ (), (1,2,3,4,5), (1,2,3,4,5), (1,5,4,3,2), (1,2,3,4,5) ] ];;
#gap> M:=ReesMatrixSemigroup(G, mat);
#<Rees matrix semigroup 5x5 over Group([ (1,2,3,4,5) ])>
#gap> AutomorphismGroup(M);
#<automorphism group of <Rees matrix semigroup 5x5 over Group([ (1,2,3,4,
#5) ])> with 1 generator>
#gap> Size(last);
#1
#gap> M:=Semigroup(Transformation( [ 3, 3, 2, 6, 2, 4, 4, 6 ] ), 
#> Transformation( [ 5, 1, 7, 8, 7, 5, 8, 1 ] ));;
#gap> R:=Range(IsomorphismReesMatrixSemigroup(M));;
#gap> AutomorphismGroup(R);
#<automorphism group of <Rees matrix semigroup 2x2 over Group([ (2,3)
#(4,6), (2,3,4,6), (2,4,6,3) ])> with 9 generators>
#gap> Size(last);
#12
#gap> G:=SmallGroup(256, 4);;
#gap> f1:=G.1;; f2:=G.2;; f3:=G.3;; f4:=G.4;; 
#gap> f5:=G.5;; f6:=G.6;; f7:=G.7;; f8:=G.8;; 
#gap> y:=f2*f3*f4*f5*f6*f7;;
#gap> iso:=IsomorphismPermGroup(G);;
#gap> G:=Range(iso);;
#gap> y:=y^iso;;
#gap> mat:=List([1..3], x-> [One(G), y, One(G)]);;
#gap> M:=ReesMatrixSemigroup(G, mat);;
#gap> AutomorphismGroup(M);;
#gap> IsomorphismPermGroup(last);; 
#
##JDM due to the non-deterministic methods in genss this will sometimes be
##incorrect... 
#gap> Size(last2);
#32768
#gap> G:=SymmetricGroup(7);; e:=One(G);; mat:=[[e], [e]];;
#gap> R:=ReesMatrixSemigroup(G, mat);
#<Rees matrix semigroup 1x2 over Sym( [ 1 .. 7 ] )>
#gap> AutomorphismGroup(R);
#<automorphism group of <Rees matrix semigroup 1x2 over Sym( [ 1 .. 7 ] )>
#  with 10080 generators>
#gap> G:=Group((1,4,3,5,2));;
#gap> mat:=[ [ (), (), () ], [ (), (1,4,3,5,2), () ], [ (), (1,3,2,4,5), () ] ];;
#gap> R:=ReesMatrixSemigroup(G, mat);;
#gap> l:=(4,6);
#(4,6)
#gap>  g:=GroupHomomorphismByImages(G, G, [(1,4,3,5,2)], [(1,2,5,3,4)]);
#[ (1,4,3,5,2) ] -> [ (1,2,5,3,4) ]
#gap> 
#gap> map:=[(), (1,5,4,2,3), (), (), (), () ];
#[ (), (1,5,4,2,3), (), (), (), () ]
#gap> 
#gap> RMSIsoByTriple(R, R, [l, g, map]);
#((4,6), GroupHomomorphismByImages( Group( [ (1,4,3,5,2) ] ), Group( 
#[ (1,4,3,5,2) ] ), [ (1,4,3,5,2) ], [ (1,2,5,3,4) ] ), 
#[ (), (1,5,4,2,3), (), (), (), () ])
#gap> G:=Group([ (2,5)(3,4) ]);;
#gap> mat:=[ [ (), (), (), (), () ], [ (), (), (2,5)(3,4), (2,5)(3,4), () ], 
#>   [ (), (), (), (2,5)(3,4), (2,5)(3,4) ], 
#>   [ (), (2,5)(3,4), (), (2,5)(3,4), () ], 
#>   [ (), (2,5)(3,4), (), (2,5)(3,4), () ] ];;
#gap> R:=ReesMatrixSemigroup(G, mat);;
#gap> A:=AutomorphismGroup(R);;
#
##JDM this is very often wrong due to genss...
#gap> Size(A);
#1
#gap> G:=Group([ (1,2) ]);;
#gap> mat:=[ [ (), (), () ], [ (), (1,2), () ], [ (), (1,2), (1,2) ], 
#>    [ (), (), () ], [ (), (1,2), () ] ];;
#gap> R:=ReesMatrixSemigroup(G, mat);;
#gap> l:=(1,2)(4,5,6);
#(1,2)(4,5,6)
#gap> gam:=One(AutomorphismGroup(G));
#IdentityMapping( Group([ (1,2) ]) )
#gap> g:=(1,2);;
#gap> RMSInducedFunction(R, l, gam, g);
#[ false, [ (1,2), (), (), (), (), (1,2), (1,2), () ] ]
#gap> RMSInducedFunction(R, (4,7), gam, ());
#[ true, [ (), (), (), (), (), (), (), () ] ]

#E#
gap> STOP_TEST( "Semigroups package: reesmat.tst");
