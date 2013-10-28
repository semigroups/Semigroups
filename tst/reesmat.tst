#############################################################################
##
#W  reesmat.tst
#Y  Copyright (C) 2011-13                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: reesmat.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

# some semigroups to which the methods in Semigroups should not apply...
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

# find a source of interesting subsemigroups of Rees 0-matrix semigroups...
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
[ <subsemigroup of 26x5 Rees 0-matrix semigroup with 128 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 148 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 140 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 132 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 124 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 144 generators> ]
gap> V:=Semigroup(MultiplicativeZero(U[3]), 
> RMSElement(U[3], 13, (1,6)(5,8),3),
> RMSElement(U[3], 1,(1,6),3), 
> RMSElement(U[3], 7,(1,6)(5,8),3), 
> RMSElement(U[3], 23,(5,8),2), 
> RMSElement(U[3], 22,(1,6),1), 
> RMSElement(U[3],11,(1,9),5), 
> RMSElement(U[3],2,(1,6),5), 
> RMSElement(U[3],24,(1,6)(5,8),4), 
> RMSElement(U[3],6,(1,9)(5,8),1), 
> RMSElement(U[3],15,(1,9)(5,8),2), 
> RMSElement(U[3],22,(1,9),1));;

# from attributes.xml...

# StuctureDescriptionMaximalSubgroups
gap> StructureDescriptionMaximalSubgroups(U[1]);
[ "1", "D12" ]
gap> StructureDescriptionMaximalSubgroups(V);
[ "1", "D12" ]

# StructureDescriptionSchutzenbergerGroups 
gap> StructureDescriptionSchutzenbergerGroups(U[5]);
[ "1", "D12" ]

# MinimalDClass
gap> List(U, MinimalDClass);
[ {0}, {0}, {0}, {0}, {0}, {0} ]
gap> MinimalDClass(V);
{0}

# MaximalDClasses
gap> MaximalDClasses(V);
[ {(13,(1,6)(5,8),3)}, {(22,(1,6),1)} ]
gap> MaximalDClasses(U[4]);
[ {(2,(),4)}, {(1,(5,8)(6,9),1)} ]

# PrincipalFactor
gap> D:=Filtered(DClasses(V), IsRegularClass)[2];
{(13,(1,6)(5,8),3)}
gap> inj:=InjectionPrincipalFactor(D);; inv:=InverseGeneralMapping(inj);;
gap> ForAll(D, x-> (x^inj)^inv=x);                                         
true
gap> ForAll(D, x-> ForAll(D, y-> (not x*y in D) or (x*y)^inj=x^inj*y^inj));
true

# SmallGeneratingSet
gap> Length(SmallGeneratingSet(V))<Length(Generators(V));
true
gap> Apply(U, x-> Semigroup(SmallGeneratingSet(x)));

#MinimalIdeal
gap> MinimalIdeal(V);
<subsemigroup of 26x5 Rees 0-matrix semigroup with 1 generator>
gap> List(U, MinimalIdeal);
[ <subsemigroup of 26x5 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 1 generator> ]

#IsomorphismPermGroup
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
MappingByFunction( <subsemigroup of 26x5 Rees 0-matrix semigroup 
 with 1 generator>, Group(()), function( x ) ... end, function( x ) ... end )
gap> inv:=InverseGeneralMapping(iso);
MappingByFunction( Group(()), <subsemigroup of 26x5 Rees 0-matrix semigroup 
 with 1 generator>, function( x ) ... end, function( x ) ... end )
gap> ForAll(MinimalIdeal(V), x-> (x^iso)^inv=x);         
true
gap> ForAll(MinimalIdeal(V), x-> ForAll(MinimalIdeal(V), y-> 
> (x*y)^iso=x^iso*y^iso));
true

#GroupOfUnits
gap> R:=Semigroup(Generators(R));
<subsemigroup of 1x1 Rees 0-matrix semigroup with 3 generators>
gap> GroupOfUnits(R);
<subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>
gap> GroupOfUnits(V);
fail
gap> GroupOfUnits(U[5]);
fail

#IdempotentGeneratedSubsemigroup
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

#IrredundantGeneratingSubset
gap> IrredundantGeneratingSubset(V);
[ (1,(1,6),3), (2,(1,6),5), (6,(1,9)(5,8),1), (7,(1,6)(5,8),3), (11,(1,9),5), 
  (13,(1,6)(5,8),3), (15,(1,9)(5,8),2), (22,(1,6),1), (23,(5,8),2), 
  (24,(1,6)(5,8),4) ]
gap> Length(last);
10
gap> U[1]=Semigroup(IrredundantGeneratingSubset(U[1]));
true

# from greens.xml...

# MultiplicativeNeutralElement (for an H-class)
gap> H:=First(HClasses(V), IsRegularClass);
{0}
gap> MultiplicativeNeutralElement(H);
0
gap> H:=First(HClasses(V), x-> not IsRegularClass(x));
{(23,(6,9),3)}
gap> MultiplicativeNeutralElement(H);
fail
gap> H:=First(HClasses(U[5]), IsRegularClass);
{(12,(1,6)(5,8),2)}
gap> e:=MultiplicativeNeutralElement(H);
(12,(1,6)(5,8),2)
gap> e^2;
(12,(1,6)(5,8),2)
gap> ForAll(H, x-> x*e=x and e*x=x);
true
gap> H:=First(HClasses(U[5]), x-> not IsRegularClass(x));
{(12,(),1)}
gap> MultiplicativeNeutralElement(H);
fail

# StructureDescription (for an H-class) 
gap> H:=First(HClasses(U[5]), IsRegularClass);;
gap> StructureDescription(H);
"D12"

# Random
gap> Random(V);;
gap> List(U, Random);;
gap> ForAll([1..5], x-> last[x] in U[x]);
true

# DClassOf.Class etc
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

# DClasses etc...
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
[ (6,(),4), (24,(1,9),4), (9,(),3), (11,(),4), (7,(),4), (15,(1,9),4), 
  (7,(1,9),3), (18,(1,9),4), (16,(1,9),3), (1,(1,9),4), (19,(),3), 
  (1,(1,6),3), (15,(),3), (6,(),3), (11,(1,9),3), (21,(),3), 
  (2,(1,9,6)(5,8),4), (20,(1,9),4), (12,(1,9),3), (10,(1,9),3), (20,(1,9),3), 
  (26,(),3), (13,(1,9),4), (8,(1,9),4), (22,(1,9),4), (4,(),4), (8,(1,9),3), 
  (18,(),3), (17,(),4), (23,(),4), (22,(1,9),3), (5,(1,9),4), (25,(1,9),4), 
  (3,(1,9),4), (13,(1,9),3), (14,(1,9),4), (14,(1,9),3), 0, (12,(1,6),4), 
  (10,(1,6),4), (9,(1,6),4), (16,(1,6),4), (19,(1,6),4), (21,(1,6),4), 
  (26,(1,6),4) ]
gap> LClassReps(U[3]);
[ (3,(),2), (3,(5,8)(6,9),4), (3,(1,9,6)(5,8),3), (3,(5,8)(6,9),5), 
  (1,(5,8)(6,9),1), (1,(5,8)(6,9),2), (1,(),4), (1,(1,9),3), (1,(),5), 0 ]
gap> DClassReps(U[4]);
[ (2,(),2), (10,(),1), (11,(),1), 0 ]

# MultiplicativeZero
gap> List(U, MultiplicativeZero);
[ 0, 0, 0, 0, 0, 0 ]
gap> ForAll(last, IsIdempotent);
true

# GroupHClass
gap> D:=Filtered(DClasses(U[1]), IsRegularClass)[2];
{(11,(),2)}
gap> GroupHClass(D);
{(11,(1,6),2)}
gap> StructureDescription(last);
"D12"
gap> D:=First(DClasses(V), IsRegularClass);         
{0}
gap> GroupHClass(D);
{0}
gap> StructureDescription(last);
"1"

# Idempotents
gap> Idempotents(V);
[ 0, (13,(1,9,6),3), (13,(1,9,6),2), (7,(),3), (7,(),2), (23,(1,6),5), 
  (23,(1,6),4), (22,(),1), (11,(1,6),3), (11,(1,6),2), (2,(5,8)(6,9),5), 
  (2,(5,8)(6,9),4), (24,(),5), (24,(),4), (6,(1,6),1) ]
gap> ForAll(last, IsIdempotent);
true
gap> Idempotents(U[2]);
[ (24,(),4), (24,(),5), (9,(1,9),3), (9,(1,9),2), (7,(),3), (7,(),2), 
  (16,(1,9,6)(5,8),3), (16,(1,9,6)(5,8),2), (19,(1,9,6)(5,8),3), 
  (19,(1,9,6)(5,8),2), (1,(),1), (15,(1,9),1), (6,(1,6),1), (11,(1,6),3), 
  (11,(1,6),2), (21,(1,9,6)(5,8),3), (21,(1,9,6)(5,8),2), (2,(5,8)(6,9),4), 
  (2,(5,8)(6,9),5), (12,(1,6)(5,8),3), (12,(1,6)(5,8),2), (10,(),1), 
  (20,(),3), (20,(),2), (26,(1,9),3), (26,(1,9),2), (4,(),4), (4,(),5), 
  (8,(1,9)(5,8),3), (8,(1,9)(5,8),2), (18,(1,9,6)(5,8),3), 
  (18,(1,9,6)(5,8),2), (17,(1,9)(5,8),4), (17,(1,9)(5,8),5), (23,(1,6),4), 
  (23,(1,6),5), (22,(),1), (5,(5,8)(6,9),4), (5,(5,8)(6,9),5), (25,(),4), 
  (25,(),5), (3,(5,8)(6,9),4), (3,(5,8)(6,9),5), (13,(1,9,6),3), 
  (13,(1,9,6),2), (14,(5,8)(6,9),1), 0 ]
gap> ForAll(last, IsIdempotent);
true

# IsRegularClass
gap> Number(RClasses(V), IsRegularClass);
9
gap> Number(DClasses(V), IsRegularClass);
3
gap> NrRegularDClasses(V);
3
gap> Number(DClasses(U[4]), IsRegularClass);
3
gap> NrRegularDClasses(U[4]);
3
gap> Number(LClasses(U[4]), IsRegularClass);
6

# NrIdempotents
gap> NrIdempotents(V)=Length(Idempotents(V));
true
gap> ForAll(U, x-> NrIdempotents(x)=Length(Idempotents(x)));
true
gap> List(DClasses(V), NrIdempotents);
[ 1, 12, 0, 2, 0, 0 ]
gap> List(RClasses(V), NrIdempotents);
[ 1, 2, 0, 2, 2, 1, 2, 2, 2, 1, 0, 0, 0 ]
gap> List(LClasses(U[3]), NrIdempotents);
[ 12, 8, 12, 8, 6, 0, 0, 0, 0, 1 ]

# PartialOrderOfDClasses
gap> PartialOrderOfDClasses(V);
[ [ 1 ], [ 1, 2, 3, 5 ], [ 1, 3, 6 ], [ 1, 4, 6 ], [ 1, 5, 6 ], [ 1, 6 ] ]
gap> PartialOrderOfDClasses(U[1]); 
[ [ 1, 3, 4 ], [ 2, 3, 4 ], [ 3, 4 ], [ 4 ] ]
gap> PartialOrderOfDClasses(U[2]);
[ [ 1, 4 ], [ 1, 2, 4 ], [ 1, 3, 4 ], [ 4 ] ]

# from properties.xml...
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
[ false, false, false, false, false, false ]
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
[ false, false, false, false, false, false ]
gap> IsRegularSemigroup(V);      
false
gap> UU:=IdempotentGeneratedSubsemigroup(U[3]);  
<subsemigroup of 26x5 Rees 0-matrix semigroup with 27 generators>
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

# from semigroups.xml ...
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

# from slp.xml...
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
[ 15, 8, 7, 1, 12 ]
gap> EvaluateWord(Generators(U[4]), last);
(26,(6,9),5)

#
#
gap> SemigroupsStopTest();
gap> STOP_TEST( "Semigroups package: reesmat.tst", 10000);
