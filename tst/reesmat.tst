#############################################################################
##
#W  reesmat.tst
#Y  Copyright (C) 2011-13                               James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

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
gap> SmallGeneratingSet(V);
[ (23,(5,8),2), (1,(1,6),3), (6,(1,9)(5,8),1), (7,(1,6)(5,8),3), 
  (22,(1,9),1), (2,(1,6),5), (11,(1,9),5), (24,(1,6)(5,8),4), 
  (15,(1,9)(5,8),2), (13,(1,6)(5,8),3) ]
gap> Length(last);
10
gap> Apply(U, x-> Semigroup(SmallGeneratingSet(x)));
gap> U;
[ <subsemigroup of 26x5 Rees 0-matrix semigroup with 32 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 33 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 31 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 33 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 29 generators>, 
  <subsemigroup of 26x5 Rees 0-matrix semigroup with 36 generators> ]



