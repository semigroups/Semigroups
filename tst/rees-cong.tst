###########################################################################
##
#W  rees-cong.tst
#Y  Copyright (C) 2015                                      Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: rees-cong.tst");
gap> LoadPackage( "semigroups", false );;

# Set info levels and user preferences
gap> SemigroupsStartTest();

#T# ReesCongTest1
# Test whether a congruence is Rees and find its ideal
gap> s := Semigroup([Transformation([2,3,4,3,1,1]),
>                    Transformation([6,4,4,4,6,1])]);;
gap> i := SemigroupIdeal(s, Transformation([4,4,4,4,4,2]),
>                           Transformation([3,3,3,3,3,2]));;
gap> cong := SemigroupCongruence( s,
> [ [ Transformation( [ 4, 4, 4, 4, 4, 2 ] ), Transformation( [ 4, 4, 4, 4, 4, 4 ] ) ], 
>   [ Transformation( [ 3, 3, 3, 3, 3, 2 ] ), Transformation( [ 4, 4, 4, 4, 4, 4 ] ) ], 
>   [ Transformation( [ 4, 3, 3, 3, 4, 3 ] ), Transformation( [ 4, 4, 4, 4, 4, 4 ] ) ], 
>   [ Transformation( [ 3, 3, 3, 3, 4, 4 ] ), Transformation( [ 4, 4, 4, 4, 4, 4 ] ) ], 
>   [ Transformation( [ 4, 4, 4, 4, 4, 3 ] ), Transformation( [ 3, 3, 3, 3, 3, 3 ] ) ] ]);;
gap> IsReesCongruence(cong);
true
gap> SemigroupIdealOfReesCongruence(cong) = i;
true
gap> cong := SemigroupCongruence( s, [] );;
gap> IsReesCongruence(cong);
false
gap> s := Semigroup([PartialPerm([1,2,3],[1,2,3]),
>                    PartialPerm([1,2,3,4],[2,4,3,5])]);;
gap> cong := SemigroupCongruence( s, [] );;
gap> IsReesCongruence(cong);
true
gap> cong := SemigroupCongruence(s, [PartialPerm([1,2,3],[2,4,3]),
>                                    PartialPerm([1,2,3,4],[2,4,3,5])] );;
gap> IsReesCongruence(cong);
false

#T# ReesCongTest2
# Create a congruence, calculate its congruence classes and try some operations
gap> s := Semigroup( [ Transformation( [ 2, 4, 3, 5, 5 ] ),
>                 Transformation( [ 3, 1, 1, 4, 4 ] ),
>                 Transformation( [ 3, 1, 4, 2, 4 ] ), 
>                 Transformation( [ 3, 4, 2, 3, 4 ] ),
>                 Transformation( [ 4, 1, 5, 1, 2 ] ) ] );
<transformation semigroup of degree 5 with 5 generators>
gap> i := SemigroupIdeal(s, [ Transformation( [ 3, 1, 1, 4, 4 ] ),
>                             Transformation( [ 1, 4, 1, 4, 1 ] ) ] );
<regular transformation semigroup ideal of degree 5 with 2 generators>
gap> cong := ReesCongruenceOfSemigroupIdeal(i);
<Rees congruence of <regular transformation semigroup ideal of degree 5 with
  2 generators> over <transformation semigroup of degree 5 with 5 generators>>
gap> NrCongruenceClasses(cong);
19
gap> cc := CongruenceClasses(cong);;
gap> Size(cc);
19
gap> List(cc, Size);
[ 1095, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> cc[1] * cc[1];
{Transformation( [ 3, 1, 1, 4, 4 ] )}
gap> cc[2] * cc[1];
{Transformation( [ 3, 1, 1, 4, 4 ] )}
gap> cc[4] * cc[6];
{Transformation( [ 2, 4, 1, 3, 1 ] )}
gap> cc[3] * cc[2] = cc[8];
true

#T# ReesCongTest3
# Convert a congruence to generating pairs
gap> s := Semigroup( [ Transformation( [ 1, 3, 2, 4, 3 ] ),
>                      Transformation( [ 1, 3, 5, 5, 3 ] ),
>                      Transformation( [ 5, 1, 2, 5, 5 ] ) ] );;
gap> i := SemigroupIdeal( s, Transformation( [ 5, 2, 1, 5, 2 ] ),
>                            Transformation( [ 5, 2, 1, 5, 2 ] ) );;
gap> cong := ReesCongruenceOfSemigroupIdeal(i);;
gap> ccong := AsSemigroupCongruenceByGeneratingPairs(cong);
<semigroup congruence over <transformation semigroup of size 61, degree 5 
 with 3 generators> with 1 generating pairs>
gap> NrCongruenceClasses(ccong);
12
gap> IsReesCongruence(ccong);
true
gap> SemigroupIdealOfReesCongruence(ccong) = i;
true

#T# ReesCongTest4
# Test the \in function
gap> s := Semigroup( [ Transformation( [ 2, 4, 3, 5, 5 ] ),
>                 Transformation( [ 3, 1, 1, 4, 4 ] ),
>                 Transformation( [ 3, 1, 4, 2, 4 ] ), 
>                 Transformation( [ 3, 4, 2, 3, 4 ] ),
>                 Transformation( [ 4, 1, 5, 1, 2 ] ) ] );;
gap> i := SemigroupIdeal(s, [ Transformation( [ 3, 1, 1, 4, 4 ] ),
>                             Transformation( [ 1, 4, 1, 4, 1 ] ) ] );;
gap> cong := ReesCongruenceOfSemigroupIdeal(i);;
gap> x := Transformation( [ 3, 4, 2, 4 ] );;      # not in i
gap> y := Transformation( [ 1, 5, 5, 5, 4 ] );;   # in i
gap> z := Transformation( [ 5, 5, 1, 1, 3 ] );;   # not even in s
gap> [x,y] in cong;
false
gap> [x,x] in cong;
true
gap> [x,y,y] in cong;
Error, Semigroups: in: usage,
the first arg <pair> must be a list of length 2,
gap> [x,z] in cong;
Error, Semigroups: in: usage,
the elements of 1st arg <pair> must be in the range of 2nd arg <cong>,
gap> t := Transformation( [ 1, 3, 4, 1, 4 ] );;   # in i
gap> [t,y] in cong;
true
gap> [x,x] in cong;
true
gap> im := ImagesElm(cong, t);;
gap> Size(im) = Size(i);
true
gap> ForAll(im, x-> x in i);
true
gap> im := ImagesElm(cong, x);;
gap> Size(im);
1
gap> ImagesElm(cong, z);
Error, Semigroups: ImagesElm: usage,
the args <cong> and <elm> must refer to the same semigroup,
gap> yclass := CongruenceClassOfElement(cong, y);;
gap> x in yclass;
false
gap> tclass := CongruenceClassOfElement(cong, t);;
gap> y in tclass;
true
gap> CongruenceClassOfElement(cong, z);
Error, Semigroups: EquivalenceClassOfElement: usage,
the second arg <elm> must be in the semigroup of first arg <cong>,
gap> xclass := CongruenceClassOfElement(cong, x);
{Transformation( [ 3, 4, 2, 4 ] )}
gap> x in xclass;
true
gap> xclass * yclass = tclass;
true
gap> yclass * xclass = yclass;
true
gap> xxclass := CongruenceClassOfElement(cong, x*x);;
gap> xclass * xclass = xxclass;
true

#T# ReesCongTest5
# Join some congruences together
gap> s := Semigroup( [ Transformation( [ 1, 1, 3, 1, 3 ] ),
>                      Transformation( [ 2, 1, 2, 2, 2 ] ),
>                      Transformation( [ 3, 1, 3, 2, 4 ] ) ] );;
gap> i := SemigroupIdeal(s, Transformation( [ 1, 1, 1, 3, 1 ] ));;
gap> j := SemigroupIdeal(s, Transformation( [ 3, 3, 3, 3, 1 ] ));;
gap> ci := ReesCongruenceOfSemigroupIdeal(i);;
gap> cj := ReesCongruenceOfSemigroupIdeal(j);;
gap> cc := JoinSemigroupCongruences(ci, cj);;
gap> NrCongruenceClasses(ci); NrCongruenceClasses(cj); NrCongruenceClasses(cc);
16
17
15
gap> k := SemigroupIdeal( FullTransformationMonoid(5),
>                         Transformation( [ 3, 2, 5, 4, 2 ] ) );;
gap> ck := ReesCongruenceOfSemigroupIdeal(k);;
gap> JoinSemigroupCongruences(ci, ck);
Error, Semigroups: JoinSemigroupCongruences: usage,
the args <c1> and <c2> must be congruences of the same semigroup,

#T# ReesCongTest6: Enumerator
gap> s := Semigroup( [ Transformation( [ 1, 1, 3, 1, 3 ] ),
>                      Transformation( [ 2, 1, 2, 2, 2 ] ),
>                      Transformation( [ 3, 1, 3, 2, 4 ] ) ] );;
gap> i := SemigroupIdeal(s, Transformation( [ 1, 1, 1, 3, 1 ] ));;
gap> cong := ReesCongruenceOfSemigroupIdeal(i);;
gap> class := CongruenceClassOfElement(cong, Transformation([3,3,3,3,3]));;
gap> enum := Enumerator(class);
<enumerator of <non-regular transformation semigroup ideal of size 5, 
 degree 5 with 1 generator>>
gap> Size(enum);
5
gap> class := CongruenceClassOfElement(cong, Transformation([2,2,2,1,2]));;
gap> enum := Enumerator(class);
[ Transformation( [ 2, 2, 2, 1, 2 ] ) ]
gap> Size(enum);
1

#E#
gap> STOP_TEST( "Semigroups package: rees-cong.tst");
