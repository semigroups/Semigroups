############################################################################
##
#W  standard/attributes/maximal.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell
##                                                          Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local D, G, M, R, S, T, U, an, contain, correct, gens, mat, max, number
#@local types, x, zero
gap> START_TEST("Semigroups package: standard/attributes/maximal.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

# maximal: IsMaximalSubsemigroup, 1
gap> S := Semigroup([
>   Transformation([1, 2, 4, 4]),
>   Transformation([4, 4, 1, 4]),
>   Transformation([2, 1, 4, 2])]);
<transformation semigroup of degree 4 with 3 generators>
gap> T := Semigroup([
> Transformation([2, 1, 4, 2]),
> Transformation([2, 1, 2, 2]),
> Transformation([4, 4, 1, 4])]);
<transformation semigroup of degree 4 with 3 generators>
gap> IsMaximalSubsemigroup(S, T);  # maximal
true
gap> U := Semigroup([
>   Transformation([2, 2, 1, 1]),
>   Transformation([2, 2, 3, 4]),
>   Transformation([3, 4, 2, 4])]);
<transformation semigroup of degree 4 with 3 generators>
gap> IsSubsemigroup(S, U);
false
gap> IsMaximalSubsemigroup(S, U);  # not a subsemigroup
false
gap> IsSubsemigroup(U, S);
false
gap> IsMaximalSubsemigroup(U, S);  # not a subsemigroup
false
gap> IsSubsemigroup(S, S);
true
gap> S <> S;
false
gap> IsMaximalSubsemigroup(S, S);  # equal semigroups
false
gap> IsMaximalSubsemigroup(S,
> Semigroup(RepresentativeOfMinimalIdeal(S)));  # non-maximal
false
gap> IsMaximalSubsemigroup(FreeSemigroup(1), FreeSemigroup(1));
false
gap> IsMaximalSubsemigroup(FreeSemigroup(1),
>                          Subsemigroup(FreeSemigroup(1), []));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `IsMaximalSubsemigroup' on 2 arguments

# maximal: MaximalSubsemigroups, error checking, 1
gap> S := FreeSemigroup(1);;
gap> MaximalSubsemigroups(S);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `MaximalSubsemigroupsNC' on 2 arguments
gap> MaximalSubsemigroups(S, rec());
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `MaximalSubsemigroups' on 2 arguments
gap> S := TrivialSemigroup(IsTransformationSemigroup);;
gap> T := FullTransformationMonoid(2);;
gap> x := S.1;
IdentityTransformation
gap> MaximalSubsemigroups(S, rec(number := 1));
Error, the record component <number> of the optional 2nd argument <r> should b\
e true or false
gap> MaximalSubsemigroups(S, rec(number := false));
[  ]
gap> MaximalSubsemigroups(S, rec(contain := 1));
Error, the record component <contain> of the optional 2nd argument <r> should \
be a duplicate-free list of elements of the semigroup in the 1st argument, <S>
gap> MaximalSubsemigroups(S, rec(contain := [0, Group(())]));
Error, the record component <contain> of the optional 2nd argument <r> should \
be a duplicate-free list of elements of the semigroup in the 1st argument, <S>
gap> MaximalSubsemigroups(S, rec(contain := [0, 0]));
Error, the record component <contain> of the optional 2nd argument <r> should \
be a duplicate-free list of elements of the semigroup in the 1st argument, <S>
gap> MaximalSubsemigroups(S, rec(contain := [0]));
Error, the record component <contain> of the optional 2nd argument <r> should \
be a duplicate-free list of elements of the semigroup in the 1st argument, <S>
gap> MaximalSubsemigroups(S, rec(contain := [Transformation([2, 1])]));
Error, the record component <contain> of the optional 2nd argument <r> should \
be a duplicate-free list of elements of the semigroup in the 1st argument, <S>
gap> MaximalSubsemigroups(S, rec(contain := [x]));
[  ]
gap> MaximalSubsemigroups(S, rec(D := 0));
Error, the record component <D> of the optional 2nd argument <r> should be a D\
-class of the semigroup in the 1st argument, <S>
gap> MaximalSubsemigroups(S, rec(D := HClass(S, x)));
Error, the record component <D> of the optional 2nd argument <r> should be a D\
-class of the semigroup in the 1st argument, <S>
gap> MaximalSubsemigroups(S, rec(D := DClass(T, x)));
Error, the record component <D> of the optional 2nd argument <r> should be a D\
-class of the semigroup in the 1st argument, <S>
gap> MaximalSubsemigroups(S, rec(D := DClass(S, x)));
[  ]
gap> MaximalSubsemigroups(S, rec(gens := 0));
Error, the record component <gens> of the optional 2nd argument <r> should be \
true or false
gap> MaximalSubsemigroups(S, rec(gens := true));
[  ]
gap> MaximalSubsemigroups(S, rec(types := 0));
[  ]
gap> R := ReesMatrixSemigroup(FullTransformationMonoid(2), [[x]]);;
gap> MaximalSubsemigroups(R, rec(types := 0));
[ <subsemigroup of 1x1 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 1x1 Rees matrix semigroup with 3 generators> ]
gap> R := ReesZeroMatrixSemigroup(S, [[x, 0]]);;
gap> MaximalSubsemigroups(R, rec(types := 0));
[ <Rees 0-matrix semigroup ideal with 2 generators>, 
<subsemigroup of 2x1 Rees 0-matrix semigroup with 2 generators> ]
gap> R := ReesMatrixSemigroup(S, [[x]]);;
gap> MaximalSubsemigroups(R, rec(types := 0));
Error, the record component <types> of the optional 2nd argument <r> should be\
 a subset of [ 1 .. 6 ]
gap> MaximalSubsemigroups(R, rec(types := [0, Group(())]));
Error, the record component <types> of the optional 2nd argument <r> should be\
 a subset of [ 1 .. 6 ]
gap> MaximalSubsemigroups(R, rec(types := [0, 0]));
Error, the record component <types> of the optional 2nd argument <r> should be\
 a subset of [ 1 .. 6 ]
gap> MaximalSubsemigroups(R, rec(types := [0]));
Error, the record component <types> of the optional 2nd argument <r> should be\
 a subset of [ 1 .. 6 ]
gap> MaximalSubsemigroups(R, rec(types := [1]));
[  ]
gap> MaximalSubsemigroups(R, rec(types := [3, 4, 6]));
[  ]

# maximal: MaximalSubsemigroups, for a Rees matrix semigroup, 1

# Easy example, 2x1
gap> R := ReesMatrixSemigroup(SymmetricGroup(3), [[(), ()]]);
<Rees matrix semigroup 2x1 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R);
[ <Rees matrix semigroup 1x1 over Sym( [ 1 .. 3 ] )>, 
  <Rees matrix semigroup 1x1 over Sym( [ 1 .. 3 ] )>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 2 generators> ]

# Example example, 1x2
gap> R := ReesMatrixSemigroup(SymmetricGroup(3), [[()], [()]]);
<Rees matrix semigroup 1x2 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R);
[ <Rees matrix semigroup 1x1 over Sym( [ 1 .. 3 ] )>, 
  <Rees matrix semigroup 1x1 over Sym( [ 1 .. 3 ] )>, 
  <subsemigroup of 1x2 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 1x2 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 1x2 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 1x2 Rees matrix semigroup with 2 generators> ]

# Easy example, 2x2
gap> R := ReesMatrixSemigroup(SymmetricGroup(3), [[(), ()], [(), ()]]);
<Rees matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R);
[ <Rees matrix semigroup 2x1 over Sym( [ 1 .. 3 ] )>, 
  <Rees matrix semigroup 2x1 over Sym( [ 1 .. 3 ] )>, 
  <Rees matrix semigroup 1x2 over Sym( [ 1 .. 3 ] )>, 
  <Rees matrix semigroup 1x2 over Sym( [ 1 .. 3 ] )>, 
  <subsemigroup of 2x2 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 2x2 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 2x2 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 2x2 Rees matrix semigroup with 2 generators> ]

# Easy example, 1x1
gap> R := ReesMatrixSemigroup(SymmetricGroup(2), [[()]]);
<Rees matrix semigroup 1x1 over Sym( [ 1 .. 2 ] )>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees matrix semigroup with 1 generator> ]

# maximal: MaximalSubsemigroups, for a Rees matrix semigroup, 2
gap> S := FreeSemigroup(1);;
gap> R := ReesMatrixSemigroup(S, [[S.1]]);  # not finite
<Rees matrix semigroup 1x1 over <free semigroup on the generators [ s1 ]>>
gap> MaximalSubsemigroups(R);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `MaximalSubsemigroupsNC' on 2 arguments
gap> S := FullTransformationMonoid(2);;  # not over a group
gap> R := ReesMatrixSemigroup(S, [[One(S)]]);
<Rees matrix semigroup 1x1 over <full transformation monoid of degree 2>>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 1x1 Rees matrix semigroup with 3 generators> ]

# maximal: MaximalSubsemigroups, for a Rees matrix semigroup, 3

# no options specified - default options chosen
gap> R := ReesMatrixSemigroup(Group(()), [[()]]);
<Rees matrix semigroup 1x1 over Group(())>
gap> MaximalSubsemigroupsNC(R, rec());
[  ]

# maximal: MaximalSubsemigroups, for a Rees matrix semigroup, 4

# over a non-IsGroup group
gap> S := GroupOfUnits(FullTransformationMonoid(3));
<transformation group of degree 3 with 2 generators>
gap> R := ReesMatrixSemigroup(S, [[One(S)], [One(S)]]);;
gap> MaximalSubsemigroups(R, rec(types := [6]));
[ <subsemigroup of 1x2 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 1x2 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 1x2 Rees matrix semigroup with 2 generators>, 
  <subsemigroup of 1x2 Rees matrix semigroup with 2 generators> ]

# maximal: MaximalSubsemigroups, for a Rees matrix semigroup, 5

# Specifying various options
gap> R := ReesMatrixSemigroup(SymmetricGroup(2), [[(), (1, 2)], [(), (1, 2)]]);
<Rees matrix semigroup 2x2 over Sym( [ 1 .. 2 ] )>
gap> MaximalSubsemigroups(R, rec(D := DClasses(R)[1]));  # <opts.D> is specified
[ <Rees matrix semigroup 2x1 over Sym( [ 1 .. 2 ] )>, 
  <Rees matrix semigroup 2x1 over Sym( [ 1 .. 2 ] )>, 
  <Rees matrix semigroup 1x2 over Sym( [ 1 .. 2 ] )>, 
  <Rees matrix semigroup 1x2 over Sym( [ 1 .. 2 ] )>, 
  <subsemigroup of 2x2 Rees matrix semigroup with 2 generators> ]
gap> MaximalSubsemigroups(R, rec(types := [6]));  # <opts.types> is specified
[ <subsemigroup of 2x2 Rees matrix semigroup with 2 generators> ]
gap> x := [RMSElement(R, 1, (), 1),
>          RMSElement(R, 1, (1, 2), 1)];
[ (1,(),1), (1,(1,2),1) ]
gap> MaximalSubsemigroups(R, rec(contain := x));  # <opts.contain> is specified
[ <Rees matrix semigroup 2x1 over Sym( [ 1 .. 2 ] )>, 
  <Rees matrix semigroup 1x2 over Sym( [ 1 .. 2 ] )> ]
gap> Add(x, RMSElement(R, 1, (), 2));
gap> MaximalSubsemigroups(R, rec(contain := x));
[ <Rees matrix semigroup 1x2 over Sym( [ 1 .. 2 ] )> ]
gap> Add(x, RMSElement(R, 2, (), 1));
gap> MaximalSubsemigroups(R, rec(contain := x));
[  ]
gap> MaximalSubsemigroups(R, rec(contain := x, number := true));  # <opts.number>
0

# maximal: MaximalSubsemigroups, for a Rees matrix semigroup, 6
gap> R := ReesMatrixSemigroup(Group([(1, 2), (3, 4)]), [[(), ()], [(), ()]]);
<Rees matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>
gap> NrMaximalSubsemigroups(R);
7
gap> Length(MaximalSubsemigroupsNC(R, rec()));
7
gap> MaximalSubsemigroups(R, rec(contain := [RMSElement(R, 1, (1, 2), 1)]));
[ <Rees matrix semigroup 2x1 over Group([ (1,2), (3,4) ])>, 
  <Rees matrix semigroup 1x2 over Group([ (1,2), (3,4) ])>, 
  <subsemigroup of 2x2 Rees matrix semigroup with 2 generators> ]
gap> MaximalSubsemigroups(R, rec(contain := [RMSElement(R, 1, (1, 2), 1)],
>                                gens := true));
[ [ (1,(1,2),1), (1,(3,4),1), (2,(),1) ], 
  [ (1,(1,2),1), (1,(3,4),1), (1,(),2) ], [ (1,(1,2),1), (2,(),2) ] ]

# maximal: MaximalSubsemigroups, for a Rees matrix subsemigroup, 1

# IsReesMatrixSemigroup
gap> R := ReesMatrixSemigroup(SymmetricGroup(3), [[(), (1, 2)]]);;
gap> U := Semigroup(RMSElement(R, 1, (2, 3), 1),
>                   RMSElement(R, 1, (1, 3, 2), 1));
<subsemigroup of 2x1 Rees matrix semigroup with 2 generators>
gap> IsReesMatrixSemigroup(U);
true
gap> MaximalSubsemigroups(U);
[ <subsemigroup of 2x1 Rees matrix semigroup with 1 generator>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 1 generator>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 1 generator>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 1 generator> ]

# maximal: MaximalSubsemigroups, for a Rees matrix subsemigroup, 2

# not IsReesMatrixSemigroup
gap> U := Semigroup(RMSElement(R, 1, (), 1), RMSElement(R, 2, (1, 2), 1));
<subsemigroup of 2x1 Rees matrix semigroup with 2 generators>
gap> IsReesMatrixSemigroup(U);
false
gap> MaximalSubsemigroups(U);  # not IsReesMatrixSemigroup
[ <subsemigroup of 2x1 Rees matrix semigroup with 1 generator>, 
  <subsemigroup of 2x1 Rees matrix semigroup with 1 generator> ]

# maximal: MaximalSubsemigroups, for a Rees matrix subsemigroup, 3

# IsReesMatrixSemigroup, but different rows and columns
gap> R := ReesMatrixSemigroup(Group((1, 2)),
> [[(), (), ()],
> [(), (), ()],
> [(), (), ()],
> [(), (), ()]]);;
gap> U := ReesMatrixSubsemigroup(R, [2, 3], Group((1, 2)), [2 .. 4]);
<Rees matrix semigroup 2x3 over Group([ (1,2) ])>
gap> MaximalSubsemigroups(U);
[ <Rees matrix semigroup 2x2 over Group([ (1,2) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2) ])>, 
  <Rees matrix semigroup 2x2 over Group([ (1,2) ])>, 
  <Rees matrix semigroup 1x3 over Group([ (1,2) ])>, 
  <Rees matrix semigroup 1x3 over Group([ (1,2) ])>, 
  <subsemigroup of 3x4 Rees matrix semigroup with 3 generators> ]

# maximal: MaximalSubsemigroups, for a Rees 0-matrix semigroup, 1

# Easy example, 2x1
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[(), ()]]);
<Rees 0-matrix semigroup 2x1 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 2x1 Rees 0-matrix semigroup with 3 generators>, 
  <Rees 0-matrix semigroup 1x1 over Group([ (2,3), (1,2) ])>, 
  <Rees 0-matrix semigroup 1x1 over Group([ (2,3), (1,2) ])>, 
  <subsemigroup of 2x1 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 2x1 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 2x1 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 2x1 Rees 0-matrix semigroup with 4 generators> ]

# Example example, 1x2
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[()], [()]]);
<Rees 0-matrix semigroup 1x2 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x2 Rees 0-matrix semigroup with 3 generators>, 
  <Rees 0-matrix semigroup 1x1 over Group([ (2,3), (1,2) ])>, 
  <Rees 0-matrix semigroup 1x1 over Group([ (2,3), (1,2) ])>, 
  <subsemigroup of 1x2 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 1x2 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 1x2 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 1x2 Rees 0-matrix semigroup with 4 generators> ]

# Easy example, 2x2
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[(), ()], [(), ()]]);
<Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 4 generators>, 
  <Rees 0-matrix semigroup 2x1 over Group([ (2,3), (1,2) ])>, 
  <Rees 0-matrix semigroup 2x1 over Group([ (2,3), (1,2) ])>, 
  <Rees 0-matrix semigroup 1x2 over Group([ (2,3), (1,2) ])>, 
  <Rees 0-matrix semigroup 1x2 over Group([ (2,3), (1,2) ])>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators> ]

# Easy example, 1x1
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(2), [[()]]);
<Rees 0-matrix semigroup 1x1 over Sym( [ 1 .. 2 ] )>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators> ]

# maximal: MaximalSubsemigroups, for a Rees 0-matrix semigroup, 2

# 2-element semilattice
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> MaximalSubsemigroupsNC(R, rec());
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator> ]
gap> MaximalSubsemigroups(R, rec(types := [3 .. 5]));
[  ]
gap> MaximalSubsemigroups(R, rec(types := [1, 2]));
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator> ]
gap> MaximalSubsemigroups(R, rec(types := [1, 2], gens := true));
[ [ 0 ], [ (1,(),1) ] ]
gap> MaximalSubsemigroupsNC(R, rec(gens := true, zero := false));
[ [  ], [ (1,(),1) ] ]
gap> MaximalSubsemigroups(R, rec(number := true, D := DClass(R, R.1)));
1
gap> MaximalSubsemigroups(R, rec(number := true, D := DClass(R, R.2)));
1

# maximal: MaximalSubsemigroups, for a Rees 0-matrix semigroup, 3

# for an infinite Rees 0-matrix semigroup
gap> S := FreeGroup(1);;
gap> R := ReesZeroMatrixSemigroup(S, [[S.1]]);  # not finite
<Rees 0-matrix semigroup 1x1 over <free group on the generators [ f1 ]>>
gap> MaximalSubsemigroups(R);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 3rd choice method found for `MaximalSubsemigroupsNC' on 2 arguments

# for a Rees 0-matrix semigroup over a non-group
gap> S := MonogenicSemigroup(IsTransformationSemigroup, 2, 1);;
gap> R := ReesZeroMatrixSemigroup(S, [[S.1]]);;
gap> MaximalSubsemigroups(R);
[ <Rees 0-matrix semigroup ideal with 3 generators>, 
<subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators> ]

# for a non-regular Rees 0-matrix semigroup
gap> R := ReesZeroMatrixSemigroup(Group(()), [[0]]);;  # not regular
gap> MaximalSubsemigroups(R, rec(number := true));
1

# maximal: MaximalSubsemigroups, for Rees 0-matrix semigroup, 4
gap> R := ReesZeroMatrixSemigroup(Group((1, 2)), [[(), ()]]);
<Rees 0-matrix semigroup 2x1 over Group([ (1,2) ])>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 2x1 Rees 0-matrix semigroup with 2 generators>, 
  <Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>, 
  <Rees 0-matrix semigroup 1x1 over Group([ (1,2) ])>, 
  <subsemigroup of 2x1 Rees 0-matrix semigroup with 3 generators> ]

# maximal: MaximalSubsemigroups, for Rees 0-matrix semigroup, 5
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()], [()]]);
<Rees 0-matrix semigroup 1x2 over Group(())>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x2 Rees 0-matrix semigroup with 2 generators>, 
  <Rees 0-matrix semigroup 1x1 over Group(())>, 
  <Rees 0-matrix semigroup 1x1 over Group(())> ]

# maximal: MaximalSubsemigroups, for Rees 0-matrix semigroup, 6
gap> R := ReesZeroMatrixSemigroup(Group((1, 2)),
> [[(), 0], [0, (1, 2)]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 3 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 3 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 4 generators> ]

# maximal: MaximalSubsemigroups, for Rees 0-matrix semigroup, 7
gap> R := ReesZeroMatrixSemigroup(Group([()]),
> [[(), (), 0, 0], [(), (), (), 0],
>  [0, (), (), ()]]);
<Rees 0-matrix semigroup 4x3 over Group(())>
gap> MaximalSubsemigroups(R);
[ <Rees 0-matrix semigroup 4x2 over Group(())>, 
  <Rees 0-matrix semigroup 4x2 over Group(())>, 
  <Rees 0-matrix semigroup 3x3 over Group(())>, 
  <Rees 0-matrix semigroup 3x3 over Group(())>, 
  <Rees 0-matrix semigroup 3x3 over Group(())>, 
  <Rees 0-matrix semigroup 3x3 over Group(())>, 
  <subsemigroup of 4x3 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 4x3 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 4x3 Rees 0-matrix semigroup with 5 generators> ]

# maximal: MaximalSubsemigroups, for a Rees 0-matrix semigroup, 8
gap> R := ReesZeroMatrixSemigroup(Group([()]),
> [[(), (), 0], [0, 0, ()]]);;
gap> MaximalSubsemigroups(R, rec(types := [5]));
[ <subsemigroup of 3x2 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 3x2 Rees 0-matrix semigroup with 4 generators> ]
gap> R := ReesZeroMatrixSemigroup(Group([()]),
> [[(), 0], [(), 0], [0, ()]]);;
gap> MaximalSubsemigroups(R, rec(types := [5]));
[ <subsemigroup of 2x3 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 2x3 Rees 0-matrix semigroup with 5 generators> ]

# maximal: MaximalSubsemigroups, for a Rees 0-matrix semigroup, 9
gap> S := Semigroup([Transformation([2, 1])]);;
gap> R := ReesZeroMatrixSemigroup(S, [[S.1]]);;
gap> MaximalSubsemigroups(R);
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 2 generators> ]
gap> R := ReesZeroMatrixSemigroup(S, [[S.1, S.1], [0, S.1]]);;
gap> MaximalSubsemigroups(R, rec(types := [5]));
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 2 generators> ]

# maximal: MaximalSubsemigroups, for a Rees 0-matrix semigroup, 10
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[()]]);
<Rees 0-matrix semigroup 1x1 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, rec(types := [6],
> contain := [RMSElement(R, 1, (), 1),
>             RMSElement(R, 1, (1, 2, 3), 1),
>             RMSElement(R, 1, (1, 3, 2), 1)]));
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 3 generators> ]
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[(), 0], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
gap> MaximalSubsemigroups(R, rec(types := [6],
> contain := [RMSElement(R, 2, (), 1), RMSElement(R, 1, (1, 2, 3), 2)]));
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators> ]

# maximal: MaximalSubsemigroups, for a Rees 0-matrix semigroup, 11
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(3),
> [[(), (), (), 0, 0, 0, 0, 0],
> [(), 0, 0, 0, 0, 0, 0, 0],
> [0, 0, 0, (1, 2, 3), (), (), 0, 0],
> [0, 0, 0, (), 0, 0, 0, 0],
> [0, 0, 0, 0, 0, 0, (), ()],
> [0, 0, 0, 0, 0, 0, (), (1, 2)]]);;
gap> MaximalSubsemigroups(R, rec(
> number := true,
> types := [6],
> contain := [RMSElement(R, 1, (), 6), RMSElement(R, 8, (), 4)]));
1
gap> MaximalSubsemigroups(R, rec(
> number := true,
> types := [6],
> contain := [RMSElement(R, 1, (), 6),
>             RMSElement(R, 1, (1, 2, 3), 4),
>             RMSElement(R, 8, (), 4)]));
0

# maximal: MaximalSubsemigroups, for a Rees 0-matrix semigroup, 12
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> MaximalSubsemigroups(R, rec(gens := true));
[ [ 0 ], [ (1,(),1) ] ]
gap> MaximalSubsemigroupsNC(R, rec());
[ <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator>, 
  <subsemigroup of 1x1 Rees 0-matrix semigroup with 1 generator> ]
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2), (3, 4)]),
>                                 [[(), ()], [(), ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2), (3,4) ])>
gap> x := RMSElement(R, 1, (1, 2), 1);;
gap> NrMaximalSubsemigroups(R);
8
gap> MaximalSubsemigroups(R, rec(contain := [x]));
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 4 generators>, 
  <Rees 0-matrix semigroup 2x1 over Group([ (3,4), (1,2) ])>, 
  <Rees 0-matrix semigroup 1x2 over Group([ (3,4), (1,2) ])>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 5 generators> ]
gap> MaximalSubsemigroups(R, rec(D := DClass(R, x), number := true));
7
gap> MaximalSubsemigroups(R, rec(D := DClass(R, MultiplicativeZero(R)),
>                                number := true));
1
gap> MaximalSubsemigroups(R, rec(D := DClass(R, x),
>                                number := true,
>                                types := [2]));
0
gap> MaximalSubsemigroups(R, rec(D := DClass(R, MultiplicativeZero(R)),
>                                number := true,
>                                types := [1, 3, 4, 5, 6]));
0
gap> MaximalSubsemigroups(R, rec(types := [3], gens := true));
[ [ (1,(1,2),2), (1,(3,4),2), (2,(),2), 0 ], 
  [ (1,(1,2),1), (1,(3,4),1), (2,(),1), 0 ] ]
gap> MaximalSubsemigroups(R, rec(types := [4], gens := true));
[ [ (2,(1,2),1), (2,(3,4),1), (2,(),2), 0 ], 
  [ (1,(1,2),1), (1,(3,4),1), (1,(),2), 0 ] ]
gap> R := ReesZeroMatrixSemigroup(Group(()), [[(), 0], [(), ()]]);
<Rees 0-matrix semigroup 2x2 over Group(())>
gap> MaximalSubsemigroups(R, rec(types := [5], gens := false));
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 2 generators> ]
gap> MaximalSubsemigroups(R, rec(types := [5], gens := true));
[ [ (2,(),2), (1,(),1) ] ]
gap> R := ReesZeroMatrixSemigroup(Group((1, 2)), [[(), 0], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Group([ (1,2) ])>
gap> MaximalSubsemigroups(R, rec(types := [5, 6],
>                                contain := [MultiplicativeZero(R),
>                                            RMSElement(R, 2, (), 1)]));
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 3 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 4 generators> ]

# maximal: MaximalSubsemigroups, for a Rees 0-matrix semigroup, 13
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(2), [[(), 0], [0, ()]]);
<Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 2 ] )>
gap> SetIdempotentGeneratedSubsemigroup(R, Semigroup(Idempotents(R)));
gap> MaximalSubsemigroups(R, rec(types := [6]));
[ <subsemigroup of 2x2 Rees 0-matrix semigroup with 4 generators>, 
  <subsemigroup of 2x2 Rees 0-matrix semigroup with 4 generators> ]

# maximal: MaximalSubsemigroups, for Rees 0-matrix subsemigroup, 1
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(2), [[0, 0]]);
<Rees 0-matrix semigroup 2x1 over Sym( [ 1 .. 2 ] )>
gap> U := Semigroup([
> RMSElement(R, 1, (), 1),
> RMSElement(R, 1, (1, 2), 1),
> RMSElement(R, 2, (), 1)]);
<subsemigroup of 2x1 Rees 0-matrix semigroup with 3 generators>
gap> IsReesZeroMatrixSemigroup(U);
false
gap> MaximalSubsemigroups(U);
[ <Rees 0-matrix semigroup ideal with 3 generators>, 
<Rees 0-matrix semigroup ideal with 3 generators>, 
<Rees 0-matrix semigroup ideal with 3 generators> ]

# maximal: MaximalSubsemigroups, for a Rees 0-matrix subsemigroup, 2
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(3),
> [[(), 0, ()],
> [(), (), (1, 2)],
> [0, (1, 3), ()],
> [0, 0, ()]]);
<Rees 0-matrix semigroup 3x4 over Sym( [ 1 .. 3 ] )>
gap> U := ReesZeroMatrixSubsemigroup(R, [2, 3], SymmetricGroup(3), [2 .. 4]);
<Rees 0-matrix semigroup 2x3 over Sym( [ 1 .. 3 ] )>
gap> IsReesZeroMatrixSemigroup(U);
true
gap> MaximalSubsemigroups(U);
[ <Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>, 
  <Rees 0-matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>, 
  <Rees 0-matrix semigroup 2x2 over Group([ (2,3), (1,2) ])>, 
  <Rees 0-matrix semigroup 1x3 over Group([ (2,3), (1,2) ])>, 
  <subsemigroup of 3x4 Rees 0-matrix semigroup with 5 generators>, 
  <subsemigroup of 3x4 Rees 0-matrix semigroup with 5 generators> ]

# maximal: MaximalSubsemigroups, for a permutation group, 1
gap> List(MaximalSubsemigroups(SymmetricGroup(3)), IsGroupAsSemigroup);
[ true, true, true, true ]

# maximal: MaximalSubsemigroups, for a transformation group, 1
gap> MaximalSubsemigroups(Semigroup(Transformation([2, 3, 1])))
> = [TrivialSemigroup()];
true

# maximal: MaximalSubsemigroups, for a transformation semigroup, 1
gap> S := Semigroup([
> Transformation([1, 1, 2]),
> Transformation([1, 1, 1]),
> Transformation([2, 2, 2])]);;
gap> MaximalSubsemigroups(S, rec(
> number := true,
> contain := [Transformation([1, 1, 2]), Transformation([2, 2, 2])]));
0

# maximal: MaximalSubsemigroups, for a transformation semigroup, 2
gap> S := Semigroup([
> Transformation([2, 4, 5, 4, 4, 2]),
> Transformation([4, 1, 4, 4, 3, 3]),
> Transformation([6, 3, 6, 2, 4, 1])]);;
gap> Length(MaximalSubsemigroups(S,
> rec(D := DClass(S, Transformation([2, 4, 5, 4, 4, 2])))));
6

# maximal: MaximalSubsemigroups, for a transformation semigroup, 3
gap> S := Semigroup([
> Transformation([1, 4, 3, 4, 4, 1]),
> Transformation([2, 4, 5, 4, 4, 2]),
> Transformation([4, 3, 5, 5, 1])]);;
gap> Length(MaximalSubsemigroups(S,
> rec(D := DClass(S, Transformation([1, 4, 3, 4, 4, 1])))));
3

# maximal: MaximalSubsemigroups, for a transformation semigroup, 4
gap> S := Semigroup([
> Transformation([2, 4, 5, 4, 4, 2]),
> Transformation([4, 1, 4, 4, 3, 3]),
> Transformation([5, 1, 6, 1, 6, 3])]);;
gap> Length(MaximalSubsemigroups(S,
> rec(D := DClass(S, Transformation([2, 4, 5, 4, 4, 2])))));
6

# maximal: MaximalSubsemigroups, for a transformation semigroup, 5
gap> S := Semigroup([
>  Transformation([1, 5, 6, 5, 2, 6]),
>  Transformation([4, 6, 5, 4, 4, 3]),
>  Transformation([5, 3, 2, 2, 3, 5]),
>  Transformation([6, 4, 2, 1, 3, 6]),
>  Transformation([6, 5, 1, 5, 2, 1])]);;
gap> D := DClass(S, Transformation([4, 6, 5, 4, 4, 3]));
<Green's D-class: Transformation( [ 4, 6, 5, 4, 4, 3 ] )>
gap> max := MaximalSubsemigroups(S, rec(D := D));;
gap> List(max, Size);
[ 2382, 2380 ]

# maximal: MaximalSubsemigroups, for a transformation monoid, 1

# Trivial semigroup
gap> MaximalSubsemigroups(TrivialSemigroup());
[  ]
gap> MaximalSubsemigroupsNC(TrivialSemigroup(), rec());
[  ]

# maximal: MaximalSubsemigroups, for a transformation monoid, 2
gap> S := Monoid([
> Transformation([1, 2, 3, 4, 3]),
> Transformation([1, 2, 3, 4, 1]),
> Transformation([1, 1, 1, 1, 1]),
> Transformation([2, 2, 2, 2, 2])]);;
gap> max := MaximalSubsemigroups(S);;
gap> NrMaximalSubsemigroups(S);
5
gap> correct := [
>  Semigroup([
>    Transformation([1, 1, 1, 1, 1]),
>    Transformation([1, 2, 3, 4, 1]),
>    Transformation([1, 2, 3, 4, 3]),
>    Transformation([2, 2, 2, 2, 2])]),
>  Monoid([
>    Transformation([1, 1, 1, 1, 1]),
>    Transformation([1, 2, 3, 4, 1]),
>    Transformation([2, 2, 2, 2, 2])]),
>  Monoid([
>    Transformation([1, 1, 1, 1, 1]),
>    Transformation([1, 2, 3, 4, 3]),
>    Transformation([2, 2, 2, 2, 2])]),
>  Monoid([
>    Transformation([1, 2, 3, 4, 1]),
>    Transformation([1, 2, 3, 4, 3]),
>    Transformation([2, 2, 2, 2, 2])]),
>  Monoid([
>    Transformation([1, 1, 1, 1, 1]),
>    Transformation([1, 2, 3, 4, 1]),
>    Transformation([1, 2, 3, 4, 3])])];;
gap> max = correct;
true

# maximal: MaximalSubsemigroups, for a transformation monoid, 3
gap> S := Monoid([Transformation([1, 1, 2])]);
<commutative transformation monoid of degree 3 with 1 generator>
gap> MaximalSubsemigroups(S, rec(
> number := true,
> contain := [Transformation([1, 1, 2])]));
1
gap> MaximalSubsemigroups(S, rec(
> number := true,
> contain := [One(S)]));
1

# maximal: MaximalSubsemigroups, for a transformation monoid, 4
gap> S := Monoid([Transformation([1, 1, 2]), Transformation([2, 1])]);;
gap> MaximalSubsemigroups(S, rec(
> number := true,
> D := DClass(S, Transformation([1, 1, 2])),
> contain := [Transformation([1, 1, 2])]));
0

# maximal: MaximalSubsemigroups, for a transformation monoid, 5
gap> S := Monoid([
>  Transformation([1, 1, 6, 4, 5, 6, 4, 6, 5, 6, 1]),
>  Transformation([2, 2, 6, 7, 9, 6, 7, 6, 9, 6, 2]),
>  Transformation([3, 3, 6, 8, 10, 6, 8, 6, 10, 6, 3]),
>  Transformation([1, 1, 6, 4, 5, 6, 4, 6, 5, 6, 4]),
>  Transformation([6, 6, 1, 6, 6, 6, 6, 4, 6, 5, 5])]);;
gap> MaximalSubsemigroups(S,
> rec(D := DClass(S, Transformation([1, 1, 6, 4, 5, 6, 4, 6, 5, 6, 1]))));
[ <transformation monoid of degree 11 with 7 generators>, 
  <transformation monoid of degree 11 with 7 generators>, 
  <transformation monoid of degree 11 with 5 generators>, 
  <transformation monoid of degree 11 with 5 generators>, 
  <transformation monoid of degree 11 with 5 generators>, 
  <transformation monoid of degree 11 with 5 generators> ]

# maximal: NrMaximalSubsemigroups, for a transformation semigroup, 1
gap> S := Semigroup([
> Transformation([1, 1, 2, 1, 4, 4]),
> Transformation([1, 3, 1, 5, 1, 3])]);
<transformation semigroup of degree 6 with 2 generators>
gap> NrMaximalSubsemigroups(S);
2

# maximal: NrMaximalSubsemigroups, for a transformation semigroup, 2
gap> S := Semigroup([
> Transformation([2, 2, 2, 1]), Transformation([2, 3, 4, 3])]);
<transformation semigroup of degree 4 with 2 generators>
gap> NrMaximalSubsemigroups(S);
3

# maximal: NrMaximalSubsemigroups, for a transformation semigroup, 3
gap> S := Semigroup([
>  Transformation([1, 2, 1, 2, 6, 6]),
>  Transformation([1, 3, 1, 3]),
>  Transformation([2, 1]),
>  Transformation([3, 4, 1, 2])]);
<transformation semigroup of degree 6 with 4 generators>
gap> NrMaximalSubsemigroups(S);
5

# maximal: NrMaximalSubsemigroups, for a transformation semigroup, 4
gap> S := Semigroup([
>  Transformation([2, 1, 5, 2, 4]),
>  Transformation([2, 3, 4, 3, 1]),
>  Transformation([3, 4, 1, 4, 3]),
>  Transformation([3, 4, 2, 2, 2]),
>  Transformation([5, 1, 1, 2, 3])]);
<transformation semigroup of degree 5 with 5 generators>
gap> NrMaximalSubsemigroups(S);
8

# maximal: NrMaximalSubsemigroups, for a transformation monoid, 1
gap> S := Monoid([
> Transformation([1, 1, 2, 1, 4, 4]),
> Transformation([1, 3, 1, 5, 1, 3])]);
<transformation monoid of degree 6 with 2 generators>
gap> NrMaximalSubsemigroups(S);
3

# maximal: NrMaximalSubsemigroups, for a transformation monoid, 2
gap> S := Monoid(Transformation([1, 1, 2]));
<commutative transformation monoid of degree 3 with 1 generator>
gap> NrMaximalSubsemigroups(S);
2

# maximal: NrMaximalSubsemigroups, for a transformation monoid, 3
gap> S := Monoid([
> Transformation([1, 1, 1, 1, 1, 1]),
> Transformation([1, 2, 3, 4, 5, 4]),
> Transformation([1, 3, 2, 5, 4, 3])]);
<transformation monoid of degree 6 with 3 generators>
gap> NrMaximalSubsemigroups(S);
5

# maximal: NrMaximalSubsemigroups, for a transformation monoid, 4
gap> S := Monoid([
> Transformation([1, 1, 2, 1, 4, 1, 6, 1, 8, 6]),
> Transformation([1, 3, 1, 5, 1, 7, 1, 9, 1, 3]),
> Transformation([1, 4, 1, 2, 1, 8, 1, 6, 1, 4])]);
<transformation monoid of degree 10 with 3 generators>
gap> NrMaximalSubsemigroups(S);
5

# Fix for Issue  #230
# MaximalSubsemigroups subsemigroups for a Rees 0-matrix semigroup was failing
# to find certain maximal subsemigroups of type 6 (intersect every H-class) when
# the option 'contain' was being used.
gap> G := Group([(1, 5, 4, 3, 2), (1, 5)(2, 4)]);;
gap> mat := [
> [(), 0, 0, 0, 0, 0],
> [0, (), 0, 0, 0, 0],
> [0, 0, (), 0, 0, 0],
> [0, 0, 0, (), 0, 0],
> [0, 0, 0, 0, (), 0],
> [0, 0, 0, 0, 0, ()]];;
gap> R := ReesZeroMatrixSemigroup(G, mat);
<Rees 0-matrix semigroup 6x6 over Group([ (1,5,4,3,2), (1,5)(2,4) ])>
gap> gens := [
>  RMSElement(R, 1, (), 6),
>  RMSElement(R, 1, (1, 5, 4, 3, 2), 2),
>  RMSElement(R, 1, (1, 5, 4, 3, 2), 3),
>  RMSElement(R, 1, (1, 5, 4, 3, 2), 4),
>  RMSElement(R, 1, (1, 5, 4, 3, 2), 5),
>  RMSElement(R, 1, (1, 5)(2, 4), 1),
>  RMSElement(R, 2, (1, 2, 3, 4, 5), 1),
>  RMSElement(R, 3, (1, 2, 3, 4, 5), 1),
>  RMSElement(R, 4, (1, 2, 3, 4, 5), 1),
>  RMSElement(R, 5, (1, 2, 3, 4, 5), 1),
>  RMSElement(R, 6, (), 1)];;
gap> M := Semigroup(gens);
<subsemigroup of 6x6 Rees 0-matrix semigroup with 11 generators>
gap> contain := [
>  RMSElement(R, 1, (1, 5)(2, 4), 6),
>  RMSElement(R, 1, (), 6),
>  RMSElement(R, 2, (1, 2, 3, 4, 5), 1),
>  RMSElement(R, 2, (), 5),
>  RMSElement(R, 3, (), 2),
>  RMSElement(R, 3, (1, 3)(4, 5), 4),
>  RMSElement(R, 4, (), 3),
>  RMSElement(R, 4, (1, 3)(4, 5), 3),
>  RMSElement(R, 5, (1, 3)(4, 5), 4),
>  RMSElement(R, 5, (), 2),
>  RMSElement(R, 6, (1, 5, 4, 3, 2), 5),
>  RMSElement(R, 6, (), 1)];;
gap> IsSubset(M, contain);
true
gap> max := MaximalSubsemigroups(R, rec(types := [6], contain := contain));
[ <subsemigroup of 6x6 Rees 0-matrix semigroup with 17 generators> ]
gap> M in max;
true

#  maximal: maximal subsemigroups that contain all of a non-trivial non-maximal
# regular D-class
gap> S := Monoid([Transformation([1, 1]), Transformation([2, 2])]);;
gap> max := MaximalSubsemigroups(S, rec(contain := GeneratorsOfMonoid(S)));;
gap> Length(max) = 1;
true
gap> max[1] = Semigroup(GeneratorsOfMonoid(S));
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/attributes/maximal.tst");
