#############################################################################
##
#W  standard/pbr.tst
#Y  Copyright (C) 2015                                Attila Egri-Nagy
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/pbr.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# pbr: PBR, works, 1/4
gap> PBR([[-3, -2, -1, 2, 3], [-1], [-3, -2, 1, 2]],
>        [[-2, -1, 1, 2, 3], [3], [-3, -2, -1, 1, 3]]);
PBR([ [ -3, -2, -1, 2, 3 ], [ -1 ], [ -3, -2, 1, 2 ] ], 
  [ [ -2, -1, 1, 2, 3 ], [ 3 ], [ -3, -2, -1, 1, 3 ] ])

#T# pbr: PBR, fails 1, 2/4
gap> PBR([[]], [[], []]);
Error, Semigroups: PBR: usage,
the arguments must have equal lengths,

#T# pbr: PBR, fails 2, 3/4
gap> PBR([["a", 1]], [[2]]);
Error, Semigroups: PBR: usage,
the entries in the arguments must be homogeneous lists,

#T# pbr: PBR, fails 3, 4/4
gap> PBR([[0, 1], []], [[2, 1], []]);
Error, Semigroups: PBR: usage,
the entries in the first argument must be integers in [-2 .. -1]
 or [1 .. 2],

#T# pbr: Star, 1/1
gap> x := PBR([[3], [-4, 4], [-4, -2, 4], [4]],
>  [[-4, -1], [-3, -1, 1], [-1], [-1]]);
PBR([ [ 3 ], [ -4, 4 ], [ -4, -2, 4 ], [ 4 ] ], 
  [ [ -4, -1 ], [ -3, -1, 1 ], [ -1 ], [ -1 ] ])
gap> Star(x);
PBR([ [ 1, 4 ], [ -1, 1, 3 ], [ 1 ], [ 1 ] ], 
  [ [ -3 ], [ -4, 4 ], [ -4, 2, 4 ], [ -4 ] ])
gap> Star(Star(x)) = x;
true

#T# pbr: DegreeOfPBRCollection, 1/1
gap> x := PBR([[], [-2]], [[], []]);;
gap> y := PBR([[-1, 1, 2, 3], [-3, -1, 1, 2, 3], [1, 3]],
>  [[-3, -2, -1, 2], [-2, -1, 1, 2, 3], [-3, -2, -1, 1, 2, 3]]);;
gap> DegreeOfPBRCollection([x]);
2
gap> DegreeOfPBRCollection([y]);
3
gap> DegreeOfPBRCollection([x, y]);
Error, Semigroups: DegreeOfPBRCollection: usage,
the argument <coll> must be a collection of PBRs of equal degree,
gap> DegreeOfPBRCollection(FullPBRMonoid(1));
1

#T# pbr: IsGeneratorsOfInverseSemigroup, 1/1
gap> x := PBR([[], [-2]], [[], []]);;
gap> IsGeneratorsOfInverseSemigroup([x]);
false
gap> x := PBR([[-1, 1], [-3, 2], [-4, 3], [4]],
>  [[-1, 1], [-2], [-3, 2], [-4, 3]]);;
gap> IsGeneratorsOfInverseSemigroup([x]);
true
gap> InverseSemigroup(x);
<inverse pbr semigroup of degree 4 with 1 generator>
gap> InverseMonoid(x);
Error, Semigroups: InverseMonoidByGenerators(for a pbr collection):
not yet implemented,

#T# pbr: IsTransformationPBR, 1/1
gap> x := PBR([[-3, 1, 3], [-1, 2], [-3, 1, 3]],
>             [[-1, 2], [-2], [-3, 1, 3]]);
PBR([ [ -3, 1, 3 ], [ -1, 2 ], [ -3, 1, 3 ] ], 
  [ [ -1, 2 ], [ -2 ], [ -3, 1, 3 ] ])
gap> IsTransformationPBR(x);
true
gap> x := AsTransformation(x);
Transformation( [ 3, 1, 3 ] )
gap> AsPBR(x) * AsPBR(x) = AsPBR(x ^ 2);
true
gap> Number(FullPBRMonoid(1), IsTransformationPBR);
1
gap> x := PBR([[-2, -1, 2], [-2, 1, 2]],
>             [[-1, 1], [-2]]);
PBR([ [ -2, -1, 2 ], [ -2, 1, 2 ] ], [ [ -1, 1 ], [ -2 ] ])
gap> IsTransformationPBR(x);
false

#T# pbr: IsDualTransformationPBR, 1/1
gap> x := PBR([[-3, 1, 3], [-1, 2], [-3, 1, 3]],
>             [[-1, 2], [-2], [-3, 1, 3]]);
PBR([ [ -3, 1, 3 ], [ -1, 2 ], [ -3, 1, 3 ] ], 
  [ [ -1, 2 ], [ -2 ], [ -3, 1, 3 ] ])
gap> IsDualTransformationPBR(x);
false
gap> IsDualTransformationPBR(Star(x));
true
gap> Number(FullPBRMonoid(1), IsDualTransformationPBR);
1

#T# pbr: IsPartialPermPBR, 1/1
gap> x := PBR([[-1, 1], [2]], [[-1, 1], [-2]]);
PBR([ [ -1, 1 ], [ 2 ] ], [ [ -1, 1 ], [ -2 ] ])
gap> IsPartialPermPBR(x);
true
gap> x := PartialPerm([1, 2], [3, 1]);
[2,1,3]
gap> AsPBR(x) * AsPBR(x) = AsPBR(x ^ 2);
true
gap> Number(FullPBRMonoid(1), IsPartialPermPBR);
2

#T# pbr: IsBipartitionPBR, IsBlockBijectionPBR, 1/1
gap> x := PBR([[-1, 3], [-1, 3], [-2, 1, 2, 3]],
>             [[-2, -1, 2], [-2, -1, 1, 2, 3],
>               [-2, -1, 1, 2]]);
PBR([ [ -1, 3 ], [ -1, 3 ], [ -2, 1, 2, 3 ] ], 
  [ [ -2, -1, 2 ], [ -2, -1, 1, 2, 3 ], [ -2, -1, 1, 2 ] ])
gap> IsBipartitionPBR(x);
false
gap> x := PBR([[-2, -1, 1], [2, 3], [2, 3]],
>             [[-2, -1, 1], [-2, -1, 1], [-3]]);
PBR([ [ -2, -1, 1 ], [ 2, 3 ], [ 2, 3 ] ], 
  [ [ -2, -1, 1 ], [ -2, -1, 1 ], [ -3 ] ])
gap> IsBipartitionPBR(x);
true
gap> IsBlockBijectionPBR(x);
false

#T# pbr: NumberPBR, 1/1
gap> S := FullPBRMonoid(1);
<pbr monoid of degree 1 with 4 generators>
gap> List(S, NumberPBR);
[ 3, 15, 5, 7, 8, 1, 4, 11, 13, 16, 6, 2, 9, 12, 14, 10 ]
gap> Set(List(last, x -> PBRNumber(x, 1))) = AsSet(S);
true

#T# pbr: IsEmptyPBR, 1/1
gap> x := PBR([[]], [[]]);;
gap> IsEmptyPBR(x);
true
gap> x := PBR([[-2, 1], [2]], [[-1], [-2, 1]]);
PBR([ [ -2, 1 ], [ 2 ] ], [ [ -1 ], [ -2, 1 ] ])
gap> IsEmptyPBR(x);
false

#T# pbr: IsUniversalPBR, 1/1
gap> x := PBR([[]], [[]]);
PBR([ [  ] ], [ [  ] ])
gap> IsUniversalPBR(x);
false
gap> x := PBR([[-2, 1], [2]], [[-1], [-2, 1]]);
PBR([ [ -2, 1 ], [ 2 ] ], [ [ -1 ], [ -2, 1 ] ])
gap> IsUniversalPBR(x);
false
gap> x := PBR([[-1, 1]], [[-1, 1]]);
PBR([ [ -1, 1 ] ], [ [ -1, 1 ] ])
gap> IsUniversalPBR(x);
true

#T# pbr: AsPBR, for a transformation, 1/1
gap> x := Transformation([7, 1, 4, 3, 2, 7, 7, 6, 6, 5]);;
gap> AsPBR(x);
PBR(
  [ [ -7, 1, 6, 7 ], [ -1, 2 ], [ -4, 3 ], [ -3, 4 ], [ -2, 5 ], 
      [ -7, 1, 6, 7 ], [ -7, 1, 6, 7 ], [ -6, 8, 9 ], [ -6, 8, 9 ], 
      [ -5, 10 ] ],
  [ [ -1, 2 ], [ -2, 5 ], [ -3, 4 ], [ -4, 3 ], [ -5, 10 ], [ -6, 8, 9 ], 
      [ -7, 1, 6, 7 ], [ -8 ], [ -9 ], [ -10 ] ])
gap> AsPBR(x, 12);
PBR(
  [ [ -7, 1, 6, 7 ], [ -1, 2 ], [ -4, 3 ], [ -3, 4 ], [ -2, 5 ], 
      [ -7, 1, 6, 7 ], [ -7, 1, 6, 7 ], [ -6, 8, 9 ], [ -6, 8, 9 ], 
      [ -5, 10 ], [ -11, 11 ], [ -12, 12 ] ],
  [ [ -1, 2 ], [ -2, 5 ], [ -3, 4 ], [ -4, 3 ], [ -5, 10 ], [ -6, 8, 9 ], 
      [ -7, 1, 6, 7 ], [ -8 ], [ -9 ], [ -10 ], [ -11, 11 ], [ -12, 12 ] ])

#T# pbr: AsPBR, for a bipartition, 1/1
gap> x := Bipartition([[1, 2, -3], [3, -2], [-1]]);;
gap> AsPBR(x, 10);
PBR([ [ -3, 1, 2 ], [ -3, 1, 2 ], [ -2, 3 ], [  ], [  ], [  ], [  ], [  ], 
      [  ], [  ] ],
  [ [ -1 ], [ -2, 3 ], [ -3, 1, 2 ], [  ], [  ], [  ], [  ], [  ], [  ], [  ] 
     ])

#T# pbr: AsPBR, for a boolean mat, fail, 1/1
gap> x := Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 0], [0, 0, 0]]);;
gap> AsPBR(x);
Error, Semigroups: AsPBR: usage,
the boolean matrix <x> must be of even dimension,

#T# pbr: AsPBR, for a boolean mat and pos int, fail, 1/5
gap> x := Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 0], [0, 0, 0]]);;
gap> AsPBR(x, 6);
Error, Semigroups: AsPBR: usage,
the boolean matrix <x> must be of even dimension,

#T# pbr: AsPBR, for a boolean mat and pos int, fail, 2/5
gap> x := Matrix(IsBooleanMat, [[0, 0], [0, 1]]);;
gap> AsPBR(x, 5);
Error, Semigroups: AsPBR: usage,
the second argument <n> must be even,

#T# pbr: AsPBR, for a boolean mat and pos int, 3/5
gap> x := Matrix(IsBooleanMat, [[0, 0], [0, 1]]);;
gap> AsPBR(x, 4);
PBR([ [  ], [  ] ], [ [ -1 ], [  ] ])

#T# pbr: AsPBR, for a boolean mat and pos int, 4/5
gap> x := Matrix(IsBooleanMat, [[1, 1], [1, 1]]);;
gap> AsPBR(x, 4);
PBR([ [ -1, 1 ], [  ] ], [ [ -1, 1 ], [  ] ])

#T# pbr: AsPBR, for a boolean mat and pos int, 5/5
gap> x := Matrix(IsBooleanMat, [[1, 1, 1, 0], [0, 0, 0, 0],
>                               [0, 1, 0, 1], [1, 0, 0, 1]]);;
gap> AsPBR(x, 2);
PBR([ [ -1, 1 ] ], [ [  ] ])

#T# pbr: AsPBR, for a boolean mat and pos int, 5/5
gap> x := Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 0, 0],
>                               [0, 1, 0, 1], [1, 0, 0, 1]]);;
gap> AsPBR(x, 2);
PBR([ [ -1, 1 ] ], [ [ -1, 1 ] ])

#T# pbr: AsTransformation, for a pbr, 1/1
gap> x := PBR([[-3, 1, 3], [-1, 2], [-3, 1, 3]],
>             [[-1, 2], [-2], [-3, 1, 3]]);
PBR([ [ -3, 1, 3 ], [ -1, 2 ], [ -3, 1, 3 ] ], 
  [ [ -1, 2 ], [ -2 ], [ -3, 1, 3 ] ])
gap> IsTransformationPBR(x);
true
gap> x := AsTransformation(x);
Transformation( [ 3, 1, 3 ] )
gap> AsPBR(x) * AsPBR(x) = AsPBR(x ^ 2);
true
gap> Number(FullPBRMonoid(1), IsTransformationPBR);
1
gap> x := PBR([[-2, -1, 2], [-2, 1, 2]],
>             [[-1, 1], [-2]]);
PBR([ [ -2, -1, 2 ], [ -2, 1, 2 ] ], [ [ -1, 1 ], [ -2 ] ])
gap> IsTransformationPBR(x);
false
gap> AsTransformation(x);
Error, Semigroups: AsTransformation: usage,
the argument <x> must be a transformation PBR,

#T# pbr: AsPartialPerm, for a pbr, 1/1
gap> x := PBR([[-1, 1], [-3, 2], [-4, 3], [4], [5]],
>  [[-1, 1], [-2], [-3, 2], [-4, 3], [-5]]);;
gap> IsPartialPermPBR(x);
true
gap> AsPartialPerm(x);
[2,3,4](1)
gap> x := PBR([[4], [-4, 2, 4], [-3], [4]],
>  [[-3, -1], [-2, 3, 4], [-3, 4], []]);;
gap> AsPartialPerm(x);
Error, Semigroups: AsPartialPerm: usage,
the argument <x> must be a partial perm PBR,

#T# pbr: RandomPBR, for pos int, 1/1
gap> RandomPBR(10);;

#T# pbr: RandomPBR, for pos int and float, 1/1
gap> RandomPBR(3, 1.0);
PBR(
  [ [ -3, -2, -1, 1, 2, 3 ], [ -3, -2, -1, 1, 2, 3 ], [ -3, -2, -1, 1, 2, 3 ] 
     ],
  [ [ -3, -2, -1, 1, 2, 3 ], [ -3, -2, -1, 1, 2, 3 ], [ -3, -2, -1, 1, 2, 3 ] 
     ])
gap> RandomPBR(3, 0.0);
PBR([ [  ], [  ], [  ] ], [ [  ], [  ], [  ] ])

#T# pbr: PrintString, for a pbr, 1/1
gap> x := PBR([[-4, 1], [-1], [-2, -1, 4], []],
>  [[], [3, 4], [-4, -3], [-4, -2]]);;
gap> PrintString(x);
"\>\>PBR(\>\>[ [ -4,\<\> 1 ],\<\> [ -1 ],\<\> [ -2,\<\> -1,\<\> 4 ],\<\> [  ] \
]\<\<, \>\>[ [  ],\<\> [ 3,\<\> 4 ],\<\> [ -4,\<\> -3 ],\<\> [ -4,\<\> -2 ] ]\
\<\<\<\<)"
gap> x := PBR([[6, 7, 9, 10], [-9, 1], [-7, 2, 7], [-8], [-6, -2, 7],
>      [-10, -3, -1, 4, 7], [-7, 3], [], [4], [10]],
>  [[], [], [-7, 2, 7], [-1, 7], [-4], [-4, -1, 5],
>      [-10, 1, 4, 7], [1, 2, 3, 4, 10], [5, 9], [-10]]);;
gap> PrintString(x);
"\>\>PBR(\>\>[ [ 6,\<\> 7,\<\> 9,\<\> 10 ],\<\> [ -9,\<\> 1 ],\<\> [ -7,\<\> 2\
,\<\> 7 ],\<\> [ -8 ],\<\> [ -6,\<\> -2,\<\> 7 ],\<\> [ -10,\<\> -3,\<\> -1,\<\
\> 4,\<\> 7 ],\<\> [ -7,\<\> 3 ],\<\> [  ],\<\> [ 4 ],\<\> [ 10 ] ]\<\<,\n\>\>\
[ [  ],\<\> [  ],\<\> [ -7,\<\> 2,\<\> 7 ],\<\> [ -1,\<\> 7 ],\<\> [ -4 ],\<\>\
 [ -4,\<\> -1,\<\> 5 ],\<\> [ -10,\<\> 1,\<\> 4,\<\> 7 ],\<\> [ 1,\<\> 2,\<\> \
3,\<\> 4,\<\> 10 ],\<\> [ 5,\<\> 9 ],\<\> [ -10 ] ]\<\<\<\<)"

#T# pbr: \<, for a pbrs, 1/1
gap> x := PBR([[2], [-2, 2], [-4, -1, 3], []],
>  [[], [], [-4, 4], [-1]]);;
gap> x < x;
false

#T# pbr, InverseMutable, fail, 1/1
gap> x := PBR([[], [], []], [[], [-2], []]);;
gap> Inverse(x);
fail

#T# pbr, EmptyPBR, 1
gap> EmptyPBR(0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `EmptyPBR' on 1 arguments
gap> EmptyPBR(1);
PBR([ [  ] ], [ [  ] ])
gap> EmptyPBR(2);
PBR([ [  ], [  ] ], [ [  ], [  ] ])

#T# pbr, IdentityPBR, 1
gap> IdentityPBR(0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IdentityPBR' on 1 arguments
gap> IdentityPBR(1);
PBR([ [ -1 ] ], [ [ 1 ] ])
gap> IdentityPBR(2);
PBR([ [ -1 ], [ -2 ] ], [ [ 1 ], [ 2 ] ])

#T# pbr, UniversalPBR, 1
gap> UniversalPBR(0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `UniversalPBR' on 1 arguments
gap> UniversalPBR(1);
PBR([ [ -1, 1 ] ], [ [ -1, 1 ] ])
gap> UniversalPBR(2);
PBR([ [ -2, -1, 1, 2 ], [ -2, -1, 1, 2 ] ], 
  [ [ -2, -1, 1, 2 ], [ -2, -1, 1, 2 ] ])

#T# pbr, IsPermPBR, 1
gap> IsPermPBR(PBR([[1], [-1, 1, 2]], [[-2, -1, 1, 2], [-2, -1, 1]]));
false

#T# pbr, IsPermPBR, 2
gap> IsPermPBR(PBR([[-3, 1], [2], [-1, 3]], [[-1, 3], [-2], [-3, 1]]));
false

#T# pbr, IsPermPBR, 3
gap> IsPermPBR(PBR([[-1, 1], [-3, 2], [-2, 3]], [[-1, 1], [-2, 3], [-3, 2]]));
true

#T# pbr, IsIdentityPBR, 1
gap> x := IdentityPBR(3);;
gap> IsIdentityPBR(x);
true

#T# pbr, IsIdentityPBR, 2
gap> IsIdentityPBR(PBR([[-3, 1], [2], [-1, 3]], [[-1, 3], [-2], [-3, 1]]));
false

#T# pbr, IsIdentityPBR, 3
gap> IsIdentityPBR(PBR([[-1], [-2]], [[1], [1]]));
false

#T# pbr, IsIdentityPBR, 4
gap> IsIdentityPBR(PBR([[-1], [-2]], [[1], [2, 1]]));
false

#T# pbr, AsPermutation, for a pbr, 1
gap> x := PBR([[-3, 1], [2], [-1, 3]], [[-1, 3], [-2], [-3, 1]]);;
gap> AsPermutation(x);
Error, Semigroups: AsPermutation: usage,
the argument <x> must be a permutation PBR,

#T# pbr, AsPermutation, for a pbr, 2
gap> x := PBR([[-1, 1], [-3, 2], [-2, 3]], [[-1, 1], [-2, 3], [-3, 2]]);;
gap> AsPermutation(x);
(2,3)

#T# pbr, \<, 1
gap> x := PBR([[-1, 1], [-3, 2], [-2, 3]], [[-1, 1], [-2, 3], [-3, 2]]);;
gap> x < PBR([[-1, 1], [-3, 2], [-2, 3]], [[-1, 1], [-2, 3], [-3, 2]]);
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(x);
gap> Unbind(y);

#E#
gap> STOP_TEST("Semigroups package: standard/pbr.tst");
