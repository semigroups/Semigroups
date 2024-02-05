#############################################################################
##
#W  standard/elements/pbr.tst
#Y  Copyright (C) 2015-2022                           Attila Egri-Nagy
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local A, B, P, S, coll, f, filename, pos, x, y
gap> START_TEST("Semigroups package: standard/elements/pbr.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# pbr: PBR, works, 1/4
gap> PBR([[-3, -2, -1, 2, 3], [-1], [-3, -2, 1, 2]],
>        [[-2, -1, 1, 2, 3], [3], [-3, -2, -1, 1, 3]]);
PBR([ [ -3, -2, -1, 2, 3 ], [ -1 ], [ -3, -2, 1, 2 ] ], 
  [ [ -2, -1, 1, 2, 3 ], [ 3 ], [ -3, -2, -1, 1, 3 ] ])

# pbr: PBR, fails 1, 2/4
gap> PBR([[]], [[], []]);
Error, the arguments (dense lists) do not have equal length

# pbr: PBR, fails 2, 3/4
gap> PBR([["a", 1]], [[2]]);
Error, expected a homogeneous list in position 
1 of the 1st argument (a dense list)
gap> PBR([[2]], [["a", 1]]);
Error, expected a homogeneous list in position 
1 of the 2nd argument (a dense list)

# pbr: PBR, fails 3, 4/4
gap> PBR([[0, 1], []], [[2, 1], []]);
Error, the entries in the arguments are not integers in [-2 .. -1] or [1 .. 2]

# pbr: Star, 1/1
gap> x := PBR([[3], [-4, 4], [-4, -2, 4], [4]],
>  [[-4, -1], [-3, -1, 1], [-1], [-1]]);
PBR([ [ 3 ], [ -4, 4 ], [ -4, -2, 4 ], [ 4 ] ], 
  [ [ -4, -1 ], [ -3, -1, 1 ], [ -1 ], [ -1 ] ])
gap> Star(x);
PBR([ [ 1, 4 ], [ -1, 1, 3 ], [ 1 ], [ 1 ] ], 
  [ [ -3 ], [ -4, 4 ], [ -4, 2, 4 ], [ -4 ] ])
gap> Star(Star(x)) = x;
true

# pbr: DegreeOfPBRCollection, 1/1
gap> x := PBR([[], [-2]], [[], []]);;
gap> y := PBR([[-1, 1, 2, 3], [-3, -1, 1, 2, 3], [1, 3]],
>  [[-3, -2, -1, 2], [-2, -1, 1, 2, 3], [-3, -2, -1, 1, 2, 3]]);;
gap> DegreeOfPBRCollection([x]);
2
gap> DegreeOfPBRCollection([y]);
3
gap> DegreeOfPBRCollection([x, y]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `DegreeOfPBRCollection' on 1 arguments
gap> coll := [x, y];;
gap> IsPBRCollection(coll);
false
gap> DegreeOfPBRCollection(FullPBRMonoid(1));
1
gap> Semigroup(x, y);
Error, Usage: Semigroup(<gen>,...), Semigroup(<gens>), Semigroup(<D>),

# pbr: IsGeneratorsOfInverseSemigroup, 1/1
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
Error, not yet implemented

# pbr: IsTransformationPBR, 1/1
gap> x := PBR([[-3], [-1], [-3]],
>             [[2], [], [1, 3]]);
PBR([ [ -3 ], [ -1 ], [ -3 ] ], [ [ 2 ], [  ], [ 1, 3 ] ])
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

# pbr: IsDualTransformationPBR, 1/1
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

# pbr: IsPartialPermPBR, 1/1
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

# pbr: IsBipartitionPBR, IsBlockBijectionPBR, 1/1
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

# pbr: NumberPBR, 1/1
gap> S := FullPBRMonoid(1);
<pbr monoid of degree 1 with 4 generators>
gap> List(S, NumberPBR);
[ 3, 15, 5, 7, 8, 1, 4, 11, 13, 16, 6, 2, 9, 12, 14, 10 ]
gap> Set(List(last, x -> PBRNumber(x, 1))) = AsSet(S);
true

# pbr: IsEmptyPBR, 1/1
gap> x := PBR([[]], [[]]);;
gap> IsEmptyPBR(x);
true
gap> x := PBR([[-2, 1], [2]], [[-1], [-2, 1]]);
PBR([ [ -2, 1 ], [ 2 ] ], [ [ -1 ], [ -2, 1 ] ])
gap> IsEmptyPBR(x);
false

# pbr: IsUniversalPBR, 1/1
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

# pbr: AsPBR, for a transformation, 1/1
gap> x := Transformation([7, 1, 4, 3, 2, 7, 7, 6, 6, 5]);;
gap> AsPBR(x);
PBR([ [ -7 ], [ -1 ], [ -4 ], [ -3 ], [ -2 ], [ -7 ], [ -7 ], [ -6 ], [ -6 ], 
      [ -5 ] ],
  [ [ 2 ], [ 5 ], [ 4 ], [ 3 ], [ 10 ], [ 8, 9 ], [ 1, 6, 7 ], [  ], [  ], 
      [  ] ])
gap> AsPBR(x, 12);
PBR([ [ -7 ], [ -1 ], [ -4 ], [ -3 ], [ -2 ], [ -7 ], [ -7 ], [ -6 ], [ -6 ], 
      [ -5 ], [ -11 ], [ -12 ] ],
  [ [ 2 ], [ 5 ], [ 4 ], [ 3 ], [ 10 ], [ 8, 9 ], [ 1, 6, 7 ], [  ], [  ], 
      [  ], [ 11 ], [ 12 ] ])

# pbr: AsPBR, for a bipartition, 1/1
gap> x := Bipartition([[1, 2, -3], [3, -2], [-1]]);;
gap> AsPBR(x, 10);
PBR([ [ -3, 1, 2 ], [ -3, 1, 2 ], [ -2, 3 ], [  ], [  ], [  ], [  ], [  ], 
      [  ], [  ] ],
  [ [ -1 ], [ -2, 3 ], [ -3, 1, 2 ], [  ], [  ], [  ], [  ], [  ], [  ], [  ] 
     ])
gap> AsPBR(x);
PBR([ [ -3, 1, 2 ], [ -3, 1, 2 ], [ -2, 3 ] ], 
  [ [ -1 ], [ -2, 3 ], [ -3, 1, 2 ] ])

# pbr: AsPBR, for a boolean mat, fail, 1/1
gap> x := Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 0], [0, 0, 0]]);;
gap> AsPBR(x);
Error, the 1st argument (a boolean matrix)is not of even dimension

# pbr: AsPBR, for a boolean mat and pos int, fail, 1/5
gap> x := Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 0], [0, 0, 0]]);;
gap> AsPBR(x, 6);
Error, the 1st argument (a boolean matrix) does not have even dimension

# pbr: AsPBR, for a boolean mat and pos int, fail, 2/5
gap> x := Matrix(IsBooleanMat, [[0, 0], [0, 1]]);;
gap> AsPBR(x, 5);
Error, the 2nd argument (a pos. int) is not even

# pbr: AsPBR, for a boolean mat and pos int, 3/5
gap> x := Matrix(IsBooleanMat, [[0, 0], [0, 1]]);;
gap> AsPBR(x, 4);
PBR([ [  ], [  ] ], [ [ -1 ], [  ] ])

# pbr: AsPBR, for a boolean mat and pos int, 4/5
gap> x := Matrix(IsBooleanMat, [[1, 1], [1, 1]]);;
gap> AsPBR(x, 4);
PBR([ [ -1, 1 ], [  ] ], [ [ -1, 1 ], [  ] ])

# pbr: AsPBR, for a boolean mat and pos int, 5/5
gap> x := Matrix(IsBooleanMat, [[1, 1, 1, 0], [0, 0, 0, 0],
>                               [0, 1, 0, 1], [1, 0, 0, 1]]);;
gap> AsPBR(x, 2);
PBR([ [ -1, 1 ] ], [ [  ] ])

# pbr: AsPBR, for a boolean mat and pos int, 5/5
gap> x := Matrix(IsBooleanMat, [[1, 1, 1, 0], [1, 1, 0, 0],
>                               [0, 1, 0, 1], [1, 0, 0, 1]]);;
gap> AsPBR(x, 2);
PBR([ [ -1, 1 ] ], [ [ -1, 1 ] ])

# AsPBR for a mult. element
gap> AsPBR((1, 2, 3));
PBR([ [ -2, 1 ], [ -3, 2 ], [ -1, 3 ] ], [ [ -1, 3 ], [ -2, 1 ], [ -3, 2 ] ])
gap> AsPBR((1, 2, 3), 10);
PBR([ [ -2, 1 ], [ -3, 2 ], [ -1, 3 ], [ -4, 4 ], [ -5, 5 ], [ -6, 6 ], 
      [ -7, 7 ], [ -8, 8 ], [ -9, 9 ], [ -10, 10 ] ],
  [ [ -1, 3 ], [ -2, 1 ], [ -3, 2 ], [ -4, 4 ], [ -5, 5 ], [ -6, 6 ], 
      [ -7, 7 ], [ -8, 8 ], [ -9, 9 ], [ -10, 10 ] ])

# pbr: AsTransformation, for a pbr, 1/1
gap> x := PBR([[-3], [-1], [-3]],
>             [[2], [], [1, 3]]);
PBR([ [ -3 ], [ -1 ], [ -3 ] ], [ [ 2 ], [  ], [ 1, 3 ] ])
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
Error, the argument (a pbr) does not define a transformation

# pbr: AsPartialPerm, for a pbr, 1/1
gap> x := PBR([[-1, 1], [-3, 2], [-4, 3], [4], [5]],
>  [[-1, 1], [-2], [-3, 2], [-4, 3], [-5]]);;
gap> IsPartialPermPBR(x);
true
gap> AsPartialPerm(x);
[2,3,4](1)
gap> x := PBR([[4], [-4, 2, 4], [-3], [4]],
>  [[-3, -1], [-2, 3, 4], [-3, 4], []]);;
gap> AsPartialPerm(x);
Error, the argument (a pbr) does not define a partial perm

# pbr: RandomPBR, for pos int, 1/1
gap> RandomPBR(10);;

# pbr: RandomPBR, for pos int and float, 1/1
gap> RandomPBR(3, 1.0);
PBR(
  [ [ -3, -2, -1, 1, 2, 3 ], [ -3, -2, -1, 1, 2, 3 ], [ -3, -2, -1, 1, 2, 3 ] 
     ],
  [ [ -3, -2, -1, 1, 2, 3 ], [ -3, -2, -1, 1, 2, 3 ], [ -3, -2, -1, 1, 2, 3 ] 
     ])
gap> RandomPBR(3, 0.0);
PBR([ [  ], [  ], [  ] ], [ [  ], [  ], [  ] ])

# pbr: PrintString, for a pbr, 1/1
gap> x := PBR([[-4, 1], [-1], [-2, -1, 4], []],
>  [[], [3, 4], [-4, -3], [-4, -2]]);;
gap> ViewString(x);
"\>\>PBR(\>\>[ [ -4, 1 ], [ -1 ], [ -2, -1, 4 ], [  ] ]\<\<, \>\>[ [  ], [ 3, \
4 ], [ -4, -3 ], [ -4, -2 ] ]\<\<\<\<)"
gap> PrintString(x) = last;
true
gap> x := PBR([[6, 7, 9, 10], [-9, 1], [-7, 2, 7], [-8], [-6, -2, 7],
>      [-10, -3, -1, 4, 7], [-7, 3], [], [4], [10]],
>  [[], [], [-7, 2, 7], [-1, 7], [-4], [-4, -1, 5],
>      [-10, 1, 4, 7], [1, 2, 3, 4, 10], [5, 9], [-10]]);;
gap> ViewString(x);
"\>\>PBR(\>\>[ [ 6, 7, 9, 10 ], [ -9, 1 ], [ -7, 2, 7 ], [ -8 ], [ -6, -2, 7 ]\
, [ -10, -3, -1, 4, 7 ], [ -7, 3 ], [  ], [ 4 ], [ 10 ] ]\<\<,\n\>\>[ [  ], [ \
 ], [ -7, 2, 7 ], [ -1, 7 ], [ -4 ], [ -4, -1, 5 ], [ -10, 1, 4, 7 ], [ 1, 2, \
3, 4, 10 ], [ 5, 9 ], [ -10 ] ]\<\<\<\<)"
gap> PrintString(x) = last;
true

# pbr: \<, for a pbrs, 1/1
gap> x := PBR([[2], [-2, 2], [-4, -1, 3], []],
>  [[], [], [-4, 4], [-1]]);;
gap> x < x;
false

# pbr, InverseMutable, fail, 1/1
gap> x := PBR([[], [], []], [[], [-2], []]);;
gap> Inverse(x);
fail

# pbr, EmptyPBR, 1
gap> EmptyPBR(0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `EmptyPBR' on 1 arguments
gap> EmptyPBR(1);
PBR([ [  ] ], [ [  ] ])
gap> EmptyPBR(2);
PBR([ [  ], [  ] ], [ [  ], [  ] ])

# pbr, IdentityPBR, 1
gap> IdentityPBR(0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `IdentityPBR' on 1 arguments
gap> IdentityPBR(1);
PBR([ [ -1 ] ], [ [ 1 ] ])
gap> IdentityPBR(2);
PBR([ [ -1 ], [ -2 ] ], [ [ 1 ], [ 2 ] ])

# pbr, UniversalPBR, 1
gap> UniversalPBR(0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `UniversalPBR' on 1 arguments
gap> UniversalPBR(1);
PBR([ [ -1, 1 ] ], [ [ -1, 1 ] ])
gap> UniversalPBR(2);
PBR([ [ -2, -1, 1, 2 ], [ -2, -1, 1, 2 ] ], 
  [ [ -2, -1, 1, 2 ], [ -2, -1, 1, 2 ] ])

# pbr, IsPermPBR, 1
gap> IsPermPBR(PBR([[1], [-1, 1, 2]], [[-2, -1, 1, 2], [-2, -1, 1]]));
false

# pbr, IsPermPBR, 2
gap> IsPermPBR(PBR([[-3, 1], [2], [-1, 3]], [[-1, 3], [-2], [-3, 1]]));
false

# pbr, IsPermPBR, 3
gap> IsPermPBR(PBR([[-1, 1], [-3, 2], [-2, 3]], [[-1, 1], [-2, 3], [-3, 2]]));
true

# pbr, IsIdentityPBR, 1
gap> x := IdentityPBR(3);;
gap> IsIdentityPBR(x);
true

# pbr, IsIdentityPBR, 2
gap> IsIdentityPBR(PBR([[-3, 1], [2], [-1, 3]], [[-1, 3], [-2], [-3, 1]]));
false

# pbr, IsIdentityPBR, 3
gap> IsIdentityPBR(PBR([[-1], [-2]], [[1], [1]]));
false

# pbr, IsIdentityPBR, 4
gap> IsIdentityPBR(PBR([[-1], [-2]], [[1], [2, 1]]));
false

# pbr, AsPermutation, for a pbr, 1
gap> x := PBR([[-3, 1], [2], [-1, 3]], [[-1, 3], [-2], [-3, 1]]);;
gap> AsPermutation(x);
Error, the argument (a pbr) does not define a permutation

# pbr, AsPermutation, for a pbr, 2
gap> x := PBR([[-1, 1], [-3, 2], [-2, 3]], [[-1, 1], [-2, 3], [-3, 2]]);;
gap> AsPermutation(x);
(2,3)

# pbr, \<, 1
gap> x := PBR([[-1, 1], [-3, 2], [-2, 3]], [[-1, 1], [-2, 3], [-3, 2]]);;
gap> x < PBR([[-1, 1], [-3, 2], [-2, 3]], [[-1, 1], [-2, 3], [-3, 2]]);
false

# pbr, pickling
gap> x := PBR([[-82, -53, -35, 44, 53, 69], [4, 30, 34, 47, 76], [94], [],
>      [-18, 3, 12, 79], [-92, -80, 60], [-59], [-76, 79, 90],
>      [9, 45], [-82, -69], [-59, -44], [-50], [-39, -4, 14], [4],
>      [-28, 80], [-56, -12, 10], [-21, 97], [-39, 45], [-96, -61],
>      [], [-41, 74], [-61, 44], [-92], [34], [-98, -95, 33, 91],
>      [], [], [1, 2, 71, 79, 81], [-68, 85, 96],
>      [-83, 18, 28, 39, 42], [], [-44], [-52, 43], [-23, 98],
>      [-50, -41, 79], [38, 95], [-6], [7, 93], [], [-44, 27, 67],
>      [-86, -60, 57], [], [41], [-53], [13, 48], [45, 69, 86],
>      [-72, -12, 38], [-75, -67, -55], [-16, 52], [-44, 46], [88],
>      [8], [-65, 28], [31], [45, 83, 100], [-75, -68, -64, -40, 7],
>      [-81, -76, -2, 40], [-43, -36, -23], [-33, 24, 33, 36],
>      [-95, -38], [-78, -75, -35, 7, 55], [56], [-70, 66, 81, 96],
>      [58, 76], [], [], [28], [-66, 12, 57], [19, 68],
>      [-73, -40, 16, 81, 90], [-41, 61], [-84, -5, 86], [38, 48, 97],
>      [-87], [-79, 69, 86], [93], [36, 39, 57], [-37, 48], [41],
>      [], [19, 21, 65], [-75, -57, -54, -17, 50], [-80, -2, 5],
>      [30, 37], [-56], [25], [40], [], [-73], [-54, -8, -3],
>      [-81, -47, 16], [-17, 35, 44], [-7], [-64, -43, -30, 96],
>      [36, 70, 97], [], [], [9], [59], [-87, -55, -17]],
>  [[-17, 3, 4, 76, 93], [68], [-59, -27], [-38, 73], [],
>      [-61, 72], [-19, -9, 71], [-88, -55, -15, 70], [-94, -85],
>      [-60, -2, 46], [-71, -67, -61, 18, 25, 27, 94], [], [18],
>      [-75, -64, 32], [-24, 36, 55], [], [-55, -15, 63], [-86, -8],
>      [-66, -26, -22, 24], [-91], [-35, 49], [], [],
>      [-57, -40, 16, 73], [-42, 15, 52], [], [-48],
>      [-38, 17, 31, 95], [-55, -5, 12], [29, 84], [21], [], [66],
>      [-96, -86, 90], [-28, 72], [71, 72], [-2, 30, 75], [-53, 57],
>      [-66, 29], [-49, 42], [-74, -23, 4], [], [], [44],
>      [-78, -53, -5, 26], [-49, -33, -6], [-91, -89, 51, 91],
>      [-19, 44], [-56], [13, 80], [98], [-47, 5, 77],
>      [-17, 21, 78, 79], [-75, 45], [], [42], [-50, 55, 71, 73],
>      [], [74], [], [-36, -9, 67], [-77], [-27, 84], [67],
>      [-80, -35], [], [28, 44, 95], [-54, -51, 66],
>      [-90, -82, -48, -37, -1, 59, 83], [-38, 32, 98], [-83, 72],
>      [-54], [-96, -11, 14, 24], [-15, 20, 99], [-55, 72],
>      [-98, -23, -9, -2, 23, 33, 79], [], [15], [-16],
>      [-68, -48, 44], [51], [], [-44, 44], [-44, -36, -2], [],
>      [-20, 71], [11, 57], [-91, -32, 51, 95], [98], [75], [-14],
>      [], [], [-87], [-61, -33], [98], [-5, 3, 76, 85],
>      [-81, 86, 92], [37], [-10, 72]]);;
gap> filename := Filename(DirectoryTemporary(), "pbr.gz");;
gap> WriteGenerators(filename, [x]);
IO_OK
gap> x = ReadGenerators(filename)[1];
true

# Test for no side effects in PBRNC (Issue #193)
gap> A := [[2, 3], [-3, -2, -1, 2], [-3, -2, 2]];;
gap> B := [[-2], [-3, 2, 3], [-2, -1]];;
gap> P := PBR(A, B);
PBR([ [ 2, 3 ], [ -3, -2, -1, 2 ], [ -3, -2, 2 ] ], 
  [ [ -2 ], [ -3, 2, 3 ], [ -2, -1 ] ])
gap> P := PBR(A, B);
PBR([ [ 2, 3 ], [ -3, -2, -1, 2 ], [ -3, -2, 2 ] ], 
  [ [ -2 ], [ -3, 2, 3 ], [ -2, -1 ] ])

# Test TYPE_PBR errors
gap> TYPE_PBR("a");
Error, the argument is not a non-negative integer
gap> TYPE_PBR(-1);
Error, the argument is not a non-negative integer

# Test String
gap> x := PBR([[-1, 1], [-3, 2], [-2, 3]], [[-1, 1], [-2, 3], [-3, 2]]);;
gap> String(x);
"PBR([ [ -1, 1 ], [ -3, 2 ], [ -2, 3 ] ], [ [ -1, 1 ], [ -2, 3 ], [ -3, 2 ] ])\
"
gap> x = EvalString(String(x));
true

# Test \*
gap> x := PBR(
>  [[-6, -5, -4, -3, -1, 1, 2, 4, 6], [-2, -1, 1, 2, 4, 6],
>      [-5, -4, -2, 2, 6], [-5, -4, -3, -2, -1, 2, 3, 5, 6],
>      [-6, -5, -3, -1, 1, 2, 3, 5, 6], [-4, -3, -1, 3, 4, 5, 6]],
>  [[-5, -4, -1, 1, 2, 3, 4, 5, 6], [-5, -3, -2, -1, 1, 2, 3, 4],
>      [-5, -3, 4, 5], [-5, -2, -1, 1, 2, 6], [-6, -2, 4, 5],
>      [-6, -5, -4, -2, -1, 1, 2, 3, 5, 6]]);;
gap> y := PBR(
>  [[-5, -3, -1, 2, 6], [-5, -4, 2, 4, 5, 6], [-6, -5, -2, 1, 4, 5, 6],
>      [-6, -3, -2, 3, 4], [-5, -2, -1, 1, 2, 3, 5, 6],
>      [-6, -4, -3, -2, -1, 2, 3, 4, 5, 6]],
>  [[-6, -4, -3, 1, 2, 4, 5, 6], [-6, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6],
>      [-5, -3, -2, 2, 3, 4, 5, 6], [-6, -5, -4, -3, 2, 3, 4, 6],
>      [-6, -5, -2, -1, 1, 2, 3, 4, 6], [-5, -3, -2, -1, 1, 2, 4, 5, 6]]);;
gap> IsUniversalPBR(x * y);
true

# Test operators for PBRs of different degree
gap> x := PBR([[-2, 1, 2], [-2, -1, 2]], [[1, 2], [-1, 1]]);
PBR([ [ -2, 1, 2 ], [ -2, -1, 2 ] ], [ [ 1, 2 ], [ -1, 1 ] ])
gap> y := PBR([[-1, 1]], [[-1, 1]]);
PBR([ [ -1, 1 ] ], [ [ -1, 1 ] ])
gap> x * y;
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `*' on 2 arguments
gap> x < y;
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `<' on 2 arguments
gap> x = y;
false

# Pickling
gap> filename := Concatenation(SEMIGROUPS.PackageDir,
> "/tst/standard/elements/pbr.tst");;
gap> f := IO_File(filename, "r");;
gap> IO_Unpicklers.PABR(f);
IO_Error
gap> IO_Pickle(f, x);
IO_Error

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/elements/pbr.tst");
