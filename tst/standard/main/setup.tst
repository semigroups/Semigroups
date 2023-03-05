#############################################################################
##
#W  standard/main/setup.tst
#Y  Copyright (C) 2016-2022                              James D. Mitchell
##                                                          Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local G, M, R, S, acting, b, data, forflatplainlists, func, o, r, rank, s
#@local schutz, x, y
gap> START_TEST("Semigroups package: standard/main/setup.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# IsGeneratorsOfActingSemigroup
gap> IsGeneratorsOfActingSemigroup([Transformation([2, 2])]);
true
gap> IsGeneratorsOfActingSemigroup([PartialPerm([1])]);
true
gap> IsGeneratorsOfActingSemigroup([Bipartition([[1, -1]])]);
true
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> IsGeneratorsOfActingSemigroup(R);
true
gap> IsGeneratorsOfActingSemigroup(Elements(R));
true
gap> IsGeneratorsOfActingSemigroup(SLM(2, 2));
true
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3]), Digraph([[1], [1, 2], [1, 3]]), [1, 2]);;
gap> IsGeneratorsOfActingSemigroup(M);
true

# ActionDegree

# ActionDegree, for a partial perm
gap> ActionDegree(PartialPerm([]));
0
gap> ActionDegree(PartialPerm([2]));
2
gap> ActionDegree(PartialPerm([0, 1]));
2

# ActionDegree, for a bipartition
gap> ActionDegree(Bipartition([[1, 3], [2, 4, -2], [5, -1, -3, -4], [-5]]));
5

# ActionDegree, for an RZMS element
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> ActionDegree(R.1);
1
gap> ActionDegree(MultiplicativeZero(R));
0
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[()]]);;
gap> Set(R, ActionDegree);
[ 0, 1, 3, 4 ]

# ActionDegree, for a MTS element
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3]), Digraph([[1], [1, 2], [1, 3]]), [1, 2]);;
gap> ActionDegree(M.1);
0

# ActionDegree, for a matrix over finite field object
gap> ActionDegree(Matrix(GF(2 ^ 2),
>                        [[Z(2) ^ 0, 0 * Z(2)], [0 * Z(2), 0 * Z(2)]]));
2

# ActionDegree, for a transformation collection
gap> ActionDegree(FullTransformationMonoid(3));
3
gap> ActionDegree([IdentityTransformation]);
0

# ActionDegree, for a partial perm collection
gap> ActionDegree([PartialPerm([2, 3]), PartialPerm([2, 1, 3])]);
3
gap> ActionDegree([PartialPerm([])]);
0

# ActionDegree, for a bipartition collection
gap> ActionDegree([Bipartition([[1, 2, -2], [-1]]),
>                  Bipartition([[1], [2, -2], [-1]])]);
2

# ActionDegree, for an RZMS element collection
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> ActionDegree([R.1, MultiplicativeZero(R)]);
1
gap> ActionDegree([MultiplicativeZero(R)]);
0

# ActionDegree, for an MTS element collection
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3]),
> Digraph([[1], [1, 2], [1, 3]]), [1, 2]);;
gap> ActionDegree(Generators(M));
0

# ActionDegree for MatrixOverFiniteFieldSemigroup
gap> ActionDegree(SLM(2, 2));
2

# ActionDegree, for a transformation semigroup
gap> ActionDegree(FullTransformationSemigroup(2));
2

# ActionDegree, for a partial perm semigroup
gap> ActionDegree(MonogenicSemigroup(IsPartialPermSemigroup, 3, 3));
6

# ActionDegree, for a partial perm inverse semigroup
gap> ActionDegree(SymmetricInverseMonoid(4));
4

# ActionDegree, for a bipartition semigroup
gap> ActionDegree(PartitionMonoid(10));
10

# ActionDegree, for a Rees 0-matrix subsemigroup with generators
gap> R := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[()]]);;
gap> GeneratorsOfSemigroup(R);;
gap> ActionDegree(R);
3
gap> ActionDegree(Semigroup(MultiplicativeZero(R)));
0

# ActionDegree, for an MTS subsemigroup
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3]), Digraph([[1], [1, 2], [1, 3]]), [1, 2]);;
gap> ActionDegree(Semigroup(Representative(M)));
0

# ActionDegree, for a matrix over finite field semigroup
gap> ActionDegree(GLM(2, 2));
2
gap> ActionDegree(SLM(2, 2));
2

# ActionRank

# ActionRank, for a transformation and integer
gap> ActionRank(Transformation([2, 3, 4, 5, 5, 6]), 5);
4
gap> ActionRank(Transformation([2, 3, 4, 5, 5, 6]), 6);
5

# ActionRank, for a transformation semigroup
gap> rank := ActionRank(FullTransformationMonoid(4));;
gap> rank(IdentityTransformation);
4

# ActionRank, for a partial perm and integer
gap> ActionRank(PartialPerm([0, 3, 0, 6]), 8);
2

# ActionRank, for a partial perm semigroup
gap> rank := ActionRank(SymmetricInverseSemigroup(2));;
gap> rank(PartialPerm([]));
0
gap> rank(PartialPerm([2, 1]));
2

# ActionRank, for a bipartition and integer
gap> ActionRank(Bipartition([[1, 3], [2, -1], [-2, -3]]), 3);
1

# ActionRank, for a bipartition semigroup
gap> rank := ActionRank(PartitionMonoid(3));;
gap> rank(Bipartition([[1, 3], [2, -1], [-2, -3]]));
1

# ActionRank, for an RZMS element and integer
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> ActionRank(R.1, 10);
1
gap> ActionRank(MultiplicativeZero(R), 10);
0

# ActionRank, for a Rees 0-matrix subsemigroup
gap> R := ReesZeroMatrixSemigroup(Group([(2, 3)]), [[()]]);;
gap> rank := ActionRank(R);;
gap> rank(RMSElement(R, 1, (2, 3), 1));
3
gap> rank(MultiplicativeZero(R));
0

# ActionRank, for an MTS semigroup and subsemigroup
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3]), Digraph([[1], [1, 2], [1, 3]]), [1, 2]);;
gap> rank := ActionRank(M);;
gap> rank(Representative(M));
0
gap> S := Semigroup(Representative(M));;
gap> rank := ActionRank(S);;
gap> rank(Representative(S));
0

# ActionRank, for a matrix over FF
gap> x := Matrix(GF(2), [[0 * Z(2), 0 * Z(2)], [0 * Z(2), Z(2) ^ 0]]);;
gap> ActionRank(x, 10);
1

# ActionRank, for a matrix over FF semigroup
gap> rank := ActionRank(GLM(2, 2));;
gap> rank(Matrix(GF(2), [[0 * Z(2), 0 * Z(2)], [0 * Z(2), 0 * Z(2)]]));
0
gap> rank(Matrix(GF(2), [[Z(2) ^ 0, 0 * Z(2)], [0 * Z(2), 0 * Z(2)]]));
1

# MinActionRank

# MinActionRank, for a transformation semigroup
gap> MinActionRank(FullTransformationMonoid(2));
1

# MinActionRank, for a partial perm semigroup
gap> MinActionRank(SymmetricInverseSemigroup(2));
0

# MinActionRank, for a bipartition semigroup
gap> MinActionRank(PartitionMonoid(2));
0

# MinActionRank, for a RZMS
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> MinActionRank(R);
0

# MinActionRank, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> MinActionRank(M);
1

# MinActionRank for a matrix over FF semigroup
gap> MinActionRank(GLM(2, 2));
0

# Rho/LambdaOrbOpts

# Rho/LambdaOrbOpts, for a transformation semigroup
gap> LambdaOrbOpts(FullTransformationMonoid(2));
rec( forflatplainlists := true )
gap> RhoOrbOpts(FullTransformationMonoid(2));
rec( forflatplainlists := true )

# Rho/LambdaOrbOpts, for a partial perm semigroup
gap> LambdaOrbOpts(SymmetricInverseSemigroup(2));
rec( forflatplainlists := true )
gap> RhoOrbOpts(SymmetricInverseSemigroup(2));
rec( forflatplainlists := true )

# Rho/LambdaOrbOpts, for a bipartition semigroup
gap> LambdaOrbOpts(PartitionMonoid(2));
rec(  )
gap> RhoOrbOpts(PartitionMonoid(2));
rec(  )

# Rho/LambdaOrbOpts, for a RZMS
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> LambdaOrbOpts(R);
rec(  )
gap> RhoOrbOpts(R);
rec(  )

# Rho/lambdaOrbOpts, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> LambdaOrbOpts(M);
rec(  )
gap> RhoOrbOpts(M);
rec(  )

# Rho/LambdaOrbOpts for a matrix over FF semigroup
gap> LambdaOrbOpts(GLM(2, 2));
rec(  )
gap> RhoOrbOpts(GLM(2, 2));
rec(  )

# Rho/LambdaAct

# Rho/LambdaAct, for a transformation semigroup
gap> x := LambdaAct(FullTransformationMonoid(10));;
gap> x([2, 4, 7], Transformation([4, 2, 6, 6, 3, 1, 6, 5, 3, 7]));
[ 2, 6 ]
gap> x := RhoAct(FullTransformationMonoid(5));;
gap> x([1, 2, 1, 1, 3], Transformation([3, 2, 4, 3, 2]));
[ 1, 2, 1, 1, 2 ]

# Rho/LambdaAct, for a partial perm semigroup
gap> x := LambdaAct(SymmetricInverseMonoid(3));;
gap> x([2, 4], PartialPerm([4, 3, 2, 0]));
[ 3 ]
gap> x := RhoAct(SymmetricInverseMonoid(3));;
gap> x([2, 4], PartialPerm([4, 3, 2, 0]));
[ 1, 3 ]

# Rho/LambdaAct, for a bipartition semigroup
gap> S := PartitionMonoid(3);;
gap> r := BLOCKS_NC([[1, 2], [-3]]);;
gap> s := Bipartition([[1], [2, -1, -2], [3, -3]]);;
gap> x := LambdaAct(S);;
gap> x(r, s);
<blocks: [ 1*, 2* ], [ 3 ]>
gap> x := RhoAct(S);;
gap> x(r, s);
<blocks: [ 1 ], [ 2* ], [ 3 ]>

# Rho/LambdaAct, for an RZMS
gap> R := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[(), 0], [0, ()]]);;
gap> r := RMSElement(R, 1, (1, 3, 2), 1);;
gap> s := RMSElement(R, 1, (2, 3), 2);;
gap> x := LambdaAct(R);;
gap> x(1, MultiplicativeZero(R));
0
gap> x(0, r);
0
gap> x(-1, r);
1
gap> x(1, r);
1
gap> x(2, r);
0
gap> x(1, s);
2
gap> x(2, s);
0
gap> x := RhoAct(R);;
gap> x(1, MultiplicativeZero(R));
0
gap> x(0, r);
0
gap> x(-1, r);
1
gap> x(1, r);
1
gap> x(2, r);
0
gap> x(1, s);
0
gap> x(2, s);
1

# Rho/LambdaAct, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> r := MTSE(M, 1, (3, 4));;
gap> s := MTSE(M, 3, (2, 3));;
gap> x := LambdaAct(M);;
gap> x(3, s);
2
gap> x(2, s);
1
gap> x(2, r);
1
gap> x(3, r);
1
gap> x(1, r);
1
gap> x(0, r);
1
gap> x := RhoAct(M);;
gap> x(3, s);
1
gap> x(2, s);
3
gap> x(2, r);
1
gap> x(3, r);
1
gap> x(1, r);
1
gap> x(0, r);
1

# Rho/LambdaAct, for a matrix over FF semigroup
gap> r := Matrix(GF(2), [[Z(2) ^ 0, Z(2) ^ 0], [Z(2) ^ 0, 0 * Z(2)]]);;
gap> s := Matrix(GF(2), [[Z(2) ^ 0, Z(2) ^ 0], [0 * Z(2), 0 * Z(2)]]);;
gap> b := NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep,
>                                    GF(2),
>                                    [[Z(2) ^ 0, 0 * Z(2)],
>                                     [0 * Z(2), Z(2) ^ 0]]);
<rowbasis of rank 2 over GF(2)>
gap> x := LambdaAct(GLM(2, 2));;
gap> x(b, r);
<rowbasis of rank 2 over GF(2)>
gap> x := RhoAct(GLM(2, 2));;
gap> x(b, s);
<rowbasis of rank 1 over GF(2)>

# Rho/LambdaOrbSeed

# Rho/LambdaOrbSeed, for a transformation semigroup
gap> LambdaOrbSeed(FullTransformationMonoid(4));
[ 0 ]
gap> RhoOrbSeed(FullTransformationMonoid(4));
[ 0 ]

# Rho/LambdaOrbSeed, for a partial perm semigroup
gap> LambdaOrbSeed(SymmetricInverseSemigroup(3));
[ 0 ]
gap> RhoOrbSeed(SymmetricInverseSemigroup(3));
[ 0 ]

# Rho/LambdaOrbSeed, for a bipartition semigroup
gap> LambdaOrbSeed(PartitionMonoid(3));
<blocks: [ 1*, 2*, 3*, 4* ]>
gap> RhoOrbSeed(PartitionMonoid(3));
<blocks: [ 1*, 2*, 3*, 4* ]>

# Rho/LambdaOrbSeed, for an RZMS
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> LambdaOrbSeed(R);
-1
gap> RhoOrbSeed(R);
-1

# Rho/LambdaOrbSeed, for an MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> LambdaOrbSeed(M);
0
gap> RhoOrbSeed(M);
0

# Rho/LambdaOrbSeed, for a matrix over FF semigroup
gap> LambdaOrbSeed(GLM(2, 2));
<rowbasis of rank 4 over GF(2)>
gap> RhoOrbSeed(SLM(2, 2));
<rowbasis of rank 4 over GF(2)>

# Rho/LambdaFunc

# Rho/LambdaFunc, for a transformation semigroup
gap> S := FullTransformationMonoid(3);;
gap> x := LambdaFunc(S);;
gap> x(Transformation([2, 3, 3]));
[ 2, 3 ]
gap> x(IdentityTransformation);
[ 1, 2, 3 ]
gap> x := RhoFunc(S);;
gap> x(Transformation([2, 3, 3]));
[ 1, 2, 2 ]
gap> x(IdentityTransformation);
[ 1, 2, 3 ]

# Rho/LambdaFunc, for a partial perm semigroup
gap> S := SymmetricInverseMonoid(3);;
gap> x := LambdaFunc(S);;
gap> x(PartialPerm([1, 2], [1, 3]));
[ 1, 3 ]
gap> x := RhoFunc(S);;
gap> x(PartialPerm([1, 2], [1, 3]));
[ 1, 2 ]

# Rho/LambdaFunc, for a bipartition semigroup
gap> S := PartitionMonoid(3);;
gap> x := LambdaFunc(S);;
gap> x(Bipartition([[1], [2], [3, -1, -2, -3]]));
<blocks: [ 1*, 2*, 3* ]>
gap> x := RhoFunc(S);;
gap> x(Bipartition([[1], [2], [3, -1, -2, -3]]));
<blocks: [ 1 ], [ 2 ], [ 3* ]>

# Rho/LambdaFunc, for an RZMS
gap> S := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[(), 0], [0, ()]]);;
gap> x := LambdaFunc(S);;
gap> x(MultiplicativeZero(S));
0
gap> x(RMSElement(S, 1, (1, 3), 2));
2
gap> x := RhoFunc(S);;
gap> x(MultiplicativeZero(S));
0
gap> x(RMSElement(S, 1, (1, 3), 2));
1

# Rho/LambdaFunc, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> x := LambdaFunc(M);;
gap> x(MTSE(M, 1, ()));
1
gap> x(MTSE(M, 2, (2, 3)));
3
gap> x := RhoFunc(M);;
gap> x(MTSE(M, 1, ()));
1
gap> x(MTSE(M, 2, (2, 3)));
2

# Rho/LambdaFunc, for a matrix over FF semigroup
gap> S := GLM(2, 3);;
gap> x := LambdaFunc(S);;
gap> x(Matrix(GF(3), [[Z(3) ^ 0, 0 * Z(3)], [0 * Z(3), Z(3)]]));
<rowbasis of rank 2 over GF(3)>
gap> x := RhoFunc(S);;
gap> x(Matrix(GF(3), [[Z(3) ^ 0, 0 * Z(3)], [0 * Z(3), Z(3)]]));
<rowbasis of rank 2 over GF(3)>

# Rho/LambdaRank

# Rho/LambdaRank, for a transformation semigroup
gap> S := FullTransformationMonoid(6);;
gap> x := LambdaRank(S);;
gap> x([]);
0
gap> x([2, 4]);
2
gap> x := RhoRank(S);;
gap> x([]);
0
gap> x([2, 3, 1, 2, 3, 2]);
3

# Rho/LambdaRank, for a partial perm semigroup
gap> S := SymmetricInverseMonoid(5);;
gap> x := LambdaRank(S);;
gap> x([2, 4]);
2
gap> x([]);
0
gap> x := RhoRank(S);;
gap> x([4]);
1
gap> x([]);
0

# Rho/LambdaRank, for a bipartition semigroup
gap> S := PartitionMonoid(3);;
gap> x := LambdaRank(S);;
gap> x := RhoRank(S);;

# Rho/LambdaRank, for an RZMS
gap> S := ReesZeroMatrixSemigroup(SymmetricGroup(3), [[(), 0], [0, ()]]);;
gap> x := LambdaRank(S);;
gap> x(0);
0
gap> x(2);
4
gap> x := RhoRank(S);;
gap> x(0);
0
gap> x(1);
4

# Rho/LambdaRank, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> x := LambdaRank(M);;
gap> x(1);
1
gap> x(2);
2
gap> x(0);
0
gap> x := RhoRank(M);;
gap> x(1);
1
gap> x(2);
2
gap> x(0);
0

# Rho/LambdaRank, for a matrix over FF semigroup
gap> S := GLM(2, 3);;
gap> b := NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep,
>                                    GF(3),
>                                    [[Z(3) ^ 0, Z(3)]]);
<rowbasis of rank 1 over GF(3)>
gap> x := LambdaRank(S);;
gap> x(b);
1
gap> x := RhoRank(S);;
gap> x(b);
1

# Rho/LambdaInverse

# Rho/LambdaInverse, for a transformation semigroup
gap> S := FullTransformationMonoid(4);;
gap> x := LambdaInverse(S);;
gap> x([2, 3], Transformation([1, 4, 1, 1]));
Transformation( [ 3, 2, 3, 2 ] )
gap> x := RhoInverse(S);;
gap> x([1, 2, 2, 1], Transformation([3, 2, 2, 1]));
Transformation( [ 4, 3, 3, 4 ] )

# Rho/LambdaInverse, for a partial perm semigroup
gap> S := SymmetricInverseMonoid(4);;
gap> x := LambdaInverse(S);;
gap> x([1, 4], PartialPerm([1, 2, 4], [4, 1, 2]));
(1,2,4)
gap> x := RhoInverse(S);;
gap> x([2, 3], PartialPerm([2, 3], [3, 2]));
(2,3)

# Rho/LambdaInverse, for a bipartition semigroup
gap> S := PartitionMonoid(4);;
gap> x := LambdaInverse(S);;
gap> x(BLOCKS_NC([[1, 2], [3], [4]]),
>      Bipartition([[1], [2, -1, -2], [3, -3], [4, -4]]));
<block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ], [ 4, -4 ]>
gap> x := RhoInverse(S);;
gap> x(BLOCKS_NC([[1, 2], [3], [4]]),
>      Bipartition([[1], [2, -1, -2], [3, -3], [4, -4]]));
<bipartition: [ 1, 2, -2 ], [ 3, -3 ], [ 4, -4 ], [ -1 ]>

# Rho/LambdaInverse, for an RZMS
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0],
>                                               [(), (), ()],
>                                               [(), (), ()]]);;
gap> x := LambdaInverse(S);;
gap> x(2, MultiplicativeZero(S));
0
gap> x(0, S.1);
(1,(),1)
gap> x(2, S.1);
(1,(),2)
gap> x := RhoInverse(S);;
gap> x(2, MultiplicativeZero(S));
0
gap> x(0, S.1);
(1,(),1)
gap> x(2, S.1);
(2,(),1)

# Rho/LambdaInverse, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> x := LambdaInverse(M);;
gap> x(2, MTSE(M, 1, ()));
(1, ())
gap> x(2, MTSE(M, 2, (2, 3)));
(3, (2,3))
gap> x := RhoInverse(M);;
gap> x(2, MTSE(M, 1, ()));
(1, ())
gap> x(2, MTSE(M, 2, (2, 3)));
(3, (2,3))

# Rho/LambdaInverse, for a matrix over FF semigroup
gap> S := GLM(2, 2);;
gap> x := LambdaInverse(S);;
gap> x(RowSpaceBasis(S.2), S.2);
<a 2x2 matrix over GF2>
gap> x := RhoInverse(S);;
gap> x(RowSpaceBasis(S.2), S.2);
<an immutable 2x2 matrix over GF2>

# Rho/LambdaBound

# Rho/LambdaBound, for a transformation semigroup
gap> S := FullTransformationMonoid(4);;
gap> LambdaBound(S)(1000);
infinity
gap> LambdaBound(S)(6);
720
gap> RhoBound(S)(1000);
infinity
gap> RhoBound(S)(6);
720

# Rho/LambdaBound, for a partial perm semigroup
gap> S := SymmetricInverseMonoid(4);;
gap> LambdaBound(S)(1000);
infinity
gap> LambdaBound(S)(6);
720
gap> RhoBound(S)(1000);
infinity
gap> RhoBound(S)(6);
720

# Rho/LambdaBound, for a bipartition semigroup
gap> S := PartitionMonoid(5);;
gap> LambdaBound(S)(1000);
infinity
gap> LambdaBound(S)(6);
720
gap> RhoBound(S)(1000);
infinity
gap> RhoBound(S)(6);
720

# Rho/LambdaBound, for an RZMS
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> LambdaBound(S)(1000);
infinity
gap> LambdaBound(S)(5);
120
gap> RhoBound(S)(1000);
infinity
gap> RhoBound(S)(5);
120

# Rho/LambdaBound, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> LambdaBound(M)(5);
6
gap> LambdaBound(M)(10000);
6
gap> RhoBound(M)(5);
6
gap> RhoBound(M)(10000);
6

# Rho/LambdaBound, for a matrix over FF semigroup
gap> S := GLM(2, 2);;
gap> LambdaBound(S)(1000);
infinity
gap> LambdaBound(S)(2);
6
gap> LambdaBound(S)(0);
1
gap> RhoBound(S)(1000);
infinity
gap> RhoBound(S)(2);
6
gap> RhoBound(S)(0);
1

# Rho/LambdaIdentity

# Rho/LambdaIdentity, for a transformation semigroup
gap> S := FullTransformationMonoid(2);;
gap> LambdaIdentity(S)(2);
()
gap> RhoIdentity(S)(2);
()

# Rho/LambdaIdentity, for a partial perm semigroup
gap> S := SymmetricInverseMonoid(2);;
gap> LambdaIdentity(S)(2);
()
gap> RhoIdentity(S)(2);
()

# Rho/LambdaIdentity, for a bipartition semigroup
gap> S := PartitionMonoid(1);;
gap> LambdaIdentity(S)(1);
()
gap> RhoIdentity(S)(1);
()

# Rho/LambdaIdentity, for an RZMS
gap> S := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> LambdaIdentity(S)(2);
()
gap> RhoIdentity(S)(2);
()

# Rho/LambdaIdentity, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> LambdaIdentity(M)(2);
()
gap> RhoIdentity(M)(2);
()

# Rho/LambdaIdentity, for a matrix over FF semigroup
gap> S := SLM(2, 2);;
gap> LambdaIdentity(S)(2);
[ <a GF2 vector of length 2>, <a GF2 vector of length 2> ]
gap> RhoIdentity(S)(2);
[ <a GF2 vector of length 2>, <a GF2 vector of length 2> ]

# LambdaPerm

# LambdaPerm, for a transformation semigroup
gap> x := LambdaPerm(FullTransformationMonoid(3));;
gap> x(Transformation([2, 2]), Transformation([3, 3, 2]));
(2,3)

# LambdaPerm, for a partial perm semigroup
gap> x := LambdaPerm(SymmetricInverseMonoid(3));;
gap> x(PartialPerm([2, 0, 3]), PartialPerm([3, 0, 2]));
(2,3)

# LambdaPerm, for a bipartition semigroup
gap> x := LambdaPerm(PartitionMonoid(3));;
gap> x(Bipartition([[1, -2], [2], [3, -3], [-1]]),
>      Bipartition([[1, -3], [2], [3, -2], [-1]]));
(2,3)

# LambdaPerm, for an RZMS
gap> R := ReesZeroMatrixSemigroup(Group((1, 2, 3)), [[()]]);;
gap> x := LambdaPerm(R);;
gap> x(RMSElement(R, 1, (1, 3, 2), 1), RMSElement(R, 1, (1, 2, 3), 1));
(1,3,2)
gap> x(MultiplicativeZero(R), MultiplicativeZero(R));
()

# LambdaPerm, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> x := LambdaPerm(M);;
gap> x(MTSE(M, 1, (2, 3, 4)), MTSE(M, 2, (2, 3)));
(2,4)
gap> x(MTSE(M, 2, ()), MTSE(M, 2, (2, 3)));
(2,3)

# LambdaPerm, for a matrix over FF semigroup
gap> x := LambdaPerm(GLM(2, 3));;
gap> x(Matrix(GF(3), [[Z(3) ^ 0, Z(3) ^ 0], [0 * Z(3), 0 * Z(3)]]),
>      Matrix(GF(3), [[Z(3), Z(3)], [0 * Z(3), 0 * Z(3)]]));
[ [ Z(3) ] ]

# LambdaConjugator

# LambdaConjugator, for a transformation semigroup
gap> x := LambdaConjugator(FullTransformationMonoid(3));;
gap> x(Transformation([3, 1, 1]), Transformation([2, 3, 3]));
(1,3,2)

# LambdaConjugator, for a partial perm semigroup
gap> x := LambdaConjugator(SymmetricInverseMonoid(3));;
gap> x(PartialPerm([2]), PartialPerm([3]));
(2,3)

# LambdaConjugator, for a bipartition semigroup
gap> x := LambdaConjugator(PartitionMonoid(3));;
gap> x(Bipartition([[1, -1, -2], [2], [3, -3]]),
>      Bipartition([[1, -1], [2], [3, -2], [-3]]));
()

# LambdaConjugator, for an RZMS
gap> R := ReesZeroMatrixSemigroup(Group((1, 2, 3)), [[(), 0], [0, ()]]);;
gap> x := LambdaConjugator(R);;
gap> x(RMSElement(R, 1, (1, 3, 2), 1), RMSElement(R, 1, (1, 2, 3), 2));
()

# LambdaConjugator, for an MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> x := LambdaConjugator(M);;
gap> x(MTSE(M, 1, (2, 3, 4)), MTSE(M, 2, (2, 3)));
fail
gap> x(MTSE(M, 2, ()), MTSE(M, 2, (2, 3)));
(2,3)
gap> x(MTSE(M, 3, ()), MTSE(M, 2, (2, 3)));
()

# LambdaConjugator, for a matrix over FF semigroup
gap> x := LambdaConjugator(GLM(2, 3));;
gap> x(Matrix(GF(3), [[Z(3) ^ 0, 0 * Z(3)], [0 * Z(3), Z(3) ^ 0]]),
>      Matrix(GF(3), [[Z(3), 0 * Z(3)], [Z(3), Z(3)]]));
[ [ Z(3)^0, 0*Z(3) ], [ 0*Z(3), Z(3)^0 ] ]

# IdempotentTester and IdempotentCreator

# IdempotentTester and IdempotentCreator, for a transformation semigroup
gap> S := FullTransformationMonoid(3);;
gap> x := IdempotentTester(S);;
gap> y := IdempotentCreator(S);;
gap> x([], [1]);
false
gap> x([1, 2], [1]);
false
gap> x([], []);
true
gap> y([], []);
IdentityTransformation
gap> x([1, 2], [1, 1]);
false
gap> x([1, 2], [1, 2, 1]);
true
gap> y([1, 2], [1, 2, 1]);
Transformation( [ 1, 2, 1 ] )
gap> x([1], [1, 2]);
false

# IdempotentTester and IdempotentCreator, for a partial perm semigroup
gap> S := SymmetricInverseMonoid(3);;
gap> x := IdempotentTester(S);;
gap> y := IdempotentCreator(S);;
gap> x([], []);
true
gap> y([], []);
<empty partial perm>
gap> x([], [1]);
false
gap> x([2, 3], [2, 3]);
true
gap> y([2, 3], [2, 3]);
<identity partial perm on [ 2, 3 ]>

# IdempotentTester and IdempotentCreator, for a bipartition semigroup
gap> S := PartitionMonoid(3);;
gap> x := IdempotentTester(S);;
gap> y := IdempotentCreator(S);;
gap> x(BLOCKS_NC([[1, 2], [-3]]), BLOCKS_NC([[1, 2, 3, 4]]));
true
gap> y(BLOCKS_NC([[1, 2], [-3]]), BLOCKS_NC([[1, 2, 3, 4]]));
<bipartition: [ 1, 2, 3, -1, -2 ], [ -3 ]>
gap> x(BLOCKS_NC([[1, 2], [3]]), BLOCKS_NC([[1, 2, 3]]));
false

# IdempotentTester and IdempotentCreator, for an RZMS
gap> S := ReesZeroMatrixSemigroup(Group([(1, 2)]), [[(), 0], [0, (1, 2)]]);;
gap> x := IdempotentTester(S);;
gap> y := IdempotentCreator(S);;
gap> x(0, 0);
true
gap> y(0, 0);
0
gap> x(1, 1);
true
gap> y(1, 1);
(1,(),1)
gap> x(1, 2);
false
gap> x(2, 1);
false
gap> x(2, 2);
true
gap> y(2, 2);
(2,(1,2),2)

# IdempotentTester and IdempotentCreator, for an MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> x := IdempotentTester(M);;
gap> y := IdempotentCreator(M);;
gap> x(1, 1);
true
gap> x(2, 1);
false
gap> x(2, 2);
true
gap> x(3, 2);
false
gap> x(3, 3);
true
gap> y(2, 2);
(2, ())
gap> y(1, 2);
(1, ())
gap> y(1, 1);
(1, ())
gap> y(3, 2);
(3, ())
gap> y(3, 3);
(3, ())

# IdempotentTester and IdempotentCreator, for a matrix over FF semigroup
gap> S := GLM(2, 3);;
gap> x := IdempotentTester(S);;
gap> y := IdempotentCreator(S);;
gap> x(NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep, GF(3),
>                                 [[Z(3) ^ 0, 0 * Z(3)]]),
>      NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep, GF(3),
>                                 [[0 * Z(3), 0 * Z(3), 0 * Z(3)],
>                                  [0 * Z(3), 0 * Z(3), 0 * Z(3)],
>                                  [0 * Z(3), 0 * Z(3), 0 * Z(3)]]));
Error, Assertion failure
gap> x(NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep, GF(3),
>                                 [[0 * Z(3), Z(3) ^ 0]]),
>      NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep, GF(3),
>                                 [[0 * Z(3), Z(3) ^ 0]]));
true
gap> y(NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep, GF(3),
>                                 [[0 * Z(3), Z(3) ^ 0]]),
>      NewRowBasisOverFiniteField(IsPlistRowBasisOverFiniteFieldRep, GF(3),
>                                 [[0 * Z(3), Z(3) ^ 0]]));
[ [ 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0 ] ]

# StabilizerAction

# StabilizerAction, for a transformation semigroup
gap> x := StabilizerAction(FullTransformationMonoid(2));;
gap> x(Transformation([2, 2]), (1, 2));
Transformation( [ 1, 1 ] )

# StabilizerAction, for a partial perm semigroup
gap> x := StabilizerAction(SymmetricInverseMonoid(2));;
gap> x(PartialPerm([0, 2]), (2, 1));
[2,1]

# StabilizerAction, for a bipartition semigroup
gap> x := StabilizerAction(PartitionMonoid(3));;
gap> x(Bipartition([[1, 3], [2, -1], [-2, -3]]), ());
<bipartition: [ 1, 3 ], [ 2, -1 ], [ -2, -3 ]>

# StabilizerAction, for an RZMS
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> x := StabilizerAction(R);;
gap> x(MultiplicativeZero(R), ());
0
gap> x(RMSElement(R, 1, (), 1), ());
(1,(),1)

# StabilizerAction, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> x := StabilizerAction(M);;
gap> x(MTSE(M, 1, ()), ());
(1, ())
gap> x(MTSE(M, 2, (2, 3)), (2, 3));
(2, ())
gap> x(MTSE(M, 3, ()), (2, 4, 3));
(3, (2,4,3))

# StabilizerAction, for a matrix over FF semigroup
gap> S := GLM(2, 3);;
gap> x := StabilizerAction(S);;
gap> x(One(S), Matrix(GF(3), [[Z(3) ^ 0, 0 * Z(3)], [0 * Z(3), Z(3) ^ 0]]));
[ [ Z(3)^0, 0*Z(3) ], [ 0*Z(3), Z(3)^0 ] ]

# IsActingSemigroupWithFixedDegreeMultiplication

# IsActingSemigroupWithFixedDegreeMultiplication, for a transformation semigroup
gap> IsActingSemigroupWithFixedDegreeMultiplication(
> FullTransformationMonoid(4));
false

# IsActingSemigroupWithFixedDegreeMultiplication, for a partial perm semigroup
gap> IsActingSemigroupWithFixedDegreeMultiplication(
> SymmetricInverseMonoid(3));
false

# IsActingSemigroupWithFixedDegreeMultiplication, for a bipartition semigroup
gap> IsActingSemigroupWithFixedDegreeMultiplication(
> PartitionMonoid(5));
true

# IsActingSemigroupWithFixedDegreeMultiplication, for an RZMS
gap> S := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> IsActingSemigroupWithFixedDegreeMultiplication(Semigroup(S));
false

# IsActingSemigroupWithFixedDegreeMultiplication, for a MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]), Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> IsActingSemigroupWithFixedDegreeMultiplication(Semigroup(M));
false

# IsActingSemigroupWithFixedDegreeMultiplication, for a matrix over FF semigroup
gap> IsActingSemigroupWithFixedDegreeMultiplication(
> GLM(2, 2));
true

# SchutzGpMembership

# SchutzGpMembership, for a transformation semigroup
gap> S := Semigroup([Transformation([1, 2, 1]), Transformation([2, 3, 1])]);;
gap> o := LambdaOrb(S);; Enumerate(o);;
gap> schutz := LambdaOrbStabChain(o, 3);;
gap> SchutzGpMembership(S)(schutz, (1, 2, 3));
true

# SchutzGpMembership, for a partial perm semigroup
gap> S := InverseMonoid([PartialPerm([1, 3, 2]),
>                        PartialPerm([2, 3], [1, 2])]);;
gap> o := LambdaOrb(S);; Enumerate(o);;
gap> schutz := LambdaOrbStabChain(o, 2);;
gap> SchutzGpMembership(S)(schutz, (2, 3));
true

# SchutzGpMembership, for a bipartition semigroup
gap> S := Monoid([
>  Bipartition([[1, -1], [2, -3], [3, -2]]),
>  Bipartition([[1, -2], [2, -3], [3], [-1]]),
>  Bipartition([[1, 2, -1, -2], [3, -3]]),
>  Bipartition([[1], [2, -1], [3, -2, -3]]),
>  Bipartition([[1, -3], [2, 3, -2], [-1]]),
>  Bipartition([[1], [2, -1], [3, -2], [-3]])]);;
gap> o := LambdaOrb(S);; Enumerate(o);;
gap> schutz := LambdaOrbStabChain(o, 2);;
gap> SchutzGpMembership(S)(schutz, (2, 3));
true

# SchutzGpMembership, for an RZMS
gap> R := ReesZeroMatrixSemigroup(Group((1, 2, 3)), [[()]]);;
gap> R := Semigroup(Elements(R));;
gap> o := LambdaOrb(R);; Enumerate(o);;
gap> schutz := LambdaOrbStabChain(o, 3);;
gap> SchutzGpMembership(R)(schutz, ());
true

# SchutzGpMembership, for an MTS
gap> M := McAlisterTripleSemigroup(SymmetricGroup([2, 3, 4]),
> Digraph([[1], [1, 2], [1, 3], [1, 4]]), [1, 2, 3]);;
gap> M := Semigroup(MTSE(M, 2, (2, 3)), MTSE(M, 3, (2, 3)));;
gap> o := LambdaOrb(M);; Enumerate(o);;
gap> schutz := LambdaOrbStabChain(o, 3);;
gap> SchutzGpMembership(M)(schutz, ());
true

# SchutzGpMembership, for a matrix over FF semigroup
gap> S := Monoid([
>  Matrix(GF(2), [[0 * Z(2), Z(2) ^ 0], [0 * Z(2), 0 * Z(2)]]),
>  Matrix(GF(2), [[Z(2) ^ 0, 0 * Z(2)], [Z(2) ^ 0, 0 * Z(2)]]),
>  Matrix(GF(2), [[Z(2) ^ 0, Z(2) ^ 0], [0 * Z(2), Z(2) ^ 0]])]);;
gap> o := Enumerate(LambdaOrb(S));; 
gap> schutz := LambdaOrbStabChain(o, 2);;
gap> SchutzGpMembership(S)(schutz, LambdaIdentity(S)(3));
true

# FakeOne

# FakeOne, for a transformation semigroup
gap> FakeOne(FullTransformationMonoid(1));
IdentityTransformation

# FakeOne, for a partial perm semigroup
gap> FakeOne(SymmetricInverseMonoid(1));
<identity partial perm on [ 1 ]>

# FakeOne, for a bipartition semigroup
gap> FakeOne(PartitionMonoid(1));
<block bijection: [ 1, -1 ]>

# FakeOne, for an RZMS
gap> FakeOne(ReesZeroMatrixSemigroup(Group(()), [[()]]));
<universal fake one>

# FakeOne, for a MTS
gap> FakeOne(McAlisterTripleSemigroup(Group(()), Digraph([[1]]), [1]));
<universal fake one>

# FakeOne, for a matrix over FF semigroup
gap> FakeOne(GLM(2, 2));
<an immutable 2x2 matrix over GF2>

# ChooseHashFunction

# ChooseHashFunction, for an RZMS element and integer
gap> R := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> ChooseHashFunction(MultiplicativeZero(R), 0);
rec( data := 0, func := function( x, hashlen ) ... end )
gap> G := SymmetricGroup(IsPcGroup, 3);;
gap> R := ReesZeroMatrixSemigroup(G, [[Identity(G)]]);;
gap> x := ChooseHashFunction(MultiplicativeNeutralElement(R), 1000);
rec( data := [ 101, 1000 ], func := function( x, hashlen ) ... end )
gap> G := FullPBRMonoid(1);;
gap> R := ReesZeroMatrixSemigroup(G, [[One(G)]]);;
gap> x := ChooseHashFunction(RMSElement(R, 1, One(G), 1), 1);
rec( data := 1, func := function( x, hashlen ) ... end )

# ChooseHashFunction, for an object and integer
gap> x := ChooseHashFunction(fail, 0);
rec( data := fail, func := function( v, data ) ... end )
gap> x.func(fail, fail);
1

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/main/setup.tst");
