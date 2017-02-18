#############################################################################
##
#W  standard/setup.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/setup.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> SEMIGROUPS.DefaultOptionsRec.acting := true;;

# SEMIGROUPS.HashFunctionRZMSE
gap> SEMIGROUPS.HashFunctionRZMSE([1, (), 2], "bananas", ReturnFail, false);
Error, Semigroups: SEMIGROUPS.HashFunctionRZMSE: error,
this shouldn't happen,

# ActionDegree for an RZMS
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> ActionDegree([R.1]);
1

# ActionDegree for matrix over finite field semigroup
gap> S := GLM(2, 2);;
gap> ActionDegree([S.1]);
2

# ActionRank for an RZMS
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> ActionRank(R.1, 10);
1
gap> ActionRank(MultiplicativeZero(R), 10);
0

# ActionRank for a matrix over FF
gap> x := Matrix(GF(2), [[0*Z(2), 0*Z(2)], [0*Z(2), Z(2)^0]]);;
gap> ActionRank(GLM(2, 2))(x);
1
gap> ActionRank(x, 10);
1

# MinActionRank for a RZMS
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> MinActionRank(R);
0

# MinActionRank for a matrix over FF semigroup
gap> S := GLM(2, 2);;
gap> MinActionRank(S);
0

# Rho/LambdaInverse for a RZMS
gap> R := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> LambdaInverse(R)(2, MultiplicativeZero(R));
0
gap> LambdaInverse(R)(0, R.1);
(1,(),1)
gap> RhoInverse(R)(2, MultiplicativeZero(R));
0
gap> RhoInverse(R)(0, R.1);
(1,(),1)

# RhoInverse for a matrix over FF semigroup
gap> S := GLM(2, 2);;
gap> RhoInverse(S)(RowSpaceBasis(S.2), S.2);
Matrix(GF(2), [[0*Z(2), Z(2)^0], [Z(2)^0, 0*Z(2)]])

# LambdaBound
gap> S := PartitionMonoid(5);;
gap> LambdaBound(S)(1000);
infinity
gap> S := ReesZeroMatrixSemigroup(Group([()]), [[(), (), 0], [(), (), ()],
>  [(), (), ()]]);;
gap> LambdaBound(S)(1000);
infinity
gap> S := GLM(2, 2);;
gap> LambdaBound(S)(1000);
infinity

#
gap> STOP_TEST("Semigroups package: standard/setup.tst");
