#############################################################################
##
#W  standard/pfmat.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/pfmat.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# #T# pfmat: Matrix, checker, 1/1
# gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5) ^ 2, Z(5) ^ 0]]);
# Matrix(GF(5), [[0*Z(5), Z(5)^3], [Z(5)^2, Z(5)^0]])
#
# #T# pfmat: One, 1/1
# gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5) ^ 2, Z(5) ^ 0]]);
# Matrix(GF(5), [[0*Z(5), Z(5)^3], [Z(5)^2, Z(5)^0]])
# gap> One(mat);
# Matrix(GF(5), [[Z(5)^0, 0*Z(5)], [0*Z(5), Z(5)^0]])
#
# #T# pfmat: RandomMatrix, 1/1
# gap> mat := RandomMatrix(GF(5), 10);
# <10x10 prime field matrix>
#
# #T# pfmat: \*, works, 1/3
# gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5), 0 * Z(5)]]);
# Matrix(GF(5), [[0*Z(5), Z(5)^3], [Z(5), 0*Z(5)]])
# gap> mat ^ 2;
# Matrix(GF(5), [[Z(5)^0, 0*Z(5)], [0*Z(5), Z(5)^0]])
#
# #T# pfmat: \*, wrong dims, 2/3
# gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5), 0 * Z(5)]]);
# Matrix(GF(5), [[0*Z(5), Z(5)^3], [Z(5), 0*Z(5)]])
# gap> mat2 := Matrix(GF(5), [[Z(5)]]);
# Matrix(GF(5), [[Z(5)]])
# gap> mat * mat2;
# Error, Semigroups: * (for matrices over a prime field): usage,
# the arguments must be matrices of the same dimensions,
#
# #T# pfmat: \*, wrong base field, 3/3
# gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5), 0 * Z(5)]]);
# Matrix(GF(5), [[0*Z(5), Z(5)^3], [Z(5), 0*Z(5)]])
# gap> mat2 := Matrix(GF(7), [[0 * Z(7), Z(7) ^ 3], [Z(7), 0 * Z(7)]]);
# Matrix(GF(7), [[0*Z(7), Z(7)^3], [Z(7), 0*Z(7)]])
# gap> mat * mat2;
# Error, Semigroups: * (for matrices over a prime field): usage,
# the arguments must be matrices over the same field,
#
# #T# pfmat: Display, 1/1
# gap> mat := Matrix(GF(11), [[Z(11) ^ 9, 0 * Z(11), Z(11), Z(11) ^ 9, 0 * Z(11)],
# >   [Z(11) ^ 3, Z(11) ^ 4, 0 * Z(11), Z(11) ^ 2, Z(11) ^ 7],
# >   [Z(11) ^ 9, Z(11) ^ 3, Z(11) ^ 5, Z(11) ^ 4, Z(11) ^ 4],
# >   [Z(11) ^ 6, Z(11), Z(11) ^ 7, Z(11) ^ 3, Z(11) ^ 5],
# >   [0 * Z(11), Z(11) ^ 8, Z(11) ^ 3, Z(11) ^ 6, Z(11) ^ 0]]);
# Matrix(GF(11), [[Z(11)^9, 0*Z(11), Z(11), Z(11)^9, 0*Z(11)], 
#   [Z(11)^3, Z(11)^4, 0*Z(11), Z(11)^2, Z(11)^7], 
#   [Z(11)^9, Z(11)^3, Z(11)^5, Z(11)^4, Z(11)^4], 
#   [Z(11)^6, Z(11), Z(11)^7, Z(11)^3, Z(11)^5], 
#   [0*Z(11), Z(11)^8, Z(11)^3, Z(11)^6, Z(11)^0]])
# gap> Display(mat);
#   6  .  2  6  .
#   8  5  .  4  7
#   6  8 10  5  5
#   9  2  7  8 10
#   .  3  8  9  1
#
# #T# SEMIGROUPS_UnbindVariables
# gap> Unbind(mat);
# gap> Unbind(mat2);
#

#E#
gap> STOP_TEST("Semigroups package: standard/pfmat.tst");
