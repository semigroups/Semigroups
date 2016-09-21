############################################################################
##
#W  standard/rzms-elements.tst
#Y  Copyright (C) 2016                                Finn Smith
##                                                       
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
gap> START_TEST("Semigroups package: standard/rzms-elements.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

#T# ChooseHashFunction: Test for RZMS elements over pc group 
gap> G := SmallGroup(4, 2);;
gap> a := AsList(G)[1];; b := AsList(G)[2];;
gap> mat := [[a, 0, b], [b, 0, 0], [0, a, b]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> x := RMSElement(S, 1, a, 2);;
gap> func := ChooseHashFunction(x, 25531).func;;
gap> data := ChooseHashFunction(x, 25531).data;
[ 101, 25531 ]
gap> func(x, data);
7811
gap> x := RMSElement(S, 2, b, 3);;
gap> func(x, data);
13117
gap> x := MultiplicativeZero(S);;
gap> func(x, data);
1

#T# hash tables: Test hash table for RZMS elements over pc group

#T# ChooseHashFunction: Test for RZMS elements over transformation semigroups
gap> G := FullTransformationMonoid(3);;
gap> a := AsList(G)[1];; b := AsList(G)[2];;
gap> mat := [[a, 0, b], [b, 0, 0], [0, a, b]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> x := RMSElement(S, 1, a, 2);;
gap> func := ChooseHashFunction(x, 25531).func;;
gap> data := ChooseHashFunction(x, 25531).data;
25531
gap> func(x, 25531);
16615
gap> func(x, data);
16615
gap> x := MultiplicativeZero(S);;
gap> func(x, data);
1
gap> func := ChooseHashFunction(x, 25531).func;;
gap> data := ChooseHashFunction(x, 25531).data;;
gap> func(x, data);
1

#T# ChooseHashFunction: Test for RZMS elements over bipartition semigroups
gap> G := BrauerMonoid(3);;
gap> a := AsList(G)[1];; b := AsList(G)[2];;
gap> mat := [[a, 0, b], [b, 0, 0], [0, a, b]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> x := MultiplicativeZero(S);;
gap> func := ChooseHashFunction(x, 25531).func;;
gap> data := ChooseHashFunction(x, 25531).data;
25531
gap> func(x, 25531);
1
gap> func(x, data);
1
gap> x := RMSElement(S, 1, b, 3);;
gap> func(x, data);
12217
gap> func := ChooseHashFunction(x, 25531).func;;
gap> data := ChooseHashFunction(x, 25531).data;;
gap> func(x, data);
12217

#T# ChooseHashFunction: Test for RZMS elements over a group we can't hash yet
gap> F := FreeGroup("a", "b");;
gap> G := F / [F.1^2, F.2^3, (F.1 * F.2) ^ 5 ];;
gap> a := AsList(G)[1];; b := AsList(G)[2];;
gap> mat := [[a, 0, b], [b, 0, 0], [0, a, b]];;
gap> S := ReesZeroMatrixSemigroup(G, mat);;
gap> x := MultiplicativeZero(S);;
gap> func := ChooseHashFunction(x, 25531).func;;
Error, Semigroups: ChooseHashFunction: error, 
cannot hash RZMS elements over this underlying semigroup,

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(G);
gap> Unbind(a);
gap> Unbind(b);
gap> Unbind(mat);
gap> Unbind(S);
gap> Unbind(x);
gap> Unbind(func);
gap> Unbind(data);

#E# gap> STOP_TEST("Semigroups package: standard/rzms-elements.tst");
