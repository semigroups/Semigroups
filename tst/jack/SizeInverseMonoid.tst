gap> START_TEST("Citrus package: SizeInverseMonoid.tst");
gap> LoadPackage("citrus",false);;
gap> x1 := PartialPerm( [1,2,3,4,5,6,7,8] );;
gap> x2 := PartialPerm( [1,6,3,4,8,2,7,5] );;
gap> x3 := PartialPerm( [1,2,7,4,8,6,3,5] );;
gap> x4 := PartialPerm( [1,0,0,4,5,0,0,8] );;
gap> x5 := PartialPerm( [1,2,0,4,0,6,0,0] );;
gap> x6 := PartialPerm( [1,0,3,4,0,0,7,0] );;
gap> x7 := PartialPerm( [1,0,0,0,0,0,0,0] );;
gap> im := InverseMonoid( x1, x2, x3, x4, x5, x6, x7 );;
gap> [ Size( im ), Size( AsSet( im ) ) ];
[ 12, 12 ]
gap> STOP_TEST("Citrus package: SizeInverseMonoid.tst",1);;
Citrus package: SizeInverseMonoid.tst
GAP4stones: 0
