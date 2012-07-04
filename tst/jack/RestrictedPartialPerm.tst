gap> START_TEST("Citrus package: RestrictedPartialPerm.tst");
gap> LoadPackage("citrus",false);;
gap> x:=PartialPerm([2..7],[1..6]); RestrictedPartialPerm(x,[2..7]);
[ 2 .. 7 ] -> [ 1 .. 6 ]
[ 2 .. 7 ] -> [ 1 .. 6 ]
gap> STOP_TEST("Citrus package: RestrictedPartialPerm.tst",1);;
Citrus package: RestrictedPartialPerm.tst
GAP4stones: 0
