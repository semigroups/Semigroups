gap> START_TEST("Citrus package: MultiplicativeZero.tst");
gap> LoadPackage("citrus",false);;
gap> x1 := PartialPerm( [1,2,3,4] );;
gap> x2 := PartialPerm( [1,3,2,4] );;
gap> x3 := PartialPerm( [1,2,0,0] );;
gap> x4 := PartialPerm( [1,0,0,4] );;
gap> zr := PartialPerm( [1,0,0,0] );;
gap> im := InverseMonoid( x1, x2, x3, x4 );;
gap> zr in im;
true
gap> ForAll( im, x -> zr*x = zr and x*zr = zr );
true
gap> zr;
<identity on [ 1 ]>
gap> MultiplicativeZero(im);
<identity on [ 1 ]>
gap> STOP_TEST("Citrus package: MultiplicativeZero.tst",1);;
Citrus package: MultiplicativeZero.tst
GAP4stones: 0
