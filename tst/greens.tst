#############################################################################
##
#W  greens.tst
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

#ReadTest( Filename( DirectoriesPackageLibrary( "monoid", "tst" ), "greens.tst"));
# takes approx. 12 seconds to run currently!

gap> START_TEST("greens.tst 4.0");
gap> SetGasmanMessageStatus("none");
gap> LoadPackage("monoid");;
gap> ReadPackage("monoid/examples/test.gap");;
gap> tmptmptmp:=InfoLevel(InfoWarning);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> out:=[];;
gap> for x in gens do                            
> s:=Semigroup(x);
> Add(out, [NrGreensRClasses(s), Size(s)]);
> od;
gap> out;
[ [ 3, 4 ], [ 2, 10 ], [ 3, 14 ], [ 12, 211 ], [ 4, 28 ], [ 378, 4818 ], 
  [ 2, 5 ], [ 92, 7142 ], [ 81, 615 ], [ 2, 4 ], [ 158, 2255 ], [ 18, 99 ], 
  [ 9, 50 ], [ 16, 76 ], [ 17, 77 ], [ 6, 13 ], [ 19, 330 ], [ 120, 1263 ], 
  [ 1, 1 ], [ 14, 53 ], [ 216, 1306 ], [ 6, 12 ], [ 15, 235 ], [ 23, 235 ], 
  [ 2, 2 ], [ 3, 9 ], [ 2, 2 ], [ 17, 206 ], [ 22, 506 ], [ 24, 340 ], 
  [ 7, 39 ], [ 99, 495 ], [ 10, 18 ], [ 10, 100 ], [ 34, 843 ], [ 14, 210 ], 
  [ 546, 3538 ], [ 2, 3 ], [ 2, 3 ], [ 35, 448 ], [ 21, 515 ], [ 9, 14 ], 
  [ 5, 11 ], [ 17, 23 ], [ 28, 763 ], [ 15, 199 ], [ 21, 170 ], [ 11, 142 ], 
  [ 2, 2 ], [ 33, 1259 ], [ 6, 25 ], [ 64, 426 ], [ 9, 40 ], [ 46, 388 ], 
  [ 6, 25 ], [ 11, 49 ], [ 48, 391 ], [ 7, 40 ], [ 13, 18 ], [ 6, 48 ], 
  [ 30, 792 ], [ 7, 11 ], [ 1, 3 ], [ 2, 3 ], [ 8, 17 ], [ 15, 115 ], 
  [ 49, 1724 ], [ 8, 45 ], [ 6, 46 ], [ 8, 66 ], [ 2, 4 ], [ 1, 3 ], 
  [ 322, 4344 ], [ 30, 661 ], [ 1597, 63890 ], [ 10, 76 ], [ 173, 9084 ], 
  [ 74, 3931 ], [ 15, 117 ], [ 163, 4804 ], [ 14, 106 ], [ 10, 28 ], 
  [ 1, 2 ], [ 53, 328 ], [ 1, 1 ], [ 17, 26 ], [ 172, 1443 ], [ 230, 15176 ], 
  [ 83, 1382 ], [ 158, 1074 ], [ 2, 2 ], [ 26, 535 ], [ 3, 6 ], [ 3, 3 ], 
  [ 44, 1834 ], [ 158, 1776 ], [ 19, 326 ], [ 9, 45 ], [ 32, 379 ], 
  [ 23, 149 ] ]
# 800ms for the previous!
gap> m:=Semigroup(gens[32]);;
gap> Size(m);
495
gap> ForAll(GreensRClasses(m), x-> ForAll(Idempotents(x), y-> y in x));    
true
gap> idem:=Set(Concatenation(List(GreensRClasses(m), Idempotents)));;
gap> idem=Set(Idempotents(m));
true
gap> H:=GreensHClasses(m);;
gap> I:=Concatenation(List(GreensRClasses(m), GreensHClasses));;
gap> ForAll(H, x-> Number(I, y-> Representative(x) in y)=1);
true
gap> Set(Concatenation(List(GreensRClasses(m), GreensHClasses)))=
> Set(GreensHClasses(m));
true
gap> m:=Semigroup(gens[74]);;
gap> r:=GreensRClassOfElement(m, Transformation( [ 2, 1, 2, 2, 1, 2, 1 ] ));;
gap> d:=GreensDClass(r);;
gap> dr:=GreensRClasses(d);;
gap> r2:=First(dr, x-> x=r);;
gap> GreensDClass(r2)=d;
true
gap> m:=Semigroup(GeneratorsOfSemigroup(m));;
gap> r:=GreensRClassOfElement(m, Transformation( [ 2, 1, 2, 2, 1, 2, 1 ] ));;
gap> d:=DClassOfRClass(r);;
gap> dr:=GreensRClasses(d);;
gap> r2:=First(dr, x-> x=r);;
gap> DClassOfRClass(r2)=d;
true
gap> out:=[];;
gap> for x in gens do                            
> s:=Semigroup(x);
> Add(out, NrGreensLClasses(s));
> od;
gap> out;
[ 3, 5, 2, 19, 9, 46, 2, 39, 25, 2, 789, 21, 11, 25, 42, 10, 23, 87, 1, 24, 
  195, 9, 15, 28, 2, 7, 2, 18, 26, 25, 10, 45, 13, 11, 94, 15, 80, 2, 2, 103, 
  21, 10, 7, 14, 27, 14, 20, 13, 2, 30, 9, 23, 17, 34, 8, 13, 31, 10, 17, 12, 
  68, 10, 1, 2, 8, 22, 201, 7, 10, 11, 2, 1, 363, 68, 2423, 11, 57, 84, 12, 
  156, 16, 10, 1, 52, 1, 20, 257, 74, 333, 74, 2, 28, 3, 3, 35, 93, 18, 16, 
  25, 33 ]
gap> ForAll(GreensLClasses(m), x-> ForAll(Idempotents(x), y-> y in x));                 
true
gap> idem:=Set(Concatenation(List(GreensLClasses(m), Idempotents)));
[ Transformation( [ 1, 1, 1, 1, 1, 1, 1 ] ),
  Transformation( [ 1, 1, 1, 1, 5, 1, 1 ] ),
  Transformation( [ 1, 1, 1, 1, 5, 1, 5 ] ),
  Transformation( [ 1, 1, 1, 1, 5, 5, 1 ] ),
  Transformation( [ 1, 1, 1, 4, 1, 1, 1 ] ),
  Transformation( [ 1, 1, 1, 4, 1, 4, 1 ] ),
  Transformation( [ 1, 1, 1, 4, 4, 1, 4 ] ),
  Transformation( [ 1, 1, 1, 4, 5, 1, 5 ] ),
  Transformation( [ 1, 1, 1, 5, 5, 1, 5 ] ),
  Transformation( [ 1, 1, 4, 4, 1, 1, 1 ] ),
  Transformation( [ 1, 1, 4, 4, 4, 1, 4 ] ),
  Transformation( [ 1, 1, 5, 5, 5, 1, 5 ] ),
  Transformation( [ 1, 2, 1, 1, 1, 1, 1 ] ),
  Transformation( [ 1, 2, 1, 1, 2, 1, 1 ] ),
  Transformation( [ 1, 2, 1, 1, 2, 1, 2 ] ),
  Transformation( [ 1, 2, 1, 1, 2, 2, 2 ] ),
  Transformation( [ 1, 2, 1, 1, 5, 1, 1 ] ),
  Transformation( [ 1, 2, 1, 2, 2, 1, 2 ] ),
  Transformation( [ 1, 2, 1, 2, 2, 2, 2 ] ),
  Transformation( [ 1, 2, 2, 1, 1, 1, 1 ] ),
  Transformation( [ 1, 2, 2, 1, 1, 2, 1 ] ),
  Transformation( [ 1, 2, 2, 1, 2, 2, 1 ] ),
  Transformation( [ 1, 2, 2, 1, 5, 5, 1 ] ),
  Transformation( [ 1, 4, 1, 4, 4, 1, 4 ] ),
  Transformation( [ 1, 4, 1, 4, 4, 4, 4 ] ),
  Transformation( [ 1, 5, 1, 1, 5, 1, 1 ] ),
  Transformation( [ 1, 5, 1, 1, 5, 1, 5 ] ),
  Transformation( [ 1, 5, 1, 1, 5, 5, 5 ] ),
  Transformation( [ 1, 5, 1, 4, 5, 4, 5 ] ),
  Transformation( [ 1, 5, 1, 5, 5, 1, 5 ] ),
  Transformation( [ 1, 5, 1, 5, 5, 5, 5 ] ),
  Transformation( [ 1, 5, 5, 1, 5, 5, 1 ] ),
  Transformation( [ 2, 2, 2, 2, 2, 2, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 2, 6, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 5, 2, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 5, 2, 5 ] ),
  Transformation( [ 2, 2, 2, 2, 5, 5, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 6, 6, 2 ] ),
  Transformation( [ 2, 2, 2, 2, 7, 2, 7 ] ),
  Transformation( [ 2, 2, 2, 5, 5, 2, 5 ] ),
  Transformation( [ 2, 2, 2, 6, 2, 6, 2 ] ),
  Transformation( [ 2, 2, 2, 7, 7, 2, 7 ] ),
  Transformation( [ 2, 2, 5, 5, 5, 2, 5 ] ),
  Transformation( [ 2, 2, 7, 7, 7, 2, 7 ] ),
  Transformation( [ 3, 3, 3, 3, 3, 3, 3 ] ),
  Transformation( [ 3, 3, 3, 3, 5, 3, 3 ] ),
  Transformation( [ 3, 3, 3, 3, 5, 3, 5 ] ),
  Transformation( [ 3, 3, 3, 3, 5, 5, 3 ] ),
  Transformation( [ 3, 3, 3, 3, 7, 3, 7 ] ),
  Transformation( [ 3, 3, 3, 4, 3, 3, 3 ] ),
  Transformation( [ 3, 3, 3, 4, 3, 4, 3 ] ),
  Transformation( [ 3, 3, 3, 4, 4, 3, 4 ] ),
  Transformation( [ 3, 3, 3, 4, 5, 3, 5 ] ),
  Transformation( [ 3, 3, 3, 4, 7, 3, 7 ] ),
  Transformation( [ 3, 3, 3, 5, 5, 3, 5 ] ),
  Transformation( [ 3, 3, 3, 7, 7, 3, 7 ] ),
  Transformation( [ 3, 4, 3, 4, 4, 3, 4 ] ),
  Transformation( [ 3, 4, 3, 4, 4, 4, 4 ] ),
  Transformation( [ 3, 5, 3, 3, 5, 3, 3 ] ),
  Transformation( [ 3, 5, 3, 3, 5, 3, 5 ] ),
  Transformation( [ 3, 5, 3, 3, 5, 5, 5 ] ),
  Transformation( [ 3, 5, 3, 4, 5, 4, 5 ] ),
  Transformation( [ 3, 5, 3, 5, 5, 3, 5 ] ),
  Transformation( [ 3, 5, 3, 5, 5, 5, 5 ] ),
  Transformation( [ 3, 7, 3, 3, 7, 3, 7 ] ),
  Transformation( [ 3, 7, 3, 3, 7, 7, 7 ] ),
  Transformation( [ 3, 7, 3, 4, 7, 4, 7 ] ),
  Transformation( [ 3, 7, 3, 7, 7, 3, 7 ] ),
  Transformation( [ 3, 7, 3, 7, 7, 7, 7 ] ),
  Transformation( [ 4, 3, 3, 4, 3, 3, 4 ] ),
  Transformation( [ 4, 3, 3, 4, 4, 3, 4 ] ),
  Transformation( [ 4, 3, 3, 4, 4, 4, 4 ] ),
  Transformation( [ 4, 3, 3, 4, 5, 5, 4 ] ),
  Transformation( [ 4, 4, 4, 4, 4, 4, 4 ] ),
  Transformation( [ 4, 4, 4, 4, 5, 4, 4 ] ),
  Transformation( [ 4, 4, 4, 4, 5, 4, 5 ] ),
  Transformation( [ 4, 4, 4, 4, 5, 5, 4 ] ),
  Transformation( [ 4, 4, 4, 4, 7, 4, 7 ] ),
  Transformation( [ 4, 5, 4, 4, 5, 4, 4 ] ),
  Transformation( [ 4, 5, 4, 4, 5, 4, 5 ] ),
  Transformation( [ 4, 5, 4, 4, 5, 5, 5 ] ),
  Transformation( [ 4, 5, 5, 4, 5, 5, 4 ] ),
  Transformation( [ 4, 7, 4, 4, 7, 4, 7 ] ),
  Transformation( [ 4, 7, 4, 4, 7, 7, 7 ] ),
  Transformation( [ 5, 2, 2, 5, 5, 2, 5 ] ),
  Transformation( [ 5, 2, 2, 5, 5, 5, 5 ] ),
  Transformation( [ 5, 2, 5, 5, 5, 5, 5 ] ),
  Transformation( [ 5, 3, 3, 5, 5, 3, 5 ] ),
  Transformation( [ 5, 3, 3, 5, 5, 5, 5 ] ),
  Transformation( [ 5, 5, 3, 3, 5, 5, 5 ] ),
  Transformation( [ 5, 5, 4, 4, 5, 5, 5 ] ),
  Transformation( [ 5, 5, 5, 4, 5, 4, 5 ] ),
  Transformation( [ 5, 5, 5, 4, 5, 5, 5 ] ),
  Transformation( [ 5, 5, 5, 5, 5, 5, 5 ] ),
  Transformation( [ 6, 2, 2, 6, 6, 6, 6 ] ),
  Transformation( [ 6, 2, 6, 2, 2, 6, 2 ] ),
  Transformation( [ 6, 2, 6, 6, 2, 6, 2 ] ),
  Transformation( [ 6, 2, 6, 6, 2, 6, 6 ] ),
  Transformation( [ 6, 2, 6, 6, 6, 6, 6 ] ),
  Transformation( [ 6, 6, 6, 6, 6, 6, 6 ] ),
  Transformation( [ 6, 6, 6, 6, 7, 6, 7 ] ),
  Transformation( [ 6, 6, 6, 7, 7, 6, 7 ] ),
  Transformation( [ 6, 6, 7, 7, 7, 6, 7 ] ),
  Transformation( [ 6, 7, 6, 6, 7, 6, 7 ] ),
  Transformation( [ 6, 7, 6, 7, 7, 6, 7 ] ),
  Transformation( [ 7, 2, 2, 7, 2, 2, 7 ] ),
  Transformation( [ 7, 2, 2, 7, 5, 5, 7 ] ),
  Transformation( [ 7, 2, 2, 7, 6, 6, 7 ] ),
  Transformation( [ 7, 2, 2, 7, 7, 2, 7 ] ),
  Transformation( [ 7, 2, 2, 7, 7, 7, 7 ] ),
  Transformation( [ 7, 2, 7, 7, 2, 7, 7 ] ),
  Transformation( [ 7, 2, 7, 7, 5, 7, 7 ] ),
  Transformation( [ 7, 2, 7, 7, 7, 7, 7 ] ),
  Transformation( [ 7, 3, 3, 7, 3, 3, 7 ] ),
  Transformation( [ 7, 3, 3, 7, 7, 3, 7 ] ),
  Transformation( [ 7, 3, 3, 7, 7, 7, 7 ] ),
  Transformation( [ 7, 5, 5, 7, 5, 5, 7 ] ),
  Transformation( [ 7, 5, 7, 7, 5, 7, 7 ] ),
  Transformation( [ 7, 6, 6, 7, 6, 6, 7 ] ),
  Transformation( [ 7, 6, 6, 7, 7, 6, 7 ] ),
  Transformation( [ 7, 7, 3, 3, 7, 7, 7 ] ),
  Transformation( [ 7, 7, 4, 4, 7, 7, 7 ] ),
  Transformation( [ 7, 7, 7, 4, 7, 4, 7 ] ),
  Transformation( [ 7, 7, 7, 4, 7, 7, 7 ] ),
  Transformation( [ 7, 7, 7, 6, 7, 6, 7 ] ),
  Transformation( [ 7, 7, 7, 7, 5, 5, 7 ] ),
  Transformation( [ 7, 7, 7, 7, 5, 7, 7 ] ),
  Transformation( [ 7, 7, 7, 7, 6, 6, 7 ] ),
  Transformation( [ 7, 7, 7, 7, 7, 6, 7 ] ),
  Transformation( [ 7, 7, 7, 7, 7, 7, 7 ] ) ]
gap> idem=Set(Idempotents(m));
true
gap> m:=Semigroup(gens[30]);;
gap> r:=GreensLClassOfElement(m, Transformation( [ 3, 3, 3, 3, 3, 3, 5 ] ));;
gap> d:=DClassOfLClass(r);;
gap> dr:=GreensLClasses(d);;
gap> r2:=First(dr, x-> x=r);;
gap> DClassOfLClass(r2)=d;
true
gap> m:=Semigroup(GeneratorsOfSemigroup(m));
<semigroup with 2 generators>
gap>  r:=GreensLClassOfElement(m, Transformation( [ 3, 3, 3, 3, 3, 3, 5 ] ));
{Transformation( [ 3, 3, 3, 3, 3, 3, 5 ] )}
gap> d:=DClassOfLClass(r);;
gap> dr:=GreensLClasses(d);;
gap> r2:=First(dr, x-> x=r);;
gap> DClassOfLClass(r2)=d;
true
gap> out:=[];;
gap> for i in gens do 
> s:=Semigroup(i);
> Add(out, [NrGreensHClasses(s), Length(GreensHClasses(s))]);
> od;
gap> out;
[ [ 3, 3 ], [ 5, 5 ], [ 3, 3 ], [ 77, 77 ], [ 13, 13 ], [ 1281, 1281 ], 
  [ 2, 2 ], [ 1032, 1032 ], [ 231, 231 ], [ 2, 2 ], [ 1355, 1355 ], 
  [ 57, 57 ], [ 28, 28 ], [ 48, 48 ], [ 57, 57 ], [ 12, 12 ], [ 139, 139 ], 
  [ 508, 508 ], [ 1, 1 ], [ 36, 36 ], [ 801, 801 ], [ 10, 10 ], [ 71, 71 ], 
  [ 130, 130 ], [ 2, 2 ], [ 7, 7 ], [ 2, 2 ], [ 83, 83 ], [ 158, 158 ], 
  [ 172, 172 ], [ 22, 22 ], [ 285, 285 ], [ 17, 17 ], [ 40, 40 ], 
  [ 377, 377 ], [ 67, 67 ], [ 1285, 1285 ], [ 2, 2 ], [ 2, 2 ], [ 212, 212 ], 
  [ 153, 153 ], [ 14, 14 ], [ 9, 9 ], [ 22, 22 ], [ 239, 239 ], [ 65, 65 ], 
  [ 91, 91 ], [ 55, 55 ], [ 2, 2 ], [ 367, 367 ], [ 15, 15 ], [ 168, 168 ], 
  [ 26, 26 ], [ 207, 207 ], [ 14, 14 ], [ 29, 29 ], [ 274, 274 ], [ 22, 22 ], 
  [ 17, 17 ], [ 26, 26 ], [ 253, 253 ], [ 10, 10 ], [ 1, 1 ], [ 2, 2 ], 
  [ 13, 13 ], [ 64, 64 ], [ 605, 605 ], [ 20, 20 ], [ 25, 25 ], [ 33, 33 ], 
  [ 2, 2 ], [ 1, 1 ], [ 1520, 1520 ], [ 307, 307 ], [ 9625, 9625 ], 
  [ 41, 41 ], [ 1885, 1885 ], [ 945, 945 ], [ 54, 54 ], [ 1297, 1297 ], 
  [ 58, 58 ], [ 18, 18 ], [ 1, 1 ], [ 173, 173 ], [ 1, 1 ], [ 25, 25 ], 
  [ 737, 737 ], [ 2807, 2807 ], [ 636, 636 ], [ 495, 495 ], [ 2, 2 ], 
  [ 201, 201 ], [ 3, 3 ], [ 3, 3 ], [ 471, 471 ], [ 715, 715 ], [ 118, 118 ], 
  [ 28, 28 ], [ 197, 197 ], [ 88, 88 ] ]
gap> out:=[];; out2:=[];; out3:=[];;
gap> for i in gens do 
> s:=Semigroup(i);
> Add(out, [Number(GreensDClasses(s), IsRegularDClass), NrGreensDClasses(s)]);
> Add(out2,  List(GreensDClasses(s), x-> Length(Idempotents(x))));
> Add(out3, NrIdempotents(s));
> if not Number(GreensHClasses(s), IsGroupHClass)
> =Length(Idempotents(s)) then 
> Print("Something is wrong!");
> fi;
> od;
gap> out; out2; out3;
[ [ 1, 3 ], [ 2, 2 ], [ 2, 2 ], [ 4, 4 ], [ 3, 3 ], [ 6, 9 ], [ 1, 2 ], 
  [ 5, 5 ], [ 4, 6 ], [ 1, 2 ], [ 6, 75 ], [ 3, 10 ], [ 3, 4 ], [ 4, 8 ], 
  [ 3, 12 ], [ 3, 5 ], [ 4, 4 ], [ 4, 16 ], [ 1, 1 ], [ 4, 10 ], [ 6, 101 ], 
  [ 4, 5 ], [ 4, 4 ], [ 4, 8 ], [ 1, 2 ], [ 2, 3 ], [ 1, 2 ], [ 3, 6 ], 
  [ 5, 7 ], [ 5, 6 ], [ 3, 4 ], [ 5, 19 ], [ 3, 8 ], [ 3, 3 ], [ 5, 13 ], 
  [ 4, 4 ], [ 6, 36 ], [ 1, 2 ], [ 1, 2 ], [ 4, 14 ], [ 4, 4 ], [ 3, 7 ], 
  [ 3, 4 ], [ 4, 11 ], [ 4, 4 ], [ 4, 4 ], [ 4, 7 ], [ 3, 3 ], [ 1, 2 ], 
  [ 4, 4 ], [ 3, 4 ], [ 4, 7 ], [ 4, 6 ], [ 4, 16 ], [ 4, 4 ], [ 3, 7 ], 
  [ 6, 8 ], [ 3, 4 ], [ 3, 13 ], [ 3, 3 ], [ 4, 7 ], [ 3, 7 ], [ 1, 1 ], 
  [ 2, 2 ], [ 2, 4 ], [ 4, 9 ], [ 4, 10 ], [ 3, 3 ], [ 3, 3 ], [ 3, 3 ], 
  [ 1, 2 ], [ 1, 1 ], [ 5, 54 ], [ 3, 10 ], [ 7, 32 ], [ 3, 4 ], [ 5, 7 ], 
  [ 5, 15 ], [ 5, 5 ], [ 5, 22 ], [ 2, 7 ], [ 3, 5 ], [ 1, 1 ], [ 4, 17 ], 
  [ 1, 1 ], [ 4, 14 ], [ 5, 62 ], [ 6, 11 ], [ 5, 26 ], [ 5, 15 ], [ 1, 2 ], 
  [ 3, 8 ], [ 1, 3 ], [ 1, 3 ], [ 6, 6 ], [ 4, 19 ], [ 3, 4 ], [ 3, 5 ], 
  [ 4, 6 ], [ 3, 13 ] ]
[ [ 0, 0, 1 ], [ 1, 4 ], [ 1, 2 ], [ 7, 1, 30, 5 ], [ 1, 4, 4 ], 
  [ 0, 167, 11, 1, 1, 168, 0, 6, 0 ], [ 0, 1 ], [ 2, 42, 197, 169, 6 ], 
  [ 2, 0, 58, 18, 0, 5 ], [ 0, 1 ], 
  [ 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 159, 0, 8, 0, 0, 46, 0, 0, 0, 
      0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0 ], [ 0, 0, 0, 19, 0, 0, 1, 0, 0, 5 ], [ 2, 11, 0, 4 ], 
  [ 0, 0, 2, 1, 0, 0, 14, 4 ], [ 0, 0, 0, 0, 0, 1, 0, 0, 12, 3, 0, 0 ], 
  [ 1, 3, 0, 2, 0 ], [ 2, 17, 39, 5 ], 
  [ 0, 0, 24, 0, 0, 1, 137, 0, 0, 6, 0, 0, 0, 0, 0, 0 ], [ 1 ], 
  [ 0, 0, 1, 10, 0, 1, 0, 0, 0, 3 ], 
  [ 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 1, 0, 221, 0, 0, 1, 0, 0, 0, 0, 0, 
      0, 0, 7, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0 ], [ 2, 0, 1, 2, 3 ], [ 1, 12, 24, 4 ], 
  [ 1, 0, 0, 0, 0, 1, 34, 7 ], [ 0, 1 ], [ 1, 0, 3 ], [ 0, 1 ], 
  [ 0, 0, 0, 9, 36, 5 ], [ 1, 0, 17, 0, 50, 1, 5 ], [ 1, 0, 7, 1, 63, 7 ], 
  [ 1, 0, 8, 4 ], [ 2, 0, 0, 0, 0, 0, 13, 0, 69, 0, 0, 0, 0, 0, 7, 0, 0, 0, 1 
     ], [ 0, 0, 4, 0, 0, 1, 2, 0 ], [ 17, 4, 4 ], 
  [ 93, 0, 0, 0, 20, 0, 6, 0, 0, 0, 1, 2, 0 ], [ 1, 10, 24, 4 ], 
  [ 0, 0, 0, 0, 0, 105, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 
      199, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 1 ], [ 0, 1 ], 
  [ 0, 1, 0, 13, 0, 0, 0, 0, 0, 48, 0, 0, 0, 5 ], [ 20, 1, 51, 5 ], 
  [ 0, 0, 3, 0, 0, 1, 2 ], [ 3, 0, 1, 3 ], [ 0, 1, 0, 0, 3, 0, 0, 0, 1, 2, 0 ]
    , [ 2, 27, 82, 7 ], [ 1, 9, 24, 4 ], [ 0, 0, 1, 3, 38, 0, 5 ], 
  [ 6, 24, 4 ], [ 0, 1 ], [ 47, 1, 121, 6 ], [ 1, 0, 5, 4 ], 
  [ 14, 0, 0, 1, 0, 42, 5 ], [ 1, 0, 8, 1, 0, 3 ], 
  [ 80, 0, 0, 0, 0, 6, 6, 0, 0, 0, 1, 0, 0, 0, 0, 0 ], [ 1, 1, 6, 3 ], 
  [ 0, 0, 0, 11, 0, 1, 4 ], [ 0, 1, 20, 6, 65, 1, 4, 0 ], [ 0, 1, 10, 4 ], 
  [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0 ], [ 2, 10, 4 ], 
  [ 34, 0, 0, 1, 62, 0, 5 ], [ 0, 1, 0, 1, 0, 0, 2 ], [ 1 ], [ 1, 1 ], 
  [ 3, 0, 0, 4 ], [ 1, 0, 0, 0, 0, 0, 6, 26, 1 ], 
  [ 47, 2, 0, 121, 0, 0, 0, 6, 0, 0 ], [ 10, 2, 3 ], [ 1, 11, 4 ], 
  [ 3, 15, 4 ], [ 0, 1 ], [ 1 ], 
  [ 0, 0, 0, 0, 248, 3, 0, 0, 0, 0, 1, 0, 0, 122, 0, 7, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 111, 0, 12, 7, 0, 0 ], 
  [ 0, 1, 0, 0, 0, 0, 0, 9, 0, 0, 0, 258, 0, 0, 0, 0, 0, 1, 889, 0, 0, 0, 0, 
      0, 430, 7, 0, 0, 0, 0, 0, 0 ], [ 20, 0, 1, 4 ], 
  [ 1, 324, 0, 12, 231, 6, 0 ], 
  [ 0, 0, 143, 0, 0, 0, 1, 0, 163, 3, 0, 0, 6, 0, 0 ], [ 3, 1, 1, 24, 4 ], 
  [ 0, 5, 0, 0, 0, 0, 0, 140, 0, 277, 0, 1, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 0, 0, 23, 0, 0, 5 ], [ 1, 0, 4, 0, 4 ], [ 1 ], 
  [ 52, 1, 5, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], [ 1 ], 
  [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 1, 1 ], 
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 177, 11, 0, 0, 7, 0, 0, 0, 0, 0, 0, 1, 0, 
      0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
  [ 2, 0, 0, 0, 38, 434, 0, 1, 390, 0, 7 ], 
  [ 0, 0, 40, 0, 0, 0, 5, 0, 0, 0, 0, 0, 114, 0, 9, 0, 5, 0, 0, 0, 0, 0, 0, 
      0, 0, 0 ], [ 1, 32, 0, 0, 2, 65, 0, 6, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 1 ], 
  [ 0, 0, 0, 0, 16, 0, 74, 6 ], [ 0, 0, 1 ], [ 0, 0, 1 ], 
  [ 4, 1, 1, 114, 65, 6 ], 
  [ 0, 0, 1, 0, 0, 0, 40, 0, 0, 0, 200, 7, 0, 0, 0, 0, 0, 0, 0 ], 
  [ 8, 0, 44, 5 ], [ 0, 1, 10, 0, 3 ], [ 0, 1, 0, 6, 73, 6 ], 
  [ 0, 0, 0, 0, 0, 0, 33, 0, 1, 0, 4, 0, 0 ] ]
[ 1, 5, 3, 43, 9, 354, 1, 416, 83, 1, 220, 25, 17, 21, 16, 6, 63, 168, 1, 15, 
  240, 8, 41, 43, 1, 4, 1, 50, 74, 79, 13, 92, 7, 25, 122, 39, 314, 1, 1, 67, 
  77, 6, 7, 7, 118, 38, 47, 34, 1, 175, 10, 62, 13, 93, 11, 16, 97, 15, 4, 
  16, 102, 4, 1, 2, 7, 34, 176, 15, 16, 22, 1, 1, 381, 130, 1595, 25, 574, 
  316, 33, 430, 28, 9, 1, 63, 1, 5, 197, 872, 173, 106, 1, 96, 1, 1, 191, 
  248, 57, 14, 86, 38 ]
gap> a:=Transformation( [ 2, 1, 4, 5, 6, 3 ] );;
gap> b:=Transformation( [ 2, 3, 1, 5, 4, 1 ] );;
gap> M:=Semigroup(a,b);;
gap> GreensLClassOfElement(M,a);
{Transformation( [ 2, 1, 4, 5, 6, 3 ] )}
gap> IsGreensClassOfTransSemigp(last);
true
gap> f:=FreeSemigroup(3);;
gap> a:=f.1;; b:=f.2;; c:=f.3;; 
gap> s:=f/[[a^2, a], [b^2,b], [c^2,c], [a*b,a], [b*a,b], [a*c,a], [c*a,c], [b*c,b],[c*b,c]] ;
<fp semigroup on the generators [ s1, s2, s3 ]>
gap> Size(s);
3
gap> GreensLClassOfElement(s,a);
{s1}
gap> IsGreensClassOfTransSemigp(last);
false
gap> gens:=[ Transformation( [ 2, 2, 5, 2, 3 ] ), 
> Transformation( [ 2, 5, 3, 5, 3 ] ) ];;
gap> S:=Semigroup(gens);;
gap> f:=Transformation( [ 5, 5, 3, 5, 3 ] );;
gap> GreensHClassData(GreensHClassOfElement(S, f));;
gap> Representative(last);
Transformation( [ 5, 5, 3, 5, 3 ] )
gap> IsTrivial(SchutzenbergerGroup(last2));
true
gap> gens:=[ Transformation( [ 4, 1, 4, 5, 3 ] ),
> Transformation( [ 5, 3, 5, 4, 3 ] ) ];;
gap> S:=Semigroup(gens);;
gap> C:=GreensLClassOfElement(S, gens[1]*gens[2]*gens[1]);
{Transformation( [ 5, 3, 5, 4, 3 ] )}
gap> GreensLClassData(C);
GreensLClassData( Transformation( [ 5, 3, 5, 4, 3 ] ), 
[ [ [ 1, 3 ], [ 2, 5 ], [ 4 ] ] ], [ Binary Relation on 5 points ], 
[ Binary Relation on 5 points ], Group( [ (), (3,5,4), (3,5) ] ) )
gap> gens:=[ Transformation( [ 1, 2, 1, 2, 1 ] ), 
> Transformation( [ 3, 4, 2, 1, 4 ] ) ];;
gap> S:=Semigroup(gens);; 
gap> GreensRClassReps(S);
[ Transformation( [ 1, 2, 1, 2, 1 ] ), Transformation( [ 3, 4, 2, 1, 4 ] ),
  Transformation( [ 1, 2, 2, 1, 2 ] ), Transformation( [ 2, 1, 2, 1, 1 ] ) ]
gap> a:=Transformation( [ 2, 1, 4, 5, 6, 3 ] );;
gap> b:=Transformation( [ 2, 3, 1, 5, 4, 1 ] );;
gap> M:=Semigroup(a,b);;
gap> rc:=GreensRClassOfElement(M, a*b*a);
{Transformation( [ 3, 2, 5, 4, 1, 1 ] )}
gap> Transformation( [ 4, 1, 6, 5, 2, 2 ] ) in rc;
true
gap> GreensRClassData(rc);
GreensRClassData( Transformation( [ 3, 2, 5, 4, 1, 1 ] ),
[ [ 1, 2, 3, 4, 5 ], [ 1, 2, 4, 5, 6 ], [ 1, 2, 3, 5, 6 ], [ 1, 2, 3, 4, 6 ]
 ], [ (), (1,2)(3,6,5,4), (3,5)(4,6), (1,2)(3,4,5,6) ], Group(
[ (1,2,3)(4,5), (1,3,5) ] ) )
gap> gens:=[ Transformation( [ 4, 1, 5, 2, 4 ] ), 
> Transformation( [ 4, 4, 1, 5, 3 ] ) ];;
gap> S:=Semigroup(gens);;
gap> f:=Transformation( [ 5, 5, 3, 3, 3 ] );;
gap> GreensDClassOfElement(S, f);
{Transformation( [ 2, 2, 4, 4, 4 ] )}
gap> gens:=[ Transformation( [ 4, 4, 3, 5, 3 ] ), 
> Transformation( [ 5, 1, 1, 4, 1 ] ), 
> Transformation( [ 5, 5, 4, 4, 5 ] ) ];;
gap> S:=Semigroup(gens);;
gap> f:=Transformation( [ 4, 5, 5, 5, 5 ] );;
gap> SchutzenbergerGroup(GreensDClassOfElement(S, f));
Group([ (4,5) ])
gap> SchutzenbergerGroup(GreensRClassOfElement(S, f));
Group([ (4,5) ])
gap> SchutzenbergerGroup(GreensLClassOfElement(S, f));
Group([ (4,5) ])
gap> SchutzenbergerGroup(GreensHClassOfElement(S, f));
Group([ (4,5) ])
gap>  S:=Semigroup([ Transformation( [ 6, 4, 4, 4, 6, 1 ] ), 
> Transformation( [ 6, 5, 1, 6, 2, 2 ] ) ]);;
gap> ImagesOfTransSemigroup(S, 6);
<open orbit, 1 points with grading>
gap> Enumerate(last);
<closed orbit, 1 points with grading>
gap> ImagesOfTransSemigroup(S, 5);
<open orbit, 1 points with grading>
gap> Enumerate(last);
<closed orbit, 1 points with grading>
gap> ImagesOfTransSemigroup(S, 4);
<open orbit, 1 points with grading>
gap> Enumerate(last);
<closed orbit, 2 points with grading>
gap> AsList(last);
[ [ 1 .. 6 ], [ 1, 2, 5, 6 ] ]
gap> ImagesOfTransSemigroup(S, 3);
<open orbit, 1 points with grading>
gap> Enumerate(last);
<closed orbit, 4 points with grading>
gap> AsList(last);
[ [ 1 .. 6 ], [ 1, 4, 6 ], [ 1, 2, 5, 6 ], [ 2, 5, 6 ] ]
gap> ImagesOfTransSemigroup(S, 2);
<open orbit, 1 points with grading>
gap> Enumerate(last);
<closed orbit, 8 points with grading>
gap> AsList(last);
[ [ 1 .. 6 ], [ 1, 4, 6 ], [ 1, 2, 5, 6 ], [ 2, 6 ], [ 2, 5, 6 ], [ 1, 4 ], 
  [ 2, 5 ], [ 4, 6 ] ]
gap> ImagesOfTransSemigroup(S, 1);
<open orbit, 1 points with grading>
gap> Enumerate(last);
<closed orbit, 13 points with grading>
gap> AsList(last);
[ [ 1 .. 6 ], [ 1, 4, 6 ], [ 1, 2, 5, 6 ], [ 2, 6 ], [ 2, 5, 6 ], [ 1, 4 ], 
  [ 2, 5 ], [ 4, 6 ], [ 6 ], [ 1 ], [ 2 ], [ 4 ], [ 5 ] ]
gap> ImagesOfTransSemigroup(S);
<open orbit, 1 points>
gap> Enumerate(last);
<closed orbit, 13 points>
gap> AsList(last);
[ [ 1 .. 6 ], [ 1, 4, 6 ], [ 1, 2, 5, 6 ], [ 2, 6 ], [ 2, 5, 6 ], [ 1, 4 ], 
  [ 2, 5 ], [ 4, 6 ], [ 6 ], [ 1 ], [ 2 ], [ 4 ], [ 5 ] ]
gap> S:=Semigroup([ Transformation( [ 2, 3, 4, 1 ] ), 
> Transformation( [ 3, 3, 1, 1 ] ) ]);;
gap> Idempotents(S, 1);
[  ]
gap> Idempotents(S, 2);                        
[ Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 2, 2, 4, 4 ] ),
  Transformation( [ 1, 3, 3, 1 ] ), Transformation( [ 4, 2, 2, 4 ] ) ]
gap> Idempotents(S, 3);                        
[  ]
gap> Idempotents(S, 4);                        
[ Transformation( [ 1, 2, 3, 4 ] ) ]
gap> Idempotents(S);
[ Transformation( [ 1, 1, 3, 3 ] ), Transformation( [ 2, 2, 4, 4 ] ),
  Transformation( [ 1, 3, 3, 1 ] ), Transformation( [ 4, 2, 2, 4 ] ),
  Transformation( [ 1, 2, 3, 4 ] ) ]
gap> S:=Semigroup([ Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ]);;
gap> KernelsOfTransSemigroup(S); 
<open orbit, 1 points>
gap> Enumerate(last);
<closed orbit, 9 points>
gap> AsList(last);
[ [ [ 1 ], [ 2 ], [ 3 ], [ 4 ] ], [ [ 1, 4 ], [ 2 ], [ 3 ] ], 
  [ [ 1, 2 ], [ 3 ], [ 4 ] ], [ [ 1, 4 ], [ 2, 3 ] ], [ [ 1, 2 ], [ 3, 4 ] ], 
  [ [ 1, 3, 4 ], [ 2 ] ], [ [ 1, 2, 3 ], [ 4 ] ], [ [ 1, 2, 3, 4 ] ], 
  [ [ 1, 2, 4 ], [ 3 ] ] ]
gap> gens:= [ Transformation( [ 4, 3, 3, 6, 7, 2, 3 ] ), 
>   Transformation( [ 6, 6, 4, 4, 2, 1, 4 ] ) ];;
gap> S:=Semigroup(gens);;
gap> Length(GreensRClasses(S));
17
gap> GreensData(GreensRClasses(S)[10]);
GreensRClassData( Transformation( [ 3, 3, 3, 3, 3, 2, 3 ] ), 
[ [ 2, 3 ], [ 4, 6 ], [ 2, 6 ], [ 1, 4 ], [ 1, 6 ], [ 2, 4 ], [ 3, 6 ] ], 
[ (), (2,4,3,5,6), (3,4,5,6), (1,2)(3,4), (1,3,4,5,6,2), (3,4), (2,4,5,6,3) 
 ], Group( [ (2,3) ] ) )
gap> SchutzenbergerGroup(last);
Group([ (2,3) ])
gap> Number(GreensDClasses(S), IsRegularDClass);
3
gap> SetInfoLevel(InfoWarning, tmptmptmp);;
gap> STOP_TEST( "greens.tst 4.0", 10000);