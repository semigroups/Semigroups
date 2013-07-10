gap> START_TEST("Semigroups package: bipartition.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#the number of iterations, change here to get faster test
gap> N := 333;;

# BASICS
gap> classes:=[[1,2,3, -2], [4, -5], [5, -7], [6, -3, -4], [7], [-1], [-6]];;
gap> f:=BipartitionNC(classes);
<bipartition: [ 1, 2, 3, -2 ], [ 4, -5 ], [ 5, -7 ], [ 6, -3, -4 ], [ 7 ], 
[ -1 ], [ -6 ]>
gap> LeftProjection(f);
<bipartition: [ 1, 2, 3, -1, -2, -3 ], [ 4, -4 ], [ 5, -5 ], [ 6, -6 ], 
[ 7 ], [ -7 ]>

# different order of classes
gap> classes2:=[[-6], [1,2,3, -2], [4, -5], [5, -7], [6, -3, -4], [-1], [7]];;
gap> f = Bipartition(classes2);
true
gap> f:=BipartitionNC([[1,2,-3,-5, -6], [3,-2,-4], [4,7], [5, -7, -8, -9], 
> [6], [8,9,-1]]);
<bipartition: [ 1, 2, -3, -5, -6 ], [ 3, -2, -4 ], [ 4, 7 ], [ 5, -7, -8, -9 
 ], [ 6 ], [ 8, 9, -1 ]>
gap> LeftProjection(f);
<bipartition: [ 1, 2, -1, -2 ], [ 3, -3 ], [ 4, 7 ], [ 5, -5 ], [ 6 ], 
[ 8, 9, -8, -9 ], [ -4, -7 ], [ -6 ]>

# ASSOCIATIVITY
gap> l := List([1..3*N], i->RandomBipartition(17));;
gap> triples := List([1..N], i -> [l[i],l[i+1],l[i+2]]);;
gap> ForAll(triples, x-> ((x[1]*x[2])*x[3]) = (x[1]*(x[2]*x[3])));
true

# EMBEDDING into T_n
gap> l := List([1,2,3,4,5,15,35,1999,64999,65000],i->RandomTransformation(i));;
gap> ForAll(l,t->t=AsTransformation(AsBipartition(t)));
true

#check BlocksIdempotentTester
gap> l:=BlocksByExtRep([1,-2,-2,-3,-3,-4,-4,5,6,-7,-7]);
[ 7, 1, 2, 2, 3, 3, 4, 4, 5, 6, 7, 7, 1, 0, 0, 0, 1, 1, 0 ]
gap> r:=[-8,-8,-9,-9,10,11,-12,-12,-13,-13,14];
[ -8, -8, -9, -9, 10, 11, -12, -12, -13, -13, 14 ]
gap> r:=r-7;
[ -15, -15, -16, -16, 3, 4, -19, -19, -20, -20, 7 ]
gap> r:=BlocksByExtRep([-1,-1,-2,-2,3,4,-5,-5,-6,-6,7]);
[ 7, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 0, 0, 1, 1, 0, 0, 1 ]
gap> BlocksIdempotentTester(l, r);
true
gap> l:=BlocksByExtRep([1,-2,1,-3,1,-3,-3,4,4,-3,5,-6,-7,-7]);
[ 7, 1, 2, 1, 3, 1, 3, 3, 4, 4, 3, 5, 6, 7, 7, 1, 0, 0, 1, 1, 0, 0 ]
gap> r:=BlocksByExtRep([-1,-2,3,4,-1,-5,-6,-6,-7,-7,-8,3,-8,9]);
[ 9, 1, 2, 3, 4, 1, 5, 6, 6, 7, 7, 8, 3, 8, 9, 0, 0, 1, 1, 0, 0, 0, 0, 1 ]
gap> BlocksIdempotentTester(l, r);
true
gap> BlocksIdempotentTester(r, l);
true
gap> l:=BlocksByExtRep([1,-2,-2,-3,-3,-4,-4,5,6,-7,-7]);      
[ 7, 1, 2, 2, 3, 3, 4, 4, 5, 6, 7, 7, 1, 0, 0, 0, 1, 1, 0 ]
gap> r:=BlocksByExtRep([-1,-1,-2,-2,3,4,-5,-5,-6,-6,7]);
[ 7, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 0, 0, 1, 1, 0, 0, 1 ]
gap> BlocksIdempotentTester(r, l);
true
gap> BlocksIdempotentTester(l, r);
true
gap> l:=BlocksByExtRep([1,-2,-2,-3,-3,-4,-4,5,6,-7,-7]);
[ 7, 1, 2, 2, 3, 3, 4, 4, 5, 6, 7, 7, 1, 0, 0, 0, 1, 1, 0 ]
gap> r:=BlocksByExtRep([-1,-1,-2,-2,3,4,-5,-5,-6,-6,-7]);
[ 7, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 0, 0, 1, 1, 0, 0, 0 ]
gap> BlocksIdempotentTester(l, r);
false
gap> BlocksIdempotentTester(r, l);
false
gap> r:=BlocksByExtRep([-1,-2,3,4,-1,-5,-6,-6,-7,-7,-7]);
[ 7, 1, 2, 3, 4, 1, 5, 6, 6, 7, 7, 7, 0, 0, 1, 1, 0, 0, 0 ]
gap> r:=BlocksByExtRep([-1,-2,3,4,-1,-5,-6,-6,-7,-7,-7,3,-7,8]);
[ 8, 1, 2, 3, 4, 1, 5, 6, 6, 7, 7, 7, 3, 7, 8, 0, 0, 1, 1, 0, 0, 0, 1 ]
gap> l:=BlocksByExtRep([1,-2,1,-3,1,-3,-3,4,4,-3,5,-6,-7,-7]);
[ 7, 1, 2, 1, 3, 1, 3, 3, 4, 4, 3, 5, 6, 7, 7, 1, 0, 0, 1, 1, 0, 0 ]
gap> BlocksIdempotentTester(r, l);
false
gap> BlocksIdempotentTester(l, r);
false
gap> BlocksIdempotentTester(l, l);
true
gap> BlocksIdempotentTester(r, r);
true
gap> l:=BlocksByExtRep([1,-2,-2,-3,-3,-4,-4,5,6,-7,-7]);      
[ 7, 1, 2, 2, 3, 3, 4, 4, 5, 6, 7, 7, 1, 0, 0, 0, 1, 1, 0 ]
gap> r:=BlocksByExtRep([-1,-1,-2,-2,3,4,-5,-5,-6,-6,-7]);       
[ 7, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 0, 0, 1, 1, 0, 0, 0 ]
gap> BlocksIdempotentCreator(l, r);
Error, List Element: <list>[6] must have an assigned value in
  out[i + n] := tab1[fuseit( lambda[i + 1] )]; called from 
<function "BlocksIdempotentCreator">( <arguments> )
 called from read-eval loop at line 3 of *stdin*
you can 'return;' after assigning a value
brk> quit;
gap> BlocksIdempotentTester(l, r); 
false
gap> BlocksIdempotentTester(r, l);
false
gap> RereadPackage("semigroups/gap/bipartition.gi");
true
gap> r:=BlocksByExtRep([-1,-1,-2,-2,3,4,-5,-5,-6,-6,-7]);
[ 7, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 0, 0, 1, 1, 0, 0, 0 ]
gap> l:=BlocksByExtRep([1,-2,-2,-3,-3,-4,-4,5,6,-7,-7]);
[ 7, 1, 2, 2, 3, 3, 4, 4, 5, 6, 7, 7, 1, 0, 0, 0, 1, 1, 0 ]
gap> BlocksIdempotentTester(r, l);
false
gap> DegreeOfBlocks(r);
11
gap> DegreeOfBlocks(l);
11
gap> RankOfBlocks(r);
2
gap> RankOfBlocks(l);
3
gap> r:=BlocksByExtRep([-1,-1,-2,-2,3,4,-5,-5,-6,-6,7]); 
[ 7, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 0, 0, 1, 1, 0, 0, 1 ]
gap> BlocksIdempotentTester(r, l);
true
gap> RereadPackage("semigroups/gap/bipartition.gi");
true
gap> RereadPackage("semigroups/gap/bipartition.gi");
Syntax error: warning: unbound global variable in /Users/jdm/gap/pkg/semigroup\
s/gap/bipartition.gi line 1157
  rhonr:=left[1];
             ^
true
gap> RereadPackage("semigroups/gap/bipartition.gi");
true
gap> BlocksIdempotentTester(r, l);
true
gap> BlocksIdempotentTester(l, r);
true
gap> BlocksIdempotentCreator(l, r);
[ 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 3, 8, 8, 9, 9, 10, 10, 4, 7, 11, 11 ]
gap> l:=BlocksByExtRep([1,-2,1,-3,1,-3,-3,4,4,-3,5,-6,-7,-7]);
[ 7, 1, 2, 1, 3, 1, 3, 3, 4, 4, 3, 5, 6, 7, 7, 1, 0, 0, 1, 1, 0, 0 ]
gap> r:=BlocksByExtRep([-1,-2,3,4,-1,-5,-6,-6,-7,-7,-7,3,-7,8]);
[ 8, 1, 2, 3, 4, 1, 5, 6, 6, 7, 7, 7, 3, 7, 8, 0, 0, 1, 1, 0, 0, 0, 1 ]
gap> BlocksIdempotentTester(l, r); 
false
gap> r:=BlocksByExtRep([-1,-2,3,4,-1,-5,-6,-6,-7,-7,-8,3,-8,9]); 
[ 9, 1, 2, 3, 4, 1, 5, 6, 6, 7, 7, 8, 3, 8, 9, 0, 0, 1, 1, 0, 0, 0, 0, 1 ]
gap> BlocksIdempotentTester(l, r);
true
gap> BlocksIdempotentCreator(l, r);
Error, List Element: <list>[1] must have an assigned value in
  out[i + n] := tab1[fuseit( lambda[i + 1] )]; called from 
<function "BlocksIdempotentCreator">( <arguments> )
 called from read-eval loop at line 26 of *stdin*
you can 'return;' after assigning a value
brk> out;
[ 1 ]
brk> i+n;
15
brk> tab1;
[ , 9, 4 ]
brk> fuseit(lambda[i+1]);
1
brk> rhonr;
9
brk> RankOfBlocks(rho);
3
brk> quit;                                                      
gap> RereadPackage("semigroups/gap/bipartition.gi");
true
gap> BlocksIdempotentCreator(l, r);
[ 1, 2, 3, 4, 1, 5, 6, 6, 7, 7, 8, 3, 8, 9, 3, 8, 3, 9, 3, 9, 9, 4, 4, 9, 9, 
  10, 11, 11 ]
gap> RereadPackage("semigroups/gap/bipartition.gi");
true
gap> BlocksIdempotentCreator(l, r);
[ 1, 2, 3, 4, 1, 5, 6, 6, 7, 7, 8, 3, 8, 9, 3, 10, 3, 11, 3, 11, 11, 4, 4, 
  11, 9, 12, 13, 13 ]
gap> BlocksIdempotentCreator(l, l);
[ 1, 2, 1, 3, 1, 3, 3, 4, 4, 3, 5, 6, 7, 7, 1, 8, 1, 9, 1, 9, 9, 4, 4, 9, 5, 
  10, 11, 11 ]
gap> BlocksIdempotentCreator(r, r);
[ 1, 2, 3, 4, 1, 5, 6, 6, 7, 7, 8, 3, 8, 9, 10, 11, 3, 4, 10, 12, 13, 13, 14, 
  14, 15, 3, 15, 9 ]
gap> RereadPackage("semigroups/gap/bipartition.gi");
true
gap> BlocksIdempotentCreator(r, r);
<bipartition: [ 1, 5 ], [ 2 ], [ 3, 12, -3, -12 ], [ 4, -4 ], [ 6 ], 
[ 7, 8 ], [ 9, 10 ], [ 11, 13 ], [ 14, -14 ], [ -1, -5 ], [ -2 ], [ -6 ], 
[ -7, -8 ], [ -9, -10 ], [ -11, -13 ]>
gap> BlocksIdempotentCreator(l, l);
<bipartition: [ 1, 3, 5, -1, -3, -5 ], [ 2 ], [ 4, 6, 7, 10 ], 
[ 8, 9, -8, -9 ], [ 11, -11 ], [ 12 ], [ 13, 14 ], [ -2 ], [ -4, -6, -7, -10 
 ], [ -12 ], [ -13, -14 ]>
gap> x:=last;     
<bipartition: [ 1, 3, 5, -1, -3, -5 ], [ 2 ], [ 4, 6, 7, 10 ], 
[ 8, 9, -8, -9 ], [ 11, -11 ], [ 12 ], [ 13, 14 ], [ -2 ], [ -4, -6, -7, -10 
 ], [ -12 ], [ -13, -14 ]>
gap> x^2;
<bipartition: [ 1, 3, 5, -1, -3, -5 ], [ 2 ], [ 4, 6, 7, 10 ], 
[ 8, 9, -8, -9 ], [ 11, -11 ], [ 12 ], [ 13, 14 ], [ -2 ], [ -4, -6, -7, -10 
 ], [ -12 ], [ -13, -14 ]>
gap> x^2=x;
true
gap> BlocksIdempotentCreator(r, r);
<bipartition: [ 1, 5 ], [ 2 ], [ 3, 12, -3, -12 ], [ 4, -4 ], [ 6 ], 
[ 7, 8 ], [ 9, 10 ], [ 11, 13 ], [ 14, -14 ], [ -1, -5 ], [ -2 ], [ -6 ], 
[ -7, -8 ], [ -9, -10 ], [ -11, -13 ]>
gap> x:=last;;
gap> x^2=x;   
true
gap> InverseOp(x)=x; 
true
gap> x:=BlockIdempotentCreator(l, l);
Error, Variable: 'BlockIdempotentCreator' must have a value
not in any function at line 42 of *stdin*
gap> x:=BlocksIdempotentCreator(l, l);
<bipartition: [ 1, 3, 5, -1, -3, -5 ], [ 2 ], [ 4, 6, 7, 10 ], 
[ 8, 9, -8, -9 ], [ 11, -11 ], [ 12 ], [ 13, 14 ], [ -2 ], [ -4, -6, -7, -10 
 ], [ -12 ], [ -13, -14 ]>
gap> InverseOp(x)=x;
true

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST( "Semigroups package: bipartition.tst", 0);
