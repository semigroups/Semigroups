#############################################################################
##
#W  schreier.tst
#Y  Copyright (C) 2011                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

gap> START_TEST("schreier.tst 0.4");

gap> TestTraceRClassRepsTree:=s-> ForAll([1..NrGreensRClasses(s)], i->
> EvaluateWord(Generators(s), TraceRClassRepsTree(s, i))=
> GreensRClassReps(s)[i]);;

gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
> Transformation( [ 4, 2, 1, 5, 5 ] ),
> Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
> s:=Semigroup(gens);;
gap> TestTraceRClassRepsTree(s); 
true

gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
> s:=Monoid(gens);;
gap> TestTraceRClassRepsTree(s); 
true

gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 2, 4, 1, 1 ] ),
> Transformation( [ 3, 4, 2, 2 ] ) ];;
> s:=Semigroup(gens);;
gap> TestTraceRClassRepsTree(s);     
true

gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 3, 4, 2, 2 ] ),
> Transformation( [ 4, 1, 2, 1 ] ) ];;
> s:=Monoid(gens);;
gap> TestTraceRClassRepsTree(s);
true

gap> STOP_TEST("schreier.tst 0.4", 0);
