#############################################################################
##
#W  schreier.tst
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#
gap> START_TEST("Citrus package: schreier.tst");
gap> LoadPackage("citrus", false);;

#
gap> InfoLevelInfoWarning:=InfoLevel(InfoWarning);;
gap> InfoLevelInfoCitrus:=InfoLevel(InfoCitrus);;
gap> SetInfoLevel(InfoWarning, 0);;
gap> SetInfoLevel(InfoCitrus, 0);

#
gap> TestTraceRClassRepsTree:=s-> ForAll([1..NrRClasses(s)], i->
> EvaluateWord(Generators(s), TraceRClassRepsTree(s, i))=
> RClassReps(s)[i]);;

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
> Transformation( [ 4, 2, 1, 5, 5 ] ),
> Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
> s:=Semigroup(gens);;
gap> TestTraceRClassRepsTree(s); 
true

#
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
> s:=Monoid(gens);;
gap> TestTraceRClassRepsTree(s); 
true

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 2, 4, 1, 1 ] ),
> Transformation( [ 3, 4, 2, 2 ] ) ];;
> s:=Semigroup(gens);;
gap> TestTraceRClassRepsTree(s);     
true

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 3, 4, 2, 2 ] ),
> Transformation( [ 4, 1, 2, 1 ] ) ];;
> s:=Monoid(gens);;
gap> TestTraceRClassRepsTree(s);
true

#
gap> gens:=[ Transformation( [ 4, 6, 5, 2, 1, 3 ] ),
>  Transformation( [ 6, 3, 2, 5, 4, 1 ] ),
>  Transformation( [ 1, 2, 4, 3, 5, 6 ] ),
>  Transformation( [ 3, 5, 6, 1, 2, 3 ] ),
>  Transformation( [ 5, 3, 6, 6, 6, 2 ] ),
>  Transformation( [ 2, 3, 2, 6, 4, 6 ] ),
>  Transformation( [ 2, 1, 2, 2, 2, 4 ] ),
>  Transformation( [ 4, 4, 1, 2, 1, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> ForAll(s, f-> EvaluateWord(gens, Factorization(s,f))=f);
true

#
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>  Transformation( [ 4, 2, 1, 5, 5 ] ),
>  Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> ForAll(s, f-> EvaluateWord(gens, Factorization(s,f))=f);
true

#
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> ForAll(s, f-> EvaluateWord(gens, Factorization(s,f))=f);
true

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 2, 4, 1, 1 ] ),
> Transformation( [ 3, 4, 2, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> ForAll(s, f-> EvaluateWord(gens, Factorization(s,f))=f);
true

#
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 3, 4, 2, 2 ] ),
> Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> ForAll(s, f-> EvaluateWord(gens, Factorization(s,f))=f);
true

#
gap> SetInfoLevel(InfoWarning, InfoLevelInfoWarning);;
gap> SetInfoLevel(InfoCitrus, InfoLevelInfoCitrus);;
gap> Unbind(InfoLevelInfoCitrus);; Unbind(InfoLevelInfoWarning);;

#
gap> STOP_TEST("Citrus package: schreier.tst", 10000);
