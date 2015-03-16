#%T##########################################################################
##
#W  factor.tst
#Y  Copyright (C) 2011-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#
gap> START_TEST("Semigroups package: factor.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

#T# FactorTest1: Factorization for a transformation semigroup 1
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
> Transformation( [ 4, 2, 1, 5, 5 ] ),
> Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
> s:=Semigroup(gens);;
gap> ForAll(s, f-> EvaluateWord(gens, Factorization(s,f))=f);
true

#T# FactorTest2: Factorization for a transformation semigroup 2
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 2, 4, 1, 1 ] ),
> Transformation( [ 3, 4, 2, 2 ] ) ];;
> s:=Semigroup(gens);;
gap> ForAll(s, f-> EvaluateWord(gens, Factorization(s,f))=f);
true

#T# FactorTest3: Factorization for a transformation semigroup 3
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

#T# FactorTest4: Factorization for a transformation semigroup 4
gap> gens:=[ Transformation( [ 3, 4, 1, 2, 1 ] ),
>  Transformation( [ 4, 2, 1, 5, 5 ] ),
>  Transformation( [ 4, 2, 2, 2, 4 ] ) ];;
gap> s:=Semigroup(gens);;
gap> ForAll(s, f-> EvaluateWord(gens, Factorization(s,f))=f);
true

#T# FactorTest5: Factorization for a transformation semigroup 5
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 2, 4, 1, 1 ] ),
> Transformation( [ 3, 4, 2, 2 ] ) ];;
gap> s:=Semigroup(gens);;
gap> ForAll(s, f-> EvaluateWord(gens, Factorization(s,f))=f);
true

#T# FactorTest6: Factorization for a transformation monoid 1
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
> s:=Monoid(gens);;
gap> ForAll(s, f-> EvaluateWord(GeneratorsOfSemigroup(s), 
> Factorization(s,f))=f);
true

#T# FactorTest7: Factorization for a transformation monoid 2
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 3, 4, 2, 2 ] ),
> Transformation( [ 4, 1, 2, 1 ] ) ];;
> s:=Monoid(gens);;
gap> ForAll(s, f-> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s,f))=f);
true

#T# FactorTest8: Factorization for a transformation monoid 3
gap> gens:=[ Transformation( [ 1, 3, 4, 1 ] ),
> Transformation( [ 2, 4, 1, 2 ] ),
> Transformation( [ 3, 1, 1, 3 ] ),
> Transformation( [ 3, 3, 4, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> ForAll(s, f-> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s,f))=f);
true

#T# FactorTest9: Factorization for a transformation monoid 4
gap> gens:=[ Transformation( [ 1, 3, 2, 3 ] ),
> Transformation( [ 1, 4, 1, 2 ] ),
> Transformation( [ 3, 4, 2, 2 ] ),
> Transformation( [ 4, 1, 2, 1 ] ) ];;
gap> s:=Monoid(gens);;
gap> ForAll(s, f-> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s,f))=f);
true

#T# FactorTest10: Factorization for a partial perm semigroup
gap> gens:=[ PartialPerm( [ 1, 2 ], [ 3, 1 ] ), 
> PartialPerm( [ 1, 2, 3 ], [ 1, 3, 4 ] ), 
> PartialPerm( [ 1, 2, 3 ], [ 2, 4, 1 ] ), 
> PartialPerm( [ 1, 3, 4 ], [ 3, 4, 1 ] ) ];;
gap> s:=Semigroup(gens);;
gap> ForAll(s, f-> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s,f))=f);
true

#T# FactorTest11: Factorization for a partial perm inverse semigroup
gap> gens:=[ PartialPerm( [ 1, 2, 4, 5 ], [ 2, 6, 1, 4 ] ), 
>  PartialPerm( [ 1, 2, 5 ], [ 4, 3, 6 ] ), 
>  PartialPerm( [ 1, 3, 4, 5 ], [ 5, 1, 6, 4 ] ), 
>  PartialPerm( [ 1, 3, 4, 5 ], [ 5, 2, 6, 1 ] ) ];;
gap> s:=InverseSemigroup(gens);;
gap> ForAll(s, f-> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s,f))=f);
true

#T# FactorTest12: Factorization for a known regular semigroup
gap> s:=OrderEndomorphisms(7);;
gap> ForAll(s, f-> EvaluateWord(GeneratorsOfSemigroup(s),
> Factorization(s,f))=f);
true

#T# FactorTest13: Factorization regularity is learned after creation
gap> s:=SingularTransformationSemigroup(5);
<regular transformation semigroup ideal on 5 pts with 1 generator>
gap> s:=Semigroup(GeneratorsOfSemigroup(s));
<transformation semigroup on 5 pts with 16 generators>
gap> IsRegularSemigroup(s);
true
gap> ForAll(s, f-> EvaluateWord(GeneratorsOfSemigroup(s), 
> Factorization(s,f))=f);
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(s);
gap> Unbind(gens);

#E#
gap> STOP_TEST("Semigroups package: factor.tst");
