gap> START_TEST("Semigroups package: bipartition.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();

# EMBEDDING into T_n
gap> l := List([1,2,3,4,5,15,35,1999,64999,65000],i->RandomTransformation(i));;
gap> ForAll(l,t->t=AsTransformation(AsBipartition(t)));
true

#
gap> SemigroupsStopTest();

#
gap> STOP_TEST( "Semigroups package: bipartition.tst", 0);