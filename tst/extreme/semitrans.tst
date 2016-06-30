#############################################################################
##
#W  extreme/semitrans.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: extreme/semitrans.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#
gap> file := Concatenation(SEMIGROUPS.PackageDir, 
>                          "/tst/extreme/data/graphc8.gz");;
gap> nr := 0;;
> if IO_FindExecutable("gzip") <> fail then 
>   iter := IteratorFromGeneratorsFile(file);;
>   i := 0;;
>   nr := 0;
>   last_collect := 0;;
>   for gens in iter do
>     S := Semigroup(gens, rec(generic := false));
>     if LargestElementSemigroup(S) <> Maximum(AsList(S)) then
>       nr := nr + 1;
>     fi;
>     if i = last_collect + 100 then
>       last_collect := i;
>       GASMAN("collect");
>     fi;
>     i := i + 1;
>   od;
> fi;
> nr;
0

#E#
gap> STOP_TEST("Semigroups package: extreme/semitrans.tst");
