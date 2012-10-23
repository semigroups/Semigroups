#############################################################################
##
#W  init.g
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

if (not IsBound(SEMIGROUPSC)) and ("semigroups" in SHOW_STAT()) then
  LoadStaticModule("semigroups");
fi;
if (not IsBound(SEMIGROUPSC)) and
   (Filename(DirectoriesPackagePrograms("semigroups"), "semigroups.so") <> fail) then
  LoadDynamicModule(Filename(DirectoriesPackagePrograms("semigroups"), 
    "semigroups.so"));
fi;

ReadPackage("semigroups/gap/pperm.gd");
ReadPackage("semigroups/gap/partition.gd");
ReadPackage("semigroups/gap/binary.gd");
ReadPackage("semigroups/gap/matrix.gd");

ReadPackage("semigroups/gap/greens.gd");
ReadPackage("semigroups/gap/acting.gd");

ReadPackage("semigroups/gap/semigroups.gd");
ReadPackage("semigroups/gap/enums.gd");
ReadPackage("semigroups/gap/iterators.gd");

ReadPackage("semigroups/gap/setup.gd");
ReadPackage("semigroups/gap/regular.gd");
ReadPackage("semigroups/gap/inverse.gd");

ReadPackage("semigroups/gap/attributes.gd");
ReadPackage("semigroups/gap/properties.gd");
ReadPackage("semigroups/gap/orbits.gd");

ReadPackage("semigroups/gap/slp.gd");

ReadPackage("semigroups/gap/examples.gd");
ReadPackage("semigroups/gap/utils.gd");
ReadPackage("semigroups/gap/options.g");

DeclareInfoClass("InfoSemigroups");;

if not IsBound(SEMIGROUPSC) then 
  Info(InfoWarning, 1, "Semigroups: the Semigroups package binary is not available,",
  " and so the");
  Info(InfoWarning, 1, "Semigroups: functions in Semigroups for partial permutations",
  " are not available.");
fi;

BindGlobal("Semigroups_C", IsBound(SEMIGROUPSC));
