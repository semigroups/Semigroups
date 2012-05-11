#############################################################################
##
#W  init.g
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


if (not IsBound(CITRUSC)) and ("citrus" in SHOW_STAT()) then
  LoadStaticModule("citrus");
fi;
if (not IsBound(CITRUSC)) and
   (Filename(DirectoriesPackagePrograms("citrus"), "citrus.so") <> fail) then
  LoadDynamicModule(Filename(DirectoriesPackagePrograms("citrus"), 
    "citrus.so"));
fi;

ReadPackage("citrus/gap/utils.gd");
ReadPackage("citrus/gap/orbits.gd");
ReadPackage("citrus/gap/greens.gd");
ReadPackage("citrus/gap/transform.gd");
ReadPackage("citrus/gap/pperm.gd");
ReadPackage("citrus/gap/properties.gd");
ReadPackage("citrus/gap/inverse.gd");
ReadPackage("citrus/gap/semigroups.gd");
ReadPackage("citrus/gap/examples.gd");
ReadPackage("citrus/gap/options.g");

DeclareInfoClass("InfoCitrus");;

if not IsBound(CITRUSC) then 
  Info(InfoWarning, 1, "Citrus: the Citrus package binary is not available,",
  " and so the");
  Info(InfoWarning, 1, "Citrus: functions in Citrus for partial permutations",
  " are not available.");
fi;

BindGlobal("Citrus_C", IsBound(CITRUSC));
