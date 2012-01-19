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

ReadPkg("citrus/gap/utils.gd");
ReadPkg("citrus/gap/orbits.gd");
ReadPkg("citrus/gap/greens.gd");
ReadPkg("citrus/gap/properties.gd");
ReadPkg("citrus/gap/transform.gd");
ReadPkg("citrus/gap/semigroups.gd");
ReadPkg("citrus/gap/options.g");

DeclareInfoClass("InfoCitrus");;
