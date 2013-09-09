#############################################################################
##
#W  init.g
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

if not IsBound(ORBC) then 
  BindGlobal("HTAdd_TreeHash_C", fail);
  BindGlobal("HTValue_TreeHash_C", fail);
fi;

ReadPackage("semigroups/gap/acting-semigroups/setup.gd");
ReadPackage("semigroups/gap/acting-semigroups/greens.gd");
ReadPackage("semigroups/gap/acting-semigroups/lambda-rho.gd");
ReadPackage("semigroups/gap/acting-semigroups/acting.gd");
ReadPackage("semigroups/gap/acting-semigroups/graded.gd");
ReadPackage("semigroups/gap/acting-semigroups/semigroups.gd");
ReadPackage("semigroups/gap/acting-semigroups/enums.gd");
ReadPackage("semigroups/gap/acting-semigroups/iterators.gd");
ReadPackage("semigroups/gap/acting-semigroups/regular.gd");
ReadPackage("semigroups/gap/acting-semigroups/properties.gd");
ReadPackage("semigroups/gap/acting-semigroups/attributes.gd");
ReadPackage("semigroups/gap/acting-semigroups/attributes-inverse.gd");
ReadPackage("semigroups/gap/acting-semigroups/pictures.gd");
ReadPackage("semigroups/gap/acting-semigroups/slp.gd");
ReadPackage("semigroups/gap/acting-semigroups/examples.gd");

ReadPackage("semigroups/gap/acting-ideals/ideals.gd");

ReadPackage("semigroups/gap/bipartitions/bipartition.gd");
ReadPackage("semigroups/gap/bipartitions/semibipart.gd");
ReadPackage("semigroups/gap/bipartitions/blocks.gd");

ReadPackage("semigroups/gap/general/orbits.gd");
ReadPackage("semigroups/gap/general/utils.gd");
ReadPackage("semigroups/gap/general/options.g");

ReadPackage("semigroups/gap/inverse-semigroups/freeinverse.gd");

DeclareInfoClass("InfoSemigroups");;
