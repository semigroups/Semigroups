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

ReadPackage("semigroups/gap/utils.gd");
ReadPackage("semigroups/gap/options.g");

ReadPackage("semigroups/gap/semigroups-acting/setup.gd");
ReadPackage("semigroups/gap/semigroups-acting/greens.gd");
ReadPackage("semigroups/gap/semigroups-acting/lambda-rho.gd");
ReadPackage("semigroups/gap/semigroups-acting/orbits.gd");
ReadPackage("semigroups/gap/semigroups-acting/acting.gd");
ReadPackage("semigroups/gap/semigroups-acting/graded.gd");
ReadPackage("semigroups/gap/semigroups-acting/semigroups.gd");
ReadPackage("semigroups/gap/semigroups-acting/enums.gd");
ReadPackage("semigroups/gap/semigroups-acting/iterators.gd");
ReadPackage("semigroups/gap/semigroups-acting/properties.gd");
ReadPackage("semigroups/gap/semigroups-acting/attributes.gd");
ReadPackage("semigroups/gap/semigroups-acting/factor.gd");
ReadPackage("semigroups/gap/semigroups-acting/examples.gd");

ReadPackage("semigroups/gap/bipartitions/bipartition.gd");
ReadPackage("semigroups/gap/bipartitions/semibipart.gd");
ReadPackage("semigroups/gap/bipartitions/blocks.gd");

ReadPackage("semigroups/gap/ideals-acting/ideals.gd");

ReadPackage("semigroups/gap/inverse-semigroups-acting/attributes-inverse.gd");

ReadPackage("semigroups/gap/inverse-semigroups-general/freeinverse.gd");

ReadPackage("semigroups/gap/semigroups-general/display.gd");

DeclareInfoClass("InfoSemigroups");;
