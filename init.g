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

ReadPackage("semigroups/gap/setup.gd");
ReadPackage("semigroups/gap/acting.gd");
ReadPackage("semigroups/gap/ideals-acting.gd");
ReadPackage("semigroups/gap/semigroups.gd");

ReadPackage("semigroups/gap/bipartition.gd");
ReadPackage("semigroups/gap/semibipart.gd");
ReadPackage("semigroups/gap/semitrans.gd");
ReadPackage("semigroups/gap/semipperm.gd");
ReadPackage("semigroups/gap/blocks.gd");

ReadPackage("semigroups/gap/greens.gd");
ReadPackage("semigroups/gap/lambda-rho.gd");
ReadPackage("semigroups/gap/ideals-lambda-rho.gd");
ReadPackage("semigroups/gap/orbits.gd");
ReadPackage("semigroups/gap/graded.gd");
ReadPackage("semigroups/gap/enums.gd");
ReadPackage("semigroups/gap/iterators.gd");
ReadPackage("semigroups/gap/properties.gd");
ReadPackage("semigroups/gap/attributes.gd");
ReadPackage("semigroups/gap/factor.gd");
ReadPackage("semigroups/gap/examples.gd");

ReadPackage("semigroups/gap/ideals.gd");

ReadPackage("semigroups/gap/attributes-inverse.gd");

ReadPackage("semigroups/gap/freeinverse.gd");

ReadPackage("semigroups/gap/display.gd");

ReadPackage("semigroups/gap/isomorph.gd");
ReadPackage("semigroups/gap/reesmat.gd");
ReadPackage("semigroups/gap/reesmat-iso.gd");
ReadPackage("semigroups/gap/maximal.gd");
ReadPackage("semigroups/gap/normalizer.gd");

DeclareInfoClass("InfoSemigroups");;

ReadPackage("semigroups/gap/reesmat-cong.gd");
ReadPackage("semigroups/gap/univcong.gd");
