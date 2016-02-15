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

ReadPackage("semigroups/gap/grpperm.gd");

ReadPackage("semigroups/gap/utils.gd");
ReadPackage("semigroups/gap/io.gd");
ReadPackage("semigroups/gap/options.g");

ReadPackage("semigroups/gap/hash.gd");
ReadPackage("semigroups/gap/matrix.gd");
ReadPackage("semigroups/gap/semimat.gd");
ReadPackage("semigroups/gap/grpsmat.gd");

ReadPackage("semigroups/gap/setup.gd");
ReadPackage("semigroups/gap/acting.gd");
ReadPackage("semigroups/gap/ideals-acting.gd");
ReadPackage("semigroups/gap/semigroups.gd");
ReadPackage("semigroups/gap/semigroups-acting.gd");

ReadPackage("semigroups/gap/bipartition.gd");
ReadPackage("semigroups/gap/semibipart.gd");
ReadPackage("semigroups/gap/semitrans.gd");
ReadPackage("semigroups/gap/semipperm.gd");
ReadPackage("semigroups/gap/blocks.gd");

ReadPackage("semigroups/gap/greens-generic.gd");
ReadPackage("semigroups/gap/greens-acting.gd");
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

ReadPackage("semigroups/gap/quotients.gd");
ReadPackage("semigroups/gap/pairs-cong.gd");
ReadPackage("semigroups/gap/reesmat-cong.gd");
ReadPackage("semigroups/gap/univ-cong.gd");
ReadPackage("semigroups/gap/inverse-cong.gd");
ReadPackage("semigroups/gap/simple-cong.gd");
ReadPackage("semigroups/gap/rees-cong.gd");

ReadPackage("semigroups/gap/freeband.gd");

DeclareInfoClass("InfoMatrixSemigroups");
DeclareInfoClass("InfoSemigroups");
